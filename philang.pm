=head1 phi language elements
This is where we start to define phi as a language, not just an execution model.
In particular, we need a syntax and things like an editor frontend. This module
gets us there albeit in a circuitous way. (See C<dev/frontend.md> for details.)

The first thing we need is the notion of an abstract value. This is similar to
an AST node but is more aware of its runtime role; in particular, abstract
values constant-fold at parse time and then locally alter the grammar of the
language by specifying their own suffix parsers. Local variables are
dereferenced at parse time.
=cut

package philang;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philocal;
use phiparse;
use phiobj;

our @EXPORT =
our @EXPORT_OK = qw//;


=head2 Abstracts, the interpreter, and parse states
The compilation cycle involves a few steps:

  text -> parsed <-> abstracts -> concatenative     # parsing
                  -> abstracts -> concatenative     # optimization
                  -> parsers -> code                # compiler backends

Implementation:

1. Atom parser (derived from the parse state)
2. Expr parser (atom >>= op nil)
3. ???
4. PROFIT


=head3 Integer parsing
The usual radix conversion:

  cs list-int = 0 cs list-int'
  n cs list-int' = match cs with
    []    -> n
    c:cs' -> (n*10 + (c-48)) cs' list-int'

=cut

use constant list_int1_mut => pmut;
use constant list_int1 => l
  dup, nilp,
    l(drop),
    l(i_uncons,                         # n cs' c
      lit 48, i_neg, i_plus, rot3l,     # cs' c-48 n
      lit 10, i_times, i_plus, swap,    # (n*10+(c-48)) cs'
      list_int1_mut, i_eval),
    if_;

list_int1_mut->set(list_int1);

use constant list_int => l lit 0, swap, list_int1, i_eval;


use constant int_type => mktype
  bind(val                => isget 0),
  bind(parse_continuation => drop, phiparse::none);

use constant int_literal => l
  l(list_int, i_eval, pnil, swons, int_type, swons),
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Base syntax definitions
Stuff like whitespace, comments, etc. It's worth having these ready.
=cut

use constant line_comment => l
  l(l(pstr "#", phiparse::str, i_eval),
    l(l(pstr "\n", lit 0, phiparse::oneof, i_eval), phiparse::rep, i_eval),
    l(pstr "\n", phiparse::str, i_eval)),
  phiparse::seq, i_eval;

use constant any_whitespace => l
  pstr " \n\r\t", lit 1, phiparse::oneof, i_eval;

use constant ignore => l
  l(drop, pnil),
  l(l(l(line_comment, any_whitespace), phiparse::alt, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Parse state
The parse state contains the obligatory C<str index> pair, then an object that
contains information about lexical scopes. Internally it's a link in the scope
chain. Its object state looks like this:

  [
    parent-scope|nil                    # link to parent lexical scope
    [locals...]                         # binding table
    [captured...]                       # list of capture expressions
    [ignore...]                         # whitespace/comment parsers
  ]

The basic grammar is, where C<//> is non-backtracking alternation:

  atom     ::= local // capture
  'op expr ::= ignore* atom >>= ['op parse-continuation]
  local    ::= alt(locals)
  captured ::= parent->locals // parent->captured
  ignore   ::= alt(ignore...) | parent->ignore

Here's the full set of methods we support:

  scope parent          = scope'|nil
  scope locals          = [locals...]
  scope captured        = [captured...]
  scope ignore          = [ignore...]

  x [f] scope if_parent = match parent with
                            | [] -> x
                            | s' -> x s' f

  scope parser_ignore   = let i  = scope ignore in
                          let pi = parent parser_ignore in
                          [[[[i alt .] pi] alt .] rep .]

  scope parser_atom     = [[[locals...] parser_capture] alt .]

C<parser_capture> is unusual because it mutates the scope, which of course
involves returning a new one. Specifically, the parse state we get after parsing
a captured variable will reflect that capture; so we have a post-parsing filter
that modifies the state accordingly. This modification is encapsulated into a
call to C<'name capture>, which means that the capture...

TODO: how do we negotiate around inherited literals and capturing? Literals
don't get captured (I think), but given lexical scoping there's no reason they
wouldn't have a scoped dependency.

Actually, this deserves some discussion. Philosophically there's no reason
lexical capture needs to be tied to names; the only reason it is in practice is
that names are the sole mechanism for nonlocal reference. It wouldn't be
difficult to imagine some non-named way to refer to external values, however;
for instance, if Perl had some notation like C<@^_> to refer to the enclosing
C<@_>, this could be an unnamed lexical capture.

The other thing is that there's also no reason to implement capture at the parse
level; we could have a situation where abstract optimizations eliminate a
capture. For example, C<[x].tail> doesn't capture C<x> even though it appears
to. So maybe we cons up the graph first, then detect capture from there.

...actually, that's silly: let's just capture up front like we're doing now and
let the abstract evaluator sort out the fictitious references. We can reasonably
assume or assert that any captured value will have a name; literals and similar
won't carry any runtime state.

  scope parser_capture = parent parser_atom >>= pulldown

=head3 How C<pulldown> works
Let's back out for a minute and talk about the interfacing that got us to
C<parser_capture>. We start with a global C<capture> parser, which fetches the
scope as the third element in the parse state; then it invokes
C<parser_capture>. That parser is then run on the same parse state, returning a
new parse state and a reference to the captured value. The new parse state
contains a modified scope that reflects the pulldown.

TODO: can we support literals? I suspect we can.

=cut


use constant scope_chain_type => mktype
  bind(parent   => isget 0),
  bind(locals   => isget 1),
  bind(captured => isget 2),
  bind(ignore   => isget 3),

  bind(child =>                         # scope
    dup, tail, swap,                    # scope.type, scope
    l(pnil, pnil, pnil), swons,         # scope.type [scope [] [] []]
    i_cons),                            # [[scope [] [] []] scope.type...]

  bind(if_parent =>                     # x [f] scope
    mcall"parent", dup, nilp,           # x [f] s'? 1|0
      l(drop, drop),                    # x
      l(swap, i_eval),                  # x s' f
    if_),

  bind(parser_ignore =>                 # scope
    dup, mcall"ignore",                 # scope i
      l(phiparse::alt, i_eval), swons,  # scope [i a.]
    swap,                               # [i a.] scope
    l(mcall"parser_ignore",             # [i a.] pi
      pnil, swons,                      # [i a.] [pi]
      swons,                            # [[i a.] pi]
      l(phiparse::alt, i_eval), swons), # [[[i a.] pi] a.]
    swap, mcall"if_parent",
    l(phiparse::rep, i_eval), swons),   # [[[[i a.] pi] a.] r.]

  bind(parser_locals =>
    mcall"locals",
    l(phiparse::alt, i_eval), swons),

  bind(parser_atom =>
    dup, mcall"parser_capture",         # scope capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    l(phiparse::alt, i_eval), swons),   # [[locals capture] a.]

  bind(parser_capture =>
    phiparse::fail, swap,               # fail scope
    l(mcall"parser_atom", swap, drop),  # fail scope [...]
    swap, mcall"if_parent");            # fail ...?
