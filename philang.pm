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


=head2 Parse state
The parse state contains the obligatory C<str index> pair, then an object that
contains information about lexical scopes. This object supports a few methods:

1. C<state parse-atom>: parse a single atom
2. C<state "op"|nil parse-expr>: parse an expression with a precedence limit
3. C<state parse-ignore>: advance the parse state beyond whitespace/comments
4. C<child>: push a child scope with no initial bindings
5. C<pop-child>: pop a child scope into an abstract fn
6. C<'name val bind>: bind an abstract value to a name
7. C<'name capture>: capture a name from a parent scope

Internally it's a link in the scope chain. Its object state looks like this:

  [ parent-scope|nil                    # link to parent lexical scope
    [locals...]                         # binding table
    [captured...]                       # list of capture expressions
    [ignore...] ]                       # whitespace/comment parsers

The basic grammar is, where C<//> is non-backtracking alternation:

  atom     ::= local // capture
  'op expr ::= ignore* atom >>= ['op parse-continuation]
  local    ::= alt(locals)
  captured ::= parent->locals // parent->captured
  ignore   ::= alt(ignore...) | parent->ignore

Here's the full set of methods we support:

  scope parent            = scope'|nil
  scope locals            = [locals...]
  scope captured          = [captured...]
  scope ignore            = [ignore...]

  x [f] scope if_parent   = match parent with
                              | [] -> x
                              | s' -> x s' f

  scope parser_ignore     = [[[[(scope ignore) alt .]
                               (parent parser_ignore)] alt .] rep .]

  scope parser_capture    = [[(parent parser_atom) (parent parser_capture)] alt .]
  scope parser_atom       = [[[locals...] parser_capture] alt .]

=cut

use constant scope_chain_type => mktype
  bind(parent   => isget 0),
  bind(locals   => isget 1),
  bind(captured => isget 2),
  bind(ignore   => isget 3),

  bind(if_parent => mcall"parent", dup, nilp, l(drop), l(swap, i_eval), if_),

  bind(parser_ignore => dup, mcall"ignore", l(phiparse::alt, i_eval), swons,
                        swap, l(mcall"parser_ignore", pnil, swons, swons,
                                l(phiparse::alt, i_eval), swons),
                        swap, mcall"if_parent",
                        l(phiparse::rep, i_eval), swons),

  bind(parser_atom => dup, # TODO
                      );
