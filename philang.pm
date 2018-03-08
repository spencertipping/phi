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

use constant ignore_primitive => l
  l(drop, pnil),
  l(l(l(line_comment, any_whitespace), phiparse::alt, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Individual parser delegates
These hand control over to the currently-active scope chain, retrieved from the
parse state (see below for details). It's quite important to have these parsers
because they make it possible for things like C<expr> to introduce recursion
into the grammar without using any forward references.

  state atom = state state.tail.tail.head parser_atom .

=cut

use constant atom   => l dup, tail, tail, head, mcall"parser_atom",   i_eval;
use constant expr   => l dup, tail, tail, head, mcall"parser_expr",   i_eval;
use constant ignore => l dup, tail, tail, head, mcall"parser_ignore", i_eval;


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

  scope parent   = scope'|nil
  scope locals   = [locals...]
  scope captured = [captured...]
  scope ignore   = [ignore...]

  x [f] scope if_parent = match parent with
                            | [] -> x
                            | s' -> x s' f

  scope parser_ignore   = let i  = scope ignore in
                          let pi = parent parser_ignore in
                          [[[[i alt .] pi] alt .] rep .]

  scope parser_atom     = [[[locals...] parser_capture] alt .]

  scope parser_capture  = parent parser_atom >>= pulldown

  val scope capture     = captured-val scope'
  val scope pulldown    = captured-val scope'

=head3 How C<pulldown> works
Anytime the parent parses something for us, we need to make sure that value gets
forwarded into the child scope via value capture. Mechanically speaking,
forwarding C<x> from a parent to a child looks like this:

  child.captured = x :: child.captured
  return nthlast(child.captured, length(child.captured) - 1) within the child

This cascades down the scope chain as far as is required.

So ... what happens for constant expressions; do we forward those too? Yep. The
abstract evaluation layer will be able to constant-fold those so we don't
allocate memory for them.

C<pulldown> mutates the scope chain, which involves returning a new one. This
turns out to be simple: the parse state contains C<[s n scope]>, so
C<parser_capture> can replace C<scope> when it returns its continuation state
(assuming it succeeds).

=head3 How C<pulldownify> works
Ok, now we have a parser that recognizes a captured variable. In functional
terms:

  state parser -> val state'

Cool, but we need to do some stuff with C<state'>. First, its position is
obviously advanced by whatever space the captured atom took up; but the scope
aspect of it is much more subtle. The obvious behavior, of course, is that the
capture-parser waits for the result, adds the captured value to the capture
list, and updates the scope within the parse state. And this will fail horribly
unless we're very careful.

Let's walk through a two-level capture example.

  x -> y -> z -> x
                |-| <- we're parsing this

We have three scopes:

  xscope -> [parent=nil    locals=[x->xv] captured=[]]
  yscope -> [parent=xscope locals=[y->yv] captured=[]]
  zscope -> [parent=yscope locals=[z->zv] captured=[]]

The end state after parsing C<x> is this:

  yscope' -> [parent=xscope  locals=[x->c0 y->yv] captured=[c0->xv]]
  zscope' -> [parent=yscope' locals=[x->c1 z->zv] captured=[c1->c0]]

From the parser perspective, here's what happens (where the parse state is on
the right):

  x -> zscope.atom                                            [s n  zscope]
      -> zscope.local // zscope.capture                       [s n  zscope]
        -> ----FAIL---- // zscope.capture = yscope.atom       [s n  zscope]
          -> yscope.atom                                      [s n  YSCOPE]
            -> yscope.local // yscope.capture                 [s n  yscope]
              -> ----FAIL---- // yscope.capture = xscope.atom [s n  yscope]
                -> xscope.atom                                [s n  XSCOPE]
                  -> xscope.local -> SUCCEED = xv             [s n' xscope]
                -> xv                                         [s n' xscope]
              -> let c0, ys' = ys.pulldown(n, n', xv) in c0   [s n' yscope']
            -> c0                                             [s n' yscope']
          -> c0                                               [s n' yscope']
        -> let c1, zs' = zs.pulldown(n, n', c0) in c1         [s n' zscope']
      -> c1                                                   [s n' zscope']
    -> c1                                                     [s n' zscope']

Before I get into any more explanation, I want to mention an important forcing
element of this design: Things like paren-groups can't be inherited via capture.
Once we ascend, we're done; so we need to isolate ascent to values, not even
things like parse continuations.

In practice this means our root-scope "atom" parser can't accept C<(expr)> as
one of its cases, which is the usual way you'd define a grammar. If we did that,
we'd capture the entire C<()> group and would be locked into the parent scope
within those parens. The child scope must itself be aware of the fact that C<()>
groups are atoms -- and that means we need something that behaves similarly to
capturing, but that instead works by inheritance.

Luckily C<(> and C<)> are values, not grammar rules, so no extra machinery is
required.
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
    l(phiparse::alt, i_eval), swons),   # [[locals...] a.]

  bind(parser_atom =>
    dup, mcall"parser_capture",         # scope capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    l(phiparse::alt, i_eval), swons),   # [[locals capture] a.]

  bind(parser_capture =>
    dup,                                # self
    phiparse::fail, swap,               # self fail scope
    l(mcall"parser_atom", swap, drop),  # self fail scope [...]
    swap, mcall"if_parent",             # self parser
    swap, mcall"pulldownify"),          # parser'

  bind(pulldownify =>                   # parser self
    drop    # FIXME
    );


1;
