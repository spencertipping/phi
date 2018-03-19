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
use philist;
use phiobj;
use phiparse;
use phiabstract;                        # read this if you haven't yet

our @EXPORT =
our @EXPORT_OK = qw/ local_for local_ /;


=head2 Parse state
Just like the normal string parse state, but includes a slot for scope.
=cut

use phitype scoped_state_type =>
  bind(is_error => drop, lit 0),

  bind(value       => isget 0),
  bind(offset      => isget 1),
  bind(length      => isget 2),
  bind(string      => isget 3),
  bind(scope       => isget 4),
  bind(with_value  => isset 0),
  bind(with_offset => isset 1),
  bind(with_scope  => isset 4),

  bind(at => mcall"string", swap, i_sget),

  bind(consume =>                       # n self
    dup, mcall"offset",                 # n self offset
    rot3l, i_plus, swap,                # n+offset self
    mcall"with_offset");


=head2 Individual parser delegates
These hand control over to the currently-active scope chain, retrieved from the
parse state (see below for details). It's quite important to have these parsers
because they make it possible for things like C<expr> to introduce recursion
into the grammar without using any forward references.

  state atom = state.scope.parser_atom().parse(state)

=cut

use phi atom => l                       # state
  dup, mcall"scope", mcall"parser_atom",# state a
  mcall"parse";


=head3 C<expr>
This one is a little different because it's a closure over the operator.
Specifically:

  state (op expr) . = state.scope.parser_expr(op).parse(state)

=cut

use phi expr => l                       # op
  quote, i_eval,                        # 'op
  l(                                    # state 'op
    i_eval,                             # state op
    swap, dup, mcall"scope",            # op state scope
    rot3l, swap,                        # state op scope
    mcall"parser_expr",                 # state parser
    mcall"parse"),
  swons;


=head2 Scopes
The C<scope> field of the parse state is a link in the scope chain. Its object
state looks like this:

  [
    parent-scope|nil                    # link to parent lexical scope
    [locals...]                         # binding table
    [captured...]                       # list of capture expressions
  ]

The basic grammar is, where C<//> is non-backtracking alternation:

  atom     ::= local // capture
  'op expr ::= atom >>= ['op parse-continuation]
  local    ::= alt(locals)
  captured ::= parent->locals // parent->captured

Here's the full set of methods we support:

  scope parent   = scope'|nil
  scope locals   = [locals...]
  scope captured = [captured...]

  x [f] scope if_parent = match parent with
                            | [] -> x
                            | s' -> x s' f

  scope parser_atom     = [[[locals...] parser_capture] alt .]

  scope parser_capture  = parent ? parent parser_atom >>= pulldown : fail

  val scope capture     = captured-val scope'
  val scope pulldown    = captured-val scope'

  op scope parser_expr  =
    let v f = 'op v v.parse_continuation() in
    [scope.parser_atom (v c -> c) f flatmap .]

NB: C<parse_continuation> takes the receiver as a separate argument because we
have proxy objects that will replace C<self> but still need to end up in the
resulting graph.


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

...anyway, let's get back to C<pulldownify>. The idea here is fairly simple:
when we delegate into the parent parser via a C<capture> element, we need to
stash the current parse state and create a new one centered around the parent
scope. (We don't just stash the current state to preserve the child scope; we
also need the pre-parsed string offset.) So C<pulldownify> converts the parent's
regular C<atom> parser into one that does the pulldown stuff if it succeeds.
Specifically:

  pulldownify(p).parse(state) =
    let state'   = p.parse(state.with_scope(state.scope.parent)) in
    let capture' = state.value :: state.scope.capture in
    let v'       = capture_abstract(state.scope.capture.length) in
    state'.with_value(v')
          .with_scope(state'.scope.with_capture(capture')
                                  .with_parent(state'.scope))

We can treat C<pulldownify> as a closure generator over C<p>, so all of the work
actually happens in C<pulldown>, whose signature is:

  state p pulldown = state'

Just a higher-order parser. Right then -- let's get to it.
=cut


# NB: the state must provide a parent in order for pulldown to work. This should
# be knowable when you build the parser.
use phi pulldown => l                   # state p
  swap, dup,                            # p state state
  mcall"scope", mcall"parent",          # p state sc.parent
  stack(0, 1),                          # p state sc.parent state
  mcall"with_scope",                    # p state state.ws(...)
  rot3l, mcall"parse",                  # state state'
  dup, mcall"is_error",
    l(stack(2, 0)),                     # state'
    l(rot3l, dup, tail, tail, head,     # v state' state sc
      mcall"capture", dup,              # v state' state sc.capture sc.capture
      list_length, i_eval,              # v state' state sc.capture len(sc.c)
      swap, stack(0, 4), i_cons,        # v state' state len(sc.c) capture'
      dup, rot3r,                       # v state' state capture' len capture'

      # FIXME
      capture_abstract, i_eval,         # v state' state capture' v'

      rot3r, swap,                      # v state' v' capture' state
      tail, tail, head,                 # v state' v' capture' sc
      mcall"with_capture",              # v state' v' sc'
      rot3l, dup, tail, tail, head,     # v v' sc' state' parent'
      rot3l, mcall"with_parent",        # v v' state' sc''
      lit 2, lset, i_eval,              # v v' [s' n' sc'']
      rot3l, drop),                     # v' [s' n' sc'']
  if_;


=head3 Expression parsing, in general
We don't need a particular scope to apply the parse-continuation stuff, and
there are cases where we want to transform an atom parser into an expression
parser without having a scope in mind. This function encapsulates that logic.
=cut

use phi continuation_combiner => l      # v c
  swap, drop;                           # c

use phi expr_parser_for => l            # value-parser op
  continuation_combiner,                # vp op c
  swap, quote, i_eval,                  # vp c 'op
  l(                                    # v 'parse 'op
    swap, drop,                         # v 'op
    i_eval, stack(2, 1, 1, 0),          # op v v
    mcall"parse_continuation"),
  swons,                                # vp c [op swap dup .parse_k]
  pnil, swons, swons, swons,            # [vp c [op...]]
  phiparse::flatmap_type, swons;        # parser


=head3 Binding locals
This is a bit of a pain to do normally, so let's automate it a little.
=cut

use phi local_for => l                  # p v
  l(swap, drop, swap, drop), swons,     # p [v swap drop]
  pnil, swons, swons,                   # [p f]
  phiparse::map_type, swons;            # [[p f] map...]

sub local_($$) { le @_, local_for, i_eval }


use phitype scope_chain_type =>
  bind(parent  => isget 0),
  bind(locals  => isget 1),
  bind(capture => isget 2),
  bind(capture_fn => isget 3),

  bind(with_parent  => isset 0),
  bind(with_locals  => isset 1),
  bind(with_capture => isset 2),

  bind(bind_local =>                    # parser value self
    rot3r, local_for, i_eval,           # self p
    swap, dup, mcall"locals",           # p self locals
    rot3l, i_cons, swap,                # locals' self
    mcall"with_locals"),

  bind(child =>                         # scope
    dup, tail, swap,                    # scope.type, scope
    l(pnil, pnil, pnil), swons,         # scope.type [scope [] [] []]
    i_cons),                            # [[scope [] [] []] scope.type...]

  bind(if_parent =>                     # x [f] scope
    mcall"parent", dup, nilp,           # x [f] s'? 1|0
      l(drop, drop),                    # x
      l(swap, i_eval),                  # x s' f
    if_),

  bind(parser_locals =>
    mcall"locals",
    phiparse::alt, swons),              # [[locals...] a.]

  bind(parser_atom =>
    dup, mcall"parser_capture",         # scope capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    pnil, swons,                        # [[locals capture]]
    phiparse::alt_type, swons),         # alt([locals capture])

  bind(parser_expr =>                   # op scope
    mcall"parser_atom", swap,           # atom-parser op
    expr_parser_for, i_eval),           # expr-parser

  bind(parser_capture =>
    dup, mcall"parent", dup, nilp,      # self parent parent-nil?
      l(drop, drop, phiparse::fail),    # fail
      l(mcall"parser_atom", pulldown,
        swons, swap, drop),             # [parent-atom pulldown.]
    if_);


1;
