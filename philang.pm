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
use phieval;                            # read this if you haven't yet

our @EXPORT =
our @EXPORT_OK = qw/ local_for local_ /;


=head2 Parse state
Just like the normal string parse state, but includes a slot for scope.
=cut

use phitype scoped_state_type =>
  bind(is_error => drop, lit 0),

  bind(value       => isget 0),
  bind(offset      => isget 1),
  bind(string      => isget 2),
  bind(scope       => isget 3),
  bind(with_value  => isset 0),
  bind(with_offset => isset 1),
  bind(with_scope  => isset 3),

  bind(length => mcall"string", i_slen),
  bind(at     => mcall"string", i_sget),

  bind(consume =>                       # n self
    dup, mcall"offset",                 # n self offset
    rot3l, i_plus, swap,                # n+offset self
    mcall"with_offset"),

  bind(enter_child_scope =>             # self -> self'
    dup, mcall"scope", mcall"child",    # self scope'
    swap, mcall"with_scope"),

  bind(exit_child_scope =>              # self -> childscope self'
    dup, mcall"scope",                  # self child
    dup, mcall"parent",                 # self child parent
    rot3l, mcall"with_scope"),          # child self'

  bind(bind_local =>                    # parser value self
    dup, mcall"scope",                  # p v self scope
    stack(4, 0, 2, 3, 1),               # self p v scope
    mcall"bind_local",                  # self scope'
    swap, mcall"with_scope");           # self'


=head2 Individual parser delegates
These hand control over to the currently-active scope chain, retrieved from the
parse state (see below for details). It's quite important to have these parsers
because they make it possible for things like C<expr> to introduce recursion
into the grammar without using any forward references.

  atom.parse(state) = state.scope.parser_atom().parse(state)

=cut

use phitype atom_parser_type =>
  bind(parse =>                         # state self
    drop, dup, mcall"scope",            # state scope
    mcall"parser_atom", mcall"parse");  # state'

use phi atom => pcons pnil, atom_parser_type;


=head3 Expression parsing, in general
We don't need a particular scope to apply the parse-continuation stuff, and
there are cases where we want to transform an atom parser into an expression
parser without having a scope in mind. This function encapsulates that logic.

NB: C<parse_continuation> takes the receiver as a separate argument because we
have proxy objects that will replace the receiver but still need to end up in
the resulting graph.
=cut

use phi continuation_combiner => l      # v c
  swap, drop;                           # c

use phi expr_parser_for => l            # parser op
  continuation_combiner,                # p op c
  swap, quote, i_eval,                  # p c 'op
  l(i_eval, stack(2, 1, 1, 0),          # op v v
    mcall"parse_continuation"),         # p c 'op uf
  swons,                                # p c f
  pnil, swons, swons, swons,            # [p c f]
  phiparse::flatmap_type, swons;        # flatmap

use phi expr => l                       # op
  atom, swap,                           # atom op
  expr_parser_for, i_eval;


=head2 Scopes
The C<scope> field of the parse state is a link in the scope chain. Its object
state looks like this:

  [
    parent-scope|nil                    # link to parent lexical scope
    [locals...]                         # binding table
    capture_list                        # capture list object
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

  scope parser_atom     = [[[locals...] parser_capture] alt .]

  scope parser_capture  = parent ? parent parser_atom >>= pulldown : fail

  val scope capture     = captured-val scope'
  val scope pulldown    = captured-val scope'


=head3 How C<pulldown> works
Anytime the parent parses something for us, we need to make sure that value gets
forwarded into the child scope via value capture. Mechanically speaking,
forwarding C<x> from a parent to a child looks like this:

  child.captured = abstract_cons(x, child.captured)
  child.capture_list_length += 1
  return abstract_nthlast(child.captured, child.capture_list_length - 1)
    within the child

This cascades down the scope chain as far as is required.

So ... what happens for constant expressions; do we forward those too? Yep. The
abstract evaluation layer will be able to constant-fold those so we don't
allocate memory for them.

C<pulldown> mutates the scope chain, which involves returning a new one. This
turns out to be simple: the parse state supports C<with_scope>, so
C<parser_capture> can replace C<scope> when it returns its continuation state
(assuming it succeeds).

Ok, now we have a parser that recognizes a captured variable. In functional
terms:

  state parser -> state' {val = ...}

Cool, but we need to do some stuff with C<state'>. First, its position is
obviously advanced by whatever space the captured atom took up; but the scope
aspect of it is much more subtle. The obvious behavior, of course, is that the
capture-parser waits for the result, adds the captured value to the capture
list, and updates the scope within the parse state. And this will fail horribly
unless we're very careful.

Let's walk through a two-level capture example.

  x -> y -> z -> x
                |-| <- we're parsing this

We have one parse state with three scopes:

  state  -> [value=arg ... scope=zscope]
  xscope -> [parent=nil    locals=[x->xv] captured=[]]
  yscope -> [parent=xscope locals=[y->yv] captured=[]]
  zscope -> [parent=yscope locals=[z->zv] captured=[]]

The end state after parsing C<x> is this:

  state'  -> [value=c1 ... scope=zscope']
  yscope' -> [parent=xscope  locals=[y->yv] captured=[c0->xv]]
  zscope' -> [parent=yscope' locals=[z->zv] captured=[c1->c0]]

From the parser perspective, here's what happens (where the parse state is on
the right):

  x -> zscope.atom                                            state[zscope]
      -> zscope.local // zscope.capture                       state[zscope]
        -> ----FAIL---- // zscope.capture = yscope.atom       state[zscope]
          -> yscope.atom                                      state[YSCOPE]
            -> yscope.local // yscope.capture                 state[yscope]
              -> ----FAIL---- // yscope.capture = xscope.atom state[yscope]
                -> xscope.atom                                state[XSCOPE]
                  -> xscope.local -> SUCCEED = xv             state'[xscope]
                -> xv                                         state'[xscope]
              -> let c0, ys' = ys.pulldown(xv) in c0          state'[yscope']
            -> c0                                             state'[yscope']
          -> c0                                               state'[yscope']
        -> let c1, zs' = zs.pulldown(c0) in c1                state'[zscope']
      -> c1                                                   state'[zscope']
    -> c1                                                     state'[zscope']

The idea here is fairly simple: when we delegate into the parent parser via a
C<capture> element, we need to stash the current parse state and create a new
one centered around the parent scope. So C<pulldown> converts the parent's
regular C<atom> parser into one that does the pulldown stuff if it succeeds.
Specifically:

  pulldown(state, p) =
    match p.parse(state.with_scope(state.scope.parent)) with
      | error -> error
      | state' ->
          let v', capture' = state.scope.capture.add(state'.value) in
          state'.with_value(v')
                .with_scope(state'.scope.with_capture(capture')
                                        .with_parent(state'.scope))

Before I can write this, though, we need to talk about how capture lists work.

=head3 The capture list problem
C<phiabstract> defines a single C<capture> abstract that gets passed into a
function to set up all captured references. That one entry point provides access
to all of the things we capture into a scope. Nicely simple, so what's wrong?

Well, let's suppose we're parsing a function like this:

  x -> 2 + 1

C<2> is captured, so we create a capture list entry and return it. We ask that
captured value (which is aware that it's a capture list reference) for its parse
continuation, then continue and do the same thing for C<1>.

Now ... when we parsed C<2>, we didn't know that we'd subsequently parse C<1>
and capture it. So we don't have a capture list of known size. And that's the
problem: we need to commit to enough of a C<capture> value to make C<2> work,
but then leave room to update it for C<1> _without changing the now-immutable
C<2>_.

This turns out to be slightly nontrivial. We have a few options:

1. C<capture> is a regular list, but we refer to nth-from-last indexes
2. C<capture> is a list that ends in a C<mut>, so we can extend it
3. C<capture> stores the current length, from which we subtract

(1) is a bit clunky but ultimately workable. I don't love it simply because it
involves walking the list in a way that may be difficult to optimize.

(2) is asking for trouble unless we're very careful.

(3) produces code that's easy to optimize and is compact. The idea is to have
C<capture> prepend its list-length as an abstract const int, then subtract that.

=head4 But wait, there's more
Each of the above options has yet another problem. What happens when we want to
fold a value out of the capture list because it's free (e.g. a constant)? If
we've encoded numerical offsets into the lists, we can't optimize intermediate
cons cells out of the picture -- at least, not in some easy automatic way.

I'm going to kick this problem down the road for now. The existing capture model
will work, albeit slowly. We'll be in a much better position to solve this
problem with a more advanced abstract value model that is aware of value
mobility, and that's something I'd rather implement with infix syntax.

=head4 So for now...
Let's go with option (3) because it's fast and not-insane. We can simplify this
with an object that manages the capture list and generates abstracts that refer
into it.

One final thing. For parsing purposes, we won't have a live C<capture> value
available -- so we need the C<capture_nth> node to store the value it captures
so it can provide a parse continuation.
=cut

use phi capture_list_nth => l           # [rnth capture_list]
  unswons, head,                        # rnth capture_list
  i_uncons,                             # rnth cvals length
  rot3l, i_neg, i_plus,                 # cvals nth
  lget, i_eval;                         # cvals[nth]


use phitype capture_list_type =>
  bind(xs          => isget 0),
  bind(length      => isget 1),
  bind(with_xs     => isset 0),
  bind(with_length => isset 1),

  # TODO: "abstract" and "op" within this function appear to be getting reversed
  # at some point. I'm not sure where; this function looks correct to me. It's
  # possible something is up with op reconstruction after eval().
  bind(add =>                           # abstract self
    dup, mcall"length",                 # abstract self len
    rot3r, dup, mcall"xs",              # len abstract self xs
    stack(2, 0, 2, 1),                  # len abstract self abstract xs
    pnil, swons, swons,                 # len abstract self [abstract xs]
    phiabstract::op_cons, i_eval,       # len abstract self xs'
    swap, mcall"with_xs",               # len abstract self'
    stack(0, 2),                        # len abstract self' len
    phieval::native_const, i_eval,      # len abstract self' la
    phieval::capture,                   # len abstract self' la capture
    pnil, swons, swons,                 # len abstract self' [la capture]
    op_capture_list_nth, i_eval,        # len abstract self' op
    rot3l, swap,                        # len self' abstract op
    pnil, swons, swons,                 # len self' [abstract op]
    capture_list_nth_abstract_type, swons,  # len self' abstract'
    rot3l, lit 1, i_plus,               # self' abstract' len+1
    rot3l, mcall"with_length"),         # abstract' self''

  bind(capture_list =>                  # self
    dup, mcall"xs", swap, mcall"length",# xs length
    phieval::native_const, i_eval, swap,# length_abstract xs
    pnil, swons, swons,                 # [l xs]
    phiabstract::op_cons, i_eval);      # abstract(length_abstract::xs)


use phi nil_abstract       => le pnil, phieval::native_const, i_eval;
use phi empty_capture_list => pcons l(nil_abstract, 0), capture_list_type;


=head3 A quick aside: we can only capture atoms
This is an important forcing element of this design: things like paren-groups
can't be inherited via capture. Once we ascend, we're done; so we need to
isolate ascent to values, not even things like parse continuations.

In practice this means our root-scope "atom" parser can't accept C<(expr)> as
one of its cases, which is the usual way you'd define a grammar. If we did that,
we'd capture the entire C<()> group and would be locked into the parent scope
within those parens. The child scope must itself be aware of the fact that C<()>
groups are atoms -- and that means we need something that behaves similarly to
capturing, but that instead works by inheritance.

Luckily C<(> and C<)> are values, not grammar rules, so no extra machinery is
required.
=cut

use phitype pulldown_parser_type =>
  bind(parser => isget 0),
  bind(parse =>                         # state self
    mcall"parser",                      # state p
    stack(0, 1, 1),                     # state p state state
    mcall"scope", dup, mcall"parent",   # state p state sc sc.parent
    swap, mcall"with_parent",           # state p state sc'
    swap, mcall"with_scope",            # state p statep
    swap, mcall"parse",                 # state state'
    dup, mcall"is_error",

    # If error, return directly
    l(                                  # state state'
      stack(2, 0)),                     # state'

    # Otherwise, do the capture list stuff
    l(                                  # state state'
      stack(0, 1), mcall"scope",        # state state' sc
      dup, mcall"capture",              # state state' sc capture
      stack(0, 2), mcall"value",        # state state' sc capture value
      swap, mcall"add",                 # state state' sc v' capture'
      stack(0, 3), mcall"scope",        # state state' sc v' capture' sc'
      mcall"with_capture",              # state state' sc v' sc''
      stack(1, 0, 2), mcall"with_parent", # state state' sc v' sc'''
      stack(0, 3), mcall"with_scope",   # state _ sc v' state''
      mcall"with_value",                # state _ sc state'''
      stack(4, 0)),                     # state'''

    if_);


=head3 Binding locals
This is a bit of a pain to do normally, so let's automate it a little.
=cut

use phi local_for => l                  # p v
  l(stack(2, 0)), swons,                # p f=(_ v -> v)
  pnil, swons, swons,                   # [p f]
  phiparse::map_type, swons;            # [[p f] map...]

sub local_($$) { le @_, local_for, i_eval }


=head2 The scope chain
Now we're ready to tie all of this stuff together.
=cut

use phitype scope_type =>
  bind(parent  => isget 0),
  bind(locals  => isget 1),
  bind(capture => isget 2),
  bind(dialect => isget 3),

  bind(with_parent  => isset 0),
  bind(with_locals  => isset 1),
  bind(with_capture => isset 2),
  bind(with_dialect => isset 3),

  bind(bind_local =>                    # parser value self
    rot3r, local_for, i_eval,           # self p
    swap, dup, mcall"locals",           # p self locals
    rot3l, i_cons, swap,                # locals' self
    mcall"with_locals"),

  bind(child =>                         # scope
    dup, tail, swap,                    # scope.type scope
    l(pnil, empty_capture_list), swons, # scope.type [scope [] ecl]
    i_cons),                            # [[scope [] ecl] scope.type...]

  bind(parser_locals =>
    mcall"locals", pnil, swons,         # [[locals...]]
    phiparse::alt_type, swons),         # alt([locals...])

  bind(parser_atom =>
    dup, mcall"parser_capture",         # scope capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    pnil, swons,                        # [[locals capture]]
    phiparse::alt_type, swons),         # alt([locals capture])

  bind(parser_capture =>
    dup, mcall"parent", dup, nilp,      # self parent parent-nil?
    l(drop, drop, phiparse::fail),      # fail
    l(mcall"parser_atom", pnil, swons,  # self [p]
      pulldown_parser_type, swons,      # self pulldown-parser
      stack(2, 0)),                     # p
    if_);


1;
