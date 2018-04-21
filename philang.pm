=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
use phioptree;
use phifuzz;

our @EXPORT =
our @EXPORT_OK = qw/ local_for local_ /;


=head2 Dialects
A dialect is the translation layer between C<phioptree> nodes and syntax. This
makes it possible to address the same value using different syntaxes, which is
used to simulate other languages without implementing tons of proxy objects.

More concretely, a dialect provides this function:

  dialect.inflect(v) -> syntax_v

The resulting C<syntax_v> must implement three methods:

  syntax_v.parse_continuation(op) -> parser(k -> v')
  syntax_v.postfix_modify(op, v)  -> v'
  syntax_v.abstract()             -> v

NB: C<parse_continuation>'s resulting parser returns an uninflected value.

Syntax values are transient by design; the assumption is that you can (and will)
frequently call C<abstract> to reduce them back to semantic values, then re-lift
those values into new syntax values, possibly using a different dialect.

Dialects' interaction with aliases and C<syntax> nodes is a bit subtle. Alias
nodes provide a compile-time shortcut to a captured value, which we should use
iff it immediately resolves to a syntax node. Otherwise we just wrap the alias
referent with the inflection type.
=cut

use phitype class_wrapper_dialect_type =>
  bind(inflection_type => isget 0),

  bind(construct =>                     # v self
    nip, node_type_is(t_syntax),        # v self v-syn?
    l(drop, mcall"syntax"),
    l(mcall"inflection_type",           # v t
      swap, pnil, swons, i_cons),       # [v]::t
    if_),

  bind(inflect =>                       # v self
    nip, node_type_is(t_alias),         # v self v-is-alias?
    l(                                  # v self
      # If the alias proxy is a syntax value, then return that; otherwise wrap
      # the referent in the inflection type.
      nip, mcall"proxy_node",           # v self proxy
      dup, node_type_is(t_syntax),      # v self proxy p-syn?
      l(stack(3, 0), mcall"syntax"),    # proxy.syntax()
      l(drop, mcall"construct"),        # self.construct(v)
      if_),
    l(                                  # v self
      mcall"construct"),
    if_);


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
=cut

use phi expr_parser_for => l            # parser opgate
  l(top), swap,                         # p c opgate
  l(                                    # state v opgate
    stack(2, 2, 1, 0),                  # state opgate v state
    mcall"scope", mcall"dialect",       # state opgate v dialect
    mcall"inflect",                     # state opgate v'
    mcall"parse_continuation"           # state parser
  ),                                    # p c op uf
  swons,                                # p c f
  pnil, swons, swons, swons,            # [p c f]
  phiparse::flatmap_type, swons;        # flatmap

use phi expr => l                       # opgate
  atom, swap,                           # atom opgate
  expr_parser_for, i_eval;              # parser


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
  scope dialect  = dialect

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
    let pstate = state.with_scope(state.scope.parent) in
    match p.parse(pstate) with
      | error -> error
      | pstate' ->
          let v', capture' = state.scope.capture.add(pstate'.value) in
          let newscope     = state.scope.with_capture(capture')
                                        .with_parent(pstate'.scope) in
          pstate'.with_value(v')
                 .with_scope(newscope)

Here's the logic in detail:

1. We're pulling a value down from the parent scope: C<pstate>
2. We parse within that scope: C<pstate'>
3. We then add a value to the child scope's capture list: C<v', capture'>
4. We then rebuild the child scope's capture and parent with the modified values
5. Finally, we inherit parse state and return the captured proxy + new scope

Before I can write this, though, we need to talk about how capture lists work.

=head3 The capture list problem
C<phioptree> defines a single C<capture> node that gets passed into a function
to set up all captured references. That one entry point provides access to all
of the things we capture into a scope. Nicely simple, so what's wrong?

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
=cut

use phitype capture_list_type =>
  bind(xs          => isget 0),
  bind(length      => isget 1),
  bind(with_xs     => isset 0),
  bind(with_length => isset 1),

  bind(add =>                           # v self -> v' self'
    # First: don't capture syntax nodes; they get erased at runtime, so we can
    # just return them directly.
    swap, dup, node_type_is(t_syntax),  # self v syntax?
    l(swap),                            # v self
    l(swap,
      dup, mcall"length",               # v self len
      dup, lit 1, i_plus,               # v self len len+1
      rot3l, mcall"with_length",        # v len self'
      swap, rot3r, dup, mcall"xs",      # len v self xs
      stack(2, 0, 2, 1),                # len v self v xs
      op_cons, i_eval,                  # len v self xs'
      swap, mcall"with_xs",             # len v self'
      stack(0, 2),                      # len v self' len
      capture_nth, i_eval,              # len v self' cnode
      stack(0, 2), alias, i_eval,       # len v self' alias(v->cnode)
      stack(4, 1, 0)),                  # alias self'
    if_),

  bind(capture_list =>                  # self
    dup, mcall"xs", swap, mcall"length",# xs length
    native_const, i_eval, swap,         # length_node xs_node
    op_cons, i_eval);                   # node(length::xs)

use phi empty_capture_list => pcons l(c_nil, 0), capture_list_type;


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
required. We'll parse C<(> by ascending, then return to the child scope to parse
the continuation. C<)> is effectively a literal, which is scope-independent.
=cut

use phitype pulldown_parser_type =>
  bind(parser => isget 0),
  bind(parse =>                         # state self
    mcall"parser",                      # state p
    stack(0, 1, 1),                     # state p state state
    mcall"scope", mcall"parent",        # state p state sc.parent
    swap, mcall"with_scope",            # state p pstate
    swap, mcall"parse",                 # state pstate'
    dup, mcall"is_error",

    # If error, return directly
    l(                                  # state pstate'
      top),                             # pstate'

    # Otherwise, do the capture list stuff
    l(                                  # state pstate'
      nip, mcall"scope",                # state pstate' sc
      dup, mcall"capture",              # state pstate' sc capture
      stack(0, 2), mcall"value",        # state pstate' sc capture v
      swap, mcall"add",                 # state pstate' sc v' capture'
      stack(0, 2),                      # state pstate' sc v' capture' sc
      mcall"with_capture",              # state pstate' sc v' sc
      stack(0, 3), mcall"scope",        # state pstate' sc v' sc psc
      swap, mcall"with_parent",         # state pstate' sc v' sc'
      stack(0, 3), mcall"with_scope",   # state pstate' sc v' pstate'
      mcall"with_value",                # state pstate' sc pstate'
      stack(4, 0)),                     # pstate'

    if_);


=head3 Binding locals
This is a bit of a pain to do normally, so let's automate it a little.
=cut

use phi local_for => l                  # p v
  l(top), swons,                        # p f=(_ v -> v)
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
    mcall"with_locals"),                # self'

  bind(child =>                         # self
    dup,                                # parent self
    mcall"with_parent",                 # child
    empty_capture_list, swap,           # ecl child
    mcall"with_capture",                # child'
    pnil, swap, mcall"with_locals"),    # child''

  bind(parser_locals =>                 # self
    mcall"locals", pnil, swons,         # [[locals...]]
    phiparse::alt_type, swons),         # alt([locals...])

  bind(parser_atom =>                   # self
    dup,  mcall"parser_capture",        # self capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    pnil, swons,                        # [[locals capture]]
    phiparse::alt_type, swons),         # alt([locals capture])

  bind(parser_capture =>                # self
    dup, mcall"parent", dup, nilp,      # self parent parent-nil?
    l(drop, drop, phiparse::fail),      # fail
    l(mcall"parser_atom", pnil, swons,  # self [p]
      pulldown_parser_type, swons,      # self pulldown-parser
      top),                             # p
    if_);                               # fail|p


1;
