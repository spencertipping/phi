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

use phi list_int1_mut => pmut;
use phi list_int1 => l
  dup, nilp,
    l(drop),
    l(i_uncons,                         # n cs' c
      lit 48, i_neg, i_plus, rot3l,     # cs' c-48 n
      lit 10, i_times, i_plus, swap,    # (n*10+(c-48)) cs'
      list_int1_mut, i_eval),
    if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;


use phitype int_type =>
  bind(val                => isget 0),
  bind(parse_continuation => drop, phiparse::none);

use phi int_literal => l
  l(list_int, i_eval, pnil, swons, int_type, swons),
  l(l(pstr join('', 0..9), lit 1, phiparse::oneof, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Base syntax definitions
Stuff like whitespace, comments, etc. It's worth having these ready.
=cut

use phi line_comment => l
  l(l(pstr "#", phiparse::str, i_eval),
    l(l(pstr "\n", lit 0, phiparse::oneof, i_eval), phiparse::rep, i_eval),
    l(pstr "\n", phiparse::str, i_eval)),
  phiparse::seq, i_eval;

use phi any_whitespace => l
  pstr " \n\r\t", lit 1, phiparse::oneof, i_eval;

use phi ignore_primitive => l
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

use phi atom   => l dup, tail, tail, head, mcall"parser_atom",   i_eval;
use phi expr   => l dup, tail, tail, head, mcall"parser_expr",   i_eval;
use phi ignore => l dup, tail, tail, head, mcall"parser_ignore", i_eval;


=head2 A quick aside: some supporting functions
We'll use these later, and I don't want to interrupt the narrative to introduce
them since their implementation is pretty boring.

=head3 C<list-length> function

  []        list-length = 0
  [x xs...] list-length = xs... list-length inc

Derivation:

  xs  dup nilp              = xs <1|0>
  []  drop 0                = 0
  xs  tail list-length inc  = 1 + length(xs.tail)

=cut

use phi list_length_mut => pmut;
use phi list_length => l
  dup, nilp,
    l(drop, lit 0),
    l(tail, list_length_mut, i_eval, lit 1, i_plus),
    if_;

list_length_mut->set(list_length);


=head3 C<nthlast>
Functionally:

  xs i nthlast = xs rev i nth
  xs i nth     = i == 0 ? xs.head : xs.tail i-1 nth

Concatenatively:

  xs i      swap rev swap nth

  xs i      dup if
    xs i    swap tail swap 1 neg + nth
    xs 0    drop head

=cut

use phi nth_mut => pmut;
use phi nth => l
  dup,
    l(swap, tail, swap, lit 1, i_neg, i_plus, nth_mut, i_eval),
    l(drop, head),
    if_;

use phi nthlast => l swap, phiparse::rev, i_eval, swap, nth, i_eval;


=head3 C<quote>
We need a way to force phi to quote stuff that might not be self-quoting. For
example, if we want to bind C<x> to C<5>, we can't just put C<[x 5]> into the
resolver because phi would run C<5> as an instruction (cons).

So instead, we do what C<lit> does and put the value into a list. Here's the
equation:

  quote(v) = [[v] head]

Concatenative:

  v     [] swons [head] swons     = [[v] head]

=cut

use phi quote => l pnil, swons, l(head), swons;


=head3 C<substr>
Functionally:

  s start len      substr  = s start len (len mkstr) substr'
  s start len into substr' = len > 0
    ? let len' = len - 1 in
      into[len'] = s[start + len'];
      s start len' into substr'
    : into

=cut

use phi substr1_mut => pmut;
use phi substr1 => l                    # s start len into
  swap, dup,                            # s start into len len
    l(lit 1, i_neg, i_plus,             # s start into len'
      stack(0, 0, 2, 3),                # s start into len' s start len'
      i_plus, swap, i_sget,             # s start into len' s[start+len']
      stack(1, 0, 1, 2),                # s start into len' into len' s[...]
      i_sset, drop, swap,               # s start into len'
      substr1_mut, i_eval),             # s start len' into substr'
    l(drop, swap, drop, swap, drop),    # into
  if_;

substr1_mut->set(substr1);

use phi subs => l dup, i_str, substr1, i_eval;


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

...anyway, let's get back to C<pulldownify>. The idea here is fairly simple:
when we delegate into the parent parser via a C<capture> element, we need to
stash the current parse state and create a new one centered around the parent
scope. (We don't just stash the current state to preserve the child scope; we
also need the pre-parsed string offset.) So C<pulldownify> converts the parent's
regular C<atom> parser into one that does the pulldown stuff if it succeeds.
Specifically:

  [s n sc] (p pulldownify) . =
    let v, [s' n' parent'] = [s n sc.parent] p . in
    let capture' = v :: sc.capture in
    let v'       = capture_abstract(sc.capture.length) in
    let locals'  = (str_parser(s.substr(n, n' - n)) -> v') :: sc.locals in
    (v', [s' n' sc.with_capture(capture')
                  .with_parent(parent')
                  .with_locals(locals')])

The patch to C<locals> isn't strictly necessary, but it's an easy optimization
and we might as well take it. The idea there is that once you've captured a
value, it's already in the capture list and you can reuse it -- there's no
reason to repeat the capture process. This matters more than some of the other
optimizations we might make because forced alias detection is a bit more
involved than just constant folding (so it demands more from the abstract
evaluator).

We can treat C<pulldownify> as a closure generator over C<p>, so all of the work
actually happens in C<pulldown>, whose signature is:

  state p pulldown = v state'
                   | e []

Just a higher-order parser. Right then -- let's get to it.
=cut


use phi capture_abstract => l           # nth-from-end capture-list
  # TODO
  drop, l(psym"capture"), swons;        # nth-from-end


use phi local_parser_for => l           # v s start len
  subs, i_eval,                         # v s[start..+len]
  swap, quote, i_eval,                  # s[start..+len] 'v
    lit i_eval, i_cons,                 # s[start..+len] [. 'v]
    l(drop), i_cons,                    # s[start..+len] [[drop] . 'v]
  swap, phiparse::str, swons,           # [[drop] . 'v] s[start..+len]::str
  phiparse::pmap, swons, swons;         # [[[drop] . 'v] parser map...]


# NB: the state must provide a parent in order for pulldown to work. This should
# be knowable when you build the parser.
use phi pulldown => l                   # state p
  swap, dup,                            # p state state
  tail, tail, head, mcall"parent",      # p state sc.parent
  swap, dup, rot3l,                     # p state state sc.parent
  lit 2, lset, i_eval,                  # p state [s n sc.parent]
  rot3l, i_eval,                        # state v|e state'|[]
  dup, nilp,
    l(rot3l, drop),                     # e []
    l(rot3l, dup, tail, tail, head,     # v state' state sc
      mcall"capture", dup,              # v state' state sc.capture sc.capture
      list_length, i_eval,              # v state' state sc.capture len(sc.c)
      swap, stack(0, 4), i_cons,        # v state' state len(sc.c) capture'
      dup, rot3r,                       # v state' state capture' len capture'
      capture_abstract, i_eval,         # v state' state capture' v'
      stack(0, 3, 2, 0), tail, head,    # v state' state capture' v' v' state n'
      swap, tail, head,                 # v state' state capture' v' v' n' n
      dup, rot3r, i_neg, i_plus,        # v state' state capture' v' v' n n'-n
      stack(0, 6), head,                # v state' state capture' v' v' n n'-n s'
      rot3r, local_parser_for, i_eval,  # v state' state capture' v' parser
      stack(0, 3), tail, tail, head,    # v state' state capture' v' parser sc
      dup, rot3r,                       # v state' state capture' v' sc p sc
      mcall"locals", swons,             # v state' state capture' v' sc locals'
      swap, mcall"with_locals",         # v state' state capture' v' sc'
      rot3l, swap, mcall"with_capture", # v state' state v' sc''
      rot3l, drop,                      # v state' v' sc''
      rot3l, dup, tail, tail, head,     # v v' sc'' state' parent'
      rot3l, mcall"with_parent",        # v v' state' sc'''
      lit 2, lset, i_eval,              # v v' [s' n' sc''']
      rot3l, drop),                     # v' [s' n' sc''']
  if_;


use phitype scope_chain_type =>
  bind(parent  => isget 0),
  bind(locals  => isget 1),
  bind(capture => isget 2),
  bind(ignore  => isget 3),

  bind(with_parent  => isset 0),
  bind(with_locals  => isset 1),
  bind(with_capture => isset 2),
  bind(with_ignore  => isset 3),

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
      phiparse::alt, swons,             # scope [i a.]
    swap,                               # [i a.] scope
    l(mcall"parser_ignore",             # [i a.] pi
      pnil, swons,                      # [i a.] [pi]
      swons,                            # [[i a.] pi]
      phiparse::alt, swons),            # [[[i a.] pi] a.]
    swap, mcall"if_parent",
    phiparse::rep, swons),              # [[[[i a.] pi] a.] r.]

  bind(parser_locals =>
    mcall"locals",
    phiparse::alt, swons),              # [[locals...] a.]

  bind(parser_atom =>
    dup, mcall"parser_capture",         # scope capture
    swap, mcall"parser_locals",         # capture locals
    swap, pnil, swons, swons,           # [locals capture]
    phiparse::alt, swons),              # [[locals capture] a.]

  bind(parser_capture =>
    dup, mcall"parent", dup, nilp,      # self parent parent-nil?
      l(drop, drop, phiparse::fail),    # fail
      l(mcall"parser_atom", pulldown,
        swons, swap, drop),             # [parent-atom pulldown.]
    if_);


1;
