=head1 Abstract type definitions
Parameterizable objects that become abstract types. At a high level, abstract
types support a few different things:

1. Partial evaluation and evaluation state
2. Postfix modification, if supported
3. Parse continuations, split into postfix and prefix

Abstract values, via (1), form an interpreter -- and that's a bit nontrivial, so
let's talk about what it entails.


=head2 Using abstracts to interpret code
This is actually very, very simple even for lexical closures. Let's work through
a quick example:

  (x -> x + 1)(5)

Here's what this looks like in object terms:

  call(fn(capture=cons(int(1), nil()),
          body=intplus(cons_head(arg()), head(capture()))),
       cons(int(5), nil()))

Evaluating something like C<intplus(int(3), int(4))> is obviously trivial and
doesn't involve any stack state -- but functions require a bit more machinery.
Specifically, we need to make sure two things are true:

1. All args from parent functions are dereferenced to non-arg values
2. The capture list for the child function is as evaluated as it can be

phi is all about partial evaluation, so both of these things use the same
mechanism: we just call C<eval> on anything we want to resolve.

=head3 Evaluating C<call> nodes
C<call> interacts with C<fn> to move the body into the calling context. Normally
this isn't possible because the function body (and anything that refers to
C<arg> or C<capture>, really) can't move through a C<fn> node. C<call> performs
the motion by substituting the C<capture> and C<arg> values into things anchored
in the parent context. That means the C<eval> method needs to carry C<capture>
and C<arg> as stack state, which it does indirectly with a context object.


=head2 Abstract value protocol
Abstract values all support the following methods:

  node.eval(context) -> node'
  node.is_const()    -> 1|0

Constants additionally support a C<val()> method to return their logical value a
as a phi primitive.

The C<context> value tracks two things:

1. The current C<arg> value
2. The current C<capture> value

I realize that it probably seems like overkill to even have a context value just
for this, but the next layer uses it to much greater effect.

The most important invariant is that every node always exists in its
most-evaluated state. This means that node constructors will constant-fold
whenever possible; for instance, trying to construct C<plus(int(1), int(2))>
would actually produce C<int(3)>.
=cut


package phiabstract;
use strict;
use warnings;

use phiboot;
use phibootmacros;
use phiparse;
use phiobj;


=head2 Evaluation context
Let's go ahead and define this. At a minimum we need to store the current arg
and capture values. Down the line we can add more stuff like speculative entropy
limiting/etc.
=cut

use phitype context_type =>
  bind(arg          => isget 0),
  bind(capture      => isget 1),
  bind(with_arg     => isset 0),
  bind(with_capture => isset 1);

use phi root_context => pcons l(pnil, pnil), context_type;


=head2 Primitive nodes
These wrap fully-specified phi values and function as constants.

C<const> means that the value _and everything it refers to_ are constants,
whereas C<cons> is a cons of two abstract values.
=cut

use phitype const_type =>
  bind(val      => isget 0),
  bind(eval     => stack(2, 0)),
  bind(is_const => drop, lit 1);

use phi const => l                      # v
  pnil, swons, const_type, swons;       # [v]::const-type


=head2 C<fn>
Function nodes are a bit subtle. Evaluating them isn't the same as calling them;
instead, we just apply a context to both the capture list. There isn't any point
to applying the context to the function body (yet). The mechanics of
implementing a function call are handled by the C<call> node.
=cut

use phitype fn_type =>
  bind(capture      => isget 0),
  bind(body         => isget 1),
  bind(with_capture => isset 0),
  bind(with_body    => isset 1),

  bind(is_const => mcall"capture", mcall"is_const"),
  bind(val      => ),                   # self

  bind(eval =>                          # context self
    dup, rot3r,                         # self context self
    mcall"capture", mcall"eval",        # self capture'
    swap, mcall"with_capture");         # self'


use phi fn => l                         # capture body
  pnil, swons, swons, fn_type, swons;


=head2 C<arg> and C<capture>
The context will sometimes provide values for C<arg> and C<capture> nodes. This
is set up by C<call> nodes.
=cut

use phitype arg_capture_type =>
  bind(context_method => isget 0),

  bind(is_const => drop, lit 0),
  bind(eval     =>                      # context self
    dup, mcall"context_method",         # context self method
    rot3l, i_eval, dup, nilp,           # self v 1|0
    l(stack(2, 1)),                     # self
    l(stack(2, 0)),                     # v
    if_);


use phi arg     => pcons l(psym"arg"),     arg_capture_type;
use phi capture => pcons l(psym"capture"), arg_capture_type;


=head2 Operator nodes
Alright, now it's time to combine values together. Let's get into the low-level
details just a bit.

First, operator nodes are strictly used to represent _pending_ operations: once
an operation can happen, the node collapses into the result value. So, for
instance, C<(+ 3 4)> isn't a node that would exist; the constructor would fold
the node and turn it into C<7> immediately. So we have an invariant:

  Operator nodes will always exist in their most-evaluated state.

There's a lot more subtlety we can bring to the topic, but I want to avoid it in
the bootstrap interpreter. So let's carry on by using simple strict evaluation
here; then we can implement the fun stuff in the next layer.

For now, we have another, overly conservative, invariant:

  Operator nodes can be evaluated iff all arguments are constants.

This means C<if> is implemented in terms of lambdas, which is exactly how it
works at the stack layer.
=cut

use phi op_args_are_constant_mut => pmut;
use phi op_eval_args_mut         => pmut;

use phitype op =>
  bind(args => isget 0),
  bind(ctor => isget 1),

  bind(is_const => drop, lit 0),

  bind(eval =>                          # context self
    # Eval all of the args, then re-invoke the constructor to try to fold again.
    dup, rot3r, mcall"args",            # self context args
    op_eval_args_mut, i_eval,           # self args'
    swap, mcall"ctor", mcall"apply");   # ctor.apply(args')


use phi op_args_are_constant => l       # args
  dup, nilp,
  l(drop, lit 1),                       # 1
  l(i_uncons, mcall"is_const",          # args' const?
    op_args_are_constant_mut,
    l(drop, lit 0),
    if_),
  if_;

use phi op_eval_args => l               # context args
  dup, nilp,
  l(stack(2, 0)),                       # []
  l(i_uncons, stack(0, 2), swap,        # c args' c a
    mcall"eval",                        # c args' a'
    rot3r, op_eval_args_mut, i_eval,    # a' evaled
    swons),
  if_;

op_args_are_constant_mut->set(op_args_are_constant);
op_eval_args_mut->set(op_eval_args);


=head3 Op constructors
Op args come in lists, which makes all of this quite trivial. An op constructor
is just an object that can either evaluate or make an op node.
=cut

use phitype op_constructor_type =>
  bind(name              => isget 0),
  bind(apply_fn          => isget 1),
  bind(can_be_applied_fn => isget 2),

  bind(can_be_applied =>                # args self
    mcall"can_be_applied_fn", i_eval),  # applied?

  bind(apply =>                         # args self
    stack(0, 0, 1),                     # args self args self
    mcall"can_be_applied",              # args self apply?
    l(mcall"apply_fn", i_eval),         # apply_fn(args)
    l(pnil, swons, swons,               # [args self]
      op, swons),                       # [args self]::op
    if_);

use phi strict_op_constructor => l      # name apply-fn
  l(op_args_are_constant, i_eval),      # name apply-fn can-be-applied
  pnil, swons, swons, swons,            # [name apply-fn can-be-applied]
  op_constructor_type, swons,           # ...::op_ctor_type
  l(mcall"apply"), swons;               # [...::op_ctor_type .apply]


=head2 Unary ops
These all work the same way: take a phi value and return a phi value. Our job is
to wrap them in abstract values.
=cut

use phi op_unary => l                   # name fn
  lit i_eval, i_cons,                   # name [. fn.]
  l(head, mcall"val"), i_cons,          # name [[head .val] . fn . const.]
  strict_op_constructor, i_eval;


use phi op_type => le lit"type", l(i_type, const, i_eval), op_unary, i_eval;
use phi op_head => le lit"head", l(head,   const, i_eval), op_unary, i_eval;
use phi op_tail => le lit"tail", l(tail,   const, i_eval), op_unary, i_eval;

use phi op_ineg => le lit"i-",   l(i_neg,  const, i_eval), op_unary, i_eval;
use phi op_iinv => le lit"i~",   l(i_inv,  const, i_eval), op_unary, i_eval;
use phi op_inot => le lit"i!",   l(i_inv,  const, i_eval), op_unary, i_eval;


=head2 Binary ops
Same idea as above. These cover everything except conditionals.
=cut

use phi op_binary => l                  # name fn
  lit i_eval, i_cons,                   # name [. fn.]
  l(dup,  head, mcall"val", swap,
    tail, head, mcall"val"), i_cons,    # name [[dup  car  .val
                                        #        swap cadr .val] . fn.]
  strict_op_constructor, i_eval;


use phi op_iplus  => le lit"i+",  l(i_plus,  const, i_eval), op_binary, i_eval;
use phi op_itimes => le lit"i*",  l(i_times, const, i_eval), op_binary, i_eval;
use phi op_ilsh   => le lit"i<<", l(i_lsh,   const, i_eval), op_binary, i_eval;
use phi op_irsh   => le lit"i>>", l(i_rsh,   const, i_eval), op_binary, i_eval;
use phi op_iand   => le lit"i&",  l(i_and,   const, i_eval), op_binary, i_eval;
use phi op_ixor   => le lit"i^",  l(i_xor,   const, i_eval), op_binary, i_eval;
use phi op_ilt    => le lit"i<",  l(i_lt,    const, i_eval), op_binary, i_eval;

use phi op_seql   => le lit"seql", l(drop,       const, i_eval), op_binary, i_eval;
use phi op_seqr   => le lit"seqr", l(swap, drop, const, i_eval), op_binary, i_eval;

use phi op_cons   => le lit"cons", l(i_cons, const, i_eval), op_binary, i_eval;


=head3 Function calls
Alright, time for the good stuff. And it turns out to be pretty straightforward
too.

The basic idea is that a function call node is just a regular op whose arg list
contains two elements, a function object and an argument object. Once both are
constant, we do the substitution.
=cut

use phi op_call => le lit"call",
  l(                                    # fn argval
    const, i_eval,                      # fn arg
    swap, dup, mcall"capture",          # arg fn fncapture
    rot3r, mcall"body",                 # capture arg body
    rot3r, root_context,                # body c a context
    mcall"with_arg",                    # body c context'
    mcall"with_capture",                # body context''
    swap, mcall"eval"),
  op_binary, i_eval;


1;
