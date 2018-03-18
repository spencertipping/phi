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

  call(fn(capture=[int(1)], intplus(cons_head(arg()), head(capture()))),
       cons(int(5), nil()))

NB: C<cons(x, y)> is an abstract node object, whereas C<[...]> is a native list.
Because capture lists are always fully-specified, we don't need to encode them
as abstract values. (Also note that C<capture(0)> refers to the _rightmost_
capture list entry due to the way capture nodes work.)

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
  bind(with_capture => isset 0);

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

use phi fn_typesym => pcons l(psym 'fn'), const;

use phitype fn_type =>
  bind(capture      => isget 0),
  bind(body         => isget 1),
  bind(with_capture => isset 0),
  bind(with_body    => isset 1),

  bind(is_const => mcall"capture", mcall"is_const"),
  bind(val      => ),                   # return self

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

use phitype op_type =>
  bind(name => isget 0),
  bind(fn   => isget 1),                # [abstract] -> op|const
  bind(args => isget 2),

  bind(is_const => drop, lit 0),

  bind(eval =>                          # context self
    # Eval all of the args, then re-invoke the constructor to try to fold again.
    dup, rot3r, mcall"args",            # self context args
    op_eval_args_mut, i_eval,           # self args'
    swap, mcall"fn", i_eval);           # fn(args')


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
function ends up being a self-referential closure over a name; mechanically:

  [args] ('name [eval-fn] op_constructor) =
    op_args_are_constant(args)
      ? args eval-fn
      : [name ['name [eval-fn] op_constructor] [args]]::op_type

=cut

use phi op_constructor_mut => pmut;
use phi op_constructor     => l         # name eval-fn
  l(                                    # [args] 'name eval-fn
    rot3l, dup,                         # 'name eval-fn [args] [args]
    op_args_are_constant_mut, i_eval,   # 'name eval-fn [args] const?
    l(                                  # 'name eval-fn [args]
      stack(3, 1, 0),                   # [args] eval-fn
      i_eval),                          # eval-fn([args])
    l(                                  # 'name eval-fn [args]
      rot3r, op_constructor_mut, swons, # [args] 'name eval-fn::op_ctor
      stack(3, 2, 0, 1, 1),             # 'name 'name e::o [args]
      pnil, swons,                      # 'name 'name e::o [[args]]
      rot3r, swons, i_cons,             # 'name [('name::e::o) [args]]
      swap, i_eval, i_cons,             # [name ('name::e::o) [args]]
      op_type, swons),                  # [name ('name::e::o) [args]]::op_type
    if_),                               # name eval-fn innerfn
  rot3l, philang::quote, i_eval, rot3r, # 'name eval-fn innerfn
  swons, swons;                         # ['name eval-fn innerfn...]

op_constructor_mut->set(op_constructor);


=head2 Unary ops
These all work the same way: take a phi value and return a phi value. Our job is
to wrap them in abstract values.
=cut

use phi op_unary => l                   # name fn
  l(i_eval, const, i_eval), swons,      # name [fn . const.]
  lit i_eval, i_cons,                   # name [. fn . const.]
  l(head, mcall"val"), i_cons,          # name [[head .val] . fn . const.]
  op_constructor i_eval;


use phi op_type => le lit"type", l(i_type), op_unary, i_eval;
use phi op_head => le lit"head", l(head),   op_unary, i_eval;
use phi op_tail => le lit"tail", l(tail),   op_unary, i_eval;

use phi op_ineg => le lit"i-",   l(i_neg),  op_unary, i_eval;
use phi op_iinv => le lit"i~",   l(i_inv),  op_unary, i_eval;
use phi op_inot => le lit"i!",   l(i_inv),  op_unary, i_eval;


=head2 Binary ops
Same idea as above. These cover primitives, but not function calls and
conditionals.
=cut

use phi op_binary => l                  # name fn
  l(i_eval, const, i_eval), swons,      # name [fn . const.]
  lit i_eval, i_cons,                   # name [. fn . const.]
  l(dup, head, mcall"val", swap,
    tail, head, mcall"val"), i_cons,    # name [[dup  car  .val
                                        #        swap cadr .val] . fn . const.]
  op_constructor i_eval;


use phi op_iplus  => le lit"i+",  l(i_plus),  op_binary, i_eval;
use phi op_itimes => le lit"i*",  l(i_times), op_binary, i_eval;
use phi op_ilsh   => le lit"i<<", l(i_lsh),   op_binary, i_eval;
use phi op_irsh   => le lit"i>>", l(i_rsh),   op_binary, i_eval;
use phi op_iand   => le lit"i&",  l(i_and),   op_binary, i_eval;
use phi op_ixor   => le lit"i^",  l(i_xor),   op_binary, i_eval;
use phi op_ilt    => le lit"i<",  l(i_lt),    op_binary, i_eval;


=head2 Function calls
Alright, time for the good stuff. And it turns out to be pretty straightforward
too.

The basic idea is that a function call node is just a regular op whose arg list
contains two elements, a function object and an argument object. Once both are
constant, we do the substitution.

There's a bit of machinery around figuring out whether the function call is
pure/impure, etc. That's integrated into the constructor.
=cut

use phi fncall_op_fn => l               # fn arg context
  # The arg and the function's capture value both exist in fully evaluated form,
  # so all we need to do is lift the function body with a child evaluation
  # context.
  rot3l, dup, mcall"capture",           # arg context fn fcapture
  rot3l, mcall"with_capture",           # arg fn context'
  rot3l, swap, mcall"with_arg",         # fn context''
  swap, mcall"body",                    # context'' body
  mcall"eval";                          # body.eval(context'')

use phi fncall_type_fn => l             # fn arg
  drop, mcall"body", mcall"type";       # fn.body.type

use phi op_fncall => l                  # fn arg
  pnil, swons, swons,                   # [fn arg]
  l(psym"call", fncall_op_fn, fncall_type_fn, 1),
  op, i_eval;


print le(lit 3, const, i_eval,
         arg,
         op_iplus, i_eval,              # 3 + arg

         pnil, const, i_eval,           # (3 + arg) []
         swap,
         fn, i_eval,                    # fn([], (3 + arg))

         lit 4, const, i_eval,          # fn([], (3 + arg)) 4
         op_fncall, i_eval);



print le(lit 3, const, i_eval,
         lit 4, const, i_eval,
         lit 5, const, i_eval,
         op_iplus, i_eval,
         op_itimes, i_eval);


1;

__END__

use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);


=head2 Example type: integers
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
  l(i_uncons,                           # n cs' c
    lit 48, i_neg, i_plus, rot3l,       # cs' c-48 n
    lit 10, i_times, i_plus, swap,      # (n*10+(c-48)) cs'
    list_int1_mut, i_eval),
  if_;

list_int1_mut->set(list_int1);

use phi list_int => l lit 0, swap, list_int1, i_eval;


=head3 Ops for testing: C<+>, C<*>, and C<^>
These operate live as opposed to building an expression tree.
=cut

use phi int_type_mut => pmut;

use phi times_fn => l                   # v1 v2
  mcall"val", swap, mcall"val",         # n2 n1
  i_times, pnil, swons,                 # [n2*n1]
  int_type_mut, swons;                  # [n2*n1]::int_type

use phi plus_fn => l                    # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_plus, pnil, swons,                  # [nl+nr]
  int_type_mut, swons;                  # [nl+nr]::int_type

use phi xor_fn => l                     # rhs lhs
  mcall"val", swap, mcall"val",         # nl nr
  i_xor, pnil, swons,                   # [nl^nr]
  int_type_mut, swons;                  # [nl^nr]::int_type

use phi plus_op_mut  => pmut;
use phi times_op_mut => pmut;
use phi xor_op_mut   => pmut;

use phi plus_op => pcons l(pcons(l(3, 0), op_precedence_type),
                           str_(pstr"+"),
                           l(plus_op_mut, philang::expr, i_eval, i_eval),
                           plus_fn),
                         owned_op_type;

use phi times_op => pcons l(pcons(l(2, 0), op_precedence_type),
                            times_fn,
                            l(times_op_mut, philang::expr, i_eval, i_eval),
                            abstract_fail,
                            pnil),
                          unowned_op_type;

use phi xor_op => pcons l(pcons(l(1, 0), op_precedence_type),
                          str_(pstr"^"),
                          l(xor_op_mut, philang::expr, i_eval, i_eval),
                          xor_fn),
                        owned_op_type;

plus_op_mut->set(plus_op);
times_op_mut->set(times_op);
xor_op_mut->set(xor_op);

use phi timesop_literal => local_ str_(pstr "*"), times_op;


use phitype int_type =>
  bind(val      => isget 0),
  bind(with_val => isset 0),

  # Reject all postfix modifications; ints aren't operators
  bind(postfix_modify => stack(3), abstract_fail),

  bind(parse_continuation =>            # op vself self
    drop,
    pnil,                               # op self []

    # none case (must be last in the list, so first consed)
    stack(0, 1),                        # op self [cases] self
    identity_null_continuation, i_eval, # op self [cases] p
    i_cons,                             # op self [cases']

    # unowned op case
    stack(0, 1, 2),                     # op self [cases] op self
    unowned_as_postfix, i_eval,         # op self [cases] p
    i_cons,                             # op self [cases']

    # Now build up the list of other possibilities, then filter it down by
    # applicable precedence.
    stack(3, 2, 1, 0),                  # [cases] self op
    rot3l,                              # self op [cases]
    l(plus_op, xor_op),                 # self op [cases] +op
    applicable_ops_from, i_eval,        # [cases']

    phiparse::alt, swons);

int_type_mut->set(int_type);

use phi int_literal => l
  rep_ oneof_(pstr join('', 0..9), lit 1),
  l(list_int, i_eval, pnil, swons, int_type, swons),
  phiparse::pmap, i_eval;


1;
