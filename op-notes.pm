This means that operator nodes need some knowledge of the operation they
represent -- they could become foldable at any point. They also need to know
when they can be applied, which is nontrivial in some cases.

=head3 What does it mean to apply an operator?
We need to answer this question before getting into the specifics -- it isn't
always as simple as "turn something into a concrete (constant) value." Operators
can apply to values that, for whatever reason, aren't yet realized; for
instance, maybe they're placeholder values or unresolved conditionals.

Applying an operator is really just about advancing the evaluation state by a
little bit. For example, we could apply the C<head> operator to C<cons(x, y)> to
get C<x>; and we don't know what C<x> is, but we do know that conses have enough
structure for us to destructure them in cases like this.

=head3 When can an operator be applied?
Let's start with the easy stuff:

1. All args are constants and the op is pure
2. All args are constants, the op is impure, and the context provides a timeline
3. All args are pure and the op is pure
4. Args are pure, the op is impure, and the context provides a timeline

That will get us to a strict evaluator: evaluate the args before the op. Nothing
demands that we do this, but phi tends to use strict semantics to avoid things
like the IO monad (the infliction of which on unsuspecting users should be
labeled a war crime).

=head3 Purity and function calls
This is where the question of applicability isn't quite as straightforward.

First, a function _value_ being pure has nothing to do with whether it will
create side effects when you _call_ it. All we care about for function purity is
whether it can be instantiated without side effects: specifically, whether its
capture value is pure.

The function call itself depends on the purity of three things:

1. The function value: C<print("x") ? f : g> isn't pure, for instance
2. The argument values: C<(print(x), print(y))> isn't pure
3. The body of the function: C<< x -> print(x) >> will produce an impure result

The only reason we care about purity is that we need to make sure any effects
against the timeline are sequenced correctly. Aside from that we can do whatever
we want.

=head3 ...but wait, there's more: C<f_pure>
Oh yes -- see, we have an interesting problem right now. What happens when we
have a function like C<< f = x -> x("hi") >>? The application is pure until we
do something like C<f(print)>. So its call purity depends on the values of its
arguments.

Superficially that doesn't seem like a problem; we could pessimistically assume
every function call is impure until we prove otherwise. But then we'd fail at
things like this:

  factorial n = match n with
    | 0 -> 1
    | _ -> n * factorial(n - 1)

The problem here is that we can always expand another iteration of C<factorial>,
but we shouldn't have to in order to understand that it's a pure function. We'd
need some way to deal with the recurrence relation, and from there the logic
isn't entirely obvious.

So let's back up for a moment: a function call is pure iff it has no calls into
impure functions. And we can figure this out by looking at the set of functions
referred to by call nodes; some of them may be unknown or undecided, but that's
ok because unknowns are marked with a purity disposition and undecided things
can be enumerated and reduced.

=head3 Purity and cons cells
phi arbitrarily defines the following rule for cons cells:

  timeline(cons(x, y)) = timeline(x) + timeline(y)

That is, the head is evaluated first. This is important and it creates some
interesting downstream effects: specifically, C<head(cons(x, y))> can't be
reduced to C<x>; instead, it needs to be reduced to C<seql(x, y)> -- that is,
evaluate C<x>, then C<y>, and return C<x>.
