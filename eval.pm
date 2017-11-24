=head1 Evaluation semantics
Just so I have this all written down and can be confident it isn't horribly
broken.

  ().type == niltype        ().#type    == constant
  0.type  == int            0.#type     == constant
  "".type == string         "".#type    == constant
  x.type  == x.type         x.#type     == constant

  0.foo.type == 0.foo.type  0.foo.#type == method
  0().type   == 0().type    0().#type   == call

I have reservations about all of this because C<#type> can entangle evaluation
state with semantics -- but let's assume we can type-gate it enough not to be a
problem. (For instance, we can have definitions use scope continuations that
immediately resolve bound unknowns to deferred objects.)

Let's talk about how values get type-wrapped.

  t   = some type;
  x:t = x.#type == t.container && x.#typeid == t.id

Ok this is nice. We have generalized containers and type IDs, and each ID is
presumably distinct since types are nominal.
