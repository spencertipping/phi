=head1 Bootstrapping classes
Ok, the goal is to end up with compiled structs in the phi image. Broadly
speaking, there are a few ways to do that:

1. Use thin perl abstraction: no struct encoding, just cheat
2. Write wrapper classes in perl, then manually compile each
3. Write a struct-metaclass in perl, then instantiate it for each class
4. Write a struct-metaclass metaclass in perl, then instantiate it in binary

(4) is the least-lossy and (1) is the most-lossy _in theory_. Underlying all of
this is the question of how much portability we get between a memory-managed
world like Perl and the flat world of machine code.


=head2 Thin perl abstraction
This is more or less what we're doing now with C<asm> shellout stuff, but
without perl wrapper classes. I don't think it's a good idea at all; way too
much maintenance to try to maintain.


=head2 perl wrapper classes
Structurally we'd have stuff like this:

  package phi::code
  {
    ...
    sub asm
    {
      return phi::asm($name)->...;
    }
  }

So we're tracking an incremental object state in perl, which we then compile on
demand. I like this for a couple of reasons:

1. Staging is different from final rendering
2. It provides idiomatic decoupling: perl != machine code for representation

I have some reservations about committing to (2) though. Isn't the whole point
of phi that data is portable to different backends? That brings us to...


=head2 perl struct-metaclass, instantiations for classes
This is the first world that looks like what phi is ultimately trying to be. Our
perl implementation of the struct metaclass would be matched by one in machine
code, and it would encode its own instance data using one of itself:

  package phi::metaclass
  {
    ...
  }

  BEGIN { do_magic_setup() }
  use phi::class 'phi::class'  => instance_data;
  use phi::class 'phi::code'   => instance_data;
  use phi::class 'phi::vtable' => instance_data;
  ...

Now we have the instance data that describes how each class works, which _might_
be enough to target the full set of backends -- it depends on how portable our
code objects are, and maybe this is a good time to sort that out.

=head3 Code object portability
Ideally speaking, classes refer to backend-agnostic code fragment objects and we
get full portability with a minimal set of primitives. In the real world there's
probably a little more to it: in particular, not all backends support the same
set of primitive operations, so we have an idiom-aware compilation step. The
question then is at what layer those idioms are applied. Do perl and machine
code share any layer with fully mechanical portability?

Arguably structs themselves are pretty portable. We should have some logical
representation that is sufficient to encode both binary and perl without too
much fuss. Data is data. The only ambiguity we have is which underlying
representation we want to target; for instance, we could do binary-packed
objects or perl idiomatic ones. We need to be able to go either way.

Which code objects we use to implement this is partially a result of that
decision, at least as far as accessors go. Above that, though, we're dealing in
primitives and should be portable enough.

Q: what does it look like to target perl with concatenative primitive lists?

Q: what does that look like in machine code, since we're asking?

=head3 Implementing a class in concatenative terms
First question here: do we want a macro-compile step that takes us from source
to executable code? Obviously we have a compile-to-backend step, but do we want
an idiom translation step too? The idea is that structs can provide inlined
accessors that may not make any sense to compile as function calls -- but
arguably we can inline them when we backend-compile if our constant folding is
good enough.

I think that's a pretty small "if": all our struct delegate needs to do is JIT
the accessors into function objects; then our JITted methods would look like
this:

  [ [<get-field "x">] mcall("eval") ... ]

Any backend compiler should be able to inline the inner list and eliminate the
C<eval>. (Or more accurately, we should pass these lists through a shared
concatenative-level optimizer to take care of this type of thing.)


=head2 perl struct-metaclass metaclass, binary instantiation
This is unnecessary; a struct metaclass should be self-porting, so there's no
sense in tying its instantiation to any particular format. A fixed point in perl
should be fine.
