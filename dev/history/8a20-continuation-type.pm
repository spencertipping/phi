=head1 Calling continuations and GC
Well guys, it's come to this: C<call> isn't a viable bytecode. And it's for
pretty interesting reasons, so let's get into this a bit.

As a quick review, C<call> swaps the program/instruction pointer with the
address of the byte immediately following it. This pushes a return address,
which I call C<cc> for "calling continuation" for the callee, which then uses
C<goto> to return there. Simple enough.

In the previous implementation of phi, I described C<cc>'s type vaguely;
sometimes it was a hereptr, other times an integer. Really it's neither because
no type can describe how it behaves.


=head2 The GC problem
I've been assuming that C<cc> is a weak reference: someone else like our frame
class would mark the calling function, so C<cc> would be covered in the live
set. This can all be true.

The problem is that C<cc> needs to be rewritten when the caller is relocated.
The only things we can rewrite are ptrs and hereptrs. We can't write
pointers-to-the-middle of things without a here marker.


=head2 Implications
Primarily, C<call> needs to push a return address that can be tied back to an
object. This means it needs to be prefixed with a here-marker of some sort. I
think we're fine if we add four bytes of padding; that should cover even a
32-bit here marker, which should be more than enough.
