=head1 More register-oriented design
Let's just go all-in on functions as classes. Then we don't have a C<frame>
register; we have an C<object> register.

  state = ( insn, ivec, object, x, y, z )

C<call> is now a heavier instruction: C<call(x=object, y=method, z=arg)> results
in C<object=x, x=cc, y=method, z=arg> within the caller.

Nope, this won't work: we lose the calling object.
