=head1 Stackless design
phi's existing stack isn't very useful because it's impossible to manage from a
GC point of view; that is, we can't rely on the stack to store pointers because
we won't be able to trace/move those values. (I'm not interested in crap like
conservative GC.)

How far can we get if we fully commit to frame objects and design the bytecode
around this?


=head2 Registers
Right now phi has four registers:

  state = ( insn, ivec, stack, frame )

A stackless design could work in a few ways:

  state = ( insn, ivec, frame, x )        # operand RHS as memory
  state = ( insn, ivec, frame, x, y, z )  # operands as registers

If we use registers for operands then we want three; that's the maximum number
of arguments to any primitive except for C<syscall>, which I think I'm going to
drop as a bytecode instruction.


=head2 Instructions and addressing
The basic idea would be something like this:

  m64get : x = m[x]
  m64set : m[y] = x
  iplus  : x += y
  if     : x = x ? y : z
  memcpy : from=x, to=y, size=z
  call   : swap(insn, x)
  xy     : swap(x, y)
  xz     : swap(x, z)
  fgetXX : x = frame[XX]
  fsetXX : frame[XX] = x
  ...


=head2 Callee-oriented frame allocation
We can use OS-provided stack memory for frames. I think the idea would be to
store the OS stack in the interpreter up front, then use that as the frame
allocation pointer. If functions are callee-setup, then we'd have something like
this:

  const(fn)                             # x = fn
  call                                  # ->fn(x = cc)

  fn:                                   # cc _ _
    # enter:
    xz                                  # _ _ cc
    const(fsize)                        # size _ cc
    ineg xy                             # _ -size cc
    getframe xy                         # -size frame cc
    iplus                               # frame' frame cc
    setframe                            # frame' frame cc
    const(fclass) fset0                 # fclass frame cc
    xy fset1                            # frame fclass cc
    xz fset2                            # frame fclass cc

    ...

    # exit:
    fget2 xy                            # _ cc _
    fget1 setframe                      # frame cc _
    xy goto                             # ->caller

...so the expectation is that a function call obliterates all register contents.
I think that's reasonable enough.

As for argument/return storage, I think we can either use negative frame offsets
to write values into the callee frame, or we can use some interpreter state to
maintain a list of arguments. Maybe we should have the caller allocate the
frame...


=head2 Alternative: caller allocates the frame
This aligns more closely with the idea that frames are classes that implement
functions. The big advantage is that we have a simple way to store arg/return
values. A call would then look like this:

  const(hash("allocate")) xy            # _ .allocate _
  const(frameclass) call xy             # _ frame' _

  arg1... xz                            # _ frame' arg1
  const(hash("set_arg1")) xy            # frame' .set_arg1 arg1
  call                                  # frame' _ _

TODO: not quite; we need to C<m64get> objects before calling them.

In practice we'd stuff the new frame instance into a local variable rather than
trying to keep it resident in an operand register.
