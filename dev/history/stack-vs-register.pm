=head1 Stack vs register encoding
This really is worth considering: what are the tradeoffs here, particularly if
we already have call frames to store our locals? Our data stack seems like a
long way around an addressing scheme. No sense in complicating things if we
don't need to.

For example, the C<point2d> class I mentioned in the concatenative compiler doc
is easier to write in terms of call frame accesses:

  point2d.plus.framestruct =
    object *p                 # field 0
    object *lhs               # field 1
    object *rhs               # field 2
    double x'                 # field 3
    double y'                 # field 4

  point2d.plus = [            # 11 ops total
    fset 1 (arg 0)
    fset 2 (arg 1)
    fset 0 (const point2d)
    .allocate 0 (fget 0)
    fset 3 (get-field "x" fget 1)
    fset 4 (get-field "y" fget 1)
    float+ 3 (get-field "x" fget 2)
    float+ 4 (get-field "y" fget 2)
    set-field "x" 0 (fget 3)
    set-field "y" 0 (fget 4)
    set-return (fget 0) ]

Here's a more realistic stack encoding (one op per line) for comparison:

  # 15 ops total
  point2d.plus = [                      # rhs self
    nth(0)                              # rhs self self
    get-field"x"                        # rhs self self.x
    nth(2)                              # rhs self self.x rhs
    get-field"x"                        # rhs self self.x rhs.x
    float+                              # rhs self x'
    nth(1)                              # rhs self x' self
    get-field"y"                        # rhs self x' self.y
    nth(3)                              # rhs self x' self.y rhs
    get-field"y"                        # rhs self x' self.y rhs.y
    float+                              # rhs self x' y'
    const(point2d)                      # rhs self x' y' point2d
    .allocate                           # rhs self x' y' p
    set-field"y"                        # rhs self x' p
    set-field"x"                        # rhs self p
    keepdrop(1, 3) ]                    # p

We have more operations here as expected, although a fairer comparison is to
inline stack-manipulation with the the operators:

  # 11 "ops"
  point2d.plus = [                      # rhs self
    nth(0) get-field"x"                 # rhs self self.x
    nth(2) get-field"x"                 # rhs self self.x rhs.x
    float+                              # rhs self x'
    nth(1) get-field"y"                 # rhs self x' self.y
    nth(3) get-field"y"                 # rhs self x' self.y rhs.y
    float+                              # rhs self x' y'
    const(point2d)                      # rhs self x' y' point2d
    .allocate                           # rhs self x' y' p
    set-field"y"                        # rhs self x' p
    set-field"x"                        # rhs self p
    keepdrop(1, 3) ]                    # p


=head2 Pros and cons
Stack representation pros:

1. Multiple return values are trivial to implement
2. It works without any struct dependencies
3. It puts args/returns and locals into the same addressing space

Stack representation cons:

1. It's very easy to get off-by-N errors that are impossible to debug
2. RTL translation is another compiler step
3. In general, consumers and producers both need to model the stack
4. We have to spill stack values through the call frame for GC atomicity
5. Conditional branches aren't guaranteed to have equal displacements

Register representation pros:

1. Multiple return values are trivial to implement (stack for args/return)
2. We get GC atomicity for free (no risk of errors)
3. No translation required for most backends
4. No translation required for most _frontends_ either
5. We can easily detect low-level type errors
6. Debuggers are easier to implement
7. Fewer distinct operations -- although code size is probably the same/larger
8. No risk of displacement bugs
9. Bytecode interpreters are faster

Register representation cons:

None.

...well that was easy.


=head2 Primitive instructions in a register world
Probably the big question here is, how do struct field accessors work? This is
especially relevant when we have variable-offset struct layouts. If we have a
register-level primitive instruction set we can probably run an interpreter that
emulates struct accessors, but we'll want better inlining for native code
generation.

Actually, this is probably fine: once we're at the register/struct level things
are easy. We can compile register functions down to low-level memory access for
flat backends and indexed-field access for managed backends. This happens at the
backend idiom-translation layer, so we're all good there. It's a natural
compilation step.

How do we bottom this out, if at all? It seems like we'd want a base frame that
provides something like four pointer slots, four hereptr slots, and four value
slots -- or something similarly general-purpose. Then more specific structures
are a space optimization.

Q: how do we handle the return address/callee pointer -- are these passed in as
args, or do structs automatically build them in upon allocation?

Q: are call frames real objects, or are they lightweight structs of some sort?


=head2 Fusing the stacks
The frame stack can be a single thing, and frames themselves can be written in
such a way that args are preallocated into them. For example:

          ...
          local2
          local1
  %rbp -> caller-frame-vtable
          return-addr
          arg1                          # these get written in by the caller
          arg2
          ...
          callee-frame-vtable           # ...then this to do the allocation

Once the caller has allocated the callee frame, it can then decrement C<%rbp>
and do the control transfer. The callee frame resets all pointer fields _except_
those reserved for incoming args (which are otherwise normal, addressible
registers) -- then the function call happens and return values are left in the
top cells of the frame for the caller to retrieve.


=head2 Do we care about fastcalling/can we do it?
I'm going with "not at the moment", and "maybe". Spilling stuff to memory isn't
completely cheap, but it's not the worst problem either; if those extra cycles
end up being our biggest bottleneck I'll call it a huge win. I think we can also
mitigate some of this by selectively inlining and doing some sort of
register/slot mapping, at least on primitive slots.
