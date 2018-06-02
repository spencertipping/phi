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
