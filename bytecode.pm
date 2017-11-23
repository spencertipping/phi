=head1 Bytecode idea
I think it's possible to do all of this stuff using a Forth-like concatenative
setup. Then we get a fast bytecode without too much work, and a JIT if we want
one.

It's worth condensing the low-level semantics a bit. Stuff about maps is silly
and overly complex; really we want to think about things in terms of these
parser bytecodes that form the execution layer. For example:

  00000000  xxxxxxxx yyyyyyyy : function call
  00000001  xxxxxxxx xxxxxxxx : int64 constant
  00000002  llllllll ssss...  : string constant

  00000003..ffffffff xxxxxxxx : method

We can write actual code if we have a boot parser that contains enough rewrite
bindings to be useful. I think it can all be specified in terms of rewriting
though, just like it is now. Using bytecode is just a question of optimization.

=head2 Builtin methods
We'd want enough of these to describe fundamental language constructs like
scopes, introspection, etc. I think we need to be able to write a parser using
builtin methods.

Q: does it make sense to get operator precedence/etc sorted out before we do
this?

Q: does it make sense to continue prototyping in perl?
