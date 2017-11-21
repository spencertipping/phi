=head1 Iteration notes
Let's focus on parsers.

  x:int       # this is a value parser that defines a scope continuation
  fn x        # the parse continuation is an expr within an extended scope

C<fn> interprets its arg as a parser.

C<int> is a parser whose return value is C<x>; the C<x:int> grammar is handled
by the parser context.

How about method invocations against structs?

  (x:int).inc        = x.plus(1);
  (x:int).inc.dec    = x;
  (x:int).foo(y:int) = x.times(y.plus(x));

These are structural parsers. Methods are self-quoting symbols that exist on
their own, and parens get collapsed as usual. Parse continuations can accumulate
symbolic context awaiting a rewrite, which then dies if you try to compile it.

Functions are a special case of methods, and unbound methods form data
structures:

  x:any.cons(y:any).head = x;
  x:any.cons(y:any).tail = y;

  x:int.to_s            = internal_.int_to_string x;
  stdout.print x:string = internal_.write_string x;
  stdout.print x:int    = internal_.write_string x.to_s;
  (x:string|int).print  = stdout.print x;

Eager expansion is safe if we use strong types like this, and if we don't have
pure-data types. (Q: do parse continuations create a problem? It seems like they
might if we're not careful.)

Fundamental syntax includes juxtaposition, including against method calls. Are
operators quoted? Then it's up to values to parse them. (...but if we do this,
how are parens and mixed types handled?)

  x:int + y:int         = x.plus(y);
  x:int * y:int + z:int = x.times(y).plus(z);
  x:int + y:int * z:int = x.plus(y.times(z));

We don't want to go through every possible combination here, but maybe it's ok
if we define for "any" types and rewrite operators into method calls across the
board:

  x:any + y:any = x.plus(y);

This might work.

NB: methods aren't C<any> values. They aren't values at all; they're some type
of syntactic literal that doesn't have a type. This means you won't run into
this problem:

  x:any op:any y:any = x.op(y)    # NOPE CAN'T DESIGN THIS

I think we can easily define parse continuations:

  x:int.parse_continuation "mm" = dimension(x, mm);
  x:int.parse_continuation "ft" = dimension(x, ft);
  5mm.type            # this is now a thing, at least within this scope

Literal parsing:

  path_part            = P([\w]+ "/"?);
  path                 = path_part+;
  P("/" p:path_part*)  = path("/", ps);
  P("./" p:path_part*) = path("./", ps);

  P("20" yy:[\d]{2} "-" mm:[\d]{2} "-" dd:[\d]{2}) = date(2000 + yy.to_i,
                                                          mm.to_i,
                                                          dd.to_i);
  2017-11-02.epoch    # this now works

Nothing seems wrong with this stuff.

  0.factorial     = 1;
  x:int.factorial = x.times(x.dec.factorial);

=head2 IO/T monad
Let's start with arrays; suppose we have a function that allocates and accesses
an array in memory:

  (xs:int.array).reverse = do
    result = int.array(xs.size);
    xs.each_index(i -> result.set(i, xs.get(xs.size.minus(i).dec)));
    result
  end;

What's the side effect structure here? In particular, let's talk about
C<each_index>:

  (xs:int.array).each_index f:(int -> any) = do
    xs.size.times f;
    xs
  end;

C<do..end> is a monadic expansion structure, just like in Haskell. Let's
translate it, with type signatures:

  do                          # (int.array, int -> any) -> T int.array
    xs.size.times(f);         # T int
    xs                        # int.array
  end

NB: this won't work. We have arbitrary expression bindings in the middle of this
construct, each of which has the potential to impact the scope. We need
automatic T chaining at the expression level rather than in some type of control
flow construct; i.e. it should be possible to do this:

  (print("hi"), print("there"))

and have the return type be C<T (int, int)>.

=head2 It doesn't matter yet
Let's design the pure end of the language first and then tackle the T type. It
should all just work I think; the notational stuff is just the evaluator finding
implicit functions to bridge type gaps in an otherwise pure world.
=cut
