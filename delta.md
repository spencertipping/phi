# Next steps
1. Build a program that tests all instructions and will verify that a VM is
   implemented correctly
2. Write a formal-ish spec for the [frontend language](phifront.pm)
3. Fix repl tests (right now a bunch fail from the phifront port)

## The `phieval` problem
Ok, basically the current self-reference strategy is fraught with peril. I
suspect it can be fixed to get the current tests to pass, but even then we'll
have problems with side-effect timing. Let's go through some ways to deal with
this.

1. Implement mutable state cells for variables
2. Implement an "access but don't impact timelines" op type for caching

I think those are the only two options.

## Non-timeline op wrapper
Initially, I really like this approach. Let's see how well it holds up, for
instance on something like `let x = print(3+4)+1 in x + 1`. The optree here
would be:

```
print_expr = (+ (print (+ 3 4)) 1)
(seqr
  print_expr
  (+ (no-timelines print_expr) 1))
```

So far so good. Now, we may have a problem around GC: how do we know the result
value from `print_expr` can't be reclaimed eagerly? (Maybe it's a general GC
problem, then again: trace the nodes' parse-result lifetimes to determine the
right caching strategy.)
