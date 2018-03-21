# Node-based evaluation
phi evaluates while it parses, so it's worth thinking about this a bit.

Let's start with what the parsers produce normally. As operators fold up, we
have various combinations of LHS/RHS with named ops:

```
lhs rhs <some-combiner> -> <some-value>
```

...so parsers need to lift everything into roughly the same space of values.

## Simple case: no partial evaluation

