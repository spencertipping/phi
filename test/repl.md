# REPL-driven tests
```bash
$ test/repl -n <<<'3'
3
$ test/repl -n <<<'(3)'
3
$ test/repl -n <<<'3 + 4'
7
$ test/repl -n <<<'3 + 4 * 5'
23
$ test/repl -n <<<'3 * 4 + 5'
17
$ test/repl -n <<<'(3 + 4) * 5'
35
```

## Unowned operators
```bash
$ test/repl -n <<<'3::[]' 2>/dev/null
(3 :: nil)
$ test/repl -n <<<'4; 5' 2>/dev/null
5
$ test/repl -n <<<'1+2::3+4;5+6::7+8::[]' 2>/dev/null
(11 :: (15 :: nil))
```

## Functions
```bash
$ test/repl -n <<<'(\x->x+1) 5' 2>/dev/null
6
$ test/repl -n <<<'(\x -> x + 1) 5' 2>/dev/null
6
$ test/repl -n <<<'(\x -> (\y -> x + y)) 3 4' 2>/dev/null
7
$ test/repl -n <<<'(\x -> \y -> x + y) 3 4' 2>/dev/null
7
```

## Capture
```bash
$ test/repl -n <<<'(\x -> \y -> 1) 3 4' 2>/dev/null
1
$ test/repl -n <<<'(\x -> \y -> x + y + 1) 3 4' 2>/dev/null
8
$ test/repl -n <<<'(\x -> \y -> x + y+y + 1) 3 4' 2>/dev/null
12
```

## Let-binding backend
```bash
$ test/repl -n <<<'(\x -> \y -> x y) (\z -> z + 1) 4' 2>/dev/null
5
$ test/repl -n <<<'(\x -> \y -> x y) (\x -> x + 1) 4' 2>/dev/null
5
```

## Function composition
```bash
$ test/repl -n <<<'(\f -> (\g -> (\x -> f (g x)))) (\x -> x + x) (\x -> x + 1) 5'
12
$ test/repl -n <<<'(\f -> \g -> \x -> f (g x)) (\x -> x + x) (\x -> x + 1) 5'
12
```

## Symbol parsing
```bash
$ test/repl -n <<<'(\x -> \xs -> xs) 5 7' 2>/dev/null
7
$ test/repl -n <<<'(\x -> \xs -> xs + 1) 5 7' 2>/dev/null
8
```

## Shadowing
```bash
$ test/repl -n <<<'(\x -> \x -> x) 5 7' 2>/dev/null
7
$ test/repl -n <<<'(\x -> \x -> x + 1) 5 7' 2>/dev/null
8
```
