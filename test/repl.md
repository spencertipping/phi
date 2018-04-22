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

## Cons accessors
```bash
$ test/repl -n <<<'(3::[]).h' 2>/dev/null
3
$ test/repl -n <<<'(3::19).t' 2>/dev/null
19
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

Let's also test right-hand function calls:

```bash
$ test/repl -n <<<'(\x -> x::1) ((\x -> x::2) 3)' 2>/dev/null
((3 :: 2) :: 1)
```

### Functions as values
```bash
$ test/repl -n <<<'(\x -> x.h 17) ((\x -> x + 100)::[])' 2>/dev/null
117
$ test/repl -n <<<'(\x -> \y -> x.h 17) ((\x -> x + 100)::[]) 333' 2>/dev/null
117
```

## Capture
```bash
$ test/repl -n <<<'(\x -> \y -> 1) 3 4' 2>/dev/null
1
$ test/repl -n <<<'(\x -> \y -> x::y) 3 4' 2>/dev/null
(3 :: 4)
$ test/repl -n <<<'(\x -> \y -> x + y + 1) 3 4' 2>/dev/null
8
$ test/repl -n <<<'(\x -> \y -> x + y+y + 1) 3 4' 2>/dev/null
12
$ test/repl -n <<<'(\x -> \y -> \z -> x::y::z::[]) 3 4 5' 2>/dev/null
(3 :: (4 :: (5 :: nil)))
```

## Let-binding backend
```bash
$ test/repl -n <<<'(\x -> \y -> x.h y) ((\z -> z + 1)::[]) 4' 2>/dev/null
5
$ test/repl -n <<<'(\x -> \y -> x.h y) ((\x -> x + 1)::[]) 4' 2>/dev/null
5
```

## Function composition
First a couple of sanity checks:

```bash
$ test/repl -n <<<'(\f -> \g -> \x -> f.h x) ((\y -> y::2)::[]) ((\z -> z::1)::[]) 5' 2>/dev/null
(5 :: 2)
$ test/repl -n <<<'(\f -> \g -> f.h (g.h 1)) ((\y -> y::2)::[]) ((\z -> z::3)::[])' 2>/dev/null
((1 :: 3) :: 2)
```

```bash
$ test/repl -n <<<'(\f -> \g -> \x -> f.h (g.h x)) ((\y -> y::2)::[]) ((\z -> z::1)::[]) 5' 2>/dev/null
((5 :: 1) :: 2)
$ test/repl -n <<<'(\f -> \g -> \x -> f.h (g.h x)) ((\x -> x::2)::[]) ((\x -> x::1)::[]) 5' 2>/dev/null
((5 :: 1) :: 2)
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
