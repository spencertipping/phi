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

## Bindings
```bash
$ test/repl -n <<<'\x = 5; x + 1' 2>/dev/null
6
$ test/repl -n <<<'\f = \x -> x + 1; f 5' 2>/dev/null
6
```

## Closures
```bash
$ test/repl -n <<<'\adder = \x -> \y -> x + y; \f = adder 5; f 6' 2>/dev/null
11
```
