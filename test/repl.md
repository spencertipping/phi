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
$ test/repl -n <<<'4; 5'
5
$ test/repl -n <<<'1+2::3+4;5+6::7+8::[]'
(11 :: (15 :: nil))
```

## Variable assignment
```bash
$ { echo "x = 3 + 4"; echo "x + 1"; } | test/repl -n
7
8
```

## Functions
```bash
$ test/repl -n <<<'inc @ 4'
5
$ test/repl -n <<<'(x->x+1) @ 5'
6
```

```bash
$ test/repl -n <<<'(x -> x + 1) @ 5' 2>/dev/null
6
$ test/repl -n <<<'((x -> (y -> x + y)) @ 3) @ 4'
7
```

## Closures
```bash
$ test/repl -n <<<'adder = (x -> (y -> x + y)); f = adder @ 5; f @ 6'
11
```
