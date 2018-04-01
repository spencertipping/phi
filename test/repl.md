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

## Variable assignment
```bash
$ { echo "x = 3 + 4"; echo "x + 1"; } | test/repl -n 2>/dev/null
7
8
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

## Closures
```bash
$ test/repl -n <<<'let adder = (x -> (y -> x + y)) in
                   let f     = adder 5 in
                   f 6' 2>/dev/null
11
```
