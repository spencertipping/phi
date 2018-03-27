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
```

```sh
# this test fails due to the scope escaping problem
$ test/repl -n <<<'(x -> x + 1) @ 5' 2>/dev/null
6
```
