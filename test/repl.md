# REPL-driven tests
```bash
$ test/repl -n <<<'3'
3
$ test/repl -n <<<'3 + 4'
7
$ test/repl -n <<<'3 + 4 * 5'
23
$ test/repl -n <<<'3 * 4 + 5'
17
```

## Variable assignment
```bash
$ { echo "x = 3 + 4"; echo "x + 1"; } | test/repl -n
7
8
```
