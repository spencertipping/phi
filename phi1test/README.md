# phi1 unit tests
Sanity checks for various aspects of phi1 functionality.

## Syscalls
This is the first thing that needs to work; otherwise `write`, `mmap`, and
pretty much everything will fail horribly.

```bash
$ phi1test/syscall-native
phi1 is a thing
```

## Method calls
```bash
$ phi1test/method-calls
baseptr method calls work
hereptr method calls work
```
