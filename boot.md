# Bootstrap process
phi comes into existence in three layers, each of which creates the next:

1. `phi0`: written in perl, produces `phi1` in AMD64 native code
2. `phi1`: linked with a simplistic OOP protocol and with no GC, produces `phi2`
3. `phi2`: GC support and efficient OOP/vtable encoding, self-reproducing

Here's what the bootstrap process looks like:

```bash
$ ./phi0.pl > phi1
$ chmod 0755 phi1
$ ./phi1 | tee phi2
$ chmod 0755 phi2
```

Also see [./build](./build) for [custom build flags](phi0.pl#L55) and debugging
outputs (which aren't committed into the repo).
