# phi bootstrap process
phi begins with two abstraction layers written in Perl (phi0 and phi1). This
produces the `phi1` native code executable image, which is a compiler for a
subset of phi2 source.

```bash
$ rm -f phi1.elf && ./phi1.pl > phi1.elf && chmod 755 phi1.elf
$ ./phi1.elf                            # should run successfully
phi1 is a thing
```
