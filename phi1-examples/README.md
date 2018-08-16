# Writing directly in bytecode, which you probably won't want to do
This exists to make it easier to tinker with the concatenative layer of phi.
It inherits the awkward and capricious [`bin()`
notation](../phi0/image.pm#L267), which is awful and slow.

From the [phi root directory](..) you should be able to do this:

```sh
$ ./phi1i phi1-examples/hello-world.phi
```
