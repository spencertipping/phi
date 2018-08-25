# Writing directly in bytecode, which you probably won't want to do
This exists to make it easier to tinker with the concatenative layer of phi.
It inherits the awkward and capricious [`bin()`
notation](../phi0/image.pm#L267), which is awful and slow.

From the [phi root directory](..) you should be able to do this:

```sh
$ ./phi1i phi1-examples/hello-world.phi
```

## WTF is this?
Well, it's a mixture of `bin()` macros, hex bytes, and bytecode instructions.
`lit8` and `lit32` are the latter. You can find a full list of bytecode
instructions [in their machine code
implementations](../phi0/interpreter.pm#L63).

## Quick guide to instructions
OK, so you've got a stack and you're running bytecode. Let's talk about what you
can do. The first instruction should probably push a value onto the stack, and
there are a few that do that:

- `lit8 XX`: push hex literal `0xXX`
- `lit16 XX YY`: push hex literal `0xXXYY`
- `lit32 XX YY ZZ AA`: push hex literal `0xXXYYZZAA`
- `lit64 XX YY ZZ AA BB CC DD EE`: you can probably guess

There's a `bin` shorthand, `=X`, for numbers between 0 and 127 that compiles
down to `lit8 XX` for you. So you can write `=23` to push the decimal number
`23` onto the stack. I use this a lot in the phi1 source.

These are how you get numbers, which everything in phi ultimately is. For
example, you can add a couple of numbers like this:

```
lit8 05                         # 5
lit8 09                         # 5 9
iplus                           # 14
```

`iplus` means "integer plus": it adds stack cells interpreted as integers.
There's a bunch of integer operators, most of which begin with `i`:

- `iplus`
- `itimes`
- `idivmod` (pushes the quotient first, then remainder)
- `ishl` (left shift)
- `isar` (signed right shift)
- `ishr` (unsigned right shift)
- `iand` (bitwise and)
- `ior` (bitwise or)
- `ixor` (bitwise xor)
- `ilt` (less-than)
- `ieq` (equality comparison)
- `iinv` (bitwise inversion, like C's `~`)
- `ineg` (negative)

phi also has three operators that swap byte orderings between big and little
endian:

- `bswap16`
- `bswap32`
- `bswap64`

You don't typically need these unless you're addressing memory in a portable
way, for instance writing bytecode that itself writes bytecode.
