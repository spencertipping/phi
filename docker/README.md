# Development docker images
You shouldn't have to install anything on your system to work on phi (although
you might find it more convenient to). Roughly speaking, here's my setup on top
of Ubuntu 18.04:

- `apt install perl nasm`
- `git clone git://github.com/spencertipping/ni` with
  [installation](https://github.com/spencertipping/ni#getting-started)

That should get you up and running if your system is AMD64 (x86-64). `nasm` is
just for debugging; all you really need is Perl 5.14 or later.
