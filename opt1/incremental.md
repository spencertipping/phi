# Incremental optimization
Let's look at optimization in a much simpler and more naive way.

Most code isn't complex: it wasn't generated via JIT, it doesn't have insane and
rapidly-changing dynamic patterns, etc. If it did, we'd be more allocation-bound
than execution-bound. Instead, most code is relatively static, which means that
inasmuch as we have slow stuff, we have the same slow thing many times.

This, of course, is wonderful because it means we can profile the continuation
stack _at runtime_ and find common patterns. Then we can re-specialize the
interpreter to handle those cases.

...in other words, we don't have to be clever at all; we can just inline stuff
that gets repeated a lot.

## Quick experiment: let's get an instruction trace
I added the `-t` option to `phii` to emit each instruction by number. This isn't
a complete trace because we have stuff like `restack` arguments, but it's a
start.

I'm assembling the stream into basic blocks by reading until we get `12` (`if`)
or `2` (`eval`). `eval` against a constant list, of course, is still within a
basic block. (This model is conservative: you could `restack` a constant into
place before `eval` and it would still be a basic block, but I assume that
doesn't happen much -- which I think is correct given how I've written the
code.)

```sh
$ ./phii -t test/boot.phi \
  | ni r/^INSN/fBp'my $is_const = 0;
                   r ru { my $branch = /^12$/ || !$is_const && /^2$/;
                          $is_const = /^[CN]/;
                          $branch }, rl' z \>insn-trace
```

Awesome, let's find out where our branching tends to come from. I'm expecting
way more conditional branching than function polymorphism:

```sh
$ ni insn-trace p'final F_' UO
7124336 12
4270934 2
```

Interesting, those numbers aren't as far apart as I had anticipated. That's good
to know.

```sh
$ ni insn-trace S4pF_ UOrp'a>E6'        # single instruction counts
23620225        7
15252808        6
7239984 C1019660150
7124336 12
6126707 2
6012741 39
4267098 C294630982
3615206 C84844153
3080488 3
3077404 C849518754
3059812 C819549450
2968724 C643001188
2927822 C922771466
2927822 C675312657
2927822 C584178244
2927822 C577485257
2124691 C1018614307
2048957 5
```

Some of the lists that come up a lot:

```sh
$ ./phiml --lookup 1019660150 < test/images/phii
(2 :: (0 :: nil))
$ ./phiml --lookup 294630982 < test/images/phii
(1 :: nil)
$ ./phiml --lookup 84844153 < test/images/phii
(0 :: (0 :: nil))
$ ./phiml --lookup 849518754 < test/images/phii
('nil :: nil)
```

### `eval` polymorphism
```sh
$ ni insn-trace rp'/\t2$/' p'F_ FM-1' UOrp'a>E4'
```

### Basic block profile
Let's get distribution entropy, absolute count, and mean length.

```sh
$ ni insn-trace Ue'wc -l'
11763                                   # uniform entropy would be ~13.5 bits
$ ni insn-trace UfAp'entropy rw{1}'
4.42113886813104                        # massive compression
$ ni insn-trace pFM+1 ,ar+1
10.1693344546172                        # average basic block length, in #insns
```

### Conditional entropy
We have a trace of basic blocks in their proper runtime order, so we should be
able to ask about the follower for each one: that is, how did the condition
_actually_ resolve. Let's facet by call site and look at the distribution of
callee entropy.

```sh
$ ni insn-trace F^Cp'r pl 2' UOxgp'r entropy b_ rea' ,q.01 ocx G:W%l
```

![image](http://pix.toile-libre.org/upload/original/1525344502.png)

_Interesting_: most of these are quantizing out to zero, which means either that
my heuristic is bogus, or that we're getting some very biased conditions. Let's
check that:

```sh
$ ni insn-trace F^Cp'r pl 2' UOxgp'r a, entropy b_ rea' rp'!b' e'wc -l'
11242
```

OK, the heuristic is completely bogus. That's good to know.

...so in our instruction trace, how many real conditions are there? (521 call
sites, but I want to know how many decisions were made.)

```sh
$ ni insn-trace F^Cp'r pl 2' UOxgp'my @ls = rea; @ls > 1 ? r sum b_ @ls : ()' \
     ,sr+1
7703805
```

Aha, there we go. So a few call sites are being hit a bunch of times and
therefore owning a bunch of decisions.
