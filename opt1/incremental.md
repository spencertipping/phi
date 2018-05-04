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

Let's count polymorphic vs megamorphic:

```sh
$ ni insn-trace F^Cp'r pl 2' UOxgp'my ($c, @ls) = (a, b_ rea);
                                   my $n = sum @ls;
                                   return r $c, $n, "monomorphic" if @ls == 1;
                                   return r $c, $n, "polymorphic" if @ls == 2;
                                   r $c, $n, "megamorphic:".scalar(@ls)' \
                fBC O

2927822 polymorphic
2114211 megamorphic:3
2114209 monomorphic                     # we should inline this
813613  megamorphic:262
350011  polymorphic
345998  polymorphic
345998  monomorphic                     # ...and this
71903   megamorphic:4710
48915   monomorphic
48915   megamorphic:6
47419   monomorphic
47330   megamorphic:507
47330   megamorphic:501
46549   megamorphic:419
39112   megamorphic:247
35999   monomorphic
33886   polymorphic
33886   monomorphic
33886   monomorphic
...
```

Awesome. Now let's see what these blocks are.

```sh
$ ni insn-trace F^Cp'r pl 2' UOxgp'my ($c, @ls) = (a, b_ rea);
                                   my $n = sum @ls;
                                   return r $c, $n, "monomorphic" if @ls == 1;
                                   return r $c, $n, "polymorphic" if @ls == 2;
                                   r $c, $n, "megamorphic:".scalar(@ls)' \
     fBCA Op'r s/C(\d+)/`.\/phiml --lookup $1 < test\/images\/phii`/egr'

2927822 polymorphic       6,6,(3 :: (3 :: (0 :: (1 :: (2 :: nil))))),7,39,((3 :: (0 :: nil)) :: (7 :: nil)),(((1 :: nil) :: (7 :: nil)) :: (2 :: M[...])),12
2114211 megamorphic:3     ((1 :: nil) :: (7 :: nil)),2
2114209 monomorphic       (1 :: nil),7,(0 :: (0 :: nil)),7,3,('nil :: nil),6,(2 :: (0 :: nil)),7,39,((1 :: nil) :: (7 :: (('failed_to_resolve :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: (65 :: nil))))))),(6 :: (6 :: ((3 :: (3 :: (0 :: (1 :: (2 :: nil))))) :: (7 :: (39 :: (((3 :: (0 :: nil)) :: (7 :: nil)) :: ((((1 :: nil) :: (7 :: nil)) :: (2 :: M[...])) :: (12 :: nil)))))))),12
813613  megamorphic:262   (3 :: (0 :: nil)),7,2
350011  polymorphic       (0 :: (0 :: (2 :: (1 :: nil)))),7,34,23,26,((3 :: nil) :: (7 :: ((1 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: nil)))))),((1 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: (16 :: (M[...] :: (2 :: nil))))))),12
345998  polymorphic       (0 :: (2 :: (0 :: nil))),7,33,(2 :: (1 :: (0 :: nil))),7,25,((0 :: (0 :: (2 :: (1 :: nil)))) :: (7 :: (34 :: (23 :: (26 :: (((3 :: nil) :: (7 :: ((1 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: nil)))))) :: (((1 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: (16 :: (M[...] :: (2 :: nil))))))) :: (12 :: nil)))))))),((3 :: nil) :: (7 :: ((0 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: nil)))))),12
345998  monomorphic       (1 :: nil),6,(2 :: (0 :: nil)),7,16,2
71903   megamorphic:4710  ('string :: nil),6,(3 :: (2 :: (0 :: nil))),7,2
48915   monomorphic       (1 :: nil),6,(2 :: (0 :: nil)),7,17,16,(2 :: (2 :: (0 :: (1 :: nil)))),7,6,(1 :: nil),7,(2 :: (1 :: (0 :: nil))),7,(3 :: (2 :: (0 :: (1 :: nil)))),7,(2 :: (1 :: (0 :: nil))),7,2
48915   megamorphic:6     (0 :: (0 :: nil)),7,((1 :: nil) :: (6 :: ((2 :: (0 :: nil)) :: (7 :: (17 :: (16 :: ((2 :: (2 :: (0 :: (1 :: nil)))) :: (7 :: (6 :: ((1 :: nil) :: (7 :: ((2 :: (1 :: (0 :: nil))) :: (7 :: ((3 :: (2 :: (0 :: (1 :: nil)))) :: (7 :: ((2 :: (1 :: (0 :: nil))) :: (7 :: (M[...] :: (2 :: ((2 :: (1 :: (0 :: nil))) :: (7 :: (6 :: ((2 :: (0 :: nil)) :: (7 :: (5 :: nil))))))))))))))))))))))))),((1 :: nil) :: (7 :: ((2 :: (1 :: (0 :: nil))) :: (7 :: (6 :: ((1 :: nil) :: (7 :: ((2 :: (1 :: (0 :: nil))) :: (7 :: (5 :: nil)))))))))),12
...
```

Alright, let's get into some of these.

#### 2927822 polymorphic
```
6,6,(3 :: (3 :: (0 :: (1 :: (2 :: nil))))),7,
  39,((3 :: (0 :: nil)) :: (7 :: nil)),
  (((1 :: nil) :: (7 :: nil)) :: (2 :: M[...])),12
```

This looks the inner symbol resolver list:

```
                                        # s [[sym b...] bs...]
uncons uncons                           # s [bs...] [b...] sym
[3 3 0 1 2] restack                     # s [bs...] [b...] sym s
sym=                                    # s [bs...] [b...] match?
[[3 0] restack]                         # [b...]
[[[1] restack] . resolver]              # s [bs...] resolve
if                                      # binding
```

If we look at this as a basic-block optimization problem we'll completely miss
the obvious and optimal solution: just memoize the resolver when we have
repeated arguments (which we often will).

...although let's get into that a little bit. We won't derive the truly optimal
prehash/dispatch implementation, nor will we get to a place where we're
speculatively inlining.

**TODO:** figure out what method calls _should_ optimize into; then it will be
easier to figure out how we want to get there

#### 2114211 megamorphic:3
```
((1 :: nil) :: (7 :: nil)),2
```

This is the next-symbol `if` branch of the symbol resolver, just before the tail
call:

```
[[1] restack] .
```

It's not immediately obvious to me why this is megamorphic, but that could be an
artifact of the way I'm building out basic blocks. The continuation from this
particular `.` operation is clearly monomorphic.

#### 2114209 monomorphic
```
(1 :: nil),7,(0 :: (0 :: nil)),7,3,('nil :: nil),6,(2 :: (0 :: nil)),7,
  39,((1 :: nil) :: (7 :: (('failed_to_resolve :: nil) ::
                           (6 :: ((2 :: (0 :: nil)) :: (7 :: (65 :: nil))))))),
  (6 :: (6 :: ((3 :: (3 :: (0 :: (1 :: (2 :: nil))))) ::
               (7 :: (39 :: (((3 :: (0 :: nil)) :: (7 :: nil)) ::
                             ((((1 :: nil) :: (7 :: nil)) :: (2 :: M[...])) ::
                              (12 :: nil)))))))),12
```

This is the full symbol resolver function; the `M[...]` refers back to the whole
list. Here's what it looks like in terms of low-level instructions:

```
# NB: the function begins with [1] restack, but this is an artifact of the fact
# that the recursive case tail-calls into it. In practice the list begins with
# the dup instruction, which is [0 0] restack.

                                        # [[sym b...] bs...]
dup type ['nil]                         # [[sym b...] bs...] 'cons|'nil ['nil]
uncons [2 0] restack                    # [[sym b...] bs...] 'cons|'nil 'nil
sym=                                    # [[sym b...] bs...] is-nil?
[ drop                                  #
  ['failed_to_resolve] uncons           # [] 'failed_to_resolve
  [2 0] restack                         # 'failed_to_resolve
  crash ]                               # <crash>
[ uncons uncons ... ]                   # <the symbol resolver stepper above>
if
```

This `if` is monomorphic because it's acting as an assertion: every method can
be resolved, so we never take the `crash` branch.
