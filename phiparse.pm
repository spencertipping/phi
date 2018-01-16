=head1 Parser library for concatenative phi
This is the first step for bootstrapping phi into a "regular" language; that is,
one with applicative syntax. These parsers are designed to operate both on
strings and on lists (and any other data structure, really). That way you can
define phi's evaluation semantics in terms of parsers, which is used in backend
compilers.

The calling convention works like this:

  [parser] . :: state -> error? []            # failure
                       | result state'        # success

As far as higher-order parsers like C<seq> and C<alt> are concerned, C<state>
can be any value. The equations treat it as opaque:

  seq(a, b) :: state -> let r1 state'  = a state in
                        let r2 state'' = b state' in
                        [r1 r2] state''

  alt(a, b) :: state -> let r1 state' = a state in
                        state' == [] ? b state : r1 state'

=head2 Specifying parsers
Parsers are just functions that close over their required arguments. For
example, the grammar C<"foo" | "bar"> might be specified like this:

  [[["foo" str] ["bar" str]] alt]

Both C<alt> and C<seq> operate on arbitrarily large lists rather than being
limited to pairs.
=cut

package phiparse;
use phiboot;
use phibootmacros;


=head2 C<reverse> implementation
We need to be able to efficiently reverse a list, which is pretty
straightforward:

  xs rev  = xs [] rev'

  [x xs...] [ys...]  rev' = [xs...] [x ys...] rev'
  []        [ys...]  rev' = [ys...]

Concatenative derivation:

  [xs...] [ys...]  swap dup type 'nil sym=  = [ys...] [xs...] <1|0>

  [ys...] []         drop
  [ys...] [x xs...]  uncons [0 2 1] 3 restack cons rev'

=cut

use constant rev1_mut => pmut;
use constant rev1 => l
  swap, dup, i_type, lit psym 'nil', i_symeq,
    l(drop),
    l(i_uncons, l(3, 0, 2, 1), i_uncons, i_restack, i_cons, rev1_mut, i_eval),
    if_;

rev1_mut->set(rev1);

use constant rev => l pnil, rev1, i_eval;


=head2 C<seq> parser implementation
C<seq> takes a list of parsers and applies each one, consing up a list of
results. The equations are:

  <state> [ps...] seq = [] <state> [ps...] seq'

  [rs...] <state> []        seq' = reverse([rs...]), <state>
  [rs...] <state> [p ps...] seq' = match <state> p with
    | e []       -> e []
    | r <state'> -> [r rs...] <state'> [ps...] seq'

Concatenative derivation:

  <state> [ps...]     [] rot3> seq'               = [] <state> [ps...] seq'

  [rs...] <state> xs  dup type 'nil sym=          = [rs...] <state> xs <0|1>
  [rs...] <state> xs  drop swap reverse swap      = reverse([rs...]), <state>

  [rs...] <state> [p ps...]  uncons rot3< swap .  = [rs...] [ps...] (<state> p)
  [rs...] [ps...] r|e s|[]   dup type 'nil sym=   = [rs...] [ps...] r|e s|[] 0|1

    [rs...] [ps...] e []     [0 1] 4 restack      = e []

    [rs...] [ps...] r s      [1 3 2 0] 4 restack  = s [ps...] [rs...] r
    s [ps...] [rs...] r      cons rot3> seq'      = [r rs...] s [ps...] seq'

=cut

use constant seq1_mut => pmut;
use constant seq1 => l
  dup, i_type, lit psym 'nil', i_symeq,
    l(drop, swap, rev->unlist, swap),
    l(i_uncons, rot3l, swap, i_eval, dup, i_type, lit psym 'nil', i_symeq,
      l(l(4, 0, 1), i_uncons, i_restack),
      l(l(4, 1, 3, 2, 0), i_uncons, i_restack, i_cons, rot3r, seq1_mut, i_eval),
      if_),
    if_;

seq1_mut->set(seq1);

use constant seq => l pnil, rot3r, seq1, i_eval;


=head2 C<rep> parser implementation
C<rep> repeats a parser as long as it matches, forming a list of results. It
works a lot like C<seq> internally. Equations:

  <state> p rep     = <state> p [] rep'
  <state> p rs rep' = match (<state> p) with
    | r s' -> s' p (r:rs) rep'
    | e [] -> reverse(rs) <state>

Concatenative derivation:

  <state> p [rs...]            [1 2] 0 restack .   = ... (<state> p)
  <state> p [rs...] r|e s'|[]  dup type 'nil sym=  = ... r|e s'|[] <1|0>

    <state> p [rs...] r s'  [1 2 3 0] 5 restack    = s' p [rs...] r
    s' p [rs...] r          cons rep'

    <state> p [rs...] e []  [2 4] 5 restack rev swap  = reverse([rs...]) <state>

=cut

use constant rep1_mut => pmut;
use constant rep1 => l
  l(0, 1, 2), i_uncons, i_restack, i_eval, dup, i_type, lit psym 'nil', i_symeq,
    l(l(5, 2, 4), i_uncons, i_restack, rev, i_eval, swap),
    l(l(5, 1, 2, 3, 0), i_uncons, i_restack, i_cons, rep1_mut, i_eval),
  if_;

rep1_mut->set(rep1);

use constant rep => l pnil, rep1, i_eval;


=head2 C<alt> parser implementation
C<alt> takes a series of parsers and returns the first one whose continuation is
non-nil. Unlike C<seq>, this function is directly recursive; we don't need any
auxiliary storage. Equations:

  <state> []        alt  = [] []
  <state> [p ps...] alt  = match <state> p with
    | e [] -> <state> [ps...] alt
    | r s' -> r s'

Concatenative derivation:

  <state> []                 swap drop dup        = [] []
  <state> [p ps...]          uncons rot3< dup     = [ps...] p <state> <state>
  [ps...] p <state> <state>  rot3< .              = [ps...] <state> (<state> p)

    [ps...] <state> e []     drop drop swap alt   = <state> [ps...] alt
    [ps...] <state> r s'     [0 1] 4 restack      = r s'

=cut

use constant alt_mut => pmut;
use constant alt => l
  dup, i_type, lit psym 'nil', i_symeq,
    l(swap, drop, dup),
    l(i_uncons, rot3l, dup, rot3l, i_eval, dup, i_type, lit psym 'nil', i_symeq,
      l(drop, drop, swap, alt_mut, i_eval),
      l(l(4, 0, 1), i_uncons, i_restack),
      if_),
    if_;

alt_mut->set(alt);


=head2 C<flatmap> parser implementation
C<flatmap> allows you to create computed grammars. That is, if one parser
succeeds, we transform its result to form the follow-on parser. That follow-on
parser then picks up where the first one left off.

Put another way, a flatmap is just a computed C<seq> with a C<map> to combine
the two outputs. Here's how it works:

  <state> [p] [c] [f] flatmap  = let r s'   = <state> p in
                                 let p'     = r f in
                                 let r' s'' = s' p' in
                                 (r r' c) s''

Equation:

  <state> [p] [c] [f] flatmap = match <state> p with
    | e [] -> e []
    | r s' -> match s' (r f) with
                | e  []  -> e []
                | r' s'' -> (r r' c) s''

Concatenative derivation:

  <state> [p] [c] [f]  [2 3 0 1] 4 restack .  = [c] [f] (<state> p)

    [c] [f] e []       [0 1] 4 restack        = e []

    [c] [f] r s'       [2 1 0 1] 3 restack .  = [c] r s' (r f)
    [c] r s' [p']      .                      = [c] r r'|e s''|[]

      [c] r e []       [0 1] 4 restack        = e []
      [c] r r' s''     [3 1 2 0] 4 restack .  = s'' (r r' c)
      s'' (r r' c)     swap                   = (r r' c) s''

=cut

use constant flatmap => l
  l(4, 2, 3, 0, 1), i_uncons, i_restack, i_eval,
  dup, i_type, lit psym 'nil', i_symeq,
    l(l(4, 0, 1), i_uncons, i_restack),
    l(l(3, 2, 1, 0, 1), i_uncons, i_restack, i_eval, i_eval,
      dup, i_type, lit psym 'nil', i_symeq,
      l(l(4, 0, 1), i_uncons, i_restack),
      l(l(4, 3, 1, 2, 0), i_uncons, i_restack, i_eval, swap),
      if_),
    if_;


=head2 C<str> parser implementation
This is a bit of a departure from the above in that it's bound to a string parse
state. String parse states look like C<[str index stuff...]>. Equations:

  [s i stuff...] "text" str = [stuff...] "text" s i 0 str'

  [stuff...] s2 s1 i1 i2 str' = i2 < s2.length
    ? i1 < s1.length
      ? s1[i1] == s2[i2]
        ? s2 s1 (i1+1) (i2+1) str'
        : "text" []
      : "text" []
    : "text" [s1 i1 stuff...]

C<stuff...> is a way for you to store auxiliary information in the parse state;
the applicative grammar uses it to keep track of the current stack layout.

Concatenative derivation:

  [s i xs...] "text"  swap uncons swap uncons swap         = "text" s i [xs...]
  s2 s1 i1 [xs...]    [1 2 3 0] 4 restack 0 str'

  [xs...] s2 s1 i1 i2    [3 0] 0 restack slen swap <       = ... i2s2
    [xs...] s2 s1 i1 i2  [2 1] 0 restack slen swap <       = ... i1s1
      ...                [2 1 3 0] 0 restack sget          = ... i2 s2 s1[i1]
      ... i2 s2 s1[i1]   rot3> sget xor not                = ... s1[i1]==s2[i2]

        [xs...] s2 s1 i1 i2  1 + swap 1 + swap str'
        [xs...] s2 s1 i1 i2  [3] 5 restack pnil            = s2 []

    [xs...] s2 s1 i1 i2  [1 4 2 3] 5 restack               = s2 s1 [xs...] i1
    s2 s1 [xs...] i1     cons swap cons                    = s2 [s1 i1 xs...]

=cut

use constant str1_mut => pmut;
use constant str1 => l
  l(0, 3, 0),     i_uncons, i_restack, i_slen, swap, i_lt,
    l(l(0, 2, 1), i_uncons, i_restack, i_slen, swap, i_lt,
        l(l(0, 2, 1, 3, 0), i_uncons, i_restack, i_sget, rot3r, i_sget,
          i_xor, i_not,
            l(lit 1, i_plus, swap, lit 1, i_plus, swap, str1_mut, i_eval),
            l(l(5, 3), i_uncons, i_restack, pnil),
            if_),
        l(l(3), i_uncons, i_restack, pnil),
        if_),
    l(l(5, 1, 4, 2, 3), i_uncons, i_restack, i_cons, swap, i_cons),
    if_;

str1_mut->set(str1);

use constant str => l
  swap, i_uncons, swap, i_uncons, swap, l(4, 1, 2, 3, 0), i_uncons, i_restack,
  lit pint 0, str1, i_eval;


=head2 C<contains> implementation
We need to know whether a string contains a given character.

  cs c   contains  = cs c 0 contains'
  cs c i contains' = i < cs.length
    ? cs[i] == c
      ? 1
      : cs c (i+1) contains'
    : 0

Concatenative derivation:

  cs c i    [2 0] 0 restack slen swap <     = cs c i (i<csl)
    cs c i  [2 0 1] 0 restack sget xor not  = cs c i (cs[i]==c)

    cs c i  [] 3 restack 1                  = 1

=cut

use constant contains1_mut => pmut;
use constant contains1 => l
  l(0, 2, 0), i_uncons, i_restack, i_slen, swap, i_lt,
    l(l(0, 2, 0, 1), i_uncons, i_restack, i_sget, i_xor, i_not,
        l(l(3), i_uncons, i_restack, lit 1),
        l(lit 1, i_plus, contains1_mut, i_eval),
      if_),
    l(l(3), i_uncons, i_restack, lit 0),
  if_;

contains1_mut->set(contains1);

use constant contains => l(lit 0, contains1, i_eval);


=head2 C<oneof> parser implementation
This one lets you either accept or reject any of a set of characters, stored as
a string representing the list. You also specify whether you want inclusion or
exclusion. Equation:

  [s i xs...] cs <1|0> oneof = i < s.length
    ? cs.contains(s[i]) == <1|0>
      ? s[i] [s i+1 xs...]
      : cs   []
    : cs []

Concatenative derivation:

  [s i xs...] cs <1|0>  rot3< uncons swap uncons swap  = cs <1|0> s i [xs...]
  cs <1|0> s i [xs...]  [1 2 3 4 0] 5 restack          = [xs...] cs <1|0> s i

  xs cs <1|0> s i    [1 0] 0 restack slen swap <      = xs cs <1|0> s i (i<sl)
    xs cs <1|0> s i  [1 0 3 2 0 1] 3 restack sget     = xs cs s i <1|0> cs s[i]
    xs cs s i <1|0> cs s[i]  contains xor not         = xs cs s i contains?

    xs cs s i        [1 0 0 1] 3 restack sget rot3>   = xs s[i] s i
    xs s[i] s i      1 + [0 3 1 2] 4 restack cons swap cons

    xs cs s i        drop drop swap drop []           = cs []
  xs cs <1|0> s i    drop drop drop swap drop []      = cs []

=cut

use constant oneof => l
  rot3l, i_uncons, swap, i_uncons, swap,
  l(5, 1, 2, 3, 4, 0), i_uncons, i_restack,
  l(0, 1, 0),          i_uncons, i_restack,
  i_slen, swap, i_lt,
    l(l(3, 1, 0, 3, 2, 0, 1), i_uncons, i_restack, i_sget, contains, i_eval,
      i_xor, i_not,
        l(l(3, 1, 0, 0, 1), i_uncons, i_restack, i_sget, rot3r,
          lit 1, i_plus, l(4, 0, 3, 1, 2), i_uncons, i_restack,
          i_cons, swap, i_cons),
        l(drop, drop, swap, drop, pnil),
      if_),
    l(drop, drop, drop, swap, drop, pnil),
    if_;


=head2 C<map> parser implementation
C<map> lets you transform the result of a parser, but only calls your function
if the parser succeeds. Equation:

  <state> [f] [p] map = match (<state> p) with
    | r s' -> (r f) s'
    | e [] -> e []

Concatenative derivation:

  <state> [f] [p]  [0 2 1] 3 restack .      = [f] (<state> p)
  [f] (<state> p)  dup type 'nil symeq      = [f] r|e s'|[] <1|0>
    [f] r s'       rot3> swap . swap        = (r f) s'
    [f] e []       rot3< drop               = e []

=cut

use constant pmap => l
  l(3, 0, 2, 1), i_uncons, i_restack, i_eval, dup, i_type, lit psym 'nil',
  i_symeq,
    l(rot3l, drop),
    l(rot3r, swap, i_eval, swap),
  if_;


=head2 C<filter> parser implementation
C<filter> lets you computationally reject parse results. Equation:

  <state> [f] [p] filter = match (<state> p) with
    | r s' -> r f ? r s' : r []
    | e [] -> e []

Concatenative derivation:

  <state> [f] [p]  [0 2 1] 3 restack .      = [f] (<state> p)
  [f] (<state> p)  dup type 'nil symeq      = [f] r|e s'|[] <1|0>
    [f] r s'       [2 1 1 0] 3 restack .    = s' r (r f)
      s' r         swap                     = r s'
      s' r         swap drop pnil           = r []
    [r] e []       rot3< drop               = e []

=cut

use constant pfilter => l
  l(3, 0, 2, 1), i_uncons, i_restack, i_eval, dup, i_type, lit psym 'nil',
  i_symeq,
    l(rot3l, drop),
    l(l(3, 2, 1, 1, 0), i_uncons, i_restack, i_eval,
        l(swap),
        l(swap, drop, pnil),
      if_),
  if_;


1;
