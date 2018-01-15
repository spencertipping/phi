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

1;
