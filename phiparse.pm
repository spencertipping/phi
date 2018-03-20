=head1 Parser library for concatenative phi
This is the first step for bootstrapping phi into a "regular" language; that is,
one with applicative syntax. These parsers are designed to operate both on
strings and on lists (and any other data structure, really). That way you can
define phi's evaluation semantics in terms of parsers, which is used in backend
compilers.


=head2 Object protocol
Parsers and parse states are objects. This makes it much easier to manage
abstraction; for instance, you might have a basic string as input, or you might
have a line-array from an editor; either way, if you surface the same API then
the same set of string parsers will work over either one.

Parsers have a simple protocol:

  parser.parse(state) -> state'

Parse states have a similarly straightforward core protocol:

  state.is_error      -> 1|0            # 1 = parse failed, 0 = ok
  state.value         -> v              # value or error
  state.with_value(v) -> state'         # change a state's value

String parse states support three more methods:

  state.offset     -> n                 # current offset into the string
  state.length     -> n                 # total string length
  state.at(i)      -> charcode          # ascii value, e.g. from sget
  state.consume(n) -> state'            # move the offset forward

Parsers will never create non-error parse states; instead, they use
C<with_value()> and C<consume()> to create derivatives. This is important
because parse states are allowed to carry arbitrary extra data, and parsers need
to preserve that if the parse succeeds.


=head2 Parser generality
Only the lowest-level parsers care about the specifics of the parse state.
Higher-order parsers like C<alt>, C<seq>, and C<rep> limit themselves to the
core protocol, which makes it possible to use them for non-string, and even
non-linear, inputs.
=cut


package phiparse;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phiobj;

our @EXPORT =
our @EXPORT_OK = qw/ seq_ rep_ alt_ maybe_ flatmap_ str_ oneof_ map_ /;


=head2 Basic parse states
One for strings and a generic failure state.
=cut

use phitype fail_state_type =>
  bind(is_error => drop, lit 1),
  bind(value    => isget 0);

use phitype string_state_type =>
  bind(is_error => drop, lit 0),

  bind(value       => isget 0),
  bind(offset      => isget 1),
  bind(length      => isget 2),
  bind(string      => isget 3),
  bind(with_value  => isset 0),
  bind(with_offset => isset 1),

  bind(at => mcall"string", i_sget),

  bind(consume =>                       # n self
    dup, mcall"offset",                 # n self offset
    rot3l, i_plus, swap,                # n+offset self
    mcall"with_offset");


use phi fail_state => l                 # error-value
  pnil, swons, fail_state_type, swons;


=head2 C<seq> parser
A list of parsers to parse in a sequence.
=cut

use phi seq1_mut => pmut;
use phi seq1 => l                       # state r ps
  dup, nilp,
  l(                                    # state r []
    drop,                               # state r
    philist::rev, i_eval,               # state rev(r)
    swap, mcall"with_value"),           # state'
  l(                                    # state r p::ps'
    i_uncons, stack(0, 3),              # state r ps' p state
    swap, mcall"parse",                 # state r ps' state'
    dup, mcall"is_error",               # state r ps' state' e?
    l(stack(3, 0)),                     # state'
    l(                                  # state r ps' state'
      dup, mcall"value",                # state r ps' state' v
      stack(5, 0, 3, 2, 1),             # state' ps' r v
      i_cons, swap,                     # state' v::r ps'
      seq1_mut, i_eval),                # ... seq'
    if_),
  if_;

seq1_mut->set(seq1);


use phitype seq_type =>
  bind(ps    => isget 0),
  bind(parse =>                         # state self
    mcall"ps",                          # state ps
    pnil, swap, seq1, i_eval);          # state [] ps seq'


sub seq_ { pcons l(l(@_)), seq_type }


=head2 C<rep> parser
C<rep> repeats a parser as long as it matches, forming a list of results. It
works a lot like C<seq> internally. Equations:

  <state> p rep     = <state> p [] rep'
  <state> p rs rep' = match p.parse(<state>) with
    | error -> rs == [] ? error
                        : <state>.with_value(reverse(rs))
    | s' -> s' p (s'.value:rs) rep'

=cut

use phi rep1_mut => pmut;
use phi rep1 => l                       # s p rs
  stack(0, 1, 2),                       # s p rs s p
  mcall"parse",                         # s p rs s'
  dup, mcall"is_error",                 # s p rs s' e?
  l(
    swap, dup, nilp,                    # s p s' rs nil?
    l(stack(4, 1)),                     # s'
    l(                                  # s p s' rs
      philist::rev, i_eval,             # s p s' rev(rs)
      stack(4, 3, 0),                   # rev(rs) s
      mcall"with_value"),
    if_),
  l(                                    # s p rs s'
    dup, mcall"value",                  # s p rs s' v
    rot3l, swons,                       # s p s' rs'
    stack(4, 0, 2, 1),                  # s' p rs'
    rep1_mut, i_eval),
  if_;

rep1_mut->set(rep1);


use phitype rep_type =>
  bind(parser => isget 0),
  bind(parse =>                         # state self
    mcall"parser", pnil,                # s p []
    rep1, i_eval);


sub rep_($) { pcons l(shift), rep_type }


=head2 C<none> parser implementation
C<none> parses nothing and returns nil, successfully. It's really simple:

  none.parse(state) = state.with_value([])
  fail.parse(state) = fail_state([])

=cut

use phitype none_type => bind(parse => drop, pnil, swap, mcall"with_value");
use phitype fail_type => bind(parse => stack(2), pnil, fail_state, i_eval);

use phi none => pcons pnil, none_type;
use phi fail => pcons pnil, fail_type;


=head2 C<alt> parser implementation
C<alt> takes a series of parsers and returns the first one whose continuation
succeeds.
=cut

use phi alt_mut => pmut;
use phi alt => l                        # state ps
  dup, nilp,                            # state ps nil?
  l(stack(2), pnil, fail_state, i_eval),# fail
  l(i_uncons,                           # state ps' p
    stack(0, 2),                        # state ps' p state
    swap, mcall"parse",                 # state ps' s'
    dup, mcall"is_error",               # state ps' s' e?
    l(drop, alt_mut, i_eval),
    l(stack(3, 0)),
    if_),
  if_;

alt_mut->set(alt);


use phitype alt_type =>
  bind(parsers => isget 0),
  bind(parse =>                         # state self
    mcall"parsers", alt, i_eval);


sub alt_ { pcons l(@_), alt_type }


=head2 C<maybe> parser
Parses either the thing, or C<none>.
=cut

use phitype maybe_type =>
  bind(parser => isget 0),
  bind(parse =>                         # state self
    mcall"parser", swap,                # p state
    dup, rot3l,                         # state state p
    mcall"parse",                       # state s'
    dup, mcall"is_error",               # state s' e?
    l(drop, pnil, swap, mcall"with_value"),
    l(stack(2, 0)),                     # s'
    if_);


sub maybe_($) { pcons l(shift), maybe_type }


=head2 C<flatmap> parser implementation
C<flatmap> allows you to create computed grammars. That is, if one parser
succeeds, we transform its result to form the follow-on parser. That follow-on
parser then picks up where the first one left off.

Put another way, a flatmap is just a computed C<seq> with a C<map> to combine
the two outputs. Here's how it works:

  <state> [p] [c] [f] flatmap  = let s'  = p.parse(<state>) in
                                 let p'  = f(s'.value) in
                                 let s'' = p'.parse(s') in
                                 s''.with_value(c(s'.value, s''.value))

Equation:

  <state> [p] [c] [f] flatmap = match p.parse(<state>) with
    | error -> error
    | s'    -> match s'.value.parse(s') with
                 | error -> error
                 | s''   -> s''.with_value(c(s'.value, s''.value))

=cut

use phitype flatmap_type =>
  bind(parser   => isget 0),
  bind(next_fn  => isget 1),
  bind(combiner => isget 2),

  bind(parse =>                         # state self
    dup, mcall"parser",                 # state self p
    rot3l, swap, mcall"parse",          # self s'
    dup, mcall"is_error",               # self s' e?
    l(stack(2, 0)),                     # s'
    l(                                  # self s'
      dup, mcall"value",                # self s' v
      stack(0, 2), mcall"next_fn",      # self s' v f
      i_eval,                           # self s' p'
      stack(1, 0, 1),                   # self s' s' p'
      mcall"parse",                     # self s' s''
      dup, mcall"value",                # self s' s'' v''
      rot3l, mcall"value",              # self s'' v'' v'
      stack(0, 3), mcall"combiner",     # self s'' v'' v' c
      i_eval,                           # self s'' c(...)
      swap, mcall"with_value",          # self s'''
      stack(2, 0)),
    if_);


sub flatmap_($$$) { pcons l(@_), flatmap_type }


=head2 C<str> parser implementation
This is a bit of a departure from the above in that it's bound to a string parse
state. It parses a literal substring from the input.
=cut

use phi str1_mut => pmut;
use phi str1 => l                       # state s i
  swap, dup, i_slen,                    # state i s sl
  stack(0, 2), i_lt,                    # state i s i<sl?
  l(                                    # state i s
    stack(0, 2, 1, 2), mcall"offset",   # state i s state i o
    i_plus, swap, mcall"at",            # state i s c1
    stack(0, 1, 2), i_sget,
    i_xor, i_not,                       # state i s c1==c2
    l(                                  # state i s
      swap, lit 1, i_plus,              # state s i+1
      str1_mut, i_eval),                # ...
    l(                                  # state i s
      stack(3, 0), fail_state, i_eval), # fail
    if_),
  l(                                    # state i s
    stack(3, 0, 2, 0), i_slen,          # s state s.length
    swap, mcall"consume",
    mcall"with_value"),
  if_;

str1_mut->set(str1);


use phitype str_type =>
  bind(string => isget 0),
  bind(parse =>                         # state self
    # Make sure we don't overflow the string
    mcall"string", dup, i_slen,         # state s sl
    rot3l, dup, mcall"offset",          # s sl state o
    swap, dup, mcall"length",           # s sl o state len
    rot3l, i_neg, i_plus,               # s sl state len-o
    rot3l, i_lt,                        # s state len-o<sl?

    l(swap, lit 0, str1, i_eval),
    l(drop, fail_state, i_eval),
    if_);


sub str_($) { pcons l(shift), str_type }


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

use phi contains1_mut => pmut;
use phi contains1 => l
  stack(0, 2, 0), i_slen, swap, i_lt,
    l(stack(0, 2, 0, 1), i_sget, i_xor, i_not,
        l(stack(3), lit 1),
        l(lit 1, i_plus, contains1_mut, i_eval),
      if_),
    l(stack(3), lit 0),
  if_;

contains1_mut->set(contains1);

use phi contains => l(lit 0, contains1, i_eval);


=head2 C<oneof> parser implementation
This one lets you either accept or reject any of a set of characters, stored as
a string representing the list. You also specify whether you want inclusion or
exclusion. Equation:

  [s i xs...] cs <1|0> oneof = i < s.length
    ? cs.contains(s[i]) == <1|0>
      ? s.consume(1).with_value(s[i])
      : fail_state
    : fail_state

=cut

use phitype oneof_type =>
  bind(chars     => isget 0),
  bind(inclusive => isget 1),

  bind(parse =>                         # state self
    swap, dup, mcall"length",           # self state len
    swap, dup, mcall"offset",           # self len state offset
    lit 1, i_plus, rot3l,               # self state offset+1 len
    swap, i_lt,                         # self state offset+1<len?
    l(
      swap, dup, mcall"chars",          # state self cs
      stack(0, 2), dup, mcall"offset",  # state self cs state o
      swap, mcall"at",                  # state self cs c
      stack(0, 0, 1),                   # state self cs c cs c
      contains, i_eval,                 # state self cs c contains?
      stack(0, 3), mcall"inclusive",    # state self cs c contains? inclusive?
      i_xor, i_not,                     # state self cs c contains==inclusive?
      l(                                # state self cs c
        stack(0, 3), mcall"with_value", # state self cs state'
        lit 1, swap, mcall"consume",    # state self cs state''
        stack(4, 0)),                   # state''
      l(                                # state self cs c
        stack(4, 2), fail_state, i_eval),
      if_),
    l(                                  # state self cs
      stack(3, 1), fail_state, i_eval),
    if_);


sub oneof_($$) { pcons l(@_), oneof_type }


=head2 C<map> parser implementation
C<map> lets you transform the result of a parser, but only calls your function
if the parser succeeds.
=cut

use phitype map_type =>
  bind(parser => isget 0),
  bind(fn     => isget 1),

  bind(parse =>                         # state self
    dup, mcall"parser",                 # state self p
    rot3l, swap, mcall"parse",          # self state'
    dup, mcall"is_error",
    l(stack(2, 0)),                     # state'
    l(dup, mcall"value",                # self state' v'
      rot3l, mcall"fn", i_eval,         # state' f(v')
      swap, mcall"with_value"),         # state''
    if_);


sub map_($$) { pcons l(@_), map_type }


1;
