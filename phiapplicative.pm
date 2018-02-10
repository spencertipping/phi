=head1 Applicative grammar for phi
Writing concatenative code is miserable. I'd much rather use something with
ocaml or python syntax than have to keep track of the stack state all the time.
So I'm going to write a self-hosting grammar for phi that compiles applicative
to concatenative code.

=head2 How this works
I think it's simple: store a stack offset for each abstract value. Then restack
when we need it for concatenative. Use the stack the way it's used in C, except
relative to C<%rsp> not C<%rbp>. This doesn't handle lexical closures, but we
can simply copy the whole stack and send it in, then let the abstract-value
layer sort out which ones actually get used.
=cut

package phiapplicative;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philocal;
use phiparse;

our @EXPORT =
our @EXPORT_OK = qw/ line_comment any_whitespace ignore /;


=head2 Base syntax definitions
Stuff like whitespace, comments, etc. It's worth having these ready.
=cut

use constant line_comment => l
  l(l(pstr "#", phiparse::str, i_eval),
    l(l(pstr "\n", lit 0, phiparse::oneof, i_eval), phiparse::rep, i_eval),
    l(pstr "\n", phiparse::str, i_eval)),
  phiparse::seq, i_eval;

use constant any_whitespace => l
  pstr " \n\r\t", lit 1, phiparse::oneof, i_eval;

use constant ignore => l
  l(drop, pnil),
  l(l(l(line_comment, any_whitespace), phiparse::alt, i_eval),
    phiparse::rep, i_eval),
  phiparse::pmap, i_eval;


=head2 Expression syntax and parse state
This grammar leverages the parse state more than most. In addition to the
string/offset, we're storing the relative stack depth -- really the number of
expression slots we've allocated. For example, let's go through a simple
function:

  f x = (x + 1) * (x + 2)

C<x> comes in on the stack, so it has position 0 and we begin with a stack depth
of one. If we want to fetch C<x>, we restack C<[depth - position - 1] 0>.
Internally we store a mapping from the name to its abstract, which contains
position, type, and concatenative information:

  x -> [0 any]

Now let's talk about expression allocation. In total, we have seven in this
function (eight if we count the slot we get from C<x> itself):

  x                   depth=1 : [1 any 0 get]
  1                   depth=2 : [2 any drop [1] head]
  x + 1               depth=3 : [3 any dup 0 get 1 get +]   # BUG
  x                   depth=4 : [4 any 0 get]
  2                   depth=5 : [5 any drop [2] head]
  x + 2               depth=6 : [6 any dup 3 get 4 get +]   # BUG
  (x + 1) * (x + 2)   depth=7 : [7 any dup 2 get 5 get *]   # BUG
                      depth=8

The concatenative code to fetch a value takes the current stack depth as an
incoming argument; C<depth i get = [depth - i - 1] 0 restack>.

(3), (6), and (7) are broken: C<get> is relative to the I<current> stack depth,
and after a C<dup> we'll have one more entry on the stack. We need to replace
C<dup> with C<dup inc> to fix this:

  x + 1               depth=3 : [3 any dup inc 0 get 1 get +]
  x + 2               depth=6 : [6 any dup inc 3 get 4 get +]
  (x + 1) * (x + 2)   depth=7 : [7 any dup inc 2 get 5 get *]

The end of the function involves one more restack to fetch the returned
expression and reset the stack:

  [0] depth restack

Now we can put all of this together.

=head2 How this maps into parse-land
Conceptually, everything works as above: every value we parse turns into a new
stack entry and we increment the depth in the parse state. In real terms this is
only sometimes true, because the grammar knows some things about linearity. So
let's start there.

If we're parsing an expression like C<x + y>, this would normally net three
stack values: C<x>, C<y>, and C<x + y> -- but obviously C<x> and C<y> are going
to be copies of existing values and don't need to hang around; C<+> is a linear
operator. So the world is a bit better if we can net just C<x + y>. And, of
course, the expression parser has enough information to do exactly that. (The
key is that the two arguments to C<+> must be the top two stack items if we've
done this optimization for subexpressions.) Here's what this new world looks
like:

  f x = (x + 1) * (x + 2)

  x                   depth=1 : [1 any 0 get]
  1                   depth=2 : [2 any drop [1] head]
  x + 1               depth=3 : [1 any dup 0 inc get 1 get +] : depth=1
  x                   depth=2 : [2 any 0 get]
  2                   depth=3 : [3 any drop [2] head]
  x + 2               depth=4 : [2 any dup inc 2 get 3 get +] : depth=2
  (x + 1) * (x + 2)   depth=3 : [3 any dup inc 1 get 2 get *] : depth=1

This, of course, is great because every expression nets exactly one value, so
there's no return value management. The final piece is that C<;> works by
dropping its left argument and keeping its right one; it's no different from any
other binary operator.

=head3 Variables, closures, and capture
The function parser has a nontrivially complex job, the most involved of which
is tracking closure state. Let's talk about how that happens.

  f x xs = xs.map y -> x + y

The closure C<< y -> x + y >> will end up being a list, which is convenient
because we have useful identities like argument preloading. For example,
C<[1 +]> works like a closure in that one of the arguments to C<+> is already
supplied. We use the same mechanism to send C<x> into C<fn y> -- so internally,
the function becomes C<fn [x] y>: all captured values are passed in a list.

The last piece is that the inner function does two things:

1. Capturable values are rebound to things that unpack the closure list
2. The closure appends a new capture list entry for each I<reference> to a
captured value


=head2 How this works, concretely
Let's go back to our parsing example, this time as a parse state list:

  ["f x xs = xs.map y -> x + y" 0]

The tail of this, C<[]>, means we have no surrounding lexical scope and no
bindings. It's up to C<f> to push a new lexical context once it's clear we're
defining a function.

From now on I'm going to use C<|> to refer to the parse position within the
string, and leave the offset abstract as C<n>:

  ["|f x xs = xs.map y -> x + y" n]

=head3 Lexical scope encoding
The tail of the parse state contains a stack of lexical scopes, the top of which
is the innermost function we're parsing. For example, if C<f x xs = ...> were
inside another function, we'd have a scope for the parent function and C<f>
would create its own scope to parse the body. That would look something like
this:

  [
    [[] [[xs 0 any] [x 1 any]] 2]     # scope inside f x xs = ...
    [[] [[f ...]]              1]     # parent lexical scope
  ]

Let's talk about what these lists are made of. Each one encodes a single scoping
level, which looks like this:

  [closure-captures locals stackdepth]

The parsers themselves manage linear abstracts, and any locals are added to the
C<locals> list. The stack depth is incremented when this happens; we effectively
have a reserved stack slot for the value.

=head3 Initial parse state for the global scope
phi doesn't have a "global scope" per se. Each file or compilation unit or
whatever is considered to have a local scope, and those local scopes can be
returned/chained between files or modified in first-class ways. (This leverage
comes from the deep connection between parsers and types.)

Anyway, this is a reasonable parse state tail to start with:

  [[] [] 1]

Any lexical scope will have a depth of 1 to accept the list of captured closure
variables, although often this list will be nil.

=head3 The destructuring bind
C<f> is parsed as a symbol and doesn't resolve to anything, so we bind it to an
"unbound symbol" value. Its parse continuation provides a few alternatives:

1. C<< ":expr" >>: become a qualified lvalue
2. C<< "lvalue* -> expr" >>: create a lambda
3. C<< "lvalue+ = expr" >>: define a named function
4. C<< "= expr" >>: define a local

Cases (2), (3), and (4) all create a closure scope.

In this case we take (3), which parses everything else. Let's break into the
parse just after C<=>; at this point the LHS has pushed a new closure layer,
which consists of the capture list, the list of locals, and the stack depth:

  [[] [[x any 1 get] [xs any 2 get]] 3]

Now the parse state is:

  ["f x xs = |xs.map y -> x + y" n [[] [[x any 1 get] [xs any 2 get]] 3]
                                   [[] []                             1]]

=head3 C<xs.map>
C<xs> is parsed as a symbol and matched to stack position 1, so we generate the
description of the abstract value and push a stack entry:

  [[] [[x any 0 get] [xs any 1 get]] 3]

We don't see C<xs> in the parse state yet because it's being stored by the
symbol parser; its return value is the abstract:

  [2 any 1 get]

This value doesn't have a specific type, but it does have a parse continuation
that consumes C<.map> and returns a new abstract. Here's what that looks like:

  ["f x xs = xs.map |y -> x + y" n [[] [[x any 1 get] [xs any 2 get]] 4]
                                   [[] []                             1]]

  parse("xs.map") = [2 any [1 get] . 'map method]

The method parser knows that its LHS is linear, so there's no need to C<dup inc>
the stack depth.

=head3 C<< y -> x + y >>
Aw yeah, time for the fun part. C<y> is parsed as an unbound symbol, which gives
us the alternatives we had for C<f>. This time we take case (2), at which point
we have a new sub-scope:

  ["f x xs = xs.map y -> |x + y" n
      [[] [[y any 1 get]]                2]     # innermost scope
      [[] [[x any 1 get] [xs any 2 get]] 4]     # scope of f
      [[] []                             1]]    # "global" scope

OK, right off the bat we refer to C<x>, which is only bound in the parent scope.
Let's walk through that process in some detail.

First, we fail to find C<x> in the immediate scope, so we begin looking upwards
in the lexical chain. There are two possibilities: either it's defined somewhere
and can be dereferenced, or it's an unbound symbol. In this case it exists one
scope up.

Once we find the reference, we then "pull" it downwards into the current scope.
That involves pulling it into every parent scope until we find it bound locally;
this is important for multi-level closure nesting, e.g. something like this:

  f x = y -> z -> x + y + z

The inner closure references a variable two scopes up, so in reality the C<y>
function closes over C<x> -- even though the only purpose of that closure is to
then forward C<x> to C<< z -> ... >>.

Anyway, the parse state after we've pulled C<x> is this:

  [ [[[1 get]] [[y any 1 get] [x any 0 get 0 nthlast]] 2]
    [[]        [[x any 1 get] [xs any 2 get]]          4]
    [[]        []                                      1] ]

A couple of important points:

1. C<nthlast> is C<nth> in reverse: "nth from the end"
2. We bind C<x> as a local once we capture it

(1) matters because once we generate a local entry for a variable, we can't
change anything about how we get to it; and since we cons new variables onto the
beginning of the closure capture list, this means positional references need to
be relative to the end.

(2) is a nice optimization we can make to prevent the same closure variable from
cluttering up the capture space if we refer to it multiple times. Now that C<x>
is a local binding, any further references to it will just generate the stored
code C<depth 0 get 0 nthlast>.

(NB: if all of this seems like it will end up being horrifically inefficient,
don't worry; the optimizer will take care of everything.)

=head3 OK, so what about unbound symbols?
Unbound symbols don't work like captured variables because there's no capture
happening: they're literals rather than references. This is a parse-level
distinction. The parser that handles pulldown is gated on finding the value in
the scope chain somewhere; if that parser fails, then we parse an unbound symbol
literal.


=head2 Helper functions: C<get> and C<nthlast>
These are pretty simple. Let's start with C<get>:

  depth i get = [depth-i-1] 0 restack

Concatenatively:

  depth i  neg + 1 neg + [] swons 0 restack

=cut

use constant get_fn => l i_neg, i_plus, lit 1, i_neg, i_plus, pnil, swons,
                         lit 0, i_restack;


=head3 C<nthlast>
Functionally:

  xs i nthlast = xs rev i nth
  xs i nth     = i == 0 ? xs.head : xs.tail i-1 nth

Concatenatively:

  xs i      swap rev swap nth

  xs i      dup if
    xs i    swap tail swap 1 neg + nth
    xs 0    drop head

=cut

use constant nth_mut => pmut;
use constant nth => l
  dup,
    l(swap, tail, swap, lit 1, i_neg, i_plus, nth_mut, i_eval),
    l(drop, head),
    if_;

use constant nthlast => l swap, phiparse::rev, i_eval, swap, nth, i_eval;


=head2 Parsers
Let's kick this off with the symbol literal parser. We'll need a few helper
functions here, including list-length and list->string.

=head3 C<list-length> function

  []        list-length = 0
  [x xs...] list-length = xs... list-length inc

Derivation:

  xs  dup nilp              = xs <1|0>
  []  drop 0                = 0
  xs  tail list-length inc  = 1 + length(xs.tail)

=cut

use constant list_length_mut => pmut;
use constant list_length => l
  dup, nilp,
    l(drop, lit 0),
    l(tail, list_length_mut, i_eval, lit 1, i_plus),
    if_;

list_length_mut->set(list_length);


=head3 C<list-string> function
Takes a list of integers and returns a string. Function:

  cs list-string = cs (cs list-length str) 0 list-string'

  cs s i list-string' = match cs with
    | []    -> s
    | c:cs' -> s[i] = c; cs' s i+1 list-string'

Derivation:

  cs  dup list-length str 0 list-string'

  cs s i  rot3< dup nilp    = s i cs <1|0>

  s i cs  drop drop         = s

  s i cs       uncons                      = s i cs' c
  s i cs' c    [0 2 3 2 1] 4 restack       = cs' i s i c
  cs' i s i c  sset swap inc list-string'  = cs' s i+1 list-string'

=cut

use constant list_string1_mut => pmut;
use constant list_string1 => l
  rot3l, dup, nilp,
    l(drop, drop),
    l(i_uncons, stack(4, 0, 2, 3, 2, 1), i_sset, swap, lit 1, i_plus,
      list_string1_mut, i_eval),
    if_;

list_string1_mut->set(list_string1);

use constant list_string => l
  dup, list_length, i_eval, i_str, lit 0, list_string1, i_eval;


=head3 C<symbol-parser> parser
Basically C<oneof> that returns a symbol.
=cut

use constant symbol_parser => l
  l(list_string, i_eval, i_strsym),
  l(l(pstr join('', 'a'..'z', '_', "\'", '-'),
      lit 1,
      phiparse::oneof,
      i_eval),
    phiparse::rep,
    i_eval),
  phiparse::pmap,
  i_eval;


=head2 Parsing local variables
The idea here is that we parse a symbol, then look through the locals to see if
we find it. If we do, then we succeed and return the tail of that list;
otherwise we fail.

...and, of course, we'll want a helper function to search the local scope. It's
almost the same as the resolver; the only difference is that we return nil
instead of the symbol if it fails to match.

=head3 C<scope-search> function

  'sym [scope]  scope-search  = [] | [...]

  'sym []                scope-search  = []
  'sym [[s1 ...] ss...]  scope-search  = s1 == sym
    ? [...]
    : 'sym [ss...] scope-search

Derivation:

  'sym [s ss...]  dup nilp          = 'sym [s ss...] <1|0>

  'sym []                swap drop           = []
  'sym [s ss...]         uncons uncons       = 'sym [ss...] [s...] s
  'sym [ss...] [s...] s  [3] 0 restack sym=  = 'sym [ss...] [s...] <1|0>

  'sym [ss...] [s...]    [0] 3 restack       = [s...]
  'sym [ss...] [s...]    drop scope-search

=cut

use constant scope_search_mut => pmut;
use constant scope_search => l
  dup, nilp,
    l(swap, drop),
    l(i_uncons, i_uncons, stack(0, 3), i_symeq,
      l(stack(3, 0)),
      l(drop, scope_search_mut, i_eval),
      if_),
    if_;

scope_search_mut->set(scope_search);


=head3 C<local-variable> parser
Starting with the full output state of C<symbol_parser>, we either pass on the
failure or map the symbol through the local scope. This is a function
composition against the parser itself, so we'll have two input values
C<state, val>, and two output values.

  x   []                                    local-variable = x []
  sym ["str" 0 [closure locals depth] ...]  local-variable =
    match sym locals scope-search with
      | []  -> [] []
      | def -> def ["str" 0 [closure locals depth] ...]

Derivation:

  x []  dup nilp  = x [] 1

  sym [s i [c l d] ...]    dup tail tail head tail head  =
  sym [s i [c l d] ...] l  rot3< swap scope-search       =
  [s i [c l d] ...] def?   dup nilp

  [s i [c l d] ...] []     swap drop dup                 = [] []

  [s i [c l d] ...] def    swap

=cut

use constant local_variable => l
  dup, nilp,
    pnil,
    l(dup, tail, tail, head, tail, head, rot3l, swap, scope_search, i_eval,
      dup, nilp,
      l(swap, drop, dup),
      l(swap),
      if_),
    if_;


=head3 C<closure-pulldown> function
Pulls a variable down through the lexical scope chain by binding it within each
closure. If we're using this parser, we know the variable isn't a local.

  'sym [s i [c l d] ...] closure-pulldown = [s i [c' l' d] ...] | []

Equations:

  'sym [s i s1 ss...] closure-pulldown = match 'sym ss... closure-pulldown' with
    | []  -> []
    | def -> 'sym def [s i s1 ss...] bind-locals

  s ls closure-pulldown' = match ls with
    | []          -> []
    | [c l d]:ls' -> match s l scope-search with
      | []  -> s ls' closure-pulldown'
      | def -> def

=cut

use constant closure_pulldown_mut  => pmut;
use constant closure_pulldown1_mut => pmut;
use constant bind_locals_mut       => pmut;


=head3 C<bind-locals> function
This is where we closure-convert each layer by capturing a closure variable into
the closure list. There are really two functions involved here: C<bind-locals>,
which walks the scope chain, and C<bind-capture>, which manipulates a single
pair of scopes. Let's implement C<bind-capture> first.

  'sym def [c1 l1 d1] bind-capture = [c1' l1' d1]

Equations:

  'sym def [c l d] bind-capture =
    let cn   = c list-length in
    let ldef = ['sym 0 get cn nthlast] in
    [def:c ldef:l d]

Derivation:

  'sym def [c l d]                uncons                                =
  'sym def [l d] c                dup list-length [nthlast] swap quote  =
  'sym def [l d] c [nthlast] 'cn  cons 'get cons '0 quote cons          =
  'sym def [l d] c ['0 get 'cn nthlast]  [4] 0 restack cons             =
  'sym def [l d] c ldef           rot3< uncons                          =
  'sym def c ldef [d] l           rot3< cons cons rot3> swons cons      =
  'sym [def:c ldef:l d]           swap drop

=cut

use constant bind_capture => l
  i_uncons, dup, list_length, i_eval, nthlast, swap, quote, i_eval, i_cons,
  get_fn, i_cons, lit 0, quote, i_eval, i_cons, stack(0, 4), i_cons, rot3l,
  i_uncons, rot3l, i_cons, i_cons, rot3r, swons, i_cons, swap, drop;


=head3 ...back to C<bind-locals>
OK, we have a way to pull the variable down by one scope level. Now we need a
way to chain it all the way down. Here's the equation:

  'sym scopes... bind-locals = 'sym scopes... [] bind-locals'

  s scopes pushed bind-locals' = match scopes with
    | [sc sp ss...] -> match s sp scope-search with
      | []  -> s [sp ss...] sc:pushed bind-locals'
      | def -> let sc' = s def sc bind-capture in
               match pushed with
                 | []   -> [sc' sp ss...]
                 | c:p' -> s [c sc' sp ss...] p' bind-locals'
    | _             -> []

Derivation:

  s scopes pushed              swap dup nilp                                 =

  s pushed []                  [0] 3 restack
  s pushed sc:pushed'          unswons dup nilp

  s pushed sc []               [0] 4 restack
  s pushed sc sp:ss...         uncons

  s pushed sc [ss...] sp       dup [5] 0 restack swap tail head scope-search =
  s pushed sc [ss...] sp def?  dup nilp

  s pushed sc [ss...] sp []    drop cons rot3> cons bind-locals'

  s pushed sc [ss...] sp def   [3 0 5] 1 restack bind-capture                =
  s pushed sc [ss...] sp sc'   rot3> cons swons                              =
  s pushed sc [sc' sp ss...]   swap drop swap                                =
  s [sc' sp ss...] pushed      dup nilp

  s [sc' sp ss...] []          drop swap drop

  s [sc' sp ss...] c:p'        uncons rot3< swons                            =
  s p' [c sc' sp ss...]        swap bind-locals'

=cut

use constant bind_locals1_mut => pmut;
use constant bind_locals1 => l
  swap, dup, nilp,
    l(stack(3, 0)),
    l(unswons, dup, nilp,
      l(stack(4, 0)),
      l(i_uncons, dup, stack(0, 5), swap, tail, head, scope_search, i_eval,
        dup, nilp,
        l(drop, i_cons, rot3r, i_cons, bind_locals1_mut, i_eval),
        l(stack(1, 3, 0, 5), bind_capture, i_eval, rot3r, i_cons, swons, swap,
          drop, swap, dup, nilp,
          l(drop, swap, drop),
          l(i_uncons, rot3l, swons, swap, bind_locals1_mut, i_eval),
          if_),
        if_),
      if_),
    if_;

bind_locals1_mut->set(bind_locals1);

use constant bind_locals => l pnil, bind_locals1, i_eval;


=head3 C<closure-variable> parser
If something isn't a C<local-variable>, it might be a C<closure-variable> --
that is, a reference to a local variable in a lexical parent. This parser
detects that case and, if it finds the variable, does the pulldown.

This parser modifier is actually very very simple: we already have all of the
arguments ready for C<bind-locals>, which will either bind stuff or return nil.
The only thing we need to do is hang onto the symbol to make sure we can return
it if we get a match.

  x state? closure-variable = match state? with
    | []          -> x []
    | [s i ss...] -> match x ss bind-locals with
      | []                   -> x []
      | [[c l d] ...] as ss' -> let def = x l scope-search in
                                def [s i ss'...]

Derivation:

  x state?       dup nilp

  x [s i ss...]  unswons unswons [3] 0 restack swap bind-locals dup nilp  =
  x s i ss'|[] <1|0>

  x s i []       [0] 3 restack

  x s i ss'      dup head tail head [4] 0 restack swap scope-search       =
  x s i ss' def  [1 2 3 0] 5 restack swons swons

=cut

use constant closure_variable => l
  dup, nilp,
    pnil,
    l(unswons, unswons, stack(0, 3), swap, bind_locals, i_eval, dup, nilp,
      l(stack(3, 0)),
      l(dup, head, tail, head, stack(0, 4), swap, scope_search, i_eval,
        stack(5, 1, 2, 3, 0), swons, swons),
      if_),
    if_;


=head3 C<compile-capture-list> function
Once we have a list of captured values, we'll need to generate the code that
packs those values into a list to be consumed by the function. This is basically
a regular list-map operation with the subtlety that we'll need to know how many
stack entries we've added so we can adjust the depth accordingly.

Note that we aren't evaluating the list entries at this point; we're generating
code that will build the list the closure expects to get. For example, let's
compile the closure in this code:

  f x = y -> x + y

Let's assume C<x> is encoded as C<[int 1 get]>, which would mean the closure
capture list is C<[[int 1 get]]>. We want to end up with C<[value-of-x]>. So the
code we want to generate looks like this:

  [closure-code] [] depth 2 + [1 get] . cons cons
                 |------------------------------|

                    we're generating this part

C<depth> is a constant, so we can do the C<2 +> up front.

Functionally:

  depth cl compile-capture-list = (depth + 2) (rev cl) ccl' [] cons

  d cl ccl' = match cl with
    | []    -> [cons]
    | c:cl' -> 'd:(tail c):(d cl' ccl')

Concatenative:

  depth cl        swap 2 + swap rev ccl'

  d cl            dup nilp

  d []            drop drop [cons]

  d c:cl'         uncons rot3> [0 1 1] 2 restack ccl'       =
  c d xs          rot3> quote rot3> tail cons swons

=cut

use constant compile_capture_list1_mut => pmut;
use constant compile_capture_list1 => l
  dup, nilp,
    l(drop, drop, l(i_cons)),
    l(i_uncons, rot3r, stack(2, 0, 1, 1), compile_capture_list1_mut, i_eval,
      rot3r, philocal::quote, i_eval, rot3r, tail, i_cons, swons),
    if_;

compile_capture_list1_mut->set(compile_capture_list1);

use constant compile_capture_list => l
  swap, lit 2, i_plus, swap, phiparse::rev, i_eval,
  compile_capture_list1, i_eval;


1;
