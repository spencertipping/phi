=head1 Abstract values
phi's concatenative evaluator needs to be as simple as possible not because I
dislike writing interpreter code in Perl (I don't), but rather because the
interpreter itself needs to be very easy to simulate. One such simulation
involves "abstract values", which are collections of constraints that runtime
values will carry. Abstracts are like types but much more specific than most
type systems would support.
=cut

package phiabstract;
use strict;
use warnings;

use Exporter qw/import/;
use Scalar::Util qw/looks_like_number/;
use phiboot;
use phibootmacros;
use phiobj;

our @EXPORT =
our @EXPORT_OK = grep /^a/, keys %{phiabstract::};


=head2 What you can do with abstracts
Abstracts give phi programs access to the same kind of API we have in Perl: you
can build an interpreter, set its initial state, and step through it one
instruction at a time. Each time-step is fully inspectable because the
interpreter is just data, and every value's constant/unknown disposition is also
modeled. This is how phi implements constant folding and representational
optimization.

For example, let's suppose we have a function that operates on a pair of
integers and we suspect it allocates unnecessary cons cells in the process. We
can abstract-evaluate it to capture its productive operations, which lets us
build a new, faster function that behaves the same way.

Let's walk through one such function:

  [cons uncons + [] swap cons]

Now let's build an abstract interpreter and step it until it's done:

  'new abstract-interpreter .
    'int 0 'new abstract-typed-unknown . 'dpush rot3< .
    'int 1 'new abstract-typed-unknown . 'dpush rot3< .
    [cons uncons + [] swap cons] 'new abstract-quote . 'cpush rot3< .
    run

The result is an abstract interpreter instance that you can inspect to figure
out what it did with its inputs. In this case (with the previous C<run> output
still on the stack):

  'dpop swap .      # -> instance of abstract-value-type

Abstract values can tell you a number of things including their type, how
specified they are, and, if they're fully specified, what that value is.


=head2 Abstract values
We need a few of these in order to build the interpreter, which models its state
in terms of abstracts:

1.  C<abstract-nil>: the nil value
2.  C<abstract-cons>: a cons cell containing more abstract things
3.  C<abstract-int>: an int at some state of known-ness
4.  C<abstract-str>: a string at some state of known-ness
5.  C<abstract-sym>: a symbol at some state of known-ness
6.  C<abstract-mut>: a mutable value at some state of known-ness
7.  C<abstract-unknown>: something we know nothing about
8.  C<abstract-op>: a primitive operation we intend to apply, but can't yet
9.  C<abstract-crash>: a value that will crash the interpreter if computed
10. C<abstract-union>: one of a set of possible values

All of these values are immutable. C<abstract-mut> is implemented with respect
to an interpreter, which stores the mutable binding in a list.

NB: if an op produces a value which contains resolved C<abstract-mut>s, those
muts will be flattened into real muts, which makes them proper circular
references. (Or at least, I think this is the way to go.)

=head3 Value protocol
Each abstract type supports a few methods:

  'is-crash   v -> abstract bool
  'type       v -> abstract sym
  'uncons     v -> t h
  'id         v -> int

  'is-const   v -> bool
  'val        v -> nil|cons|int|str|sym|<crash>

  'compile    v -> list|crash

Abstracts can't be created on their own; they belong to abstract interpreters.
This is required in order to support mutability and interpreter forking.
=cut

use constant abstract_sym_type_cons => pmut;
use constant abstract_sym_type_nil  => pmut;
use constant abstract_sym_type_int  => pmut;
use constant abstract_sym_type_str  => pmut;
use constant abstract_sym_type_sym  => pmut;
use constant abstract_sym_type_mut  => pmut;

use constant abstract_crash_val => pmut;

sub mkconsttype
{
  my $type_sym = shift;
  mktype @_,
         bind(val        => isget 0),
         bind(type       => drop, $type_sym),
         bind('is-const' => drop, lit 1),
         bind('is-crash' => drop, lit 0),

         # default implementations (you can override by specifying):
         bind(uncons => drop, abstract_crash_val);
}

# constant type instance state = [x]
use constant abstract_nil  => mkconsttype abstract_sym_type_nil,
  bind(compile => drop, l(pnil));

use constant abstract_int  => mkconsttype abstract_sym_type_int,
  bind(compile => isget 0, philocal::quote, i_eval);

use constant abstract_str  => mkconsttype abstract_sym_type_str,
  bind(compile => isget 0);

use constant abstract_sym  => mkconsttype abstract_sym_type_sym,
  bind(compile => isget 0, philocal::quote, i_eval);

use constant abstract_cons => mkconsttype abstract_sym_type_cons,
  bind(compile => isget 0),
  bind(uncons  => isget 0, i_uncons);

use constant abstract_nil_val => pcons l(pnil), abstract_nil;

abstract_sym_type_nil->set( pcons l('nil'),  abstract_sym);
abstract_sym_type_cons->set(pcons l('cons'), abstract_sym);
abstract_sym_type_int->set( pcons l('int'),  abstract_sym);
abstract_sym_type_str->set( pcons l('str'),  abstract_sym);
abstract_sym_type_sym->set( pcons l('sym'),  abstract_sym);
abstract_sym_type_mut->set( pcons l('mut'),  abstract_sym);

# unknown instance state = [gensym]
use constant abstract_op_mut      => pmut;
use constant abstract_unknown_mut => pmut;

use constant abstract_unknown => mktype
  bind(uncons     => drop, pstr "TODO: ops against unknowns", i_crash),
  bind(type       => drop, abstract_unknown_mut),
  bind(val        => pstr "cannot force unknown", i_crash),
  bind('is-const' => drop, lit 0),
  bind('is-crash' => drop, lit 0);

# op type instance state = [d c r]
# TODO: is this really a sub-interpreter?
use constant abstract_op => mktype
  bind(uncons     => drop, pstr "TODO: ops against ops", i_crash),
  bind(type       => drop, pstr "TODO: abstract op types", i_crash),
  bind(val        => pstr "cannot force abstract op", i_crash),
  bind('is-const' => drop, lit 0),
  bind('is-crash' => drop, lit 0);


=head2 Macros for abstracts
We have stuff like C<head>, C<tail>, C<nilp>, etc; it's useful to have analogs
for abstract values.

NB: we can't define C<apmut> without a hosting interpreter; otherwise we'd be
unable to fork interpreter states.
=cut

sub apnil()     { abstract_nil_val }
sub apcons($$)  { pcons l(pcons $_[0], $_[1]), abstract_cons }
sub apint($)    { pcons l(pint $_[0]),         abstract_int }
sub apstr($)    { pcons l(pstr $_[0]),         abstract_str }
sub apsym($)    { pcons l(psym $_[0]),         abstract_sym }

sub anil()      { abstract_nil_val }
sub acons()     { (i_cons, pnil, swons, abstract_cons, swons) }
sub auncons()   { mcall"uncons" }
sub ahead()     { (mcall"uncons", swap, drop) }
sub atail()     { (mcall"uncons", drop) }
sub anilp()     { (mcall"type", abstract_sym_type_nil, i_eq) }

sub alist_onto_ { @_ > 1 ? apcons(pop, alist_onto_(@_)) : shift }
sub alist_onto  { alist_onto_ shift, reverse @_ }
sub alist       { alist_onto apnil, @_ }

sub al { alist map ref ? $_ : looks_like_number $_ ? apint $_ : apsym $_, @_ }


=head2 C<abstract-interpreter>
Instance state:

  [d c r next-gensym crash? coercions mut]

Methods:

  gensym type 'coerce  i -> i'        # TODO

  val       'dpush     i -> i'
            'dpop      i -> i' val
  val       'cpush     i -> i'

            'gensym    i -> i' n      # NB: gensyms are numbers

            'd         i -> d
            'c         i -> c
            'r         i -> r
  val       'dset      i -> i'
  val       'cset      i -> i'
  val       'rset      i -> i'

            'next-insn i -> i' val
            'has-next? i -> bool
            'cpack     i -> i'
            'step      i -> i'
            'run       i -> i'
            'is-ok?    i -> bool

=head3 Interpreter nondeterminism
The interpreter's C<d>, C<c>, and C<r> are all abstract and may be unknowable at
any given moment. If C<c> becomes unknowable, or more specifically if
C<next-insn> or C<cpack> encounter unspecified values, then the interpreter
effectively halts. We don't try to encode the union of all instruction
possibilities because (1) that would be insane, and (2) it would use a ton of
memory and produce nothing useful.

You could easily write an interpreter that supported doing this, for instance if
you wanted something that worked like Prolog in stack-land, but that isn't what
phi is about and in our case it probably means that the abstract interpreter is
being misused. So we want to provide some indication of the problem rather than
failing in computationally expensive ways.
=cut

use constant abstract_interpreter => mktype
  bind(d             => isget 0),
  bind(c             => isget 1),
  bind(r             => isget 2),
  bind('next-gensym' => isget 3),
  bind('crash?'      => isget 4),
  bind('coercions'   => isget 5),
  bind(mut           => isget 6),

  bind('is-ok?'      => mcall"crash?", nilp),
  bind('has-next?'   => dup, mcall"is-ok?",
         l(mcall"c", anilp, i_not), l(drop, lit 0), if_),

  bind(dset              => isset 0),
  bind(cset              => isset 1),
  bind(rset              => isset 2),
  bind('next-gensym-set' => isset 3),
  bind('crash-set'       => isset 4),
  bind('coercions-set'   => isset 5),
  bind('mut-set'         => isset 6),

  bind(dpop  => dup, mcall"d", auncons, rot3r, swap, mcall"dset", swap),
  bind(dpush => dup, mcall"d", rot3l, acons, swap, mcall"dset"),
  bind(cpush => dup, mcall"c", rot3l, acons, swap, mcall"cset"),
  bind(cpack => dup, mcall"c", dup, anilp,
    l(drop),
    l(auncons, dup, anilp,                                # i ct ch <1|0>
      l(drop, swap, mcall"cset", mcall"cpack"),
      l(drop, drop),
      if_),
    if_),

  bind(gensym => dup, mcall"next-gensym", dup, rot3r, lit 1, i_plus, swap,
                      mcall"next-gensym-set", swap),

  bind('next-insn' => dup, mcall"c", auncons,             # i ct ch
         dup, mcall"type", abstract_sym_type_cons, i_eq,  # i ct ch <1|0>
         l(auncons, rot3r, acons,                         # i insn cht:ct
           rot3l, mcall"cset",                            # insn i'
           mcall"cpack", swap),
         l(rot3r, swap, mcall"cset", mcall"cpack", swap),
         if_),

  bind(step => mcall"next-insn", mcall"eval"),
  bind(run  => mcall"cpack", dup, mcall"has-next?",
                 l(mcall"step", mcall"run"), pnil, if_);


# Blank interpreter
use constant abstract_interpreter_new =>
  pcons l(apnil,      # d
          apnil,      # c
          apnil,      # r
          pint 0,     # next-gensym
          pnil,       # crash?
          pnil,       # coercions
          pnil),      # mut
        abstract_interpreter;


=head2 Instruction implementations
We can put these into a list and look them up numerically. Each of these has the
signature C<< i -> i' >>.
=cut

use constant reserved => l(lit "unimplemented", swap, mcall"crash-set");

use constant insns => l
  l(dup, dup, mcall"d", swap,   # i d i
         dup, mcall"c", swap,   # i d c i
              mcall"r", apnil,  # i d c r a[]
         swap, acons,
         swap, acons,
         swap, acons,           # i a[d c r]
    swap, mcall"dpush"),

  l(mcall"dpop", swap, mcall"cset"),
  l(mcall"dpop", swap, mcall"cpush"),
  l(mcall"dpop", mcall"type", swap, mcall"dpush"),
  l(mcall"dpop", mcall"id", swap,
    mcall"dpop", mcall"id", rot3l, mcall"xor", mcall"not", swap,
    mcall"dpush"),

  l();    # TODO: abstractify


=head2 C<abstract-value>
This comes in a few varieties:

1. C<abstract-constant>: a fully-specified abstract
2. C<abstract-union>: we don't know which of these it is
3. C<abstract-typed-unknown>: we know the type but not the value
4. C<abstract-unknown>: we don't know anything about the value

(3) is split out per type so we can do type-specific modeling.

It's important to note that "type" here refers to primitive,
interpreter-provided types like C<int>, C<symbol>, etc -- we're not talking
about higher-order OOP types.

Another point is that constant-ness is a single-layer thing because it needs to
be. That is, a constant cons cell can contain an unknown. For example, suppose
we're simulating this function on a stack of C<x y>:

  [[] swap cons swap cons]

The output should look like this:

  constant cons(unknown x, constant cons(unknown y, constant nil))

Because C<constant cons> is fully specified, we can constant-fold any C<uncons>
operations against those objects.

=head3 Type acquisition
phi's semantics don't provide any error handling other than crashing
catastrophically, which is obviously an undesired behavior. A convenient side
effect of this design, though, is that any function we're analyzing can be
assumed not to crash in this way. And that's a powerful assumption, particularly
when dealing with unknowns. Here's an example:

  # initial data stack = unknown x, unknown y
  [+]     # simulate this

The output here is C<abstract-typed-unknown(int, op(+, unknown x, unknown y))>
-- but we know that C<+> will crash unless C<unknown x> and C<unknown y> are
both themselves C<int>s, which in turn means that we now have more type
information. C<x> and C<y> have acquired the C<int> type through coercion.

We don't store coercions on the abstract values; instead, the interpreter keeps
track of the coercions it has made and does the replacements inline, reusing
names so you can track with respect to the original abstract values. This
immutable approach is important because it lets you fork an interpreter state
and try different assumptions with the same set of unknowns. That is, the
coercion state of an unknown is a product of its evaluation; it's not intrinsic
to that unknown.

=head3 Union reduction
Another byproduct of phi's crash-or-succeed model is that we can eliminate
branches by finding crash scenarios. This applies mostly to unions.

TODO: design this wrt coercions against decisions
TODO: do unions close over interpreter states? How would the union know about a
crash if we model coercions at the interpreter level?
=cut


1;
