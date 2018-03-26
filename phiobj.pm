=head1 Objects and polymorphism in phi
Let's model everyone's favorite object, a 2D point:

  [x y]

Boom. Pure genius right there.

OK, so how do we make this thing self-aware and polymorphic and stuff? We'll
need to affix a type: C<[point-type x y]>. Then methods will look at the type,
do some kind of dispatch, and that becomes a new calling convention. But we can
do even better:

  [[x y] point-type...]

Now we've got more than just a piece of data; we have a fully self-aware
closure. It has some convenient equations:

  obj head = state
  obj tail = type

=head2 Normal calling convention
If you have an object C<obj>, you typically do something like this to interact
with it:

  arg2 arg1 'method obj .

Methods are always symbols by convention. This is important because there are
some situations where the type will need to differentiate between methods and
instance data; it uses primitive types to do this.

=head2 Types
Going back to the 2D point example, what would C<point-type...> actually look
like? It turns out that C<point-type> itself is an object, this time an instance
of a struct type. So a concrete 2D point might look like this:

  [ [3 4] [x y] struct-type... ]

C<struct-type> provides accessors to point fields:

  'x   'get [ [3 4] [x y] struct-type... ] .
  5 'x 'set [ [3 4] [x y] struct-type... ] .

=head3 Self reference
Objects in phi aren't mutable, so any mutators will return modified copies. This
means types will need a way to refer to themselves so they can cons modified
instance state back onto the original type list. Types do this by quoting the
current continuation, which contains the type as the stack top. Here's what that
looks like:

  [ [instance-data...] i> tail head head type... ]

...so in practice, the arguments to the type end up being:

  args... 'method [instance-data...] [type...]

This makes it possible for the type to refer to itself without going through a
global resolver.
=cut

package phiobj;
use strict;
use warnings;

use Scalar::Util qw/refaddr/;
use Exporter qw/import/;

use phiboot;
use phibootmacros;
use philist;

our @EXPORT =
our @EXPORT_OK =
  qw/ mcall make_type mktype bind isget isset /;


# Great for debugging
use constant TRACE_METHOD_CALLS => 0;
use constant SAFE_METHOD_CALLS  => 0;


=head2 Type constructor
Builds a type from a list of method definitions.
=cut

use phi make_type => l                      # [mlist]
  l(resolvercode, i_eval, i_eval), swons,   # [[mlist] resolver]
  lit i_eval, i_cons,                       # [. [mlist] resolver]
  l(tail, head, head, lit i_quote,
    i_cons, swons, swap), i_cons,           # [l . [mlist] resolver]
  lit i_quote, i_cons;                      # [i> l . [mlist] resolver]


sub mcall($)
{
  my $mname = shift;
  my $m = psym $mname;
  my @safe = SAFE_METHOD_CALLS
    ? (                                     # 'method obj
       dup, i_type, lit psym"cons", i_symeq,# 'method obj is-cons?
       pnil,
       l(lit "method_call_${mname}_on_non_object" => i_crash),
       if_,                                 # 'method obj
       dup, tail, head, i_type, lit psym"int", i_symeq,
       pnil,
       l(lit "method_call_${mname}_on_non_object" => i_crash),
       if_,                                 # 'method obj
       dup, tail, head, lit i_quote, i_xor, # 'method obj head!=i>?
       l(lit "method_call_${mname}_on_non_object" => i_crash),
       pnil,
       if_)                                 # 'method obj
    : ();

  (l($m), i_uncons, stack(3, 2, 0), @safe, i_eval);
}

sub mktype(@) { le l(@_), make_type, i_eval }
sub bindl($$)
{
  my ($name, $l) = @_;
  $phiboot::explanations{refaddr $l} //= "->$name";
  pcons psym $name, $l;
}

sub bind
{
  my $name = shift;
  local $phibootmacros::real_caller = [caller];
  $$phibootmacros::real_caller[1] .= "(.$name)";
  my $caller_explain =
    "$$phibootmacros::real_caller[1]:$$phibootmacros::real_caller[2]";

  bindl $name => TRACE_METHOD_CALLS
    ? l lit pstr"$caller_explain(.$name)\n", i_write, @_,
        lit pstr"<\n", i_write
    : l @_;
}


# Enable "use phitype" for better explanations
BEGIN { ++$INC{'phitype.pm'} }
sub phitype::import
{
  no strict 'refs';
  my (undef, $name, @l) = @_;
  my $type    = mktype @l;
  my $type2   = $type;
  my $package = caller;

  *{"$package\::$name"} = sub() { $type2 };
  $phiboot::explanations{refaddr $type} = $name if ref $type;
}


=head2 Instance state accessors
...because why not.
=cut

sub isget($) { (head, lit shift, lget, i_eval) }
sub isset($) { (dup, head, rot3l, lit shift, lset, i_eval, lit 0, lset, i_eval) }


1;
