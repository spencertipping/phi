package phibinary;

use strict;
use warnings;

use Exporter qw/import/;
use Scalar::Util qw/refaddr/;

use phiboot;

our @EXPORT =
our @EXPORT_OK = qw/ export /;

no warnings 'recursion';


# Binary export
# export_one(\%refs, \@serialized, \@muts, $val) -> index into @serialized
sub export_one
{
  my ($refs, $serialized, $muts, $x) = @_;
  return $$refs{refaddr $x} if exists $$refs{refaddr $x};
  $$refs{refaddr $x} = $x->export_into($refs, $serialized, $muts);
}

# export($val) -> serialized string
sub export
{
  my %refs;
  my @serialized;
  my @muts;
  my $val = shift;
  export_one \%refs, \@serialized, \@muts, $val;

  for (my $i = 0; $i < @muts; ++$i)
  {
    # Encode it properly
    my $m = $muts[$i];
    my $index = $refs{refaddr $m};
    die "uninitialized mut! $m" unless defined $$m;
    my $referent = export_one \%refs, \@serialized, \@muts, $$m;
    $serialized[$index] = pack CL => 5, $referent;
  }

  # One final mut to refer to the thing we want to encode.
  push @serialized, pack CL => 5, $refs{refaddr $val};

  pack "L/a" => join"", @serialized;
}

sub phiboot::nil::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  push(@$serialized, "\0") - 1;
}

sub phiboot::cons::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  my $h = export_one $refs, $serialized, $muts, $self->head;
  my $t = export_one $refs, $serialized, $muts, $self->tail;
  push(@$serialized, pack CLL => 1, $h, $t) - 1;
}

sub phiboot::int::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  push(@$serialized, pack CQ => 2, $self->val) - 1;
}

sub phiboot::real::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  push(@$serialized, pack Cd => 6, $self->val) - 1;
}

sub phiboot::str::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  push(@$serialized, pack 'CL/a' => 3, $self->val) - 1;
}

sub phiboot::sym::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  my $str_index = push(@$serialized, pack 'CL/a' => 3, $self->val) - 1;
  push(@$serialized, pack CL => 4, $str_index) - 1;
}

sub phiboot::mut::export_into
{
  my ($self, $refs, $serialized, $muts) = @_;
  push @$muts, $self;
  push(@$serialized, undef) - 1;
}


1;
