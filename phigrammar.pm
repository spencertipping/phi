package phi::grammar;


package phi::syntax::node
{
  use Scalar::Util qw/refaddr/;
  use overload qw/ @{} val_array ${} val_scalar "" explain ne ne eq eq /;

  sub print_at
  {
    my $self = shift;
    my $pos  = shift;
    my ($r, $c) = map $_+1, phi::e()->pos_rowcol($pos);
    print "\033[$r;${c}H", @_;
  }

  sub print
  {
    my $self = shift;
    $self->print_at($self->result->start, @_);
  }

  sub colored
  {
    my ($self, $c) = @_;
    $self->print("\033[${c}m" . $self->val);
  }

  sub explain    { shift->val }
  sub result     { shift->{result} }
  sub val_array  { shift->{value} }
  sub val_scalar { \shift->{value} }

  sub eq { refaddr $_[0] eq refaddr $_[1] }
  sub ne { refaddr $_[0] ne refaddr $_[1] }
}


sub oc { phi::parser::strclass->one_of(@_) }
sub mc { phi::parser::strclass->many_of(@_) }
sub Mc { phi::parser::strclass->more_of(@_) }
sub oe { phi::parser::strclass->one_except(@_) }
sub me { phi::parser::strclass->many_except(@_) }
sub Me { phi::parser::strclass->more_except(@_) }

sub at($) { eval "sub {shift->[$_[0]]}" }
sub as($)
{
  no strict 'refs';
  my ($class, @parents) = split /\s+/, shift;
  my %isa = map +($_ => 1), map "phi::syntax::$_", @parents;
  eval "sub ${_}::__hey_perl_this_package_exists {}" for keys %isa;
  @{"phi::syntax::${class}::ISA"} = ("phi::syntax::node", keys %isa);
  eval "#line 1 \"as(phi::syntax::$class)\"
        sub {bless { value  => \$_[0],
                     result => \$_[1] }, 'phi::syntax::$class'}";
}

sub str($) { phi::parser::strconst->new(shift) }
sub sd($)  { str(shift) >>as"delimiter rcolor rc6 v" }

# NB: single-arg, but multi-prototype to modify precedence
sub mut(@) { phi::parser::mutable->new(shift) }


# phi programming language grammar
use constant phi_expr_ref => phi::parser::mutable->new;
use constant phi_expr     => phi_expr_ref >>as"phi_expr rf vf";

# FIXME terminal value (empty program); this starts an alt that is unlikely to
# be used
phi_expr_ref->val = phi::parser::strconst->new('__END__');

use constant
{
  ident => oc('a'..'z', 'A'..'Z', '_')
         + mc('a'..'z', 'A'..'Z', '_', 0..9) >>as"ident vjoin",

  string => sd('"')
          + ((Me("\\\"")           >>as"str_chars  rcolor rc2 v"
              | str("\\") + oe('') >>as"str_escape rcolor rc5 vjoin") * 0
             >>as"str_content rall vjoin")
          + sd('"')
        >>as"string rall v1",

  whitespace => (  str('#!') + me("\n") >>as"shebang      rcolor rc5 vjoin"
                 | str('#') + me("\n")  >>as"line_comment rcolor rc4 vjoin"
                 | Mc(" \n\r\t")        >>as"space        rcolor rc0 v") * 0
                >>as"whitespace rall"
};


# Parser constructors
use constant ebnf_expr_ref => mut;
use constant ebnf_expr     => ebnf_expr_ref >>as"ebnf_expr rf vf";

use constant ebnf_var      => ident  >>as"ebnf_var rcolor rc0 vf";
use constant ebnf_str      => string >>as"ebnf_str rf";

use constant ebnf_chars => mut oe('') + me(']') >>as"ebnf_chars rcolor rc5 vjoin";
use constant ebnf_class =>
    sd("[^") + ebnf_chars + sd("]+") >>as"ebnf_more_except ebnf_class rall"
  | sd("[^") + ebnf_chars + sd("]*") >>as"ebnf_many_except ebnf_class rall"
  | sd("[^") + ebnf_chars + sd("]")  >>as"ebnf_one_except  ebnf_class rall"
  | sd("[")  + ebnf_chars + sd("]+") >>as"ebnf_more_of     ebnf_class rall"
  | sd("[")  + ebnf_chars + sd("]*") >>as"ebnf_many_of     ebnf_class rall"
  | sd("[")  + ebnf_chars + sd("]")  >>as"ebnf_one_of      ebnf_class rall";

use constant ebnf_group => sd('(') + ebnf_expr + sd(')') >>as"ebnf_group rall v1";

use constant ebnf_atom_val => mut ebnf_var | ebnf_str | ebnf_class | ebnf_group;
use constant ebnf_atom     => whitespace
                            + ebnf_atom_val
                            + whitespace >>as"ebnf_atom rall v1";

use constant ebnf_unary_val =>
    ebnf_atom + sd("+") + whitespace          >>as"ebnf_rep1 rall"
  | ebnf_atom + sd("*") + whitespace          >>as"ebnf_rep0 rall"
  | ebnf_atom + sd("?") + whitespace          >>as"ebnf_maybe rall"
  | ebnf_atom + sd(":") + string + whitespace >>as"ebnf_as rall";

use constant ebnf_unary => ebnf_unary_val | ebnf_atom;

use constant ebnf_seq =>
  ebnf_unary + (ebnf_unary*0 >>as"ebnf_seq_rhs rall v")
  >>as"ebnf_seq rall";

use constant ebnf_alt =>
  ebnf_seq + ((sd("|") + ebnf_seq >>as"ebnf_alt_one rall v1")*0
              >>as"ebnf_alt_rhs rall v")
  >>as"ebnf_alt rall";

ebnf_expr_ref->val = ebnf_alt;

phi_expr_ref->val |= ebnf_expr;


# Syntax manipulation
# For testing flatmap: add a new toplevel expression type that accepts an
# ebnf_expr and flatmaps it into the following syntax
use constant flatmap_expr =>
  (ebnf_expr + sd(">>=") + whitespace >>as"flatmap_expr rall v0"
                                       >sub { shift->val })
  >>as"flatmap_result rall v1";

phi_expr_ref->val = flatmap_expr | phi_expr_ref->val;


# Functions
# TODO: design function syntax, and maybe design evaluation model generally


sub phi::syntax::rf::render { ${+shift}->render }
sub phi::syntax::vf::val    { ${+shift}->val }


sub phi::syntax::rcolor::render { $_[0]->colored($_[0]->color) }
sub phi::syntax::rc0::color     { '0;30' }
sub phi::syntax::rc1::color     { '0;31' }
sub phi::syntax::rc2::color     { '0;32' }
sub phi::syntax::rc3::color     { '0;33' }
sub phi::syntax::rc4::color     { '0;34' }
sub phi::syntax::rc5::color     { '0;35' }
sub phi::syntax::rc6::color     { '0;36' }
sub phi::syntax::rc7::color     { '0;37' }


sub phi::syntax::rall::render { $_->render for @{+shift} }

sub phi::syntax::v::val  { ${+shift} }
sub phi::syntax::v0::val { shift->[0]->val }
sub phi::syntax::v1::val { shift->[1]->val }

sub phi::syntax::vjoin::val { join '', @{+shift} }

sub phi::syntax::str_escape::val { shift->[1] }  # FIXME


sub phi::syntax::ebnf_str::val { str ${+shift}->val }

sub phi::syntax::ebnf_more_except::val { phi::parser::strclass->more_except(shift->[1]->val) }
sub phi::syntax::ebnf_many_except::val { phi::parser::strclass->many_except(shift->[1]->val) }
sub phi::syntax::ebnf_one_except::val  { phi::parser::strclass->one_except(shift->[1]->val) }
sub phi::syntax::ebnf_more_of::val     { phi::parser::strclass->more_of(shift->[1]->val) }
sub phi::syntax::ebnf_many_of::val     { phi::parser::strclass->many_of(shift->[1]->val) }
sub phi::syntax::ebnf_one_of::val      { phi::parser::strclass->one_of(shift->[1]->val) }

sub phi::syntax::ebnf_rep1::val  { shift->[0]->val * 1 }
sub phi::syntax::ebnf_rep0::val  { shift->[0]->val * 0 }
sub phi::syntax::ebnf_maybe::val { phi::parser::repeat->new(shift->[0]->val, 0, 1) }
sub phi::syntax::ebnf_as::val    { $_[0]->[0]->val >>as $_[0]->[2]->val }

sub phi::syntax::ebnf_seq::val
{
  my ($l, $r) = map $_->val, @{+shift};
  $l += $_->val for @$r;
  $l;
}

sub phi::syntax::ebnf_alt::val
{
  my ($l, $r) = map $_->val, @{+shift};
  $l |= $_->val for @$r;
  $l;
}


1;
