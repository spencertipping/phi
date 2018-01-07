#!/usr/bin/env perl

# Value types
use constant pnil => bless \my $nil_var, 'phi::nil';
sub pcons { bless [$_[0], $_[1]], 'phi::cons' }
sub pint  { bless \my $x = $_[0], 'phi::int' }
sub pstr  { bless \my $x = $_[0], 'phi::str' }
sub psym  { bless \my $x = $_[0], 'phi::sym' }
sub pmut  { bless \my $x,         'phi::mut' }

sub phi::nil::explain  { '[]' }
sub phi::cons::explain { $_[0]->head->explain . " :: " . $_[0]->tail->explain }
sub phi::int::explain  { ${+shift} }
sub phi::str::explain  { "\"${+shift}\"" }
sub phi::sym::explain  { ${+shift} }
sub phi::mut::explain  { defined ${$_[0]} ? "+" . ${$_[0]}->explain : 'M[]' }

sub phi::cons::head { shift->[0] }
sub phi::cons::tail { shift->[1] }
