#!/usr/bin/env perl

=head1 License
    phi programming language
    Copyright (C) 2018  Spencer Tipping

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
=cut

package phi::use;

use strict;
use warnings;


=head3 Perl C<use> helpers
The goal here is to specialize C<use constant> so we can write things like
C<use phi::code ...> and get custom handling. It's pretty simple: you can
request a new C<use> handler by registering a subroutine that takes all of the
arguments and returns a key/value binding list.

You need to call C<bind_use> from inside C<BEGIN>, of course.
=cut

sub import
{
  return unless @_ > 1;
  no strict 'refs';
  my ($self, $name, $handler) = @_;
  $INC{($name =~ s/::/\//gr) . ".pm"} //= 1;
  *{"$name\::handler"} = $handler;
  *{"$name\::import"} = sub
  {
    # Shift off the class; presumably the handler is already aware of which
    # "use" specialization it's handling.
    shift;
    my %kvs  = &$handler(@_);
    my $into = \%{caller . "::"};
    $$into{$_} = $kvs{$_} for keys %kvs;
  };
}

sub phi::const($)
{
  my $v = shift;
  sub() { $v };
}


use phi::use 'phi::const' => sub { shift() => phi::const shift };

use phi::use 'phi::val' => sub
{
  # Initialize a value as the return from a subroutine. This makes it easier
  # to locally side-effect.
  my ($name, $sub) = @_;
  $name => phi::const &$sub;
};


1;
