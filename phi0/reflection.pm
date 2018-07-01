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

package phi;

use strict;
use warnings;

no warnings 'void';

use Scalar::Util qw/refaddr/;


=head2 Reflective encoding
Let's take various parts of C<phi1> and reuse them for C<phi2> to save effort.
We now have data structures we can work with, so we can generate class instances
and link them into lists/etc.


=head3 Bytecode implementations
This is a good place to start. We can bundle each machine code snippet into a
bytecode object, then store a list of all of them.
=cut

use constant bytecode_native_list =>
  list map defined bytecodes->[$_]
             ? refless_bytecode(bytecodes->[$_])
             : 0, 0..255;


=head3 Method/vtable mapping
This makes it possible to symbolically link code to boot objects, and to
disassemble existing code with method name annotations. We export a single map
from method name to its vtable index.
=cut

use constant method_vtable_mapping =>
  str_kvmap map +(str($_) => method_lookup->{$_}),
                sort keys %{+method_lookup};


=head3 Protocols
A protocol contains a list of methods and a list of classes that implement those
methods. Here's the struct:

  struct protocol
  {
    hereptr               vtable;
    linked_list<string*> *methods;
    linked_list<class*>  *classes;
  }

=cut

use constant protocol_class => phi::class->new('protocol',
  protocol_protocol)

  ->def(
    methods => bin q{swap const8  iplus m64get swap goto},
    classes => bin q{swap const16 iplus m64get swap goto});


=head3 Classes
These objects are more involved than protocols. Here's what the struct looks
like:

  struct class                          # size = 32
  {
    hereptr                 vtable;     # offset = 0
    linked_list<protocol*> *protocols;  # offset = 8
    strmap<fn*>            *methods;    # offset = 16
    hereptr                *vtable;     # offset = 24
  }

=cut

use constant class_class => phi::class->new('class',
  class_protocol)

  ->def(
    protocols => bin q{swap const8  iplus m64get swap goto},
    methods   => bin q{swap const16 iplus m64get swap goto},
    vtable    => bin q{swap const24 iplus m64get swap goto});


=head3 Exporting perl-hosted objects
This isn't quite as simple as it sounds because we have circular pointer
references. Here's what we have to do.

First, let's reserve space for all of our objects on the heap. Then we have
mutable heap entries and we can fill in the pointers using fixed addresses.
=cut

use constant class_to_phi =>
{
  map +($_->name => phi::allocation->sized(32) >> heap),
      @{+defined_classes}
};

use constant protocol_to_phi =>
{
  map +($_->name => phi::allocation->sized(24) >> heap),
      @{+defined_protocols}
};


sub export_class_as_phi($)
{
  my $c  = shift;
  my %ms = $c->methods;
  pack QQQQ => class_class->vtable >> heap,
               list(map protocol_to_phi->{$_->name}, $c->protocols),
               str_kvmap(map +(str $_ => refless_bytecode $ms{$_}),
                             sort keys %ms),
               $c->vtable >> heap;
}


sub export_protocol_as_phi($)
{
  my $p = shift;
  warn $p->name . " has no classes" unless $p->classes;
  pack QQQ => protocol_class->vtable >> heap,
              list(map str $_, $p->methods),
              list(map class_to_phi->{$_->name}, $p->classes);
}


BEGIN
{
  ${class_to_phi->{$_->name}} = export_class_as_phi $_
    for @{+defined_classes};

  ${protocol_to_phi->{$_->name}} = export_protocol_as_phi $_
    for @{+defined_protocols};
}


use constant protocol_map =>
  str_kvmap map +(str $_->name => protocol_to_phi->{$_->name}),
                @{+defined_protocols};

use constant class_map =>
  str_kvmap map +(str $_->name => class_to_phi->{$_->name}),
                @{+defined_classes};


=head2 Tests
Just some sanity checks to make sure we've exported the globals properly.
=cut

use constant reflection_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    %bytecode_natives .length lit16 0100 ieq i.assert
    %bytecode_natives lit8 lit64 swap .[]
      .here                             # cc &lit64-data
      dup m8get              lit8 48 ieq i.assert
      dup const1 iplus m8get lit8 ad ieq i.assert
      dup const2 iplus m8get lit8 48 ieq i.assert
      dup lit8+3 iplus m8get lit8 0f ieq i.assert
      drop

    # Check method index manually
    [ :length ] const1 iplus m16get bswap16
    "length" %method_vtable_mapping .{} ieq i.assert

    # Make a manual method call to the protocol list
    %protocol_map .keys                 # cc plist
    dup .length swap                    # cc plen plist
    "length" %method_vtable_mapping .{} # cc plen plist :len

    asm
      .swap .dup .m64get .method        # cc plen plist :len asm
      swap bswap16 swap .l16            # cc plen plist asm
      .call .swap .goto
    .compile .here call                 # cc plen plen2
    ieq i.assert

    goto                                # })
  ->named('reflection_test_fn') >> heap;


1;
