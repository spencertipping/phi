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


=head3 Methods by hash
This is good to have around. We can't use it for the phi0 method resolver
because we don't have enough C<bin> macros defined to issue any real method
calls, but we can use it down the line if we want to match against any C<lit64>
constants in our bytecode.
=cut

use constant methods_by_hash =>
  int_kvmap map +(defined_methods->{$_} => str $_),
            sort keys %{+defined_methods};


=head3 Collision detection
We shouldn't have method collisions, but if we do it's better to discover it at
compile-time than to try to debug the insanity later.
=cut

{
  my %seen_hashes;
  ++$seen_hashes{$_} for values %{+defined_methods};

  my @collisions = grep $seen_hashes{defined_methods->{$_}} > 1,
                        sort keys %{+defined_methods};
  die "METHOD HASH COLLISION for @collisions" if @collisions;
}


=head3 Protocols
A protocol contains a list of methods and a list of classes that implement those
methods. Here's the struct:

  struct protocol
  {
    hereptr  vtable;
    strmap  *virtuals;
    intmap  *classes;
  }

=cut

use constant exported_protocol_class => phi::class->new('exported_protocol',
  symbolic_method_protocol,
  protocol_protocol)

  ->def(
    virtuals => bin q{swap =8      iplus m64get swap goto},
    classes  => bin q{swap =16     iplus m64get swap goto},

    symbolic_method => bin q{           # asm m self cc
      sget02 method_hash bswap64        # asm m self cc mh
      sget04                            # asm m self cc mh asm
        .dup .m64get .lit64 .l64 .swap  # [obj m fn]
        .call .call                     # asm m self cc asm'
      sset03 sset01 drop goto           # asm' });


=head3 Classes
These objects are more involved than protocols. Here's what the struct looks
like:

  struct exported_class                 # size = 24
  {
    hereptr      vtable;                # offset = 0
    intmap      *protocols;             # offset = 8
    strmap<fn*> *virtuals;              # offset = 16
  }

All phi0/phi1 methods are virtual because C<bin> isn't a typed assembler.
=cut

use constant exported_class_class => phi::class->new('exported_class',
  symbolic_method_protocol,
  class_protocol)

  ->def(
    protocols => bin q{swap =8      iplus m64get swap goto},
    virtuals  => bin q{swap =16     iplus m64get swap goto},
    methods   => bin q{strmap sset01 goto},
    fields    => bin q{"unimplemented: exported_class.fields" i.die},

    symbolic_method => bin q{           # asm m self cc
      # All exported methods are virtual, so link it directly to bypass method
      # resolution.
      sget02 sget02 .virtuals .{} .here # asm m self cc fn
      sget04 .hereptr .call             # asm m self cc asm'
      sset03 sset01 drop goto           # asm' });


=head3 Exporting perl-hosted objects
This isn't quite as simple as it sounds because we have circular pointer
references. Here's what we have to do.

First, let's reserve space for all of our objects on the heap. Then we have
mutable heap entries and we can fill in the pointers using fixed addresses.
=cut

use constant class_to_phi =>
{
  map +($_->name => phi::allocation->sized(24)->named("C" . $_->name) >> heap),
      @{+defined_classes}
};

use constant protocol_to_phi =>
{
  map +($_->name => phi::allocation->sized(24)->named("P" . $_->name) >> heap),
      @{+defined_protocols}
};


sub export_class_as_phi($)
{
  my $c  = shift;
  my %ms = $c->methods;
  pack QQQ => exported_class_class->fn >> heap,
              int_kvmap(map +(protocol_to_phi->{$_->name} => 0), $c->protocols),
              str_kvmap(map +(str $_ => refless_bytecode $ms{$_}),
                            sort keys %ms);
}


sub export_protocol_as_phi($)
{
  my $p = shift;
  pack QQQ => exported_protocol_class->fn >> heap,
              str_kvmap(map +(str $_                   => 0), $p->methods),
              int_kvmap(map +(class_to_phi->{$_->name} => 0), $p->classes);
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


=head2 Structs for our classes
Every class so far has a struct that governs its layout, but that struct doesn't
exist in phi terms yet. We need to encode it, or more specifically encode a
function to generate it, so we can interpret the heap correctly.

Structs are stored in a map keyed on vtables.
=cut

use constant generate_structs_fn => phi::allocation
  ->constant(bin q{                     # cc
    intmap                              # cc m

    struct                   "fn" i64f
                         "length" i32f
           "length" =1     "data" arrf
    swap $byte_string_class swap .{}=   # cc m

    struct "fn"             i64f
           "heap_base"      i64f
           "heap_allocator" i64f
           "heap_limit"     i64f
           "globals"        i64f
           "here_marker"    =2     fixf
           "bytecode_insns" lit16 0800 fixf
    swap $interpreter_class swap .{}=   # cc m

    struct "fn" i64f
    swap $nil_class swap .{}=           # cc m

    struct "fn"   i64f
           "head" i64f
           "tail" i64f
    swap $cons_class swap .{}=          # cc m

    # Struct structs
    struct "fn" i64f
    swap $nil_struct_link_class swap .{}=

    struct "fn"              i64f
           "tail"            i64f
           "name"            i64f
           "fget_fn"         i64f
           "fset_fn"         i64f

           "left_offset"     i64f
           "size"            i64f
           "size_fn"         i64f
           "right_offset_fn" i64f
           "getter_fn"       i64f
           "setter_fn"       i64f
    swap $cons_struct_link_class swap .{}=

    swap goto                           # m })
  ->named('generate_structs_fn') >> heap;


=head2 Tests
Just some sanity checks to make sure we've exported the globals properly.
=cut

use constant reflection_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    %bytecode_natives .length lit16 0100 ieq "bytecodelen" i.assert
    %bytecode_natives lit8 lit64 swap .[]
      .here                             # cc &lit64-data
      dup m8get              lit8 48 ieq "0:48" i.assert
      dup =1     iplus m8get lit8 ad ieq "1:ad" i.assert
      dup =2     iplus m8get lit8 48 ieq "2:48" i.assert
      dup lit8+3 iplus m8get lit8 0f ieq "3:0f" i.assert
      drop

    # Make a manual method call to the protocol list
    %protocol_map .keys                 # cc plist
    dup .length swap                    # cc plen plist
    "length" method_hash bswap64        # cc plen plist :len

    asm
      .swap .dup .m64get .lit64         # cc plen plist :len asm[...lit64]
      .l64                              # cc plen plist asm[...lit64 mh]
      .swap .call .call
      .swap .goto
    .compile .here call                 # cc plen plen2
    ieq "compiled method len" i.assert

    goto                                # })
  ->named('reflection_test_fn') >> heap;


1;
