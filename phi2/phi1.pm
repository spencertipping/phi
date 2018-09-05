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


=head2 phi1 CTTI-level interop
The goal here is to define a phi1 CTTI for each protocol we've defined. These
types know their method signatures and generate phi1-compatible virtual calls to
their underlying objects.

Because this involves a lot of porting, it's worth automating it a little bit.
The very first step is to define the method name trimming function, which gets
rid of any type specialization added by the method call compiler.
=cut

use phi::fn phi1_trim_method => bin q{  # m cc
  strbuf                                # m cc b
  [ sget02 lit8 3a ieq                  # c buf cc stop?
    [ sget01 sset02 =1 sset01 goto ]    # buf exit?=1
    [ sget02 sget02 .append_int8 sset02
      =0 sset01 goto ]                  # buf exit?=0
    if goto ]                           # m cc b fn
  sget03 .reduce                        # m cc buf
  .to_string sset01 goto                # m' };


=head3 CTTI generator
We can't generalize phi1 in a completely automatic way because some functions
return unboxed ints while others return object pointers or herepointers. Some
protocols also define overlapping methods with different signatures and/or
arities.

So we're gonna need to go through every protocol and define a phi2 signature for
it. Luckily this isn't too onerous; we just need to specify all of the argument
and return types. Method calls can be compiled automatically using a
C<symbolic_method_fn>.
=cut

use phi::fn phi1_symbolic_method => bin q{  # asm m self cc
  # The method name should be specialized here, so trim off the type
  # specialization and emit a standard virtual method call. As usual, we prefer
  # .defmethod to any virtuals defined on the underlying class.

  sget02 sget02 .methods .contains?     # asm m self cc contains?
  [ sget02 sget02 .methods .{}          # asm m self cc fn
    sget02 sset03                       # asm self self cc fn
    sget01 sset02 sset00                # asm self cc fn
    goto ]                              # ->fn
  [ sget02 phi1_trim_method             # asm m self cc m'
    sget04 .symbolic_method             # asm m self cc asm'
    sset03 sset01 drop goto ]           # asm'
  if goto                               # asm' };


=head3 Generating the CTTIs
The very first step is to automatically generate the empty CTTI instances, one
per phi1 protocol. Then we can write the return CTTI definitions.
=cut

BEGIN
{
  my @ptr_extensions;
  my @to_s_methods;
  my %defined;

  for (@{+defined_classes}, @{+defined_protocols})
  {
    my $name = $_->name;
    next if $defined{$name}++;
    phi::genconst->import("phi1ctti_$name", bin qq{
      ctti "$name"_ .defname
        dup .fields "value"_ .i64 drop

        ptr_ctti _        "to_ptr:"_ .defreturnctti
        [ sset00 goto ] _ "to_ptr:"_ .defmethod

      \$phi1_symbolic_method_fn _ .defsymbolicfn });

    push @ptr_extensions, bin qq{
      phi1ctti_$name  _ "to_$name:"_ .defreturnctti
      [ sset00 goto ] _ "to_$name:"_ .defmethod };
  }

  %defined = ();
  for (@{+defined_classes}, @{+defined_protocols})
  {
    my $name = $_->name;
    next if $defined{$name}++;
    push @to_s_methods, bin qq{
      phi1ctti_$name
        phi1ctti_byte_string_ "to_s:"_ .defreturnctti
      drop };
  }

  phi::genconst->import("ptrctti_extensions_init",
    join"", bin q{ptr_ctti},
            @ptr_extensions,
            @to_s_methods);
}


=head3 The good part
...for sufficiently generous definitions of "good".
=cut

use phi::genconst phi1ctti_init => bin q{
  phi1ctti_interpreter
    ptr_ctti_ "heap_allocate:int"_    .defreturnctti
    ptr_ctti_ "die:byte_string"_      .defreturnctti
    dup       "pnl_self:byte_string"_ .defreturnctti
  drop

  phi1ctti_list
    int_ctti_      "length:"_     .defreturnctti
    ptr_ctti_      "[]:int"_      .defreturnctti
    phi1ctti_list_ "[]=:ptr,int"_ .defreturnctti
    phi1ctti_list_ "<<:ptr"_      .defreturnctti
  drop

  phi1ctti_map
    ptr_ctti_     "{}:int"_        .defreturnctti
    phi1ctti_map_ "{}=:int,ptr"_   .defreturnctti
    phi1ctti_map_ "<<:int"_        .defreturnctti
    int_ctti_     "contains?:int"_ .defreturnctti
  drop

  phi1ctti_here
    here_ctti_ "here:"_ .defreturnctti
  drop

  phi1ctti_byte_string
    ptr_ctti_ "data:"_          .defreturnctti
    int_ctti_ "size:"_          .defreturnctti
    int_ctti_ "[]:int"_         .defreturnctti
    dup       "+:byte_string"_  .defreturnctti
    int_ctti_ "==:byte_string"_ .defreturnctti
  drop

  phi1ctti_parse_position
    ptr_ctti_ "value:"_         .defreturnctti
    int_ctti_ "fail?:"_         .defreturnctti
    dup       "with_value:ptr"_ .defreturnctti
    int_ctti_ "index:"_         .defreturnctti
  drop

  phi1ctti_parser
    phi1ctti_parse_position_ "parse:parse_position,byte_string"_ .defreturnctti
  drop

  phi1ctti_struct
    dup "i64:byte_string"_                   .defreturnctti
    dup "i32:byte_string"_                   .defreturnctti
    dup "i16:byte_string"_                   .defreturnctti
    dup "i8:byte_string"_                    .defreturnctti
    dup "array:int,byte_string,byte_string"_ .defreturnctti

    dup "here_marker:"_ .defreturnctti
    dup "ptr:"_         .defreturnctti
    dup "hereptr:"_     .defreturnctti
  drop

  phi1ctti_ctti
    int_ctti_             "constant?:"_ .defreturnctti
    ptr_ctti_             "cvalue:"_    .defreturnctti
    phi1ctti_byte_string_ "name:"_      .defreturnctti

    phi1ctti_parse_position_ "parse:parse_position,byte_string"_ .defreturnctti

    dup "defname:byte_string"_            .defreturnctti
    dup "defparser:parser"_               .defreturnctti
    dup "defreturnctti:ctti,byte_string"_ .defreturnctti

    dup "defvirtual:here,byte_string"_    .defreturnctti
    dup "defmethod:here,byte_string"_     .defreturnctti

    phi1ctti_struct_ "fields:"_ .defreturnctti

    here_ctti_ "dispatch_fn:"_ .defreturnctti

    [ sset00 _
      [ _ accessors _ goto ]_ .hereptr .call
      _ goto ]_ "accessors:"_ .defmethod
    dup         "accessors:"_ .defreturnctti

    phi1ctti_ctti_            "return_ctti:byte_string"_ .defreturnctti
    phi1ctti_macro_assembler_ "symbolic_method:macro_assembler,byte_string"_
      .defreturnctti
  drop

  phi1ctti_string_buffer
    dup "append_string:byte_string"_    .defreturnctti
    dup "append_int8:int"_              .defreturnctti
    dup "append_int16:int"_             .defreturnctti
    dup "append_int32:int"_             .defreturnctti
    dup "append_int64:int"_             .defreturnctti
    dup "append_dec:int"_               .defreturnctti
    int_ctti_ "capacity:"_              .defreturnctti
    phi1ctti_byte_string_ "to_string:"_ .defreturnctti
  drop

  phi1ctti_bytecode
    int_ctti_  "length:"_ .defreturnctti
    here_ctti_ "here:"_   .defreturnctti
    ptr_ctti_  "data:"_   .defreturnctti
    int_ctti_  "size:"_   .defreturnctti
  drop

  phi1ctti_macro_assembler
    phi1ctti_bytecode_ "compile:"_ .defreturnctti
    dup "[:"_                      .defreturnctti
    dup "]:"_                      .defreturnctti
    dup "l8:int"_                  .defreturnctti
    dup "l16:int"_                 .defreturnctti
    dup "l32:int"_                 .defreturnctti
    dup "l64:int"_                 .defreturnctti
    dup "ptr:ptr"_                 .defreturnctti
    dup "hereptr:here"_            .defreturnctti
  drop

  phi1ctti_cons
    ptr_ctti_ "head:"_ .defreturnctti
    ptr_ctti_ "tail:"_ .defreturnctti
  drop

  int_ctti
    phi1ctti_byte_string_ "to_s:"_ .defreturnctti
    [ sset00 _
      [ _ strbuf .append_dec .to_string _ goto ]_
      .hereptr .call _ goto ]_ "to_s"_ .defmethod
  drop

  ptr_ctti
    phi1ctti_byte_string_ "to_s:"_ .defreturnctti
  drop

  here_ctti
    phi1ctti_byte_string_ "to_s:"_ .defreturnctti
    [ sset00 _
      [ _ unhere .to_s _ goto ]_ .hereptr .call
      _ goto ]_ "to_s"_ .defmethod
  drop

  =0 };


=head3 phi1 object constructors
We have a bunch of stray constructor functions lying around; let's integrate
those into a single C<phi1> constant that provides a method to build each one.
=cut

use phi::genconst phi1_ctor_ctti => bin q{
  ctti "phi1_ctor"_ .defname

  phi1ctti_byte_string_   "gensym:"_ .defreturnctti
  [ gensym sset01 goto ]_ "gensym:"_ .defvirtual

  phi1ctti_ctti_        "ctti:"_ .defreturnctti
  [ ctti sset01 goto ]_ "ctti:"_ .defvirtual

  phi1ctti_list_           "list:"_ .defreturnctti
  [ intlist sset01 goto ]_ "list:"_ .defvirtual

  phi1ctti_map_           "map:"_ .defreturnctti
  [ intmap sset01 goto ]_ "map:"_ .defvirtual

  phi1ctti_macro_assembler_ "asm:"_ .defreturnctti
  [ asm sset01 goto ]_      "asm:"_ .defvirtual };


=head3 Macro assembler init
This is its own special can of worms because it's entangled with computed values
from phi0. Time for more metaprogramming.
=cut

BEGIN
{
  my @bytecode_methods = map bin qq{
    phi1ctti_macro_assembler
      dup "$_:"_ .defreturnctti
    drop },
    grep !/^s[gs]et$/, sort keys %{+insns};

  push @bytecode_methods, bin q{
    phi1ctti_macro_assembler
      dup "sget:int"_ .defreturnctti
      dup "sset:int"_ .defreturnctti
    drop };

  phi::genconst->import("asmctti_bytecodes_init",
    join"", @bytecode_methods, bin q{ =0 });
}


1;
