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


=head2 CTTI metaclass
This inherits from C<class> but supports CTTI-specific functionality including
runtime projection and parse continuations.

Here's the struct:

  struct ctti_class                             # size=88
  {
    hereptr              vtable;                # offset=0
    struct*              fields;                # offset=8
    strmap<hereptr<fn>>* methods;               # offset=16
    strmap<hereptr<fn>>* virtuals;              # offset=24
    intmap<protocol*>    protocols;             # offset=32
    strmap<ctti*>*       return_cttis;          # offset=40
    string*              name;                  # offset=48
    hereptr<fn>          parser_fn;             # offset=56
    hereptr<fn>          symbolic_method_fn;    # offset=64
    hereptr<fn>          return_ctti_fn;        # offset=72
    strmap<*>            dialect_metadata;      # offset=80
  };

It's worth noting that the C<parser_fn> takes C<in pos self> as arguments,
rather than just the usual C<in pos>. This allows you to compute the grammar on
the receiver's value.


=head3 Constant folding
This is interesting and worth talking about. CTTIs encode compile-time knowable
information about a value, which can include the state of normally
runtime-variant fields. For example, suppose I have a CTTI that defines a cons
cell and looks like this:

  struct cons                   # size = 24 bytes
  {
    hereptr vtable;
    *     head;
    cons* tail;
  };

Maybe there's a call site where we cons C<3> onto C<nil>. Because we know both
of these values at compile-time, we can generate a custom C<cons> variant CTTI
with no runtime data and whose C<head> and C<tail> getters are constant values.
The specialized struct looks like this:

  struct 3_nil_cons             # size = 8 bytes
  {
    hereptr vtable;
    # head == 3
    # tail == nil
  };

This transformation happens at the struct level using constant fields, which
have zero size and getter functions that close over their constant values.

Now, there are some limitations here. One of them is that these constants can't
ever be changed -- so degrees of specialization are permanent; we can't back out
of them at runtime if the invariant is broken. But if you have a true invariant
like static type information or C<constexpr>s, constant folding gives you a
simple way to implement it.
=cut

use phi::protocol ctti =>
  qw/ fix
      constant?

      symbolic_method_fn
      defsymbolicfn
      return_ctti_fn
      defreturncttifn
      parser_fn
      defparserfn

      name
      defname

      defreturnctti

      dialect_metadata
      symbolic_method
      return_ctti
      return_cttis
      parse /;


use phi::class ctti =>
  clone_protocol,
  ctti_protocol,
  class_protocol,
  compilable_class_protocol,
  mutable_class_protocol,

  class_class->methods_except(qw/+ clone symbolic_method/),

  "constant?" => bin q{_.fields .constant? _ goto},

  clone => bin q{                       # self cc
    =88 i.heap_allocate                 # self cc new
    sget02 sget01 =88 memcpy            # [new=self]
    dup .fields           .clone sget01 =8  iplus m64set
    dup .methods          .clone sget01 =16 iplus m64set
    dup .virtuals         .clone sget01 =24 iplus m64set
    dup .protocols        .clone sget01 =32 iplus m64set
    dup .return_cttis     .clone sget01 =40 iplus m64set
    dup .dialect_metadata .clone sget01 =80 iplus m64set
    sset01 goto                         # new },

  fix => bin q{                         # value field self cc
    sget01 .fields                      # value field self cc fs
    sget03_ .{}                         # value field self cc f
    sget04_ .fix                        # value field self cc f'
    drop sset01 sset01 goto             # self },

  return_cttis       => bin q{_=40 iplus m64get_ goto},
  defreturnctti      => bin q{          # ctti m self cc
    sget03 sget03 sget03 .return_cttis .{}= drop
    sset01 sset01 goto                  # self },

  name               => bin q{_=48 iplus m64get_ goto},
  defname            => bin q{          # name' self cc
    sget02 sget02 =48 iplus m64set      # [.name=]
    sset01 _ goto                       # self },

  dialect_metadata   => bin q{_=80 iplus m64get_ goto},
  return_ctti_fn     => bin q{_=72 iplus m64get_ goto},
  symbolic_method_fn => bin q{_=64 iplus m64get_ goto},
  parser_fn          => bin q{_=56 iplus m64get_ goto},

  defreturncttifn => bin q{             # f' self cc
    sget02 sget02 =72 iplus m64set      # [.fn=]
    sset01 _ goto                       # self },

  defsymbolicfn => bin q{               # f' self cc
    sget02 sget02 =64 iplus m64set      # [.fn=]
    sset01 _ goto                       # self },

  defparserfn => bin q{                 # f' self cc
    sget02 sget02 =56 iplus m64set      # [.parserfn=]
    sset01 _ goto                       # self },

  return_ctti => bin q{                 # m self cc
    sget01 .return_ctti_fn goto         # ->rcfn },

  symbolic_method => bin q{             # asm m self cc
    sget01 .symbolic_method_fn goto     # ->smfn },

  parse => bin q{                       # in pos self cc
    sget01 .parser_fn goto              # ->parser_fn };


use phi::fn ctti_no_parser => bin q{    # in pos self cc
  # Fail the parse to indicate that we don't accept any custom continuation.
  sset02 drop drop fail_instance _ goto # fail };


use phi::fn ctti => bin q{              # cc
  =88 i.heap_allocate                   # cc c
  $ctti_class      sget01 m64set        # [.vtable=]
  struct sget01 =8  iplus m64set        # [.fields=]
  strmap sget01 =16 iplus m64set        # [.methods=]
  strmap sget01 =24 iplus m64set        # [.virtuals=]
  intmap sget01 =32 iplus m64set        # [.protocols=]
  strmap sget01 =40 iplus m64set        # [.return_cttis=]
  "anonymous" sget01 =48 iplus m64set   # [.name=]
  $ctti_no_parser_fn
         sget01 =56 iplus m64set        # [.parser_fn=]
  $class_class :symbolic_method
         sget01 =64 iplus m64set        # [.symbolic_method_fn=]
  [ "no return CTTI fn defined" i.die ]
         sget01 =72 iplus m64set        # [.return_ctti_fn=]
  strmap sget01 =80 iplus m64set        # [.dialect_metadata=]
  _ goto                                # c };


=head3 Unit tests
NB: fairly incomplete, but enough to convince me this isn't completely screwed
up. Or maybe it is. I mean, I'll find out eventually.

These notes are written to inspire confidence.
=cut

use phi::testfn ctti_accessors => bin q{
  ctti                                  # ctti
  "accessor_test_ctti"_ .defname
  dup .fields "dispatch_fn"_ .i64
              "x"_           .i64
              "y"_           .i64 drop
  accessors                             # ctti'

  dup .name "accessor_test_ctti" .== "name=" i.assert

  .dispatch_fn                          # f

  =17 =9 sget02                         # f y x f
  get_stackptr                          # f y x f obj

  dup .dispatch_fn
    sget02 ieq "dfn==" i.assert

  dup .y =17 ieq "y17" i.assert
  dup .x =9  ieq "x9"  i.assert

  =33 sget01 .x= drop
  dup .x =33 ieq "x33" i.assert

  drop drop drop drop drop              # };


use phi::protocol ctti_array_test =>
  qw/ length
      []
      xs /;

use phi::testfn ctti_array => bin q{
  ctti
  dup .fields "dispatch_fn"_ .i64
              "length"_      .i64
              =8_ "length"_ "xs"_ .array drop

  accessors
    [                                   # i self cc
      _.xs                              # i cc &self.xs[0]
      sget02 =3 ishl iplus              # i cc &self.xs[i]
      m64get sset01 goto                # xs[i]
    ]_ "[]"_ .defvirtual

  .dispatch_fn                          # f

  =7 =5 =2 sget03 get_stackptr          # f xs[1]=7 xs[0]=5 length=2 f obj

  dup .length =2 ieq "length2" i.assert # f 7 5 length f obj
  dup =0_ .[] =5 ieq "xs0=5"   i.assert # f 7 5 length f obj
  dup =1_ .[] =7 ieq "xs1=7"   i.assert # f 7 5 length f obj

  drop drop drop drop drop drop         # };


1;
