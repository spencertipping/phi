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


=head2 Key/element comparators
These are used by set-like things (both lists and maps) to determine whether
keys are considered equivalent.
=cut

use constant intcmp_fn => phi::allocation
  ->constant(bin"                       # k1 k2 cc
    sget 02 sget 02 ieq                 # k1 k2 cc eq?
    sset 02 swap drop goto              # eq?")
  ->named('intcmp_fn') >> heap;

use constant strcmp_fn => phi::allocation
  ->constant(bin"                       # s1 s2 cc
    sget 02 sget 02 .==                 # s1 s2 cc eq?
    sset 02 swap drop goto              # eq?")
  ->named('strcmp_fn') >> heap;


=head2 Linked lists
For bootup we can define most things in terms of cons cells. It's inefficient
but simple. Here's the layout:

  struct cons
  {
    hereptr       vtable;               # offset = 0
    cell          head;                 # offset = 8
    baseptr<cons> tail;                 # offset = 16
  }

=cut


use constant nil_class => phi::class->new('nil',
  clone_protocol,
  maybe_nil_protocol,
  map_protocol,
  set_protocol,
  joinable_protocol,
  list_protocol)

  ->def(
    clone => bin q{                     # self cc
      goto                              # self },

    "nil?" => bin"                      # self cc
      =1     sset01 goto                # 1",

    length => bin"                      # self cc
      =0     sset01 goto                # 0",

    "contains?" => bin"                 # x self cc
      sset 01 drop =0     swap goto     # 0",

    "[]" => bin q{                      # i self cc
      "illegal .[] on nil" i.die        # boom },

    reduce => bin q{                    # x0 f self cc
      sset01 drop goto                  # x0 },

    "+" => bin"                         # rhs self cc
      swap drop goto                    # rhs",

    "key==_fn" => bin q{=0     sset01 goto},
    keys       => bin q{goto},
    kv_pairs   => bin q{goto},
    "{}"       => bin q{                # name self cc
      "illegal {} on nil" i.die         # boom });


# NB: this class doesn't implement set_protocol because we don't know what the
# key compare function should be. The only reason nil can implement it is that
# it always returns false.
use constant cons_class => phi::class->new('cons',
  clone_protocol,
  cons_protocol,
  joinable_protocol,
  maybe_nil_protocol,
  mutable_list_protocol,
  list_protocol)

  ->def(
    clone => bin q{                     # self cc
      lit8+24 i.heap_allocate           # self cc &c
      sget02 m64get sget01 m64set       # self cc &c [.vt=]
      sget02 .head        sget01 =8      iplus m64set   # [.h=]
      sget02 .tail .clone sget01 =16     iplus m64set   # [.t=]
      sset01 goto                       # c },

    head => bin"                        # self cc
      swap =8     iplus m64get          # cc head
      swap goto                         # head",

    tail => bin"                        # self cc
      swap =16     iplus m64get         # cc tail
      swap goto                         # tail",

    "nil?" => bin"                      # self cc
      swap drop =0     swap goto        # 0",

    "+" => bin"                         # rhs self cc
      sget 02 .nil?                     # rhs self cc rhs.nil?
      [ sset 01 swap goto ]             # self
      [ =24     i.heap_allocate         # rhs self cc &cons
        sget 02 m64get                  # rhs self cc &cons vt
        sget 01 m64set                  # rhs self cc &cons [.vtable=]

        sget 02 .head                   # rhs self cc &cons self.h
        sget 01 =8     iplus m64set     # rhs self cc &cons [.head=]

        sget 03 sget 03                 # rhs self cc &cons rhs self
        .tail .+                        # rhs self cc &cons self.tail+rhs
        sget 01 =16     iplus m64set    # rhs self cc &cons [.tail=]
        sset 02 swap drop goto ]        # &cons
      if goto",

    "[]" => bin"                        # i self cc
      swap sget 02                      # i cc self i
      [ .tail sget 02                   # i cc self.t i
        =1     ineg iplus               # i cc self.t i-1
        swap .[]                        # i cc self.t[i-1]
        sset 01 goto ]                  # self.t[i-1]
      [ .head sset 01 goto ]            # self.h
      if goto",

    '<<' => bin q{
      "cons has no sane way to implement <<" i.die},

    shift => bin q{
      "cons has no sane way to implement shift" i.die},

    "[]=" => bin q{                     # x i self cc
      sget02 dup                        # x i self cc i i?
      [ =1     ineg iplus sset02        # x i-1 self cc
        sget01 .tail dup                # x i-1 self cc t t
        sset02 :[]= goto ]              # ->t.[]=
      [ drop sget03 sget02              # x i self cc x self
        =8     iplus m64set             # x i self cc [.head=x]
        sset01 sset01 goto ]            # self
      if goto                           # self },

    reduce => bin q{                    # x0 f self cc
      sget01 .head                      # x0 f self cc x
      sget04 sget04 call                # x0 f self cc x0' exit?
      [ sset03 sset01 drop goto ]       # x0'
      [ sset03                          # x0' f self cc
        swap .tail swap                 # x0' f tail cc
        sget01 m64get :reduce           # x0' f tail cc tail.reduce
        goto ]                          # tail-call
      if goto                           # r },

    length => bin"                      # self cc
      swap .tail .length                # cc self.tail.length
      =1     iplus swap goto            # self.tail.length+1");


use constant nil_instance => phi::allocation
  ->constant(pack Q => nil_class)
  ->named("nil_instance") >> heap;


sub list
{
  my $l = nil_instance >> heap;
  $l = phi::allocation->constant(pack QQQ => cons_class,
                                             pop >> heap,
                                             $l) >> heap
    while @_;
  $l;
}


use constant cons_fn => phi::allocation
  ->constant(bin"                       # t h cc
    =24     i.heap_allocate             # t h cc &cons
    \$cons_class sget 01 m64set         # t h cc &cons [.vt=]
    sget 02 sget 01 =8     iplus m64set # t h cc &cons [.h=]
    sget 03 sget 01 =16     iplus m64set# t h cc &cons [.t=]
    sset 02 swap                        # &cons cc h
    drop goto                           # &cons")

  ->named("cons_fn") >> heap;


BEGIN
{
  bin_macros->{'nil'} = bin '$nil_instance';
  bin_macros->{'::'}  = bin '$cons_fn call';
}


=head3 Linked list managed wrapper
This is a mutable object that stores an element-compare function so you can use
it like a set. Here's the struct:

  struct linked_list
  {
    hereptr       vtable;
    hereptr<fn>   element_eq_fn;
    baseptr<cons> root_cons;
  };

=cut


use constant linked_list_class => phi::class->new('linked_list',
  clone_protocol,
  list_protocol,
  joinable_protocol,
  set_protocol,
  mutable_list_protocol,
  linked_list_protocol)

  ->def(
    clone => bin q{                     # self cc
      =24     i.heap_allocate           # self cc &l
      sget02 sget01 =16     memcpy      # self cc &l [.vt=,.fn=]
      sget02 .root_cons .clone          # self cc &l cons'
      sget01 =16     iplus m64set       # self cc &l [.root_cons=]
      sset01 goto                       # &l },

    "+" => bin"                         # rhs self cc
      =24     i.heap_allocate           # rhs self cc &l
      sget 02 m64get sget 01 m64set     # rhs self cc &l [.vt=]
      sget 02 .element==_fn             # rhs self cc &l efn
      sget 01 =8     iplus m64set       # rhs self cc &l [.efn=]

      sget 03 .root_cons                # rhs self cc &l rhs.cons
      sget 03 .root_cons .+             # rhs self cc &l cons'
      sget 01 =16     iplus m64set      # rhs self cc &l [.root_cons=]
      sset 02 swap drop goto            # &l",

    length => bin"                      # self cc
      swap .root_cons .length swap goto # l",

    "[]" => bin"                        # i self cc
      sget 02 sget 02 .root_cons .[]    # i self cc x
      sset 02 swap drop goto            # x",

    "[]=" => bin q{                     # x i self cc
      sget03 sget03 sget03 .root_cons   # x i self cc x i r
      .[]=                              # x i self cc r
      drop sset01 sset01 goto           # self },

    reduce => bin q{                    # x0 f self cc
      swap .root_cons swap              # x0 f cons cc
      sget01 m64get :reduce goto        # tail-call },

    "element==_fn" => bin"swap =8      iplus m64get swap goto",
    root_cons      => bin"swap =16     iplus m64get swap goto",

    "contains?" => bin"                 # x self cc
      sget 01 .element==_fn             # x self cc fn
      sget 02 .root_cons                # x self cc fn l
      [                                 # x self cc fn loop l
        dup .nil?                       # x self cc fn loop l nil?
        [ drop drop drop sset 01 drop =0     swap goto ]
        [                               # x self cc fn loop l
          dup .head                     # x self cc fn loop l l.h
          sget 06 sget 04 call          # x self cc fn loop l eq?
          [ drop drop drop sset 01 drop =1     swap goto ]
          [ .tail sget 01 goto ]
          if goto
        ]
        if goto
      ]                                 # x self cc fn l loop
      swap sget 01 goto                 # contains?",

    shift => bin q{                     # self cc
      swap dup .root_cons               # cc self cons
      dup .head swap .tail              # cc self h t
      sget02 =16     iplus m64set       # cc self h [.root_cons=t]
      sset00 swap goto                  # h },

    # NB: << on lists is "prepend", not "append"
    "<<" => bin"                        # x self cc
      sget 01 .root_cons                # x self cc self.cons
      sget 03 ::                        # x self cc cons'
      sget 02 =16     iplus m64set      # x self cc [.root_cons=]
      sset 01 swap goto                 # self");


use constant linked_list_fn => phi::allocation
  ->constant(bin q{                             # efn cc
    =24     i.heap_allocate                     # efn cc &list
    $linked_list_class sget 01 m64set           # efn cc &list [.vt=]
    sget 02 sget 01 =8      iplus m64set        # efn cc &list [.efn=]
    nil     sget 01 =16     iplus m64set        # efn cc &list [.root_cons=]
    sset 01 goto                                # &list })

  ->named('linked_list_fn') >> heap;


BEGIN
{
  bin_macros->{intlist} = bin '$intcmp_fn $linked_list_fn call';
  bin_macros->{strlist} = bin '$strcmp_fn $linked_list_fn call';
}


use constant rev_fn => phi::allocation
  ->constant(bin q{                     # xs t cc
    [                                   # xs t loop cc
      sget03 .nil?                      # xs t loop cc nil?
      [ sset02 drop swap goto ]         # t
      [ sget03 dup .tail                # xs t loop cc xs xt
        swap .head sget04               # xs t loop cc xt x t
        swap :: sset03 sset03           # xt x::t loop cc
        sget01 goto ]                   # ->loop
      if goto ]                         # xs t cc loop
    swap sget01 goto                    # ->loop })
  ->named('rev_fn') >> heap;


use constant sort_fn => phi::allocation
  ->constant(bin q{                     # xs cmp cc
    [ sget03 .length dup =0     ieq     # xs cmp recur cc l l==0?
      swap =1     ieq ior               # xs cmp r cc l==0|l==1?
      [ sset01 drop goto ]              # xs
      [ sget03 .head                    # xs cmp r cc pivot
        intlist intlist                 # xs cmp r cc p lt ge
        sget06 .tail                    # xs cmp r cc p lt ge xt
        [                               # xs cmp r cc p lt ge xs loop cc'
          sget02 .nil?                  # xs cmp r cc p lt ge xs l cc' nil?
          [ sset01 drop goto ]          # xs cmp r cc p lt ge
          [ sget02 dup .tail            # xs cmp r cc p lt ge xs l cc' xs xt
            sset03 .head dup            # xs cmp r cc p lt ge xt l cc' x x
            sget07 swap sget0b          # ... p lt ge xt l cc' x p x cmp
            call                        # ... p lt ge xt l cc' x x<p?
            sget06 sget06 if .<< drop   # ... p lt ge xt l cc' [<<x]
            sget01 goto ]               # ->loop
          if goto ]                     # xs cmp r cc p lt ge xs loop
        dup call                        # xs cmp r cc p lt ge

        .root_cons swap                 # ... gecons lt
        .root_cons swap                 # ... ltcons gecons

        sget05 sget05 dup call          # xs cmp r cc p lt sort(ge)
        sget02 ::                       # xs cmp r cc p lt p::sort(ge)
        swap sget05 sget05 dup call     # xs cmp r cc p p::sort(ge) sort(lt)
        .+                              # xs cmp r cc p sort(lt)+(p::sort(ge))
        sset04 drop sset01 drop goto ]  # sort(lt)+(p::sort(ge))
      if goto ]                         # xs cmp cc r
    swap sget01 goto                    # ->recur })
  ->named('sort_fn') >> heap;


=head3 Linked list unit tests
Nothing too comprehensive, just enough to make sure we aren't totally off the
mark.
=cut

use constant linked_list_test_fn => phi::allocation
  ->constant(bin q{                     # cc
    nil                                 # cc nil
    dup .length =0     ieq "ll len(0)" i.assert

    =2     ::                           # cc 2::nil
    dup .length =1     ieq "ll len(1)" i.assert

    =1     ::                           # cc 1::2::nil
    dup .length =2     ieq "ll len(2)" i.assert

    dup .+                              # cc 1::2::1::2::nil
    dup .length =4     ieq "ll len(4)" i.assert
    dup =0     swap .[] =1     ieq "ll[0] = 1" i.assert
    dup =1     swap .[] =2     ieq "ll[1] = 2" i.assert
    dup =2     swap .[] =1     ieq "ll[2] = 1" i.assert
    dup lit8+3 swap .[] =2     ieq "ll[3] = 2" i.assert

    dup .tail .length lit8+3 ieq "ll.tail len(3)" i.assert

    dup                                 # cc xs xs
    [                                   # r l cc
      sget02 sget02 ilt                 # r l cc l<r
      sset02 sset00 goto ]              # cc xs xs cmp
    $sort_fn call                       # cc xs sort(xs)

    dup .length =4     ieq "llS len(4)" i.assert
    dup =0     swap .[] =1     ieq "llS[0] = 1" i.assert
    dup =1     swap .[] =1     ieq "llS[1] = 1" i.assert
    dup =2     swap .[] =2     ieq "llS[2] = 2" i.assert
    dup lit8+3 swap .[] =2     ieq "llS[3] = 2" i.assert

    $nil_instance $rev_fn call          # cc xs rev(sort(xs))

    dup .length =4     ieq "rev len(4)" i.assert
    dup =0     swap .[] =2     ieq "rev[0] = 2" i.assert
    dup =1     swap .[] =2     ieq "rev[1] = 2" i.assert
    dup =2     swap .[] =1     ieq "rev[2] = 1" i.assert
    dup lit8+3 swap .[] =1     ieq "rev[3] = 1" i.assert

    drop                                # cc l

    dup                                 # cc l l
    =0     swap                         # cc l 0 l
    [                                   # x x0 cc
      sget02 sget02 iplus sset02        # x0' x0 cc
      =0     sset01                     # x0' 0 cc
      goto ] swap                       # cc l 0 [f] l
    .reduce lit8+6 ieq "sum = 6" i.assert     # cc l

    drop
    strlist                             # cc []
    "foo" swap .<<                      # cc ["foo"]
    dup "foo" swap .contains?      "contains(foo)"  i.assert
    dup "bar" swap .contains? inot "!contains(bar)" i.assert
    drop

    goto                                # })
  ->named('linked list test fn') >> heap;


1;
