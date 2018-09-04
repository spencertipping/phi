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


=head2 Linked associative maps
The only remarkable thing about this implementation is that the map object is
responsible for storing the key comparator function, which takes two key cells
and returns a number indicating equal/not equal. We're not doing any ordering in
the map; it's strictly a linear search.

Here are the struct definitions:

  struct kv_cons                        # size = 32 bytes
  {
    hereptr vtable;
    cell    key;
    cell    value;
    baseptr next;
  };

  struct linked_map                     # size = 24 bytes
  {
    hereptr          vtable;
    hereptr<fn>      key_eq_fn;
    baseptr<kv_cons> alist;
  };

Linked maps return their alist for the keyset; the KV cons links behave as a
list of keys if you address them using the list protocol.
=cut


use phi::class kv_cons =>
  clone_protocol,
  list_protocol,
  joinable_protocol,
  cons_protocol,
  kv_protocol,
  mutable_value_protocol,
  maybe_nil_protocol,

  clone => bin q{                     # self cc
    =32     i.heap_allocate           # self cc &c
    sget02 sget01 =24     memcpy      # self cc &c [.vt=,.k=,.v=]
    sget02 .tail .clone               # self cc &c tail'
    sget01 =24     iplus m64set       # self cc &c [.tail=]
    sset01 goto                       # &c },

  key   => bin"swap =8      iplus m64get swap goto",
  value => bin"swap =16     iplus m64get swap goto",
  head  => bin"swap =8      iplus m64get swap goto",
  tail  => bin"swap =24     iplus m64get swap goto",

  'value=' => bin q{                  # v self cc
    sget02 sget02 =16     iplus       # v self cc v &value
    m64set sset01 swap goto           # self },

  "nil?" => bin"=0     sset01 goto",

  "+" => bin"                         # rhs self cc
    sget 02 .nil?                     # rhs self cc rhs.nil?
    [ sset 01 swap goto ]             # self
    [ =32     i.heap_allocate         # rhs self cc &cons
      sget 02 m64get                  # rhs self cc &cons vt
      sget 01 m64set                  # rhs self cc &cons [.vtable=]

      sget 02 .key                    # rhs self cc &cons self.k
      sget 01 =8     iplus m64set     # rhs self cc &cons [.key=]

      sget 02 .value                  # rhs self cc &cons self.v
      sget 01 =16     iplus m64set    # rhs self cc &cons [.value=]

      sget 03 sget 03                 # rhs self cc &cons rhs self
      .tail .+                        # rhs self cc &cons self.tail+rhs
      sget 01 =24     iplus m64set    # rhs self cc &cons [.tail=]
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
    =1     iplus swap goto            # self.tail.length+1";


use phi::class linked_map =>
  clone_protocol,
  map_protocol,
  set_protocol,
  list_protocol,
  joinable_protocol,
  mutably_joinable_protocol,
  mutable_map_protocol,
  mutable_set_protocol,
  linked_map_protocol,

  clone => bin q{                     # self cc
    =24     i.heap_allocate           # self cc &m
    sget02 sget01 =16     memcpy      # self cc &m [.vt=,.fn=]
    sget02 .kv_pairs .clone           # self cc &m alist'
    sget01 =16     iplus m64set       # self cc &m [.alist=]
    sset01 goto                       # &l },

  length => bin q{swap .keys .length swap goto},
  '[]'   => bin q{                    # i self cc
    sget02 sget02 .keys .[]           # i self cc keys[i]
    sset02 sset00 goto                # keys[i] },

  '+='   => bin q{                    # rhs self cc
    sget02 .kv_pairs                  # rhs self cc rkvs
    sget02 .kv_pairs .+               # rhs self cc kvs'
    sget02 =16 iplus m64set           # rhs self cc [self.kvs=kvs']
    sset01 _ goto                     # self },

  '+' => bin q{                       # rhs self cc
    _ .clone                          # rhs cc new
    sget02_ .+=                       # rhs cc new [new+=rhs]
    sset01 goto                       # new },

  reduce => bin q{                    # x0 f self cc
    sget01 .keys sset01               # x0 f keys cc
    sget01 m64get :reduce goto        # ->keys.reduce },

  "key==_fn" => bin"                  # self cc
    swap =8     iplus m64get swap goto# fn",

  keys => bin"                        # self cc
    swap =16     iplus m64get         # cc self.alist
    swap goto                         # self.alist",

  kv_pairs => bin q{                  # self cc
    swap .keys swap goto              # keys },

  kvcell_for => bin"                  # k self cc
    swap dup =8     iplus m64get      # k cc self keyeqfn
    swap    =16     iplus m64get      # k cc keyeqfn alist
    [                                 # k cc kfn loop alist|nil
      dup .nil?                       # k cc kfn loop alist nil?
      [                               # k cc kfn loop nil
        sset 03 drop drop goto        # nil
      ]
      [                               # k cc kfn loop kvcons
        dup .key                      # k cc kfn loop kvcons key
        sget 05                       # k cc kfn loop kvcons key k
        sget 04 call                  # k cc kfn loop kvcons eq?
        [                             # k cc kfn loop kvcons
          sset 03 drop drop goto      # kvcons
        ]
        [                             # k cc kfn loop kvcons
          .tail sget 01 goto          # ...
        ]
        if goto
      ]
      if goto
    ]                                 # k cc kfn alist loop
    swap sget 01                      # k cc kfn loop alist loop
    goto                              # kvcons|nil",

  "contains?" => bin"                 # k self cc
    sget 02 sget 02                   # k self cc k self
    .kvcell_for .nil? inot            # k self cc contains?
    sset 02 swap drop goto            # contains?",

  "<<" => bin q{                      # k self cc
    sget02 sget02 .contains?          # k self cc contains?
    [ sset01 swap goto ]              # self
    [ sget01 .length                  # k self cc len
      sget03 sget03 .{}=              # k self cc self [self{k}=len]
      drop sset01 swap goto ]         # self
    if goto                           # self },

  "{}" => bin q{                      # k self cc
    sget02 dup sget03                 # k self cc k k self
    .kvcell_for dup .nil?             # k self cc k kv nil?
    [ "map key lookup failed for " i.print_string
      drop debug_trace dup .to_s i.pnl i.pnl debug_trace
      "this is a problem" i.die ]
    [ .value                          # k self cc k v
      sset03 drop sset00 goto ]       # v
    if goto                           # v|die },

  "{}=" => bin q{                     # v k self cc
    sget02 sget02 .kvcell_for         # v k self cc cell
    dup .nil?                         # v k self cc cell nil?

    [ drop                            # v k self cc
      =32     i.heap_allocate         # v k self cc &kv

      $kv_cons_class sget 01 m64set             # v k self cc &kv [.vt=]
      sget 03 sget 01 =8      iplus m64set      # [.k=]
      sget 04 sget 01 =16     iplus m64set      # [.v=]
      sget 02 =16     iplus m64get    # v k self cc &kv alist
      sget 01 =24     iplus m64set    # v k self cc &kv [.tail=]
      sget 02 =16     iplus m64set    # v k self cc [.alist=]

      sset 02 swap drop               # cc self
      swap goto ]                     # self

    [ # Existing cell: modify it in place
      sget04 swap .value= drop        # v k self cc
      sset01 sset01 goto ]            # self

    if goto                           # self };


sub kvmap
{
  my $cmp = shift;
  my $kvs = nil;

  while (@_)
  {
    my $v = pop;
    my $k = pop;
    $kvs = phi::allocation->constant(pack QQQQ => kv_cons_class,
                                                  $k,
                                                  $v,
                                                  $kvs) >> heap;
  }

  phi::allocation->constant(pack QQQ => linked_map_class,
                                        $cmp,
                                        $kvs) >> heap;
}

sub int_kvmap { kvmap intcmp_fn, @_ }
sub str_kvmap { kvmap strcmp_fn, @_ }


use phi::fn linked_map => bin q{                # kfn cc
  =24     i.heap_allocate                       # kfn cc &map
  $linked_map_class sget 01 m64set              # kfn cc &map [.vt=]
  sget 02 sget 01 =8      iplus m64set          # kfn cc &map [.kfn=]
  nil     sget 01 =16     iplus m64set          # kfn cc &map [.alist=]
  sset 01 goto                                  # &map };

use phi::binmacro intmap => bin q{$intcmp_fn linked_map};
use phi::binmacro strmap => bin q{$strcmp_fn linked_map};


use phi::testfn linked_map => bin q{  #
  intmap                              # {}

  dup .keys .length =0     ieq "keys len(0)" i.assert
  dup       .length =0     ieq "maplen(0)"   i.assert

  =2     swap =1     swap .{}=        # {1->2}
  dup .keys .length =1     ieq "keys len(1)"  i.assert
  dup .keys .head   =1     ieq "keys head(1)" i.assert
  dup .keys .value  =2     ieq "keys val(2)"  i.assert
  dup =1     swap .contains?      "contains(key 1)"  i.assert
  dup =2     swap .contains? inot "!contains(val 2)" i.assert

  dup .length =1     ieq "maplen(1)" i.assert

  =8     swap =4     swap .{}=        # {1->2, 4->8}
  dup .keys .length =2     ieq "keylen(2)" i.assert
  dup =4     swap .contains?      "contains(4)"  i.assert
  dup =8     swap .contains? inot "!contains(8)" i.assert
  dup .length =2     ieq "maplen(2)" i.assert

  dup =1     swap .{} =2     ieq "{1}=2" i.assert
  dup =4     swap .{} =8     ieq "{4}=8" i.assert

  # Assert key ordering since the map behaves like a list
  dup =0     swap .[] =4     ieq "[0]=4" i.assert
  dup =1     swap .[] =1     ieq "[1]=1" i.assert

  # Update an existing value and make sure we don't cons up a new entry
  =16     swap =4     swap .{}=       # {1->2, 4->16}

  dup .keys .length =2     ieq "keylen(2)" i.assert
  dup =4     swap .contains?      "contains(4)"  i.assert
  dup =8     swap .contains? inot "!contains(8)" i.assert
  dup .length =2     ieq "maplen(2)" i.assert

  dup =1     swap .{} =2      ieq "{1}=2"  i.assert
  dup =4     swap .{} =16     ieq "{4}=16" i.assert

  drop

  strmap                              # {}
  lit8 +55 swap "foo" swap .{}=       # {foo->55}
  lit8 +91 swap "bar" swap .{}=       # {foo->55, bar->91}

  dup "foo" swap .contains?      "contains(key foo)"  i.assert
  dup "bar" swap .contains?      "contains(key bar)"  i.assert
  dup "bif" swap .contains? inot "!contains(bif)"     i.assert
  dup "baz" swap .contains? inot "!contains(baz)"     i.assert

  dup "foo" swap .{} lit8+55 ieq "{foo}=55" i.assert
  dup "bar" swap .{} lit8+91 ieq "{bar}=91" i.assert

  dup "foo" swap .keys .head ieq inot "head!=foo" i.assert

  "bif" swap .<<                      # {foo->55, bar->91, bif->1}
  dup "bif" swap .contains? "contains(bif)" i.assert

  drop                                #

  strmap "bar"_ "foo"_ .{}=
  strmap "baz"_ "bif"_ .{}= .+        # {foo->bar,bif->baz}
  dup "foo"_ .{} "bar" .== "+foo" i.assert
  dup "bif"_ .{} "baz" .== "+foo" i.assert
  drop                                # };


1;
