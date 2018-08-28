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


=head2 Generic multichannel scope objects
Most languages bind variables in some way or another, so it's worth defining a
reusable scope setup. I'm going to keep this simple for now since all of this
gets rewritten once we have phi2.

Luckily there isn't much to scopes to begin with. A scope is just a mapping from
logical name to a pair of C<ctti, anf_symbol>. Scopes have layers to them
corresponding to block structure, but this layering tracks syntax closely; we
don't have a lot of machinery surrounding lexical capture because that's handled
by dialects if they support it.

Scope objects are manipulated by parsers, so they're immutable. This means we
don't use C<strmap> etc.


=head3 Multichannel-ness
Basically, "multichannel" in our case means that we have multiple independent
linked structures, each with its own name. This is useful for languages like C++
that effectively maintain multiple parallel scopes that have little to do with
each other. For example:

  struct foo_class
  {
    // Four scope channels here: cpp, instance, cstatic, type, val
    int z;                  // modifies instance scope
    static int a;           // modifies cstatic
  # define baz 5            // modifies cpp

    void foo(int x)         // creates child type+val scopes
    {
      typedef int bar;      // modifies type
      bar bif;              // modifies val
    }

    class nested            // creates child instance+cstatic+type+val
    {
      // ...
    };
  };

C++ requires most names refer to disjoint things, but there are different
visibility rules depending on _usage_ of some values. For example, C<nested> can
contain C<sizeof z> in C++11, despite the fact that C<z> isn't a usable value
without an instance of C<foo_class>
(L<https://en.cppreference.com/w/cpp/language/nested_types> for details).


=head3 Linked structure
Scopes are forgetful: all links are to parents, not to children. So if we're
parsing a series of functions we'll store the locals only for the function we're
in right now. All prior function states were forgotten when we navigated out to
the parent value/type scopes.

This means we have something roughly like this:

  scope
    - immutable_map<string, scope_channel> channels

  scope_channel
    - scope_channel parent
    - immutable_map<string, pair<ctti, anf_symbol>> bindings

Symbol resolution follows some priority ordering among the channels, but
dialects manage this. All scopes need to do is store the data.


=head3 C<scope_channel>
Let's start here. These objects are immutable, which means we're going to burn
some memory storing parent links due to the cursor problem. That means we might
as well just define links:

  struct scope_channel_link
  {
    hereptr        vtable;
    scope_channel* parent;
    scope_channel* tail;
    string*        name;
    ctti*          ctti;
    *              anf_symbol;          # TODO: can we use ints for ANF?
  }

  struct scope_channel_nil
  {
    hereptr        vtable;
    scope_channel* parent;
  }

=cut

# NB: mutually recursive class references, so we need pointers we can use to
# instantiate them. I'll write the dispatch functions into these pointers after
# both classes are defined.
use phi::constQ scope_channel_link_classref => 0;
use phi::constQ scope_channel_nil_classref  => 0;

use phi::protocol scope_channel_child =>
  qw/ parent /;

use phi::protocol scope_channel_lookup =>
  qw/ {}
      {}=
      child /;

use phi::protocol scope_channel_link =>
  qw/ tail
      name
      ctti
      anf_symbol /;


use phi::class scope_channel_nil =>
  maybe_nil_protocol,
  scope_channel_lookup_protocol,
  scope_channel_child_protocol,

  "nil?" => bin q{=1 sset01 goto},
  parent => bin q{_=8 iplus m64get_ goto},

  "{}" => bin q{                        # name self cc
    # Ascend a level if we have a parent; otherwise return this link.
    sget01 .parent                      # name self cc parent?
    [ sget01 .parent sset01
      sget01 m64get :{} goto ]          # ->parent.{}(name parent cc)
    [ sset01 _ goto ]                   # self
    if goto                             # self|{} },

  "{}=" => bin q{                       # anf ctti name self cc
    # Cons a new link onto this one and return it.
    =48 i.heap_allocate                 # anf ctti name self cc new
    scope_channel_link_classref m64get sget01 m64set
    sget02 .parent sget01 =8 iplus m64set
    sget02 sget01 =16 iplus m64set      # [new.tail=self]
    sget03 sget01 =24 iplus m64set      # [new.name=name]
    sget04 sget01 =32 iplus m64set      # [new.ctti=ctti]
    sget05 sget01 =40 iplus m64set      # [new.anf_symbol=anf_symbol]
    sset04 sset02 drop drop goto        # new },

  child => bin q{                       # self cc
    =16 i.heap_allocate                 # self cc new
    scope_channel_nil_classref m64get sget01 m64set
    sget02 sget01 =8 iplus m64set       # [new.parent=self]
    sset01 goto                         # new };


use phi::class scope_channel_link =>
  maybe_nil_protocol,
  scope_channel_lookup_protocol,
  scope_channel_child_protocol,
  scope_channel_link_protocol,

  "nil?"     => bin q{=0 sset01 goto},
  parent     => bin q{_=8  iplus m64get_ goto},
  tail       => bin q{_=16 iplus m64get_ goto},
  name       => bin q{_=24 iplus m64get_ goto},
  ctti       => bin q{_=32 iplus m64get_ goto},
  anf_symbol => bin q{_=40 iplus m64get_ goto},

  "{}" => bin q{                        # name self cc
    sget01 .name sget03 .==             # name self cc name==?
    [ drop sset01 _ goto ]              # self
    [ goto ]                            # name self cc
    if call

    # Search the tail. Nil links will search the parent if one exists.
    sget01 .tail sset01                 # name tail cc
    sget01 m64get :{} goto              # ->tail.{} },

  "{}=" => bin q{                       # anf_symbol ctti name self cc
    # Cons a new link onto this one and return it.
    =48 i.heap_allocate                 # anf ctti name self cc new
    sget02 sget01 =16 memcpy            # [new=vtable,parent=]
    sget02 sget01 =16 iplus m64set      # [new.tail=self]
    sget03 sget01 =24 iplus m64set      # [new.name=name]
    sget04 sget01 =32 iplus m64set      # [new.ctti=ctti]
    sget05 sget01 =40 iplus m64set      # [new.anf_symbol=anf_symbol]
    sset04 sset02 drop drop goto        # new },

  child => bin q{                       # self cc
    =16 i.heap_allocate                 # self cc new
    scope_channel_nil_classref m64get sget01 m64set
    sget02 sget01 =8 iplus m64set       # [new.parent=self]
    sset01 goto                         # new };


use phi::genconst empty_scope_channel => bin q{
  $scope_channel_link_class scope_channel_link_classref m64set
  $scope_channel_nil_class  scope_channel_nil_classref  m64set

  =16 i.heap_allocate                   # nil
  scope_channel_nil_classref m64get sget01 m64set
  =0 sget01 =8 iplus m64set             # [nil.parent=0] };


# TODO: test functions


=head3 Multichannel objects
Now we need a way to tie channels together. The most efficient option is to use
an inline k/v map that looks like this:

  struct multichannel_scope
  {
    hereptr                       vtable;
    int64                         n_pairs;
    pair<string*, scope_channel*> kvs[n_pairs];
  }

Although we'll end up copying more pairs of things, we'll save enough
vtable/tail overhead over a linked structure that it's easily worth it.
=cut

use phi::protocol multichannel_scope =>
  qw/ {}
      {}=
      child
      parent
      channel_index
      []
      defchannel /;

use phi::class multichannel_scope =>
  clone_protocol,
  multichannel_scope_protocol,

  clone => bin q{                       # self cc
    =16 sget02 =8 iplus m64get          # self cc 16 npairs
    =4 ishl iplus                       # self cc size
    dup i.heap_allocate                 # self cc size new
    sget03 sget01                       # self cc size new self new
    sget03 memcpy                       # self cc size new [new=self]
    sset02 drop goto                    # new },

  "[]" => bin q{                        # i self cc
    sget01 =24 iplus                    # i self cc &c[0]
    sget03 =4 ishl iplus                # i self cc &c[i]
    sset02 sset00 goto                  # &c[i] },

  channel_index => bin q{               # channel self cc
    sget02 =8 iplus m64get              # channel self cc n
    =0                                  # channel self cc n i
    [ sget02 sget02 ilt                 # channel self cc n i loop i<n?
      [ sget04 =16 iplus                # channel self cc n i loop &k[0]
        sget02 =4 ishl iplus m64get     # channel self cc n i loop k[i]
        sget06 .==                      # channel self cc n i loop ==?
        [ drop sset03 drop sset00 goto ]# i
        [ _=1 iplus_ dup goto ]         # ->loop(i+1)
        if goto ]
      [ drop drop drop drop drop
        "lookup failed for nonexistent channel " .+ i.die ]
      if goto ]                         # channel self cc n i loop
      dup goto                          # ->loop },

    "{}" => bin q{                      # name channel self cc
      sget02 sget02 .channel_index      # name channel self cc i
      sget02 .[] m64get                 # name channel self cc c
      sget04_ .{}                       # name channel self cc link
      sset03 sset01 drop goto           # link },

    "{}=" => bin q{                     # anf ctti name channel self cc
      sget02 sget02 .channel_index dup  # anf ctti name channel self cc i i
      sget03 .[] m64get                 # anf ctti name channel self cc i c
      sget07_ sget07_ sget07_ .{}= _    # anf ctti name channel self cc c' i

      sget03 .clone _                   # anf ctti name channel self cc c' new i
      sget01 .[]                        # a ct na ch self cc c' new &new[i]
      sget02 _ m64set                   # a ct na ch self cc c' new [new[i]=c']
      sset06 drop sset03 drop drop drop goto    # new },

    child => bin q{                     # channel self cc
      _.clone                           # channel cc new
      sget02 sget01 .channel_index      # ch cc new i
      sget01 .[]                        # ch cc new &new[i]
      dup m64get .child _ m64set        # ch cc new [new[i]=new[i].child]
      sset01 goto                       # new },

    parent => bin q{                    # channel self cc
      _.clone                           # channel cc new
      sget02 sget01 .channel_index      # ch cc new i
      sget01 .[]                        # ch cc new &new[i]
      dup m64get .parent _ m64set       # ch cc new [new[i]=new[i].parent]
      sset01 goto                       # new },

    defchannel => bin q{                # channel self cc
      # Do something awful: fuse two heap allocations. This works only in non-GC
      # land.
      _.clone =16 i.heap_allocate       # channel cc new
      dup =8 iplus m64get               # channel cc new n
      sget01 .[]                        # channel cc new &new[n]
      sget03 sget01 =8 ineg iplus m64set# [new[n]=channel]
      empty_scope_channel sget01 m64set # [ch[n]=empty]
      drop                              # channel cc new

      dup =8 iplus m64get =1 iplus      # channel cc new n+1
      sget01 =8 iplus m64set            # channel cc new [new.n=n+1]
      sset01 goto                       # new };


use phi::genconst empty_multichannel_scope => bin q{
  =16 i.heap_allocate                   # scope
  $multichannel_scope_class sget01 m64set   # [.class=]
  =0 sget01 =8 iplus m64set             # [.n=] };


# TODO: tests


1;
