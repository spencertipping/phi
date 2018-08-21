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


=head2 Parse state
Frontend parse states are slightly nontrivial because they have a bit of
negotiation to do. In particular, not all frontends support:

1. Lexical scoping and lambda-capture
2. Dynamic scoping
3. Prefix-free variable addressing
4. Variable addressing at all

We need to generalize across these differences so CTTIs that offer custom parse
continuations can refer to defined quantities by name. That makes it possible to
write things like this:

  char const *str = "hello";
  html_element *e =
    html <h1>                           // at this point html owns the parse
           <div>$str</div>              // <- reference to defined variable
         </h1>;

Another way to look at it is that CTTI and scopes are the two sides of the
semantic catalog phi offers to frontends -- just like the file tree and inode
semantics are the two sides of the catalog offered by filesystems.

Here's what the state struct looks like:

  struct phi2_parse_state
  {
    hereptr     class;
    int64       index;                  # string position
    phi2_scope* scope;
    *           value;
    *           context;                # opaque state, e.g. an opgate
  }

TODO: some languages use separate scope chains for compile-time and runtime
values, e.g. C and C++. It might be worth maintaining multiple scopes to make
this work.
=cut

use phi::protocol phi2_parse_state =>
  qw/ scope
      context
      with_scope
      with_context
      with_local
      with_captured /;

use phi::class phi2_parse_state =>
  parse_position_protocol,
  linear_position_protocol,
  phi2_parse_state_protocol,

  index   => bin q{swap =8  iplus m64get goto},
  scope   => bin q{swap =16 iplus m64get goto},
  value   => bin q{swap =24 iplus m64get goto},
  context => bin q{swap =32 iplus m64get goto},

  with_value => bin q{                # v self cc
    =40 i.heap_allocate               # v self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]
    sget03 sget01 =24 iplus m64set    # [.value=]
    sset02 sset00 goto                # self' },

  with_scope => bin q{                # scope self cc
    =40 i.heap_allocate               # scope self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]
    sget03 sget01 =16 iplus m64set    # [.scope=]
    sset02 sset00 goto                # self' },

  with_context => bin q{              # c self cc
    =40 i.heap_allocate               # c self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]
    sget03 sget01 =32 iplus m64set    # [.context=]
    sset02 sset00 goto                # self' },

  with_local => bin q{                # ctti name self cc
    sget03 sget03 sget03 .scope       # c n s cc c n scope
    .with_local                       # c n s cc scope'
    sget02 .with_scope                # c n s cc self'
    sset03 sset01 drop goto           # self' },

  with_captured => bin q{             # ctti name self cc
    sget03 sget03 sget03 .scope       # c n s cc c n scope
    .with_captured                    # c n s cc scope'
    sget02 .with_scope                # c n s cc self'
    sset03 sset01 drop goto           # self' },

  "fail?" => bin q{=0 sset01 goto},
  "+"     => bin q{                   # n self cc
    =40 i.heap_allocate               # n self cc self'
    sget02 sget01 =40 memcpy          # [self'=self]
    dup =8 iplus dup m64get           # n self cc self' &index index
    sget05 iplus swap m64set          # n self cc self' [.index+=n]
    sset02 sset00 goto                # self' };


# TODO: ctor functions
# TODO: tests


1;
