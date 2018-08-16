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


=head2 Assembling expressions
Most frontends (including phi2) convert expressions to something close to ANF to
flow-assemble them. So we'd have something like this:

  # if we compile this:
  x + y.bar(bif + baz, bok.bork())

  # ...we'd compile it like this:
  let tmp1 = bif + baz in
  let tmp2 = bok.bork() in
  let tmp3 = y.bar(tmp1, tmp2) in
  let tmp4 = x + tmp3 in
  tmp4

C<let>-bindings don't dynamically extend the scope; rather, phi2 finds them up
front and builds a frame class for the full set of referenced variables. That
makes it important for C<tmpN> variables to be unique gensyms in their own
namespace.

Because scopes aren't always defined in parse order, we use parsers to stage
flow assembler links. That is, parsers return objects that do two things:

1. Return a map of C<< name -> CTTI >> entries to describe their definition set
2. Return a set of C<name> entries to describe their reference set
3. Add new links to an existing flow assembler

NB: these links are right-inclusive, just like the tail of a cons cell. There is
no tail-tail or "rest of the list" otherwise.
=cut

use constant anf_link_protocol => phi::protocol->new('anf_link',
  qw/ defset
      refset
      link_onto /);


use constant anf_gensym_counter => phi::allocation
  ->constant(pack Q => 0)
  ->named('anf_gensym_counter') >> heap;

use constant anf_gensym_fn => phi::allocation
  ->constant(bin q{                     # cc
    $anf_gensym_counter                 # cc &n
    dup m64get =1 iplus                 # cc &n n+1
    sget01 m64set                       # cc &n [n++]
    m64get                              # cc n+1
    "/gensym/"                          # cc n+1 prefix
    strbuf .append_string               # cc n+1 strbuf
           .append_dec                  # cc strbuf
           .to_string                   # cc name
    swap goto                           # name })
  ->named('anf_gensym_fn') >> heap;

BEGIN
{
  bin_macros->{gensym} = bin q{$anf_gensym_fn call};
}


=head3 ANF let-link
This link binds a value into the frame. All values have names; it's up to the
frontend to manage "anonymous", e.g. linear, quantities by constructing suitable
gensyms.

Here's the struct:

  struct anf_let_link
  {
    hereptr        class;
    anf_link*      tail;
    string*        name;
    class*         ctti;
    list<string*>* refstack;
    asm*           code;
  }

=cut

use constant anf_let_link_protocol => phi::protocol->new('anf_let_link',
  qw/ name
      ctti
      refstack
      asm /);

use constant anf_let_link_class => phi::class->new('anf_let_link',
  cons_protocol,
  anf_let_link_protocol,
  anf_link_protocol)

  ->def(
    tail     => bin q{swap =8  iplus m64get swap goto},
    name     => bin q{swap =16 iplus m64get swap goto},
    ctti     => bin q{swap =24 iplus m64get swap goto},
    refstack => bin q{swap =32 iplus m64get swap goto},
    asm      => bin q{swap =40 iplus m64get swap goto},

    head => bin q{goto},

    # NB: tricky optimization: mutate the tail's defset. This works because we
    # always get a new map from the tail, but it also means we can't later
    # decide to cache these maps on the ANF links.
    defset => bin q{                    # self cc
      sget01 .ctti                      # self cc ctti
      sget02 .name                      # self cc ctti name
      sget03 .tail .defset              # self cc ctti name defs
      .{}=                              # self cc defs
      sset01 goto                       # defs },

    # NB: same optimization here (hence the .clone in the return-link refset
    # method, which is absolutely critical).
    refset => bin q{                    # self cc
      sget01 .tail .refset              # self cc trefs
      sget02 .refstack                  # self cc trefs refs
      [ sget02 sget02 .<<               # r trefs cc trefs
        sset02 =0 sset01 goto ]         # self cc trefs refs f
      swap .reduce                      # self cc trefs
      sset01 goto                       # trefs },

    link_onto => bin q{                 # asm self cc
      # TODO: add a link to zero out dropped refs
      # (we'll need CTTI cooperation for this)

      "TODO: link_onto" i.die

      sset02                            # asm' self cc
      swap .tail swap                   # asm' tail cc
      sget01 :link_onto goto            # ->tail.link_onto });


=head3 ANF return-link
This is the last link in an ANF expression. We need to specify the continuation
to invoke as well as a stack of values we want to return in the process. The
continuation is always the top stack entry.

Here's the struct:

  struct anf_return_link
  {
    hereptr        class;
    list<string*>* retstack;
  }

=cut

use constant anf_return_link_protocol => phi::protocol->new('anf_return_link',
  qw/ retstack /);

use constant anf_return_link_class =>
  phi::class->new('anf_return_link',
    cons_protocol,
    anf_return_link_protocol,
    anf_link_protocol)

  ->def(
    tail     => bin q{=0 sset01 goto},
    retstack => bin q{swap =8 iplus m64get swap goto},

    head   => bin q{goto},
    defset => bin q{strmap sset01 goto},
    refset => bin q{swap .retstack .clone swap goto},

    link_onto => bin q{                 # asm self cc
      "TODO: link_onto" i.die
      sset02 sset00 goto                # asm });


# TODO: tests


1;
