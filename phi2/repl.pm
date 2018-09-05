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


=head2 REPL
Read lines from the terminal, parse, and print the results as we get them. We
can detect multiline input by looking for a partial parse; the logic is that we
attempt to parse the whole thing and we only evaluate if we've parsed everything
we've seen so far.

Here's the struct:

  struct repl
  {
    hereptr          vtable;
    string*          buffer;
    string_buffer*   accumulator;
    dialect_context* state;
  }

=cut

use phi::protocol repl =>
  qw/ buffer
      accumulator
      state
      loop /;


use phi::class phi2_repl =>
  repl_protocol,

  buffer      => bin q{_=8  iplus m64get_ goto},
  accumulator => bin q{_=16 iplus m64get_ goto},
  state       => bin q{_=24 iplus m64get_ goto},

  loop => bin q{                        # self cc
    sget01 .buffer                      # self cc buf
    dup dup .data _ .size               # self cc buf buf.data size
    "> " i.print_string i.read          # self cc buf n
    dup
    [ goto ]
    [ =0 i.exit ]
    if call

    sget01 =8 iplus m32set              # self cc buf [buf.size=n]
    sget02 .accumulator .append_string  # self cc acc
      .to_string                        # self cc source

    # Reset buffer size
    sget02 .buffer =8 iplus lit16 0400_ m32set

    sget02 .state dialect_state         # self cc source state
    "(" dialect_expression_op pignore pseq_ignore .parse    # self cc state'

    # Is it a complete parse? If so, link the ANF stuff together, compile the
    # function, and run it.
    dup .fail?
    [ "!! parse failed" i.print_string
      drop                              # self cc
      sget01 .accumulator .clear drop   # self cc
      sget01 m64get :loop goto ]        # ->loop

    [ dup .index sget03 .accumulator .size _ ilt
      [ "..." i.print_string
        drop sget01 m64get :loop goto ] # ->loop
      [ dup .dialect_context            # self cc state ctx
          sget03 =24 iplus m64set       # self cc state [self.state=ctx]
        .value                          # self cc anf
          .link_return .head_anf
          anf_fn here_ctti_ "cc"_ .defarg
        dup .return_ctti                # self cc anf rctti
        _ .compile .call                # self cc rctti v

        sget01 .name i.print_string
        ": "         i.print_string
        asm                             # self cc rctti v asm
          .ptr                          # self cc rctti asm[v]
          _ .'to_s                      # self cc asm[v.to_s]
          .swap .goto
        .compile .call                  # self cc v.to_s
        i.pnl                           # self cc

        sget01 m64get :loop goto ]      # ->loop
      if goto ]
    if goto };


use phi::fn phi2_repl => bin q{         # cc
  =32 i.heap_allocate                   # cc repl
  $phi2_repl_class sget01 m64set        # [.vtable=]
  lit16 0400 =3 ishl bitset
                  sget01 =8  iplus m64set # [.buffer=]
  strbuf          sget01 =16 iplus m64set # [.accumulator=]
  =0 phi2_context sget01 =24 iplus m64set # [.state=]
  _ goto                                # repl };


1;
