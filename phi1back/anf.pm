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
assemble them. This gives us a simple way to track CTTI, generate frame objects,
and address local variables. So we'd have something like this:

  # if we compile this:
  x + y.bar(bif + baz, bok.bork())

  # ...we'd generate ANF like this:
  let tmp1 = bif + baz in               # this is one link...
  let tmp2 = bok.bork() in              # ...whose tail is this
  let tmp3 = y.bar(tmp1, tmp2) in       # ...whose tail is this
  let tmp4 = x + tmp3 in                # ...
  tmp4                                  # ...and this link has no tail

C<let>-bindings don't dynamically extend the scope; rather, phi2 finds them up
front and builds a frame class for the full set of referenced variables. That
makes it important for C<tmpN> variables to be unique gensyms in their own
namespace. I use a global gensym function for this.

Because scopes aren't always defined in parse order, we use parsers to stage
flow assembler links. That is, parsers return objects that do two things:

1. Return a map of C<< name -> CTTI >> entries to describe their definition set
2. Return a set of C<name> entries to describe their reference set

NB: these links are right-inclusive, just like the tail of a cons cell. There is
no tail-tail or "rest of the list" otherwise.
=cut


use phi::protocol anf_block        => qw/ body return_ctti /;
use phi::protocol anf_header       => qw/ value ctti /;
use phi::protocol anf_link         => qw/ defset refset into_asm /;
use phi::protocol anf_mutable_link => qw/ tail= /;


use phi::fn anf_return_name => bin q{   # link cc
  swap                                  # cc link
  [                                     # cc loop link
    dup .tail dup                       # cc loop link lt lt?
    [ sset00 sget01 goto ]              # ->loop(lt)
    [ drop sset00 swap goto ]           # link
    if goto ]                           # cc link loop
  swap sget01 goto                      # ->loop(link) };

use phi::fn anf_link_kvs_to_struct => bin q{      # struct kvs cc
  [ sget02 .nil?                        # struct kvs cc loop nil?
    [ drop sset00 goto ]                # struct
    [ sget03                            # st kvs cc loop st
      sget03 .name                      # st kvs cc loop st name
      sget04 .value .struct_link        # st kvs cc loop st'
      sset03                            # st' kvs cc loop
      sget02 .tail sset02               # st' kvs' cc loop
      dup goto ]                        # ->loop
    if goto ]                           # struct kvs cc loop
  dup goto                              # ->loop };

use phi::constQ gensym_counter => 0;
use phi::fn gensym => bin q{            # cc
  $gensym_counter                       # cc &n
  dup m64get =1 iplus                   # cc &n n+1
  sget01 m64set                         # cc &n [n++]
  m64get                                # cc n+1
  "/gensym/"                            # cc n+1 prefix
  strbuf .append_string                 # cc n+1 strbuf
         .append_dec                    # cc strbuf
         .to_string                     # cc name
  swap goto                             # name };


=head3 ANF fn header
Kicks off a new function with specified arguments. This link looks at its tail
and generates a frame class to hold all locals. It also compiles into a function
object that contains information about its CTTI (based on the declared CTTI of
the return variable).

Here's the struct:

  struct anf_fn
  {
    hereptr                      class;
    anf_link*                    body;
    ordered_map<string*, ctti*>* args;
    struct*                      frame_struct;
    anf_fn*                      fn;
  }

Function CTTIs are represented using classes that provide a method called C<()>
to call the object as a function. The class object closes over the compiled fn
hereptr.
=cut

use phi::protocol anf_fn =>
  qw/ args
      defarg
      frame_class
      generate_frame_class
      generate_fn /;

use phi::class anf_fn =>
  anf_header_protocol,
  anf_block_protocol,
  anf_fn_protocol,

  body => bin q{swap =8  iplus m64get swap goto},
  args => bin q{swap =16 iplus m64get swap goto},

  defarg => bin q{                    # ctti name self cc
    sget03 sget03 sget03 .args .{}=   # ctti name self cc args
    drop sset01 sset01 goto           # self },

  frame_class => bin q{               # self cc
    sget01 =24 iplus dup m64get       # self cc &fs fs
    [ m64get sset01 goto ]            # fs
    [ sget02 .generate_frame_class    # self cc &fs fs
      sget01 m64set                   # [fs=fs]
      m64get sset01 goto ]            # fs
    if goto                           # fs },

  value => bin q{                     # self cc
    sget01 =32 iplus dup m64get       # self cc &fn fn
    [ m64get sset01 goto ]            # fn
    [ sget02 .generate_fn             # self cc &fn fn
      sget01 m64set                   # [fn=fn]
      m64get sset01 goto ]            # fn
    if goto                           # fn },

  ctti => bin q{                      # self cc
    "TODO: fn.ctti" i.die },

  return_ctti => bin q{               # self cc
    sget01 .body                      # self cc link
    $anf_return_name_fn call          # self cc name
    sget02 .defset .{}                # self cc ctti
    sset01 goto                       # ctti },

  generate_frame_class => bin q{      # self cc
    # Prefix these fields with something special so we don't collide with
    # locals. I should probably do this better, but this will work for now.
    struct
      "/class"        i64f
      "/parent_frame" i64f            # self cc struct

    # The frame struct is positioned so we can build it to overlap with the
    # stack and place the arguments correctly. This means we first cons up the
    # defset, then add in the args.
    sget02 .defset .kv_pairs          # self cc struct def-kvs
    $anf_link_kvs_to_struct_fn call   # self cc struct'

    # Ok, now cons the args in stack ordering. The args must include the
    # continuation to return to, if one exists.
    sget02 .args .kv_pairs            # self cc struct arg-kvs
    $anf_link_kvs_to_struct_fn call   # self cc struct'

    class accessors                   # self cc class
    sset01 goto                       # class },

  generate_fn => bin q{               # self cc
    asm                               # self cc asm
    sget02 .frame_class .dispatch_fn  # self cc asm cfn
    swap                              # self cc cfn asm

    # Construct the frame to overlap with the calling stack. We do this by
    # pushing =0 elements, one per defset entry but omitting the arguments.
    # Then we can push the parent frame and the class dispatch function.
    sget03 .defset                    # self cc cfn asm defs
    [ swap .lit8 .0                   # d cc asm[=0]
      sset01 =0 swap goto ]           # self cc cfn asm defs f
    swap .reduce                      # self cc cfn asm[=0...]
      .get_frameptr                   # self cc cfn asm[=0... f]
      .hereptr                        # self cc asm[=0... f cfn]
      .get_stackptr .set_frameptr     # self cc asm[=0... f cfn|]

    # Now assemble the body, which will be able to refer to the frame using
    # get_frameptr.
    sget02 .frame_class               # self cc asm frame_ctti
    sget03 .body .into_asm            # self cc asm
    .compile .here                    # self cc fn
    sset01 goto                       # fn };


use phi::fn anf_fn => bin q{            # body cc
  =40 i.heap_allocate                   # body cc &f
  $anf_fn_class sget01 m64set           # [.class=]
  sget02 sget01 =8  iplus m64set        # [.body=]
  strmap sget01 =16 iplus m64set        # [.args=]
  =0     sget01 =24 iplus m64set        # [.frame_struct=]
  =0     sget01 =32 iplus m64set        # [.fn=]
  sset01 goto                           # &f };


=head3 ANF continuation link
This is the same thing as ANF function, but binds to a name and reuses the
calling frame. You'd use this for nonescaping local continuations, e.g.
arguments to C<if> or a loop.

Continuations can't (meaning "shouldn't") escape from the functions in which
they're defined. If they do, they'll address the frame incorrectly.
Continuations also aren't generally recursive; you're limited to tail recursion
for local looping.

Here's the struct:

  struct anf_continuation
  {
    hereptr   class;
    anf_link* tail;
    string*   name;
    anf_link* body;
  }

=cut

use phi::class anf_continuation =>
  cons_protocol,
  anf_link_protocol,
  anf_block_protocol,

  tail => bin q{swap =8  iplus m64get swap goto},
  name => bin q{swap =16 iplus m64get swap goto},
  body => bin q{swap =24 iplus m64get swap goto},

  head => bin q{goto},

  defset => bin q{                    # self cc
    sget01 .tail .defset              # self cc tdefs
    sget02 .body .defset .+           # self cc defs
    sget02 .ctti swap                 # self cc ctti defs
    sget03 .name swap                 # self cc ctti name defs
    .{}=                              # self cc defs
    sset01 goto                       # defs },

  refset => bin q{                    # self cc
    sget01 .tail .refset              # self cc trefs
    sget02 .body .refset .+           # self cc refs
    sset01 goto                       # refs },

  into_asm => bin q{                  # asm frame_ctti self cc
    asm sget03 sget03 .body .into_asm # asm f self cc body-asm
    .compile .fn                      # asm f self cc fn
    sget04                            # asm f self cc fn asm[]
      .hereptr                        # asm f self cc asm[fn]
      .get_frameptr                   # asm f self cc asm[fn f]
    "=" sget03 .name .+               # asm f self cc asm[fn f] "name="
    sget04 .symbolic_method           # asm f self cc asm[fn f .name=]
    drop                              # asm f self cc

    # Tail-call the tail assembly
    swap .tail swap                   # asm f tail cc
    sget01 m64get :into_asm goto      # ->tail.into_asm };


use phi::fn anf_continuation => bin q{  # body cc
  # TODO
  };


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
    bytecode*      code;
  }

=cut

use phi::protocol anf_let_link =>
  qw/ name
      ctti
      refstack
      code /;

use phi::class anf_let_link_class =>
  cons_protocol,
  anf_let_link_protocol,
  anf_link_protocol,

  tail     => bin q{swap =8  iplus m64get swap goto},
  name     => bin q{swap =16 iplus m64get swap goto},
  ctti     => bin q{swap =24 iplus m64get swap goto},
  refstack => bin q{swap =32 iplus m64get swap goto},
  code     => bin q{swap =40 iplus m64get swap goto},

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

  into_asm => bin q{                  # asm frame_ctti self cc
    # Emit code to pull entries from frame to stack. We need to track two
    # values inside the reduction, so I'm consing them up before entering the
    # loop.
    sget03 sget03 ::                  # asm f self cc f::asm
    sget02 .refstack                  # asm f self cc f::asm rs
    [                                 # r f::asm cc
      sget01 .tail                    # r f::asm cc asm[]
        .get_frameptr                 # r f::asm cc asm[f]
      sget03                          # r f::asm cc asm[f] r
      sget03 .head .symbolic_method   # r f::asm cc asm[f.r]
      drop swap                       # r cc f::asm
      sset01 =0 swap goto ]           # f::asm exit?=0
    swap .reduce drop                 # asm f self cc

    # Now inline the child assembler, which should yield exactly one value for
    # us to store back into the frame.
    sget01 .code                      # asm f self cc code
    sget04 .inline                    # asm f self cc asm[v]
      .get_frameptr
    sget02 .name "=" swap .+          # asm f self cc asm[v f] "name="
    sget04 .symbolic_method           # asm f self cc asm[v f.name=]
    drop                              # asm f self cc

    swap .tail swap                   # asm f tail cc
    sget01 m64get :into_asm goto      # ->tail.into_asm };


=head3 ANF return-link
This is the last link in an ANF expression. We need to specify the continuation
to invoke as well as a stack of values we want to return in the process. The
continuation is always the top stack entry.

Here's the struct:

  struct anf_return_link
  {
    hereptr class;
    string* value;
    string* continuation;
  }

=cut

use phi::protocol anf_return_link =>
  qw/ value
      continuation /;

use phi::class anf_return_link =>
  cons_protocol,
  anf_return_link_protocol,
  anf_link_protocol,

  tail         => bin q{=0 sset01 goto},
  value        => bin q{swap =8  iplus m64get swap goto},
  continuation => bin q{swap =16 iplus m64get swap goto},

  head   => bin q{goto},
  defset => bin q{strmap sset01 goto},
  refset => bin q{                    # self cc
    strmap                            # self cc m
    sget02 .name swap .<<             # self cc m [<<name]
    sget02 .continuation swap .<<     # self cc m [<<k]
    sset01 goto                       # m },

  into_asm => bin q{                  # asm frame_ctti self cc
    # Push the resulting value on the stack, then push the continuation and
    # goto it. Strictly speaking we don't use a goto instruction; we defer to
    # the continuation's CTTI so we can use its .goto() method.
    sget03                            # asm f self cc asm[]
      .get_frameptr                   # asm f self cc asm[f]
    sget02 .value                     # asm f self cc asm[f] vname
    sget04 .symbolic_method           # asm f self cc asm[f.vname]
      .get_frameptr                   # asm f self cc asm[f.vname f]
    sget02 .continuation              # asm f self cc asm[f.vname f] cname
    sget04 .symbolic_method           # asm f self cc asm[f.vname f.cname]
      .get_frameptr
    "/parent_frame"
    sget04 .symbolic_method           # asm f self cc asm[v c f0]

    # Now we have all the data we need on the stack. We need to move v and c
    # up to immediately below the parent frame pointer; then they'll be in
    # place when we restore the stack to its final location.
    #
    # Here's the code we want:
    #                                               # v c f0
    #   sget02 sget01 =8  ineg iplus m64set         # v c f0 [f0-8=v]
    #   sget01 sget01 =16 ineg iplus m64set         # v c f0 [f0-16=c]
    #   dup set_frameptr                            # v c f0 [f=f0]
    #   =16 ineg iplus set_stackptr                 # v c
    #   goto                                        # v

    =2 swap .sget =1 swap .sget       # [v c f0 v f0]
    =8 swap .lit8 .ineg .iplus .m64set

    =1 swap .sget =1 swap .sget       # [v c f0 c f0]
    =16 swap .lit8 .ineg .iplus .m64set

    .dup .set_frameptr
    =16 swap .lit8 .ineg .iplus .set_stackptr

    # NB: we really should use the continuation's CTTI instead of hardcoding a
    # goto instruction. This will work as long as everything's a hereptr<fn>,
    # which it totally is for phi2, but it's not as flexible as phi3 will end up
    # demanding.
    .goto                             # asm f self cc asm[...]

    sset03 sset01 drop goto           # asm };


=head2 Test code
Let's use ANF to assemble some functions and see where we end up. CTTI backs
into ANF (via parsers), so let's use it in that context.
=cut

use phi::fn anf_test => bin q{          # cc
  goto                                  # };


1;
