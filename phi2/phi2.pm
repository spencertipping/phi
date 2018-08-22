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


=head2 phi2 language frontend
Here's where things start to get real. We use the machinery defined in phi1 to
parse phi2, which provides a proper infix language with parse-time CTTI
propagation, expression type inference, lexical closure, etc. phi2 is a simple
language that provides enough grammar extensibility to host anything else.

Let's talk a little about how the parsers work internally.


=head3 CTTI interop and parsing
CTTIs themselves don't need to provide parsers in order to participate in a
grammar. Most languages don't have computed grammars in the first place, so
there isn't really a precedent for classes to be syntax-aware; but beyond that,
we can get a lot of mileage from a CTTI's set of offered methods. If some of
those methods look like operators, then those can be integrated into the syntax
as such (and at the appropriate precedence for the frontend in question).

Put differently, CTTIs are at liberty to rely solely on the method/virtual
tables to convey functionality and they can be confident that any frontend will
present those in a sane way. It's fine for their C<parse> method to fail on all
inputs; that just indicates that the CTTI doesn't provide any out-of-band
grammar interpolation.


=head3 ...therefore, a frontend is a single parser
Now we can define a frontend with minimal machinery. The frontend parser can ask
the parse state for the CTTI for a given binding, and use that to parse
identifiers. Once it has such a CTTI, it can parse method/operator invocations
against a given value, compiling/linking those at parse-time. It can also
delegate the parse to any custom continuation for that CTTI (and should, unless
the language just can't accommodate custom extensions).

"Primitive" operations like C's C<+> and pointer-dereference and such are all
specified by one of the CTTIs. We don't need a base case if we have a CTTI that
defines non-virtual methods that bottom out into assembler instructions we can
link using C<symbolic_method>. (C<phi1::class.symbolic_method> takes care of
this for us.)


=head3 A minimal implementation
If we want phi1 interoperability, we basically need three things:

1. Global value interop
2. Class (CTTI) definition and interop
3. ANF -> concatenative function compilation

From a CTTI parser perspective this is all pretty straightforward. For example:

  $foo = 5;                             # $ prefix for globals
  $vec = class                          # "class" begins CTTI CTTI
    field x : int,                      # "field" is a CTTI grammar constant
    field y : int,

    @$accessors,                        # @ prefix for self-transform functions

    # NB: no reason for to_s to be a method instead of a virtual
    # Q: "method" implies asm as second arg; do we static-CTTI it like we do
    #    with self?
    method to_s(self, asm)              # who owns method signature?
    {                                   # brackets = ANF fn constructor?
      # Here's the logic we want:
      # $strbuf.new                     # virtual metaclass method call
      #   .append_string("<")           # CTTI tracked via ANF?
      #   .append_int(self.x)           # CTTI of self.x is known
      #   .append_string(", ")
      #   .append_dec(self.y)
      #   .append_string(">")
      #   .to_string

      # NB: asm CTTIs probably have a better syntax than this in practice
      let sb_gensym = $gensym();
      asm.ref($global).'new.bind(sb_gensym);
      asm[sb_gensym].'append_string("<")
                    .'append_int(self.x)
                    .'append_string(", ")
                    .'append_int(self.y)
                    .'append_string(">")
                    .'to_string;
      asm
    },

    virtual +(self, rhs)                # untyped RHS = generic CTTI
    {                                   # brackets for block scope expression
      let v = self.class.new;           # self type reference
      let x' = self.x + rhs.x;          # "int" CTTI resolves +...
      let y' = self.y + rhs.y;          # ...and rhs.[xy] are untyped
      v.x = x';                         # int resolves "="?
      v.y = y';                         # ANF anonymous values from side-effects
      v;                                # implicit return?
    };                                  # semicolon ends classdef

  $vec.new                              # where does "new" come from?

OK, some things worth mentioning here:

1. We always know the CTTI of the "self" method argument*
2. I'm not sure yet where constructors come from; they're CTTI statics
3. C<$> sucks as a prefix for globals; C<@$accessors> is super lame
4. C<v.x = x'> as C<int.=> conflicts with C<self.x=> ... which is it?

About (1): in CTTI terms, C<self> is a partial type because parsing is
predicated on CTTI defined-ness. Its virtual table will be updated as we define
more methods (i.e. it will end up referring to the right class ultimately), but
you can't forward-use monomorphic asm inlines or anything because they really
don't exist yet. That limitation is fine with me; you obviously couldn't have
singly or mutually recursive inline code. Just like lisp macros.

I'm not entirely sure how the C<self> CTTI gets passed into the ANF context. The
C<virtual> or C<method> word owns the signature but can accept a regular
function body, so there's some polymorphic-ish signature handoff happening.
CTTIs should be able to handle this type of interoperability. I don't want
C<method>/C<virtual> to own the brackets, though; you should be able to write
function-constructing expressions that somehow make it work (I think):

  virtual f(self, x:int) =
    # Q: is this happening in a compile-time ANF context with access to self/x
    # CTTIs? That would be kinda cool actually.
    $asm.new.<stuff>.compile

(2) gets into metaclasses. The parse equation refers to the _type_ of a value,
so CTTIs themselves need to be of multiple types in order to be
parse-polymorphic.

(3) is probably overkill. We should be able to infer C<$> by looking at names
that escape their scope. I don't think this limits our options.

(4) seems simple but has the potential to get into the way our grammar deals
with lvalues. For example, if we're simulating Perl we're on the hook for
propagating the custom lvalue interfacing to functions that modify their aliased
arguments. Do we have proxy CTTIs from accessors that address lvalues? What
happens when we have custom C<x=> logic?

...C<=> is a virtual method unless shadowed by C<let>, which provides a custom
local-variable CTTI that has a monomorphic inline C<=> method. This is nice
because it means C<let> is just a symbol prefix; C<let x;> is a valid statement
that defines C<x> as a C<< local<*> >> CTTI. The following C<=> is a regular
parse continuation against whatever CTTI you're working with.

Setter methods, then, are just a phi1 thing. Fields are addressed with CTTIs
that contain the getter/setter logic. (Hang on; this seems like it's going to
break somehow. Like, if C<self.x> refers to another object, does an C<=> address
the field or the referent?)
=cut


1;
