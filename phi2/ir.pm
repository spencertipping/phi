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


=head2 phi2 pre-bytecode intermediate representation
phi1 bytecode is great, but it doesn't produce code that's GC-atomic by default.
If we want that to happen automatically, we need to use the frame pointer and
make sure stuff gets aligned correctly -- and that means we need a little more
structure around stack accesses. Let's talk about how this works.

phi2 is made out of imperative basic-blocks, each of which addresses frame
values as it builds up expressions. It's important to model the load/store cycle
of even linear subexpressions because every external function call needs to be
GC-atomic (consider, for instance, C<f(g(x), h(x))>: we need to persist C<g(x)>
during C<h(x)>). The phi2 IR is conservative about this at the representational
level; it's up to post-IR optimization layers to decide when any shortcuts can
be taken.


=head3 IR structure
IR containers define functions. Arguments and locals are referred to by index --
no names -- and store their compile-time types. Each function also stores a list
of basic blocks, each of which is an array of nodes of one of three types:

1. C<< val* [code] -> val* >>: run code and update one or more values
2. C<< goto (val ? then_branch : else_branch) >>
3. C<< return val* >>: pop the frame

C<val> and C<branch> quantities are indexes into the current function's local or
basic-block lists. Here are the struct definitions:

  struct ir_fn
  {
    hereptr        class;
    array<ctti*>*  local_cttis;
    array<ctti*>*  arg_cttis;
    array<ir_bb*>* basic_blocks;
  };

  struct ir_bb : array<ir_node*> {};

  struct ir_val : ir_node
  {
    hereptr class;
    array<int>* ivals;
    array<int>* ovals;
    i8d*        code;
  };

  struct ir_branch : ir_node
  {
    hereptr class;
    i32     cond;
    i32     then;
    i32     else;
  };

  struct ir_return : ir_node, array<int> {};

There's no mechanism here to refer to another function, and that's by design.
There are two ways to do it within the above framework: you can insert a literal
address using an C<ir_val> node, or you can pass the function pointer into the
function as an argument.

In the structs above, C<< array<X> >> can be either C<i64i> or C<i64d>. Nothing
is modified during IR compilation.
=cut


1;
