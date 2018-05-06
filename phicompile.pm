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


=head1 phi optree compiler
A compiler for optrees. This translates an optree graph into concatenative code
using a series of node parsers. We could use a literal translation, but it
wouldn't be optimal; instead, this compiler optimizes a few low-level details to
produce faster code.

phi is a pretty easy backend to target. Let's talk about what it looks like to
translate different elements into concatenative code.

First, binary expressions like C<(+ x y)> just become C<x y +>, with an inline
operator invocation and inlined operands. There's a bit more to it than is
obvious here: what happens if C<x> and C<y> refer to C<arg> or C<capture>? We
need to track the stack offsets of those values.

...that brings us to the next point: what does our calling convention look like?
I think it's pretty simple; let's go through a function with some captured
state.


=head2 Compiling functions and closure state
The interpreter stores C<arg> and C<capture> on the hosted data stack, which
means those values are addressible. The compiler needs to drop those values into
the target data stack, which means we need to replace the values themselves with
stack indexes.

Functions need to be proper closures in order to interoperate with concatenative
code. For example, we might use C<philist::list_map> from infix; the lambda will
need to be self-sufficient in order to follow the calling convention C<list_map>
expects.

In practice this is straightforward. We can compile the body of the function
with the expectation that its arguments are positioned like this on the stack:

  argN argN-1 ... arg2 arg1 [capturelist]

A function then compiles into code which conses the capture list onto the quoted
body. For example, for C<< \x -> x + 5 >>, where C<5> is captured (NB: we're
referring to a single argument here; no stack conversion is happening):

  # we ultimately want this (NB: this is a lie; see "Function calls" below)
  [[5] head +]

  # here's the code to generate this function:
  [head +]                              # [f...]
  [] [5] head ::                        # [f...] [capture-list]
  ::                                    # [[capture-list] f...]


=head2 C<arg> and C<capture> references
Technically, C<arg> and C<capture> refer to things that don't move. The only
variable is the number of things we've pushed onto the stack as working values,
and that's what we track.


=head2 Function calls
Functions map stacks to stacks via C<arg> -- but C<arg> is addressed as a single
list value. This means that if we want to provide a literal translation, we need
to quote the incoming data stack into a list value. So our function body would
look something like this:

  [ i> head tailⁿ [capture-list] body... d< ]

NOTE: the above is completely wrong; if we do this, we're splicing the result in
an arity-dependent way. This will break our compiled code because we'll lose
track of the C<capture> offset.

Q: how do we get around predefined arity? Do C<fn> nodes dig through their
optrees to look for the max indexed C<arg>? Are C<call> nodes variadic?
=cut


package phicompile;
use strict;
use warnings;

use Exporter qw/import/;
use phiboot;
use phibootmacros;
use philist;
use phidata;
use phiobj;
use phioptree;

our @EXPORT =
our @EXPORT_OK = qw/ compile /;


1;
