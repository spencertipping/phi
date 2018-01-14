=head1 Parser library for concatenative phi
This is the first step for bootstrapping phi into a "regular" language; that is,
one with applicative syntax. These parsers are designed to operate both on
strings and on lists (and any other data structure, really). That way you can
define phi's evaluation semantics in terms of parsers, which is used in backend
compilers.

The calling convention works like this:

  [parser] . :: state -> [error? []]          # failure
                       | [result state']      # success

As far as higher-order parsers like C<seq> and C<alt> are concerned, C<state>
can be any value. The equations treat it as opaque:

  seq(a, b) :: state -> let [r1 state']  = a state in
                        let [r2 state''] = b state' in
                        [[r1 r2] state'']

  alt(a, b) :: state -> let [r1 state'] = a state in
                        state' == [] ? b state : [r1 state']

=head2 Specifying parsers
Parsers are just functions that close over their required arguments. For
example, the grammar C<"foo" | "bar"> might be specified like this:

  [[["foo" str] ["bar" str]] alt]

Both C<alt> and C<seq> operate on arbitrarily large lists rather than being
limited to pairs.

=head2 C<seq> parser implementation
C<seq> takes a list of parsers and applies each one, consing up a list of
results. The equations are:

  <state> [ps...] seq = [] <state> [ps...] seq'

  [rs...] <state> []        seq' = [reverse([rs...]), <state>]
  [rs...] <state> [p ps...] seq' = match <state> p with
    | [e []]       -> [e []]
    | [r <state'>] -> [r rs...] <state'> [ps...] seq'

=cut
