=head1 Frames as classes
Frame objects already are classes with some interpreter-compatible RTTI method
calling convention (for GC purposes), but I haven't taken that idea to its
logical conclusion yet. If dialects are connection fabric, then frames are
containers. They can do a lot of management for us.

For example, why assemble basic blocks as loose function objects bound by
address? We should make them methods of the current frame class. They'd end up
being statically linked. Then we'd potentially have a series of micro-methods:

1. C<< enter_frame(args) -> () >>
2. C<< bb_n() -> () >>
3. C<< exit_frame(val) -> val >>

C<bb_n()> should return to the caller; then we have a single C<run()> method
that simply threads all of the micro-methods.
