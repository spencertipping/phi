=head1 Method calls against C<hereptr> values
I've been inconsistent about exactly how this works. Most of the time I've
described a hereptr method call as this sequence of operations:

  (hereptr).method(args) == (unhere(hereptr)).method(args)
                         == baseptr.method(args)

...thus losing the "here"-ness of the pointer. That will get the method call
done, but unfortunately it's neither adequate nor semantically correct for a
number of cases. Let's get into it.


=head2 Multi-receiver objects
In general, objects can be addressed at multiple points; this is the whole point
of hereptrs, and it's why we can do things like link functions directly to their
callable bytecode despite that having nothing to do with where the object
begins. We can recover the base from the hereptr, so everything's good.

But what if we care about which of those receiver-points we're addressing? This
isn't a hypothetical exercise: we absolutely do care for GC purposes, for
example. GC transparency means that if I call C<.gc_mark()> on a hereptr, I want
another equivalent hereptr to the new object. There are other use cases too; if
I address an element of a packed array, the packed array needs to know which
item I'm referring to. C<unhere> erasure makes this impossible.


=head2 Solutions
1. Extend default method calls to include both base and offset
2. Type all pointers as hereptrs, prepend a zero marker to object bases

(2) requires more runtime memory, so let's go with (1). Now every method call
will see this in stack terms:

  obj.method(args...)                   # args... offset self cc

C<offset == 0> for baseptrs and C<offset == here_marker_value> for hereptrs.
It's the caller's job to push this quantity.


=head2 Tagged here-markers
Objects are unlikely to care much about the literal offset-from-base of the
hereptr being addressed; it doesn't carry much semantic information by itself.
But we can adopt idioms like pre-heremarker tagging to encode information about
each receiver in the preceding bytes. That is:

  struct some_object
  {
    ...;
    byte[4] pre_here_marker_tag;        # == (g32 (iadd hereptr (ineg 8)))
    byte[4] here_marker;
    ... here_receiver;
    ...;
  };

This is up to objects; callers don't need to know about tagging strategies
because the here-marker itself isn't impacted.
