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
