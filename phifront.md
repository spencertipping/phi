# phi language frontend
One thing I haven't really been decisive about here is where the delegation is
between "stuff written in concatenative" and "stuff written in real phi" -- so
let's fix that. First, let's take some inventory.

1. Parse continuations are fully implemented in concatenative
2. Extensible infix/grouping are also fully implemented
3. `phifront` doesn't implement destructuring grammars yet

The question, then, is whether we want to fix (3). It shouldn't be difficult at
all, actually, but it does impact the nature of the language because it's
parser-based: if we have `alt` parsers to work with, then why not use those
instead of conditional nodes? Also, who hosts the parsers -- are they optrees,
or do the optrees delegate to concatenative parsers?
