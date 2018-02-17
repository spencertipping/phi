# phi frontend syntax + API
Time to engineer a way to use this language aside from writing concatenative
code. We have an [abstract interpreter](../phiabstract.pm), which is useful both
for backend optimization and for hypothetical evaluation, e.g. for IDE features.

phi is an unusual language in that its parser is extensible by runtime values.
This is how the entire language is parsed; for instance, if you write `3`, this
is immediately evaluated into an abstract integer and the parse continuation is
dictated by that value. This coupling makes it possible to write things like
`3mm`: `mm` converts numbers to dimensions.

Under the hood phi's syntax is incredibly simple. We have a series of literals
we can parse (encoded in the parse state), and we delegate to a parse
continuation as soon as we parse a value. The parse state stores information
about lexical scopes, local bindings, and operator precedence.
