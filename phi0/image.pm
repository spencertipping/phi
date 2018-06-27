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


=head2 Heap allocator and pointer management
Our initial machine code file is basically a heap image, so we need to simulate
the behavior of a real heap during bootup. Fortunately that isn't difficult:
objects are just allocated one after another. All we need to do is keep track of
where each object goes so we can point to it.

Most of the code for allocations and the heap is either for convenience or for
safeguarding.
=cut


package phi::allocation
{
  use overload qw/ ${} data_ref
                   ""  name
                   0+  address /;

  sub sized
  {
    my ($class, $size) = @_;
    my $data;
    bless { size => $size,
            addr => undef,
            data => \$data }, $class;
  }

  sub constant
  {
    my $class = shift;
    my $data  = \shift;
    bless { size => length $$data,
            addr => undef,
            data => $data }, $class;
  }

  sub named
  {
    my ($self, $name) = @_;
    $$self{name} = $name;
    $self;
  }

  sub name
  {
    $_[0]->{name} // sprintf "anon 0x%x+0x%x",
                             $_[0]->{addr} // -1,
                             $_[0]->{size};
  }

  sub data_ref       { shift->{data} }
  sub address        { shift->{addr} // die "not yet allocated" }
  sub size           { shift->{size} }
  sub is_allocated   { defined shift->{addr} }
  sub is_initialized { defined ${shift->{data}} }

  sub init_with
  {
    my ($self, $f) = @_;
    $$self = &$f();
    $self;
  }

  sub set_addr
  {
    my ($self, $addr) = @_;
    die "phi::allocation: modifying address of already-allocated object"
      if defined $$self{addr}
      && $$self{addr} != $addr;
    $$self{addr} = $addr;
    $self;
  }

  sub compile
  {
    my $self = shift;

    warn "phi::allocation: compiling an unallocated reference $$self{name}"
       . " (probably a mistake)"
      unless $self->is_allocated;

    die "phi::allocation: cannot compile uninitialized reference $$self{name}"
      unless $self->is_initialized;

    my $reserved = $self->size;
    my $actual   = length $$self;

    die sprintf "phi::allocation: data overflows allocation (%d > %d)",
                $actual,
                $reserved
      if $actual > $reserved;

    warn sprintf "phi::allocation: %s is undersized", $self->name
      if $actual < $reserved;

    # Make sure we pad the data out to the size of the allocation; otherwise
    # we'll accumulate offset errors when we join in the heap.
    pack "a$$self{size}", $$self;
  }
}


package phi::heap
{
  use overload qw/ << allocate
                   >> allocate_left_handed /;

  sub new
  {
    my ($class, $address) = @_;
    bless { location => $address // 0,
            address  => $address // 0,
            lookup   => {},
            objects  => [] }, $class;
  }

  sub address   { shift->{address} }
  sub addressof { shift->{lookup}{+shift}->address }
  sub size      { $_[0]->{address} - $_[0]->{location} }
  sub objects   { @{shift->{objects}} }

  sub mark
  {
    my ($self, $name) = @_;
    $self << phi::allocation->constant("")->named($name);
  }

  sub compile
  {
    my $self = shift;
    my @uninitialized = grep !$_->is_initialized, @{$$self{objects}};
    die "phi::heap: one or more uninitialized refs: "
      . join(", ", @uninitialized)
      if @uninitialized;

    join"", map $_->compile, @{$$self{objects}};
  }

  sub initialize
  {
    my $self = shift;
    while (@_)
    {
      my $name = shift;
      my $data = shift;
      die "phi::heap: no allocation named $name"
        unless exists $$self{lookup}{$name};
      ${$$self{lookup}{$name}} = $data;
    }
    $self;
  }

  sub allocate
  {
    my $self = shift;

    if (ref $_[0] eq 'ARRAY')
    {
      my @xs = @{+shift};
      for (my $i = 0; $i + 1 < @xs; $i += 2)
      {
        $self->allocate(phi::allocation->sized($xs[$i + 1])
                                       ->named($xs[$i]));
      }
      return $self;
    }

    my $thing = ref $_[0] ? shift : phi::allocation->constant(shift);
    return $self if $thing->is_allocated;

    $thing->set_addr($$self{address});
    $$self{address} += $thing->size;
    if (defined $thing->name)
    {
      die sprintf "phi::heap: redefining existing name %s",
                  $thing->name
        if exists $$self{lookup}{$thing->name};
      $$self{lookup}{$thing->name} = $thing;
    }

    push @{$$self{objects}}, $thing;
    $self;
  }

  sub allocate_left_handed
  {
    my $self       = shift;
    my $allocation = ref $_[0] ? shift : phi::allocation->constant(shift);
    $self << $allocation;
    $allocation;
  }
}


=head2 ELF image
Let's go ahead and generate the ELF headers. We have a dangling reference,
C<$elf_start_addr>, that we'll fill in later on.

There's no special reason I use C<0x400000> for the ELF base address. It could
be pretty much anything. I went with C<0x400000> instead of C<0x0> because it
makes it much easier to figure out whether something is a valid boot-image
address.
=cut

use constant elf_base_addr => 0x400000;

use constant heap =>
  phi::heap->new(elf_base_addr)
    << pack(C16SSL => 0x7f, ord"E", ord"L", ord"F", 2, 1, 1, 0,
                      0, 0, 0, 0, 0, 0, 0, 0,
                      2, 62, 1)
    << [elf_start_addr => 8]
    << pack(QQLSSSSSS => 64, 0, 0, 64, 56, 1, 0, 0, 0)
    << pack(LLQQQ => 1, 7, 0, elf_base_addr, 0)
    << [elf_memory_size => 8, elf_file_size => 8]
    << pack(Q => 0x1000);


=head2 vtable linkage
All classes share a single boot protocol for the bootup vtables. I explain this
more below, but it's pretty simple: each method gets its own vtable slot, and
all vtables are the same size.

NB: vtable slot 0 is special and can't be used to encode any particular method.
It's a backdoor to select other protocols.
=cut

sub bin($);

use constant method_lookup => { protocol_backdoor_reserved_ => 0 };
sub vtable_size() { scalar keys %{+method_lookup} }

our $methods_are_finalized = 0;

sub mi($) { method_lookup->{$_[0]} // die "unallocated method $_[0]" }
sub mc($)
{
  my $mi = mi shift;
  $mi < 256 ? bin"mcall8  >pack(C => $mi)"
            : bin"mcall16 >pack(n => $mi)";
}


=head2 Writing code in perl
We can hack this together with a mixture of C<pack> and C<bin>, the latter being
a mixed hex/octal format that's ideal for writing machine code.

NB: C<bin> defines a method-calling macro that lets you write C<.foo> to
generate a vtable invocation of the C<foo> method using the boot protocol. It's
pretty great. The reason I mention it, though, is that this method macro assumes
you've got a base pointer to a C<< polymorphic<boot_protocol> >> -- that is,
doing an C<m64get> right away will give you the vtable. So if C<foo> has index
7, C<.foo> expands to this:

  dup m64get mcall8 07

If you want a bare method call you can write C<:foo>, which just emits the
C<mcall> instruction:

  mcall8 07


=head3 String literals
Double-quoted stuff will push a heap-allocated string pointer onto the data
stack. C<"foo"> is equivalent to C<< lit64 >pack('Q>', str('foo')) >>.


=head3 Conditional jumps
phi bytecode provides the C<if> instruction, which returns one of two values
based on the truthiness (non-zeroness) of a conditional. This instruction
doesn't by itself manage control flow; that's done by using C<goto> in
conjunction with a local offset. For example:

  before_stuff...
  get_insnptr                           # offset = 0
  lit8(9)                               # offset = 2
  iplus                                 # offset = 3
  goto                                  # offset = 4

  # these drop instructions get skipped
  drop drop                             # offset = 6
  drop drop                             # offset = 8
  drop                                  # offset = 9

  # ...and goto lands here
  after_stuff...

C<bin> needs a construct that jumps over some code and instead pushes that
code's address, so the above, plus an address push, would become:

  before_stuff...
  [ drop drop drop drop drop ]
  after_stuff...

Then brackets behave like function-quoting delimiters -- and that's exactly how
you would use them. (You wouldn't inline bracketed code because it wouldn't
return to the caller.)
=cut

use constant bin_macros => {};

sub safe_eval($)
{
  my @r = eval "use strict;" . shift;
  die "eval @_: $@" if $@;
  @r;
}

sub jump_over($)
{
  my $snippet = shift;
  my $length  = length $snippet;

  # NB: using a constant offset size to keep it simple, although this is
  # suboptimal for code size.
  bin("get_insnptr                      # &prefix             [offset = 0]
       lit8 +9 iplus                    # &code               [offset = 3]
       dup lit16 >pack('S>', $length)   # &code &code cl      [offset = 7]
       iplus goto                       # &code               [offset = 9]")
  . $snippet;
}

sub str($);

sub bin_($);
sub bin_($)
{
  my $macro_regex = shift;
  my @r;

  while (1)
  {
    last if /\G(?:\]|$)/gc;
    next if /\G(?:\s+|#.*\n?)/gc;

    # Handle brackets recursively
    push(@r, jump_over bin_ $macro_regex), next if /\G\[/gc;

    push(@r, bin_macros->{$1}), next if defined $macro_regex
                                         && /\G($macro_regex)/gc;

    push(@r, pack "C", $1), next     if /\G\+(\d+)/gc;
    push(@r, pack "H*", $1), next    if /\Gx?((?:[0-9a-fA-F]{2})+)/gc;
    push(@r, pack "C", oct $1), next if /\Go([0-3][0-7]{2})/gc;
    push(@r, $1), next               if /\G'(\S+)/gc;
    push(@r, safe_eval $1), next     if /\G>(.*)\n?/gc;

    push(@r, pack'CQ>', 0x13, safe_eval $1),    next if /\G\$(\S+)/gc;
    push(@r, pack'CQ>', 0x13, str($1) >> heap), next if /\G"([^"]*)"/gc;

    push(@r, bin"dup m64get >mc q{$1}"), next if /\G\.(\S+)/gc;
    push(@r, mc $1), next                     if /\G:(\S+)/gc;

    die "phi::bin: failed to parse starting at " . substr $_, pos $_;
  }

  join"", @r;
}

sub bin($)
{
  my $macro_pattern = join"|", sort { length $b <=> length $a }
                                    keys %{+bin_macros};
  local $_ = shift;
  bin_(length $macro_pattern ? qr/$macro_pattern/ : undef);
}


=head2 Instruction macros
I define the full instruction set later on, but for now we need a way to easily
refer to bytecode instructions without using hex.
=cut

use constant insns =>
{ qw/ lit8  10 lit16 11
      lit32 12 lit64 13
      cell8 14

      const0  18 const1  19
      const2  1a const4  1b
      const8  1c const16 1d
      const24 1e const32 1f

      call        20
      call_native 21
      mcall8      22
      mcall16     23
      if          24
      syscall     25

      get_frameptr  28
      set_frameptr  29
      get_interpptr 2a
      set_interpptr 2b
      get_stackptr  2c
      set_stackptr  2d
      get_insnptr   2e
      goto          2f

      dup  30
      drop 31
      swap 32
      fget 33
      fset 34
      sget 35
      sset 36

      m8get  40 m8set  41
      m16get 42 m16set 43
      m32get 44 m32set 45
      m64get 46 m64set 47
      memcpy 48

      iplus   50
      itimes  51
      ishl    52
      isar    53
      ishr    54
      iand    55
      ior     56
      ixor    57
      ilt     58
      ieq     59
      iinv    5a
      ineg    5b
      bswap16 5c
      bswap32 5d
      bswap64 5e / };


sub insn_index($) { hex insns->{+shift} }


BEGIN
{
  bin_macros->{$_}   = bin insns->{$_} for keys %{+insns};
  bin_macros->{i}    = bin"get_interpptr";

  bin_macros->{inot} = bin"const0 const1 if";
}


=head2 Debugging macros
We need a quick way to drop print statements into machine code (and in some
cases exit). These functions provide minimally destructive implementations of
that logic.

It's also useful to be able to nondestructively print hex values; I've written a
macro for that below.
=cut

sub pp { pack shift, pack shift, @_ }

sub debug_print
{
  my ($message, $fd) = @_;
  my $len = length $message;
  $fd //= 1;
  $message = unpack "H*", $message;
  bin qq{
    50 51 52 56 57                      # save registers
    31o300 b001                         # %rax = syscall 1 (write)
    e8 >pp("l/a", "H*", q{$message})    # call +sizeof(message)
    5e                                  # pop &message -> %rsi
    ba >pack(l => $len)                 # mov size -> %rdx
    48c7o307 >pack(l => $fd)            # mov fd -> %rdi
    0f05                                # syscall
    5f 5e 5a 59 58                      # pop register state};
}

sub exit_with_status
{
  my $code = shift;
  bin qq{
    31o300 b03c                         # %rax = 60
    48c7o307 >pack l => $code           # %rdi = exit code
    0f05                                # syscall};
}

sub debug_die($)
{
  # Prints to stderr, then exits. Use this to indicate an assertion error within
  # the interpreter.
  debug_print(shift, 2) . exit_with_status(1);
}


BEGIN
{
  bin_macros->{digit_to_hex} = bin
  q{# Converts the top stack entry from 0-15 to its corresponding hex ASCII.
    lit8 0f iand                          # n
    lit64 'fedcba98 swap                  # d1 n
    lit64 '76543210 swap                  # d1 d0 n
    get_stackptr const8 iplus iplus       # d1 d0 &digit
    m8get swap drop swap drop             # digit };

  bin_macros->{debug_trace} = bin
  q{# Prints the top stack value to stderr as big-endian hex, followed by a
    # newline. Does not consume the value.
    dup dup dup dup
    dup dup dup dup
    const0                                # v v*8 bufL
    swap lit8 +28 ishr digit_to_hex lit8 +56 ishl ior
    swap lit8 +24 ishr digit_to_hex lit8 +48 ishl ior
    swap lit8 +20 ishr digit_to_hex lit8 +40 ishl ior
    swap lit8 +16 ishr digit_to_hex lit8 +32 ishl ior
    swap lit8 +12 ishr digit_to_hex lit8 +24 ishl ior
    swap lit8 +8  ishr digit_to_hex lit8 +16 ishl ior
    swap lit8 +4  ishr digit_to_hex lit8 +8  ishl ior
    swap               digit_to_hex               ior
    bswap64                               # v bufL

    lit8 0a swap                          # v \n bufL

    sget 02                               # v \n bufL v
    dup dup dup dup dup dup dup
    const0                                # v \n bufL v*7 bufH
    swap lit8 +60 ishr digit_to_hex lit8 +56 ishl ior
    swap lit8 +56 ishr digit_to_hex lit8 +48 ishl ior
    swap lit8 +52 ishr digit_to_hex lit8 +40 ishl ior
    swap lit8 +48 ishr digit_to_hex lit8 +32 ishl ior
    swap lit8 +44 ishr digit_to_hex lit8 +24 ishl ior
    swap lit8 +40 ishr digit_to_hex lit8 +16 ishl ior
    swap lit8 +36 ishr digit_to_hex lit8 +8  ishl ior
    swap lit8 +32 ishr digit_to_hex               ior
    bswap64                               # v \n bufL bufH

    get_stackptr                          # v \n bufL bufH &bufH
    const0 swap                           # v \n bufL bufH 0 &buf
    const0 swap                           # v \n bufL bufH 0 0 &buf
    const0 swap                           # v \n bufL bufH 0 0 0 &buf

    lit8 +17 swap                         # v \n bufL bufH 0 0 0 17 &buf
    const2 const1                         # v \n bufL bufH 0 0 0 17 &buf 2 1
    syscall                               # v \n bufL bufH n

    drop drop drop drop                   # v };
}


use constant runtime_fail => phi::allocation->constant(exit_with_status 1)
                                            ->named("fail") >> heap;


1;
