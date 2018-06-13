=head2 Build the image
This is pretty simple: we just write the entire heap contents into a file and
call it a day. This works because the heap begins with ELF header allocations
that locate it properly in memory.


=head3 ELF header
These behave like heap allocations because of page alignment. Basically, let's
say our virtual address space starts at 0x400000; because the ELF file is
memory-mapped rather than copied, we have the constraint that we can "move" the
file's contents only by increments of 4096 bytes -- whole pages. We can't assign
an arbitrary offset, e.g. to skip the ELF header.

This isn't much of a problem really. As far as phi is concerned, our initial
heap is maybe 120 bytes smaller than it would normally be because it's got some
mystery data (the ELF headers) prepended; then that will get garbage collected
because nothing refers into it. So the net impact of our ELF headers ends up
being zero, exactly what we want.
=cut

use constant elf_startaddr         => 0x400000;
use constant elf_return_stack_size => 0x100000;

use phi::asm entry_point => asm;
use phi::asm elf_hdr     => asm
  ->locate(elf_startaddr)
  ->pC16_SSL(
    0x7f, ord 'E', ord 'L', ord 'F',
    2, 1, 1, 0,
    0, 0, 0, 0,
    0, 0, 0, 0,

    2,                                  # e_type    = ET_EXEC
    62,                                 # e_machine = EM_X86_64
    1)                                  # e_version = EV_CURRENT
  ->pQ(entry_point)
  ->pQQ_LSS_SSSS(
    64,                                 # e_phoff
    0,                                  # e_shoff
    0,                                  # e_flags

    64,                                 # e_ehsize
    56,                                 # e_phentsize
    1,                                  # e_phnum

    0,                                  # e_shentsize
    0,                                  # e_shnum
    0)                                  # e_shstrndx
  ->pLLQQQQQQ(
    1,                                  # p_type = PT_LOAD
    7,                                  # p_flags = R|W|X (important)
    0,                                  # p_offset (must be page-aligned)
    elf_startaddr,                      # p_vaddr
    0,                                  # p_paddr
    l("iend") - l"istart",              # p_filesz
    l("iend") - l"istart",              # p_memsz
    0x1000)                             # p_align
  ->lelf_hdr_end;


=head3 Initial interpreter state
We need to create the interpreter object and set up registers. I'm going to do
something a little bit sneaky here.

C<%rbp> needs to point to some available memory for a return stack, but right
now the only non-code allocation we have is the OS-provided C<%rsp> data stack.
If we want to allocate more, we'll need to use C<syscall6>, which itself uses
the return stack to save register state. How do we bootstrap this?

Pure awesomeness, that's how. We actually _do_ have a few bytes that aren't
code: remember the ELF header? That got mapped into memory because it's in the
same page as our bootstrap code. It doesn't serve any purpose once it's loaded,
so let's set C<%rbp> to the top end of the ELF header, which is 120 bytes (15
stack entries) long. We can start there before we map a return stack in memory.
=cut

interpreter_vtable
  ->pQ(asm("interp.print_char")         # method 0 = debug_print_char
    ->pC(idrop)                         # drop self pointer
    ->pCQCC(ilit64,                     # lit64 to push native
            asm("interp.print_char_native")
              ->_59585150               # (ret, char) -> (char, ret)
              ->_488bo304               # mov %rsp, %rax
              ->_515151                 # args 4, 5, 6
              ->_6apc(1)                # count = 1
              ->_50                     # buf = &char on stack
              ->_6apc(1)                # fd = 1
              ->_6apc(1)                # n = 1
              ->syscall6                # now we have (n, char, ret) on stack
              ->_5858                   # now we have (ret)
              ->_c3,
            icall_native,               # call_native
            iret))                      # ret

  ->pQ(asm("interp.exit")               # method 1 = debug_exit
    ->pC(idrop)                         # drop self pointer
    ->pCQC(ilit64,                      # lit64 to push native
           asm("interp.exit_native")->exit_constant(42),
           icall_native));              # call_native (never returns)


use phi::asm initial_list => asm
  ->pC6(ilit8, ord"f",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"o",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"o",  iiget, idup, igm64, imcall1 + 0)
  ->pC6(ilit8, ord"\n", iiget, idup, igm64, imcall1 + 0)
  ->pC4(iiget, idup, igm64, imcall1 + 1);


entry_point
  # Set up initial %rbp
  ->_48c7o305pl(elf_hdr->resolve("elf_hdr_end"))

  ->_6apc(0)            # push offset = 0
  ->_6apc(-1)           # push fd = -1
  ->_6apc(0x22)         # push flags = MAP_ANONYMOUS | MAP_PRIVATE
  ->_6apc(7)            # push prot = R|W|X
  ->_68pl(elf_return_stack_size)
  ->_6apc(0)            # push addr = 0
  ->_6apc(9)            # push n = __NR_MMAP
  ->syscall6            # now beginning of region is on the data stack
  ->_5d                 # pop region -> %rbp

  ->_4881o305pl(elf_return_stack_size)                  # initialize %rbp
  ->_48bfpQ(interpreter_object->resolve("itable"))      # initialize %rdi
  ->_48bepQ(initial_list)                               # initialize %rsi
  ->next;                                               # start interpreter


=head3 vtable finalization
We need to put end labels onto each of our vtables. TODO: migrate this into a
more managed process.
=cut

vtable_vtable->lmend;
amd64_code_vtable->lmend;
interpreter_vtable->lmend;


=head3 Heap construction
Allocate everything into the heap, dependencies last. C<elf_hdr> must be the
first object we allocate in order to generate a valid executable.
=cut

elf_hdr->listart
       ->inline(entry_point)
       ->liend;

print elf_hdr->linked unless caller;


=head3 Debugging: build a symbol table file
This is useful to have around for GDB and general troubleshooting. We don't
really care what the addresses end up being if everything's working, but if we
get C<%rsi> pointing at 0x400bf1, it's good to be able to verify that we expect
to have valid bytecode there.
=cut

if (DEBUG_EMIT_SYMBOLS and not caller)
{
  open my $fh, '> phi.symbols';
  printf $fh "%d\t%d\t%06x\t%s\n", $phi::asm::all_located{$_}->location,
                                   $phi::asm::all_located{$_}->size,
                                   $phi::asm::all_located{$_}->location,
                                   $_
    for sort { $phi::asm::all_located{$a}->location
                  <=> $phi::asm::all_located{$b}->location
               || $a cmp $b }
             keys %phi::asm::all_located;
  close $fh;
}
