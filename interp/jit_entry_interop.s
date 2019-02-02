    .text
    .globl min_caml_call_caml_dummy_fun
min_caml_call_caml_dummy_fun:
    pushl   %ebp
    movl    %esp, %ebp
    pushl   %eax
    call    _call_caml_dummy_fun
    movl    %ebp, %esp
    popl    %ebp
    ret
    .globl min_caml_call_caml_jit_entry
min_caml_call_caml_jit_entry:
    pushl   %ebp
    movl    %esp, %ebp
    pushl   %eax
    call    _call_caml_jit_entry
    movl    %ebp, %esp
    popl    %ebp
    ret
