    .code32
    .text
    .globl min_caml_is_mj
min_caml_is_mj:
    ret
    .globl min_caml_method_entry
min_caml_method_entry:
    ret
    .globl min_caml_can_enter_jit
min_caml_can_enter_jit:
    ret
    .globl min_caml_jit_merge_point
min_caml_jit_merge_point:
    ret
    .globl min_caml_trace_entry
min_caml_trace_entry:
    ret
    .globl min_caml_jit_dispatch
min_caml_jit_dispatch:
    ret
    .globl min_caml_loop_start
min_caml_loop_start:
    ret
    .globl min_caml_loop_end
min_caml_loop_end:
    ret
    .globl min_caml_call_caml_jit_entry
min_caml_call_caml_jit_entry:
    pushl   %ebp
    movl    %esp, %ebp
    pushl   %eax
    call    call_caml_jit_entry
    movl    %ebp, %esp
    popl    %ebp
    ret
