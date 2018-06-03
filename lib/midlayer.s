.globl min_caml_trace_entry
min_caml_trace_entry:
	pushl	%eax
	call	min_caml_test_trace
	popl	%ebx
	ret
.globl min_caml_mid_layer
min_caml_mid_layer:
	movl	4(%esp), %eax
	jmp	interp.42
