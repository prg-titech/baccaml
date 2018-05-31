.globl min_caml_test_trace
min_caml_test_trace:
	pushl	%eax
	call	min_caml_test_trace1
	popl	%eax
	ret
.globl min_caml_test_trace3
min_caml_test_trace3:
	movl	4(%esp), %eax
	jmp	interp.xx
