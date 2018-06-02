.globl min_caml_test_trace
min_caml_test_trace:
	pushl	%eax
	call	min_caml_test_trace1
	popl	%edx
	ret
.globl min_caml_test_trace3
min_caml_test_trace3:
	popl	%eax
	jmp	interp.42
