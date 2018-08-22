.globl: min_caml_trace_entry
min_caml_trace_entry:
	pushl	%edx
	pushl	%ecx
	pushl	%ebx
	pushl	%eax
	call	min_caml_test_trace
	popl	%edx
	popl	%edx
	popl	%edx
	popl	%edx
	ret
