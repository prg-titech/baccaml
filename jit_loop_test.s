.globl test_loop_fun
test_loop_fun:
	cmpl	$0, %eax
	jg	jle_else.311
	movl	%eax, %ecx
	movl	$0, %eax
	movl	$0, %ebx
	call	min_caml_mid_layer
	jmp	test_loop_fun
jle_else.311:
	jmp	min_caml_test_trace
.globl min_caml_test_trace
min_caml_test_trace:
	subl	$10, %ebx
	movl	%ebx, %eax
	movl	%eax, 0(%ebp)
	addl	$8, %ebp
	call	test_loop_fun
	subl	$8, %ebp
	movl	0(%ebp), %eax
	cmpl	$0, %eax
	jg	jle_else.313
	jmp	test_loop_fun
jle_else.313:
	ret
.globl min_caml_trace_entry
min_caml_trace_entry:
	pushl	%eax
	call	min_caml_test_trace
	popl	%edx
	ret
.globl min_caml_mid_layer
min_caml_mid_layer:
	movl	8(%esp), %eax
	jmp	interp.88
