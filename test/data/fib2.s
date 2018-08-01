.globl min_caml_test_trace
min_caml_test_trace:
	movl	0(%ebp), %eax
	movl	4(%esp), %eax
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$2, %ebx
	jg	jle_else.2418
	movl	$0, %ebx
	jmp	jle_cont.2419
jle_else.2418:
	movl	$1, %ebx
jle_cont.2419:
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$0, %ebx
	jne	je_else.2420
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	subl	$1, %ebx
	movl	%ebx, 8(%eax)
	movl	$100, %ebx
	movl	$0, %ecx
	movl	$5, %edx
	movl	$3, %esi
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	movl	%ecx, %ebx
	movl	%edx, %ecx
	movl	%esi, %edx
	addl	$8, %ebp
	call	min_caml_mid_layer
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	movl	%eax, 12(%ebx)
	movl	4(%ebx), %eax
	movl	%eax, 16(%ebx)
	movl	16(%ebx), %eax
	subl	$2, %eax
	movl	%eax, 16(%ebx)
	movl	$100, %eax
	movl	$0, %ecx
	movl	$5, %edx
	movl	$5, %esi
	movl	%ecx, %ebx
	movl	%edx, %ecx
	movl	%esi, %edx
	addl	$8, %ebp
	call	min_caml_mid_layer
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	movl	%eax, 20(%ebx)
	movl	16(%ebx), %eax
	addl	$2, %eax
	movl	%eax, 16(%ebx)
	movl	16(%ebx), %eax
	ret
je_else.2420:
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %eax
	ret
.globl min_caml_trace_entry
min_caml_trace_entry:
	pushl	%eax
	pushl	%ebx
	call	min_caml_test_trace
	popl	%edx
	popl	%edx
	ret
.globl min_caml_mid_layer
min_caml_mid_layer:
	movl	4(%esp), %eax
	movl	0(%esp), %ebx
	jmp	interp.225
