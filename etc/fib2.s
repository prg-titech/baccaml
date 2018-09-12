.globl min_caml_test_trace
min_caml_test_trace:
	movl	0(%ebp), %eax
	movl	4(%esp), %eax # added
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$2, %ebx
	jg	jle_else.2995
	movl	$1, %ebx
	jmp	jle_cont.2996
jle_else.2995:
	movl	$0, %ebx
jle_cont.2996:
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$0, %ebx
	jne	je_else.2997
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	subl	$1, %ebx
	movl	4(%esp), %eax # added
	movl	%ebx, 0(%eax) # fixed
	movl	$100, %ebx
	movl	$0, %ecx
	movl	$5, %edx
	movl	$3, %esi
	movl	%eax, 8(%ebp)
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
	movl	4(%esp), %eax # added
	movl	%eax, 0(%ebx) # fixed
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
	movl	%eax, 24(%ebx)
	movl	24(%ebx), %eax
	addl	$2, %eax
	movl	%eax, 24(%ebx)
	movl	24(%ebx), %eax
	movl	%eax, 20(%ebx)
	movl	16(%ebx), %eax
	movl	%eax, 12(%ebx)
	movl	8(%ebx), %eax
	ret
je_else.2997:
	movl	$1, %eax
	ret
.globl min_caml_trace_entry
min_caml_trace_entry:
	pushl	%eax
	pushl	%ebx
	call	min_caml_test_trace
	popl	%edx
	popl	%esi
	ret
.globl min_caml_mid_layer
min_caml_mid_layer:
	movl	12(%esp), %eax
	movl	8(%esp), %ebx
	jmp	interp.221
