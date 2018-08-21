.globl min_caml_test_trace
min_caml_test_trace:
	movl	0(%ebp), %eax
	movl	8(%esp), %eax # added
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$2, %ebx
	jg	jle_else.2994
	movl	$1, %ebx
	jmp	jle_cont.2995
jle_else.2994:
	movl	$0, %ebx
jle_cont.2995:
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	cmpl	$0, %ebx
	jne	je_else.2996
	movl	0(%eax), %ebx
	movl	%ebx, 8(%eax)
	movl	8(%eax), %ebx
	subl	$1, %ebx
	movl	8(%esp), %eax # added
	movl	%ebx, 0(%eax)
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
	movl	%eax, 8(%ebx)
	movl	0(%ebx), %eax
	movl	%eax, 12(%ebx)
	subl  	$2, %eax
	movl	8(%esp), %eax # added
	movl	%eax, 0(%ebx) # fixed
	movl	$100, %eax
	movl	$0, %ecx
	movl	$5, %edx
	movl	$4, %esi
	movl	%ecx, %ebx
	movl	%edx, %ecx
	movl	%esi, %edx
	addl	$8, %ebp
	call	min_caml_mid_layer
	subl	$8, %ebp
	movl	0(%ebp), %ebx
	movl	%eax, 12(%ebx)
	movl	8(%ebx), %eax
	movl	%eax, 16(%ebx)
	movl	8(%ebx), %eax
	movl	%eax, 4(%ebx)
	movl	0(%ebx), %eax
	ret
je_else.2996:
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
