.globl min_caml_test_trace
min_caml_test_trace:
	movl	%ebx, %ecx
	subl	$1, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	$2, %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ebx, %edx
	subl	$2, %edx
	movl	(%eax,%edx,4), %edx
	movl	%ebx, %esi
	subl	$2, %esi
	cmpl	%edx, %ecx
	jg	jle_else.4134
	movl	$0, %ecx
	jmp	jle_cont.4135
jle_else.4134:
	movl	$1, %ecx
jle_cont.4135:
	movl	%ecx, (%eax,%esi,4)
	subl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	cmpl	$0, %ecx
	jne	je_else.4136
	subl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	$1, %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ebx, %edx
	subl	$2, %edx
	movl	(%eax,%edx,4), %edx
	movl	%ebx, %esi
	subl	$2, %esi
	subl	%ecx, %edx
	movl	%edx, (%eax,%esi,4)
	subl	$1, %ebx
	movl	$18, %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	jmp	min_caml_test_trace
je_else.4136:
	movl	$5, %ecx
	movl	$100, %edx
	subl	$1, %ebx
	addl	$2, %ecx
	movl	%edx, 0(%ebp)
	movl	%ecx, %edx
	movl	0(%ebp), %ecx
	jmp	min_caml_mid_layer
.globl min_caml_trace_entry
min_caml_trace_entry:
	pushl	%eax
	pushl	%ecx
	call	min_caml_test_trace
	popl	%edx
	popl	%edx
	ret
.globl min_caml_mid_layer
min_caml_mid_layer:
	movl	4(%esp), %ecx
	movl	8(%esp), %eax
	jmp	interp.230
