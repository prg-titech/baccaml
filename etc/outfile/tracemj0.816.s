.code32
.data
mj_result:
	.long	0x0
.balign	8
.text
.globl tracemj0
tracemj0:
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%eax
	movl	36(%esp),%ebx
	call	tracemj0.816
	movl	%eax, mj_result
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	movl	mj_result, %eax
	ret
.globl tracemj0.816
tracemj0.816:
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
	cmpl	%edx, %ecx
	jg	jle_else.7
	movl	$0, %ecx
	jmp	jle_cont.8
jle_else.7:
	movl	$1, %ecx
jle_cont.8:
	movl	%ebx, %edx
	subl	$2, %edx
	movl	%eax, %esi
	movl	%ecx, (%esi,%edx,4)
	subl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	subl	$1, %ebx
	cmpl	$0, %ecx
	jne	je_else.9
	movl	%ebx, %ecx
	subl	$1, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$2, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	$1, %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%eax, %ecx
	movl	%ebx, %edx
	subl	$1, %edx
	movl	(%ecx,%edx,4), %ecx
	movl	%ebx, %edx
	subl	$1, %edx
	movl	%eax, %esi
	subl	$1, %edx
	movl	(%esi,%edx,4), %edx
	movl	%ebx, %esi
	subl	$2, %esi
	subl	%ecx, %edx
	movl	%eax, %ecx
	movl	%edx, (%ecx,%esi,4)
	subl	$1, %ebx
	movl	$100, %ecx
	movl	%ecx, (%eax,%ebx,4)
	movl	%ebx, %ecx
	addl	$1, %ecx
	movl	%eax, 0(%ebp)
	movl	%ebx, 4(%ebp)
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	tracemj0.816
	subl	$8, %ebp
	movl	4(%ebp), %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	0(%ebp), %edx
	movl	%eax, (%edx,%ecx,4)
	movl	%edx, %eax
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %eax
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	%edx, %esi
	subl	$1, %ecx
	movl	(%esi,%ecx,4), %ecx
	movl	%ebx, %esi
	subl	$2, %esi
	addl	%eax, %ecx
	movl	%edx, %eax
	movl	%ecx, (%eax,%esi,4)
	subl	$1, %ebx
	movl	%ebx, %eax
	subl	$1, %eax
	movl	(%edx,%eax,4), %eax
	subl	$2, %ebx
	movl	(%edx,%ebx,4), %ebx
	cmpl	$200, %ebx
	jne	je_else.10
	ret
je_else.10:
	ret
je_else.9:
	movl	%ebx, %ecx
	subl	$1, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	subl	$2, %ebx
	movl	(%eax,%ebx,4), %eax
	cmpl	$200, %eax
	jne	je_else.12
	ret
je_else.12:
	movl	%ecx, %eax
	ret
