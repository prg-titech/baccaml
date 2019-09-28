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
	jg	jle_else.17
	movl	$0, %ecx
	jmp	jle_cont.18
jle_else.17:
	movl	$1, %ecx
jle_cont.18:
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
	jne	je_else.19
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
	movl	%ebx, %ecx
	subl	$2, %ecx
	movl	(%edx,%ecx,4), %ecx
	cmpl	$200, %ecx
	jne	je_else.20
	movl	$22, %ecx
	movl	%eax, 8(%ebp)
	movl	%edx, 12(%ebp)
	movl	%ebx, 16(%ebp)
	movl	%ecx, 20(%ebp)
	movl	min_caml_bp,%ecx
	movl	20(%ebp), %eax
	addl	$1, %eax
	movl	(%ecx,%eax,4), %eax
	movl	16(%ebp), %ebx
	movl	%ebx, %edx
	subl	$3, %edx
	movl	12(%ebp), %esi
	movl	(%esi,%edx,4), %edx
	movl	%ebx, %edi
	subl	%eax, %edi
	subl	$3, %edi
	movl	%ecx, 24(%ebp)
	movl	8(%ebp), %ecx
	movl	%ecx, (%esi,%edi,4)
	subl	%eax, %ebx
	subl	$2, %ebx
	cmpl	$22, %edx
	jl	jge_else.21
	movl	24(%ebp), %ecx
	movl	%esi, %eax
	jmp	guard_tracemj0.816
jge_else.21:
	movl	24(%ebp), %ecx
	movl	%esi, %eax
	jmp	guard_tracemj0.816
je_else.20:
	ret
je_else.19:
	movl	%ebx, %ecx
	subl	$1, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ebx, %edx
	subl	$2, %edx
	movl	(%eax,%edx,4), %edx
	cmpl	$200, %edx
	jne	je_else.22
	movl	$22, %edx
	movl	%ecx, 28(%ebp)
	movl	%eax, 32(%ebp)
	movl	%ebx, 36(%ebp)
	movl	%edx, 40(%ebp)
	movl	min_caml_bp,%ecx
	movl	40(%ebp), %eax
	addl	$1, %eax
	movl	(%ecx,%eax,4), %eax
	movl	36(%ebp), %ebx
	movl	%ebx, %edx
	subl	$3, %edx
	movl	32(%ebp), %esi
	movl	(%esi,%edx,4), %edx
	movl	%ebx, %edi
	subl	%eax, %edi
	subl	$3, %edi
	movl	%ecx, 44(%ebp)
	movl	28(%ebp), %ecx
	movl	%ecx, (%esi,%edi,4)
	subl	%eax, %ebx
	subl	$2, %ebx
	cmpl	$22, %edx
	jl	jge_else.23
	movl	44(%ebp), %ecx
	movl	%esi, %eax
	jmp	guard_tracemj0.816
jge_else.23:
	movl	44(%ebp), %ecx
	movl	%esi, %eax
	jmp	guard_tracemj0.816
je_else.22:
	movl	%ecx, %eax
	ret
guard_tracemj0.816:
	movl	%eax, min_caml_guard_stack
	movl	%ebx, min_caml_guard_sp
	movl	%ecx, min_caml_guard_bytecode
	movl	%edx, min_caml_guard_pc
	movl	$1, min_caml_guard_fail_flg
	ret
