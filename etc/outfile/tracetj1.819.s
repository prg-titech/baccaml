.code32
.data
.balign	8
.text
.globl tracetj1
tracetj1:
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%eax
	movl	36(%esp),%ebx
	call	tracetj1.819
.globl debug_tracetj1.819
debug_tracetj1.819:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
.globl tracetj1.819
tracetj1.819:
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
	addl	%ecx, %edx
	movl	%eax, %ecx
	movl	%edx, (%ecx,%esi,4)
	subl	$1, %ebx
	movl	%ebx, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ebx, %edx
	subl	$2, %edx
	movl	(%eax,%edx,4), %edx
	cmpl	$200, %edx
	jne	je_else.823
	movl	%ebx, %edx
	subl	$3, %edx
	movl	(%eax,%edx,4), %edx
	movl	%ebx, %esi
	subl	$1, %esi
	subl	$3, %esi
	movl	%ecx, (%eax,%esi,4)
	subl	$1, %ebx
	subl	$2, %ebx
	cmpl	$22, %edx
	jl	jge_else.824
	movl	%edx, 0(%ebp)
	movl	%ebx, 4(%ebp)
	movl	%eax, 8(%ebp)
	movl	min_caml_bp,%ecx
	movl	8(%ebp), %eax
	movl	4(%ebp), %ebx
	movl	0(%ebp), %edx
	jmp	guard_tracetj1.819
jge_else.824:
	jmp	tracetj1.819
je_else.823:
	movl	%ecx, %eax
	ret
guard_tracetj1.819:
	movl	%eax, min_caml_guard_stack
	movl	%ebx, min_caml_guard_sp
	movl	%ecx, min_caml_guard_bytecode
	movl	%edx, min_caml_guard_pc
	movl	$1, min_caml_guard_fail_flg
	ret
