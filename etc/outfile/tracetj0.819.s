.code32
.data
.balign	8
.text
.globl tracetj0
tracetj0:
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%eax
	movl	36(%esp),%ebx
	call	tracetj0.819
.globl debug_tracetj0.819
debug_tracetj0.819:
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
.globl tracetj0.819
tracetj0.819:
	movl	%ebx, %ecx
	subl	$2, %ecx
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
	jg	jle_else.823
	movl	$0, %ecx
	jmp	jle_cont.824
jle_else.823:
	movl	$1, %ecx
jle_cont.824:
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
	jne	je_else.825
	movl	%ebx, %ecx
	subl	$2, %ecx
	subl	$1, %ecx
	movl	(%eax,%ecx,4), %ecx
	movl	%ecx, (%eax,%ebx,4)
	addl	$1, %ebx
	movl	%ebx, %ecx
	subl	$3, %ecx
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
	movl	$21, %ecx
	movl	%ecx, (%eax,%ebx,4)
	movl	%ebx, %ecx
	addl	$1, %ecx
	movl	$200, %edx
	movl	%edx, (%eax,%ecx,4)
	addl	$2, %ebx
	jmp	tracetj0.819
je_else.825:
	movl	$6, %ecx
	movl	%ebx, 0(%ebp)
	movl	%eax, 4(%ebp)
	movl	%ecx, 8(%ebp)
	movl	min_caml_bp,%ecx
	movl	8(%ebp), %eax
	movl	%eax, %edx
	addl	$2, %edx
	movl	4(%ebp), %eax
	movl	0(%ebp), %ebx
	jmp	guard_tracetj0.819
guard_tracetj0.819:
	movl	%eax, min_caml_guard_stack
	movl	%ebx, min_caml_guard_sp
	movl	%ecx, min_caml_guard_bytecode
	movl	%edx, min_caml_guard_pc
	movl	$1, min_caml_guard_fail_flg
	ret
