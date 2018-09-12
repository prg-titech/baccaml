.data
.balign	8
.text
.globl interp.67
interp.67:
	cmpl	$0, %ebx
	jne	je_else.142
	movl	%eax, %ebx
	movl	%ecx, %eax
	jmp	min_caml_test_trace
je_else.142:
	movl	(%eax,%ebx,4), %edx
	cmpl	$0, %edx
	jne	je_else.143
	addl	$1, %ebx
	addl	$1, %ecx
	jmp	interp.67
je_else.143:
	cmpl	$1, %edx
	jne	je_else.144
	addl	$1, %ebx
	subl	$1, %ecx
	jmp	interp.67
je_else.144:
	cmpl	$10, %edx
	jne	je_else.145
	addl	$1, %ebx
	movl	(%eax,%ebx,4), %ebx
	jmp	interp.67
je_else.145:
	cmpl	$11, %edx
	jne	je_else.146
	cmpl	$0, %ecx
	jg	jle_else.147
	addl	$2, %ebx
	movl	(%eax,%ebx,4), %ebx
	jmp	interp.67
jle_else.147:
	addl	$1, %ebx
	movl	(%eax,%ebx,4), %ebx
	jmp	interp.67
je_else.146:
	cmpl	$20, %edx
	jne	je_else.148
	movl	%ecx, %eax
	ret
je_else.148:
	movl	$-1, %eax
	ret
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$20, %eax
	movl	$0, %ebx
	call	min_caml_create_array
	movl	$0, %ebx
	movl	%ebx, 0(%eax)
	movl	$11, %ebx
	movl	%ebx, 4(%eax)
	movl	$4, %ebx
	movl	%ebx, 8(%eax)
	movl	$8, %ebx
	movl	%ebx, 12(%eax)
	movl	$0, %ebx
	movl	%ebx, 16(%eax)
	movl	$0, %ebx
	movl	%ebx, 20(%eax)
	movl	$10, %ebx
	movl	%ebx, 24(%eax)
	movl	$12, %ebx
	movl	%ebx, 28(%eax)
	movl	$1, %ebx
	movl	%ebx, 32(%eax)
	movl	$1, %ebx
	movl	%ebx, 36(%eax)
	movl	$10, %ebx
	movl	%ebx, 40(%eax)
	movl	$12, %ebx
	movl	%ebx, 44(%eax)
	movl	$20, %ebx
	movl	%ebx, 48(%eax)
	movl	$0, %ebx
	movl	$0, %ecx
	call	interp.67
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
