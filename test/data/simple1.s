.data
.balign	8
.text
.globl interpret.39
interpret.39:
	movl	(%eax,%ebx,4), %edx
	cmpl	$0, %edx
	jne	je_else.85
	addl	$1, %ebx
	addl	$1, %ecx
	jmp	interpret.39
je_else.85:
	cmpl	$1, %edx
	jne	je_else.86
	addl	$1, %ebx
	subl	$1, %ecx
	jmp	interpret.39
je_else.86:
	cmpl	$2, %edx
	jne	je_else.87
	movl	%ebx, %edx
	addl	$1, %edx
	movl	(%eax,%edx,4), %edx
	cmpl	$0, %ecx
	jg	jle_else.88
	addl	$2, %ebx
	jmp	interpret.39
jle_else.88:
	movl	%edx, %ebx
	jmp	interpret.39
je_else.87:
	cmpl	$3, %edx
	jne	je_else.89
	addl	$1, %ebx
	movl	(%eax,%ebx,4), %ebx
	jmp	interpret.39
je_else.89:
	cmpl	$4, %edx
	jne	je_else.90
	movl	%ecx, %eax
	ret
je_else.90:
	movl	$-100, %eax
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
	movl	$100, %eax
	movl	$0, %ebx
	call	min_caml_create_array
	movl	$1, %ebx
	movl	%ebx, 0(%eax)
	movl	$2, %ebx
	movl	%ebx, 4(%eax)
	movl	$0, %ebx
	movl	%ebx, 8(%eax)
	movl	$4, %ebx
	movl	%ebx, 12(%eax)
	movl	$0, %ebx
	movl	$10000000000, %ecx
	call	interpret.39
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
