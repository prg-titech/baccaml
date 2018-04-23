.data
.balign	8
.text
.globl interp.125
interp.125:
	movl	(%eax,%ebx,4), %esi
	cmpl	$0, %esi
	jne	je_else.279
	movl	(%ecx,%edx,4), %esi
	movl	%edx, %edi
	subl	$1, %edi
	movl	(%ecx,%edi,4), %edi
	movl	%eax, 0(%ebp)
	movl	%edx, %eax
	subl	$1, %eax
	addl	%esi, %edi
	movl	%edi, (%ecx,%eax,4)
	addl	$1, %ebx
	subl	$1, %edx
	movl	0(%ebp), %eax
	jmp	interp.125
je_else.279:
	cmpl	$1, %esi
	jne	je_else.280
	movl	(%ecx,%edx,4), %esi
	movl	%edx, %edi
	subl	$1, %edi
	movl	(%ecx,%edi,4), %edi
	movl	%eax, 0(%ebp)
	movl	%edx, %eax
	subl	$1, %eax
	subl	%esi, %edi
	movl	%edi, (%ecx,%eax,4)
	addl	$1, %ebx
	subl	$1, %edx
	movl	0(%ebp), %eax
	jmp	interp.125
je_else.280:
	cmpl	$4, %esi
	jne	je_else.281
	movl	%ebx, %esi
	addl	$1, %esi
	movl	(%eax,%esi,4), %esi
	movl	%esi, (%ecx,%edx,4)
	addl	$2, %ebx
	addl	$1, %edx
	jmp	interp.125
je_else.281:
	cmpl	$10, %esi
	jne	je_else.282
	movl	%ebx, %esi
	addl	$1, %esi
	movl	(%eax,%esi,4), %esi
	movl	%ebx, %edi
	addl	$2, %edi
	movl	(%eax,%edi,4), %edi
	movl	%ebx, 4(%ebp)
	addl	$3, %ebx
	movl	(%eax,%ebx,4), %ebx
	movl	%esi, 8(%ebp)
	movl	%edx, %esi
	addl	$1, %esi
	movl	%ebx, (%ecx,%esi,4)
	movl	%edx, %ebx
	addl	$2, %ebx
	movl	%edi, (%ecx,%ebx,4)
	movl	%edx, %ebx
	addl	$2, %ebx
	movl	8(%ebp), %esi
	movl	%eax, 0(%ebp)
	movl	%ecx, 12(%ebp)
	movl	%edx, 16(%ebp)
	movl	%ebx, %edx
	movl	%esi, %ebx
	addl	$24, %ebp
	call	interp.125
	subl	$24, %ebp
	movl	16(%ebp), %ebx
	movl	%ebx, %ecx
	addl	$1, %ecx
	movl	12(%ebp), %edx
	movl	%eax, (%edx,%ecx,4)
	movl	4(%ebp), %eax
	addl	$4, %eax
	addl	$1, %ebx
	movl	0(%ebp), %ecx
	movl	%edx, 24(%ebp)
	movl	%ebx, %edx
	movl	%eax, %ebx
	movl	%ecx, %eax
	movl	24(%ebp), %ecx
	jmp	interp.125
je_else.282:
	cmpl	$11, %esi
	jne	je_else.283
	movl	(%ecx,%edx,4), %eax
	ret
je_else.283:
	cmpl	$12, %esi
	jne	je_else.284
	movl	(%ecx,%edx,4), %esi
	cmpl	$0, %esi
	jl	jge_else.285
	addl	$1, %ebx
	movl	(%eax,%ebx,4), %ebx
	subl	$1, %edx
	jmp	interp.125
jge_else.285:
	addl	$2, %ebx
	movl	(%eax,%ebx,4), %ebx
	subl	$1, %edx
	jmp	interp.125
je_else.284:
	cmpl	$22, %esi
	jne	je_else.286
	movl	%ebx, %esi
	addl	$1, %esi
	movl	(%eax,%esi,4), %esi
	subl	%edx, %esi
	negl	%esi
	movl	(%ecx,%esi,4), %esi
	movl	%edx, %edi
	addl	$1, %edi
	movl	%esi, (%ecx,%edi,4)
	addl	$2, %ebx
	addl	$1, %edx
	jmp	interp.125
je_else.286:
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
	movl	$10, %ebx
	movl	$0, %ecx
	movl	%eax, 0(%ebp)
	movl	%ebx, %eax
	movl	%ecx, %ebx
	addl	$8, %ebp
	call	min_caml_create_array
	subl	$8, %ebp
	movl	%eax, %ecx
	movl	$22, %eax
	movl	0(%ebp), %ebx
	movl	%eax, 0(%ebx)
	movl	$1, %eax
	movl	%eax, 4(%ebx)
	movl	$22, %eax
	movl	%eax, 8(%ebx)
	movl	$1, %eax
	movl	%eax, 12(%ebx)
	movl	$0, %eax
	movl	%eax, 16(%ebx)
	movl	$11, %eax
	movl	%eax, 20(%ebx)
	movl	$12, %eax
	movl	%eax, 24(%ebx)
	movl	$9, %eax
	movl	%eax, 28(%ebx)
	movl	$13, %eax
	movl	%eax, 32(%ebx)
	movl	$10, %eax
	movl	%eax, 36(%ebx)
	movl	$0, %eax
	movl	%eax, 40(%ebx)
	movl	$4, %eax
	movl	%eax, 44(%ebx)
	movl	$5, %eax
	movl	%eax, 48(%ebx)
	movl	$11, %eax
	movl	%eax, 52(%ebx)
	movl	$6, %eax
	movl	$1, %edx
	movl	%ebx, 8(%ebp)
	movl	%eax, %ebx
	movl	8(%ebp), %eax
	addl	$8, %ebp
	call	interp.125
	subl	$8, %ebp
	addl	$8, %ebp
	call	min_caml_print_int
	subl	$8, %ebp
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
