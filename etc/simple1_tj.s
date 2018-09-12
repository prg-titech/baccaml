.globl min_caml_test_trace
min_caml_test_trace:
  pushl %ebx
.globl min_caml_test_trace1
min_caml_test_trace1:
	subl	$1, %eax
	movl	%eax, %ecx
	cmpl	$0, %ecx
	jg	jle_else.110
	movl	$1, %eax
	movl	$0, %ebx
	movl	$2, %edx
	addl	%edx, %eax
	movl	%ebx, 0(%ebp)
	movl	%eax, %ebx
	movl	0(%ebp), %eax
	jmp	min_caml_test_trace2
jle_else.110:
	movl	%ecx, %eax
	jmp	min_caml_test_trace1
.globl min_caml_test_trace2
min_caml_test_trace2:
  popl %eax
  jmp  interpret.40
