	.file	"queue_tests.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%d  removed data: %d %d\n"
	.text
	.p2align 4,,15
	.globl	multi_th_remove
	.type	multi_th_remove, @function
multi_th_remove:
.LFB54:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	xorl	%ebx, %ebx
	subq	$24, %rsp
	.cfi_def_cfa_offset 48
	movl	4(%rdi), %eax
	testl	%eax, %eax
	jle	.L3
	.p2align 4,,10
	.p2align 3
.L5:
	movq	test_queue(%rip), %rdi
	leaq	12(%rsp), %rsi
	movl	$0, 12(%rsp)
	call	queue_deq
	movl	12(%rsp), %ecx
	movl	0(%rbp), %edx
	movl	%ebx, %r8d
	xorl	%eax, %eax
	movl	$.LC0, %esi
	movl	$1, %edi
	addl	$1, %ebx
	call	__printf_chk
	cmpl	%ebx, 4(%rbp)
	jg	.L5
.L3:
	addq	$24, %rsp
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE54:
	.size	multi_th_remove, .-multi_th_remove
	.section	.rodata.str1.1
.LC1:
	.string	"%d  added data: %d\n"
	.text
	.p2align 4,,15
	.globl	multi_th_insert
	.type	multi_th_insert, @function
multi_th_insert:
.LFB53:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rdi, %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	xorl	%ebx, %ebx
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	movl	4(%rdi), %edx
	testl	%edx, %edx
	jle	.L12
	.p2align 4,,10
	.p2align 3
.L14:
	movq	test_queue(%rip), %rdi
	movq	%rbx, %rsi
	call	queue_enq
	movl	0(%rbp), %edx
	movl	%ebx, %ecx
	xorl	%eax, %eax
	movl	$.LC1, %esi
	movl	$1, %edi
	call	__printf_chk
	leal	1(%rbx), %eax
	addq	$1, %rbx
	cmpl	%eax, 4(%rbp)
	jg	.L14
.L12:
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	xorl	%eax, %eax
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE53:
	.size	multi_th_insert, .-multi_th_insert
	.p2align 4,,15
	.globl	single_thread_instert
	.type	single_thread_instert, @function
single_thread_instert:
.LFB51:
	.cfi_startproc
	testl	%edi, %edi
	jle	.L25
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	leal	-1(%rdi), %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	addq	$1, %rbp
	xorl	%ebx, %ebx
	subq	$8, %rsp
	.cfi_def_cfa_offset 32
	.p2align 4,,10
	.p2align 3
.L20:
	movq	test_queue(%rip), %rdi
	movq	%rbx, %rsi
	addq	$1, %rbx
	call	queue_enq
	cmpq	%rbp, %rbx
	jne	.L20
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_restore 6
	.cfi_def_cfa_offset 8
.L25:
	rep
	ret
	.cfi_endproc
.LFE51:
	.size	single_thread_instert, .-single_thread_instert
	.section	.rodata.str1.1
.LC2:
	.string	"read value: %d  %d\n"
	.text
	.p2align 4,,15
	.globl	single_thread_remove
	.type	single_thread_remove, @function
single_thread_remove:
.LFB52:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movl	%edi, %ebp
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	xorl	%ebx, %ebx
	subq	$24, %rsp
	.cfi_def_cfa_offset 48
	testl	%edi, %edi
	jle	.L26
	.p2align 4,,10
	.p2align 3
.L33:
	movl	$0, (%rsp)
	.p2align 4,,10
	.p2align 3
.L28:
	movq	test_queue(%rip), %rdi
	movq	%rsp, %rsi
	call	queue_deq
	cmpl	$-1, %eax
	je	.L28
	movl	(%rsp), %ecx
	movl	%ebx, %edx
	xorl	%eax, %eax
	movl	$.LC2, %esi
	movl	$1, %edi
	addl	$1, %ebx
	call	__printf_chk
	cmpl	%ebp, %ebx
	jne	.L33
.L26:
	addq	$24, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE52:
	.size	single_thread_remove, .-single_thread_remove
	.section	.rodata.str1.1
.LC3:
	.string	"starting threads"
	.text
	.p2align 4,,15
	.globl	run_threads
	.type	run_threads, @function
run_threads:
.LFB55:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%r15
	.cfi_offset 15, -24
	movl	%edi, %r15d
	pushq	%r14
	.cfi_offset 14, -32
	leal	(%rdi,%rsi), %r14d
	movl	$.LC3, %edi
	pushq	%r13
	movslq	%r14d, %rax
	leaq	22(,%rax,8), %rax
	pushq	%r12
	.cfi_offset 13, -40
	.cfi_offset 12, -48
	xorl	%r12d, %r12d
	andq	$-16, %rax
	pushq	%rbx
	.cfi_offset 3, -56
	movl	%esi, %ebx
	subq	$8, %rsp
	subq	%rax, %rsp
	call	puts
	testl	%r15d, %r15d
	movq	%rsp, %r13
	jle	.L42
	.p2align 4,,10
	.p2align 3
.L50:
	movl	$8, %edi
	call	malloc
	movslq	%r12d, %rdx
	movl	%r12d, (%rax)
	xorl	%esi, %esi
	leaq	0(%r13,%rdx,8), %rdi
	movl	$1000, 4(%rax)
	movq	%rax, %rcx
	movl	$multi_th_insert, %edx
	addl	$1, %r12d
	call	pthread_create
	cmpl	%r15d, %r12d
	jne	.L50
.L42:
	testl	%ebx, %ebx
	leal	(%rbx,%rbx), %r12d
	jle	.L41
	.p2align 4,,10
	.p2align 3
.L49:
	movl	$8, %edi
	call	malloc
	movslq	%ebx, %rdx
	movl	%ebx, (%rax)
	xorl	%esi, %esi
	leaq	0(%r13,%rdx,8), %rdi
	movl	$1000, 4(%rax)
	movq	%rax, %rcx
	movl	$multi_th_remove, %edx
	addl	$1, %ebx
	call	pthread_create
	cmpl	%r12d, %ebx
	jne	.L49
.L41:
	xorl	%ebx, %ebx
	testl	%r14d, %r14d
	jle	.L38
	.p2align 4,,10
	.p2align 3
.L48:
	movq	0(%r13,%rbx,8), %rdi
	xorl	%esi, %esi
	addq	$1, %rbx
	call	pthread_join
	cmpl	%ebx, %r14d
	jg	.L48
.L38:
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE55:
	.size	run_threads, .-run_threads
	.section	.text.startup,"ax",@progbits
	.p2align 4,,15
	.globl	main
	.type	main, @function
main:
.LFB56:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$10000, %edi
	call	queue_init
	movl	$2, %esi
	movl	$2, %edi
	movq	%rax, test_queue(%rip)
	call	run_threads
	movl	$1, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE56:
	.size	main, .-main
	.comm	test_queue,8,8
	.ident	"GCC: (Ubuntu/Linaro 4.7.3-2ubuntu4) 4.7.3"
	.section	.note.GNU-stack,"",@progbits
