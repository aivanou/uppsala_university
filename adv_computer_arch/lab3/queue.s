	.file	"queue.c"
	.text
	.p2align 4,,15
	.globl	queue_init
	.type	queue_init, @function
queue_init:
.LFB51:
	.cfi_startproc
	movq	%rbx, -16(%rsp)
	movq	%rbp, -8(%rsp)
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -24
	.cfi_offset 6, -16
	movl	%edi, %ebp
	movl	$120, %edi
	call	malloc
	leaq	16(%rax), %rdi
	movq	%rax, %rbx
	xorl	%esi, %esi
	movq	$0, (%rax)
	movq	$0, 8(%rax)
	call	pthread_mutex_init
	leaq	56(%rbx), %rdi
	xorl	%esi, %esi
	call	pthread_cond_init
	movl	%ebp, 108(%rbx)
	movl	$1, 112(%rbx)
	movq	%rbx, %rax
	movl	$0, 104(%rbx)
	movq	16(%rsp), %rbp
	movq	8(%rsp), %rbx
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE51:
	.size	queue_init, .-queue_init
	.p2align 4,,15
	.globl	queue_top
	.type	queue_top, @function
queue_top:
.LFB52:
	.cfi_startproc
	movq	%rbp, -24(%rsp)
	.cfi_offset 6, -32
	leaq	16(%rdi), %rbp
	movq	%rbx, -32(%rsp)
	movq	%r12, -16(%rsp)
	movq	%r13, -8(%rsp)
	.cfi_offset 3, -40
	.cfi_offset 12, -24
	.cfi_offset 13, -16
	movq	%rdi, %rbx
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	%rbp, %rdi
	movq	%rsi, %r13
	call	pthread_mutex_lock
	cmpl	$3, 112(%rbx)
	movl	$3, %eax
	leaq	56(%rbx), %r12
	je	.L4
	.p2align 4,,10
	.p2align 3
.L8:
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.L13
	movq	%rbp, %rsi
	movq	%r12, %rdi
	call	pthread_cond_wait
	cmpl	$3, 112(%rbx)
	jne	.L8
	movq	%rbp, %rdi
	call	pthread_mutex_unlock
	movl	$3, %eax
.L4:
	movq	8(%rsp), %rbx
	movq	16(%rsp), %rbp
	movq	24(%rsp), %r12
	movq	32(%rsp), %r13
	addq	$40, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L13:
	.cfi_restore_state
	movq	(%rax), %rbx
	movq	%rbp, %rdi
	call	pthread_mutex_unlock
	movl	$1, %eax
	movq	%rbx, 0(%r13)
	jmp	.L4
	.cfi_endproc
.LFE52:
	.size	queue_top, .-queue_top
	.p2align 4,,15
	.globl	queue_enq
	.type	queue_enq, @function
queue_enq:
.LFB53:
	.cfi_startproc
	movq	%rbx, -24(%rsp)
	movq	%rbp, -16(%rsp)
	.cfi_offset 3, -32
	.cfi_offset 6, -24
	movq	%rdi, %rbx
	movq	%r12, -8(%rsp)
	movl	$16, %edi
	.cfi_offset 12, -16
	movq	%rsi, %r12
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	call	malloc
	movq	%r12, (%rax)
	leaq	16(%rbx), %r12
	movq	%rax, %rbp
	movq	%r12, %rdi
	call	pthread_mutex_lock
	cmpq	$0, (%rbx)
	je	.L19
	movq	8(%rbx), %rax
	movq	%rbp, 8(%rax)
.L18:
	addl	$1, 104(%rbx)
	movq	%rbp, 8(%rbx)
	movq	%r12, %rdi
	call	pthread_mutex_unlock
	leaq	56(%rbx), %rdi
	call	pthread_cond_broadcast
	movl	$1, %eax
	movq	(%rsp), %rbx
	movq	8(%rsp), %rbp
	movq	16(%rsp), %r12
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L19:
	.cfi_restore_state
	movq	%rbp, (%rbx)
	jmp	.L18
	.cfi_endproc
.LFE53:
	.size	queue_enq, .-queue_enq
	.p2align 4,,15
	.globl	queue_deq
	.type	queue_deq, @function
queue_deq:
.LFB54:
	.cfi_startproc
	pushq	%r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	movq	%rsi, %r13
	pushq	%r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	pushq	%rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	leaq	16(%rdi), %rbp
	pushq	%rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	movq	%rdi, %rbx
	movq	%rbp, %rdi
	leaq	56(%rbx), %r12
	subq	$8, %rsp
	.cfi_def_cfa_offset 48
	call	pthread_mutex_lock
	jmp	.L21
	.p2align 4,,10
	.p2align 3
.L22:
	movq	%rbp, %rsi
	movq	%r12, %rdi
	call	pthread_cond_wait
.L21:
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.L22
	movl	104(%rbx), %edx
	testl	%edx, %edx
	je	.L22
	subl	$1, %edx
	movq	8(%rdi), %rcx
	movl	%edx, 104(%rbx)
	movq	(%rdi), %rdx
	movq	%rcx, (%rbx)
	movq	%rdx, 0(%r13)
	call	free
	movq	%rbp, %rdi
	call	pthread_mutex_unlock
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	movl	$1, %eax
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%rbp
	.cfi_def_cfa_offset 24
	popq	%r12
	.cfi_def_cfa_offset 16
	popq	%r13
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE54:
	.size	queue_deq, .-queue_deq
	.p2align 4,,15
	.globl	queue_interrupt_all
	.type	queue_interrupt_all, @function
queue_interrupt_all:
.LFB55:
	.cfi_startproc
	pushq	%r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	leaq	16(%rdi), %r12
	pushq	%rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	movq	%rdi, %rbp
	movq	%r12, %rdi
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	call	pthread_mutex_lock
	movq	0(%rbp), %rdi
	movl	$3, 112(%rbp)
	testq	%rdi, %rdi
	je	.L33
	.p2align 4,,10
	.p2align 3
.L34:
	movq	8(%rdi), %rbx
	call	free
	testq	%rbx, %rbx
	movq	%rbx, 0(%rbp)
	movq	%rbx, %rdi
	jne	.L34
.L33:
	leaq	56(%rbp), %rdi
	call	pthread_cond_broadcast
	popq	%rbx
	.cfi_def_cfa_offset 24
	popq	%rbp
	.cfi_def_cfa_offset 16
	movq	%r12, %rdi
	popq	%r12
	.cfi_def_cfa_offset 8
	jmp	pthread_mutex_unlock
	.cfi_endproc
.LFE55:
	.size	queue_interrupt_all, .-queue_interrupt_all
	.ident	"GCC: (Ubuntu/Linaro 4.7.3-2ubuntu4) 4.7.3"
	.section	.note.GNU-stack,"",@progbits
