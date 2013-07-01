	.file	"instrument.ll"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4621819117588971520     # double 1.000000e+01
                                        #  (0x4024000000000000)
	.text
	.globl	polybench_flush_cache
	.align	16, 0x90
	.type	polybench_flush_cache,@function
polybench_flush_cache:                  # @polybench_flush_cache
	.cfi_startproc
# BB#0:
	subq	$40, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 48
	movl	$1048576, 36(%rsp)      # imm = 0x100000
	movslq	36(%rsp), %rdi
	movl	$8, %esi
	callq	calloc
	movq	%rax, 24(%rsp)
	movq	$0, 8(%rsp)
	movl	$0, 20(%rsp)
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_2:                                #   in Loop: Header=BB0_1 Depth=1
	movslq	20(%rsp), %rax
	movq	24(%rsp), %rcx
	movsd	8(%rsp), %xmm0
	addsd	(%rcx,%rax,8), %xmm0
	movsd	%xmm0, 8(%rsp)
	incl	20(%rsp)
.LBB0_1:                                # =>This Inner Loop Header: Depth=1
	movl	20(%rsp), %eax
	cmpl	36(%rsp), %eax
	jl	.LBB0_2
# BB#3:
	movsd	.LCPI0_0(%rip), %xmm0
	ucomisd	8(%rsp), %xmm0
	jb	.LBB0_5
# BB#4:
	addq	$40, %rsp
	ret
.LBB0_5:
	movl	$.L.str, %edi
	movl	$.L.str1, %esi
	movl	$39, %edx
	movl	$.L__PRETTY_FUNCTION__.polybench_flush_cache, %ecx
	callq	__assert_fail
.Ltmp2:
	.size	polybench_flush_cache, .Ltmp2-polybench_flush_cache
	.cfi_endproc

	.globl	polybench_timer_start
	.align	16, 0x90
	.type	polybench_timer_start,@function
polybench_timer_start:                  # @polybench_timer_start
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	callq	polybench_flush_cache
	callq	rtclock
	movsd	%xmm0, polybench_t_start(%rip)
	popq	%rax
	ret
.Ltmp5:
	.size	polybench_timer_start, .Ltmp5-polybench_timer_start
	.cfi_endproc

	.globl	polybench_timer_stop
	.align	16, 0x90
	.type	polybench_timer_stop,@function
polybench_timer_stop:                   # @polybench_timer_stop
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp7:
	.cfi_def_cfa_offset 16
	callq	rtclock
	movsd	%xmm0, polybench_t_end(%rip)
	popq	%rax
	ret
.Ltmp8:
	.size	polybench_timer_stop, .Ltmp8-polybench_timer_stop
	.cfi_endproc

	.globl	polybench_timer_print
	.align	16, 0x90
	.type	polybench_timer_print,@function
polybench_timer_print:                  # @polybench_timer_print
	.cfi_startproc
# BB#0:
	pushq	%rax
.Ltmp10:
	.cfi_def_cfa_offset 16
	movsd	polybench_t_end(%rip), %xmm0
	subsd	polybench_t_start(%rip), %xmm0
	movl	$.L.str2, %edi
	movb	$1, %al
	callq	printf
	popq	%rax
	ret
.Ltmp11:
	.size	polybench_timer_print, .Ltmp11-polybench_timer_print
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI4_0:
	.quad	4517329193108106637     # double 1.000000e-06
                                        #  (0x3eb0c6f7a0b5ed8d)
	.text
	.align	16, 0x90
	.type	rtclock,@function
rtclock:                                # @rtclock
	.cfi_startproc
# BB#0:
	subq	$40, %rsp
.Ltmp13:
	.cfi_def_cfa_offset 48
	leaq	16(%rsp), %rdi
	leaq	32(%rsp), %rsi
	callq	gettimeofday
	movl	%eax, 12(%rsp)
	testl	%eax, %eax
	je	.LBB4_2
# BB#1:
	movl	12(%rsp), %esi
	movl	$.L.str3, %edi
	xorb	%al, %al
	callq	printf
.LBB4_2:
	cvtsi2sdq	24(%rsp), %xmm1
	mulsd	.LCPI4_0(%rip), %xmm1
	cvtsi2sdq	16(%rsp), %xmm0
	addsd	%xmm1, %xmm0
	addq	$40, %rsp
	ret
.Ltmp14:
	.size	rtclock, .Ltmp14-rtclock
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	 "tmp <= 10.0"
	.size	.L.str, 12

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	 "utilities/instrument.c"
	.size	.L.str1, 23

	.type	.L__PRETTY_FUNCTION__.polybench_flush_cache,@object # @__PRETTY_FUNCTION__.polybench_flush_cache
.L__PRETTY_FUNCTION__.polybench_flush_cache:
	.asciz	 "void polybench_flush_cache()"
	.size	.L__PRETTY_FUNCTION__.polybench_flush_cache, 29

	.type	polybench_t_start,@object # @polybench_t_start
	.comm	polybench_t_start,8,8
	.type	polybench_t_end,@object # @polybench_t_end
	.comm	polybench_t_end,8,8
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	 "%0.6lfs\n"
	.size	.L.str2, 9

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	 "Error return from gettimeofday: %d"
	.size	.L.str3, 35


	.section	".note.GNU-stack","",@progbits
