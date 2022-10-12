	.text
	.file	"main"
	.globl	expand_variable_storage         # -- Begin function expand_variable_storage
	.p2align	4, 0x90
	.type	expand_variable_storage,@function
expand_variable_storage:                # @expand_variable_storage
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%r12
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	pushq	%rax
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -40
	.cfi_offset %r12, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movq	variable_storage@GOTPCREL(%rip), %r14
	movq	(%r14), %rbx
	movq	8(%r14), %rdi
	leaq	(%rbx,%rbx), %r12
	movq	%rbx, %rax
	shlq	$5, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	realloc@PLT
	movq	%rax, %r15
	shlq	$4, %rbx
	leaq	(%rbx,%rbx,2), %rdx
	leaq	(%rax,%rdx), %rdi
	xorl	%esi, %esi
	callq	memset@PLT
	movq	%r12, (%r14)
	movq	%r15, 8(%r14)
	addq	$8, %rsp
	.cfi_def_cfa_offset 40
	popq	%rbx
	.cfi_def_cfa_offset 32
	popq	%r12
	.cfi_def_cfa_offset 24
	popq	%r14
	.cfi_def_cfa_offset 16
	popq	%r15
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	expand_variable_storage, .Lfunc_end0-expand_variable_storage
	.cfi_endproc
                                        # -- End function
	.globl	initialize_variable_storage     # -- Begin function initialize_variable_storage
	.p2align	4, 0x90
	.type	initialize_variable_storage,@function
initialize_variable_storage:            # @initialize_variable_storage
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	variable_storage@GOTPCREL(%rip), %r14
	movl	$768, %esi                      # imm = 0x300
	xorl	%edi, %edi
	callq	realloc@PLT
	movq	%rax, %rbx
	movl	$768, %edx                      # imm = 0x300
	movq	%rax, %rdi
	xorl	%esi, %esi
	callq	memset@PLT
	movq	%rbx, 8(%r14)
	movq	$16, (%r14)
	addq	$8, %rsp
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%r14
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	initialize_variable_storage, .Lfunc_end1-initialize_variable_storage
	.cfi_endproc
                                        # -- End function
	.globl	allocate_backing_store          # -- Begin function allocate_backing_store
	.p2align	4, 0x90
	.type	allocate_backing_store,@function
allocate_backing_store:                 # @allocate_backing_store
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	variable_storage@GOTPCREL(%rip), %rcx
	movq	(%rcx), %rax
	movq	8(%rcx), %rbx
	.p2align	4, 0x90
.LBB2_1:                                # %loop
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB2_3
# %bb.2:                                # %loop-continued
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$0, (%rbx)
	jne	.LBB2_1
	jmp	.LBB2_4
.LBB2_3:                                # %no-unallocated-backing-store
	callq	expand_variable_storage@PLT
.LBB2_4:                                # %unallocated-backing-store-found
	movq	%rbx, %rax
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	allocate_backing_store, .Lfunc_end2-allocate_backing_store
	.cfi_endproc
                                        # -- End function
	.type	variable_storage,@object        # @variable_storage
	.bss
	.globl	variable_storage
	.p2align	3
variable_storage:
	.zero	16
	.size	variable_storage, 16

	.section	".note.GNU-stack","",@progbits
