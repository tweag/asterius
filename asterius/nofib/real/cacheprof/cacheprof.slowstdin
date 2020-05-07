	.file	"heimdall.c"
	.version	"01.01"
gcc2_compiled.:
.data
	.align 4
	.type	 num_insns_done,@object
	.size	 num_insns_done,4
num_insns_done:
	.long 0
.section	.rodata
	.align 32
.LC46:
	.string	"heimdall: fatal error near insn %d:\n\t%s\n"
.text
	.align 4
	.type	 panic,@function
panic:
	subl $12,%esp
	movl 16(%esp),%eax
	pushl %eax
	movl num_insns_done,%eax
	pushl %eax
	pushl $.LC46
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $1
	call exit
.Lfe1:
	.size	 panic,.Lfe1-panic
.section	.rodata
.LC47:
	.string	"getIMem"
.text
	.align 4
	.type	 getIMem,@function
getIMem:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $4,%edx
	jne .L341
	movl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L341:
	cmpl $1,%edx
	jne .L342
	movzbl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L342:
	cmpl $2,%edx
	je .L343
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
	jmp .L340
	.p2align 4,,7
.L343:
	movzwl (%eax),%eax
.L345:
.L340:
	addl $12,%esp
	ret
.Lfe2:
	.size	 getIMem,.Lfe2-getIMem
.section	.rodata
.LC48:
	.string	"setIMem"
.text
	.align 4
	.type	 setIMem,@function
setIMem:
	subl $12,%esp
	movl 16(%esp),%eax
	movl 20(%esp),%ecx
	movl 24(%esp),%edx
	cmpl $4,%eax
	jne .L347
	movl %edx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L347:
	cmpl $1,%eax
	jne .L348
	movb %dl,(%ecx)
	jmp .L346
	.p2align 4,,7
.L348:
	cmpl $2,%eax
	jne .L349
	movw %dx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L349:
	addl $-12,%esp
	pushl $.LC48
	call panic
	addl $16,%esp
.L346:
	addl $12,%esp
	ret
.Lfe3:
	.size	 setIMem,.Lfe3-setIMem
	.align 4
	.type	 getIRoM,@function
getIRoM:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpb $0,24(%esp)
	je .L373
	cmpl $4,%edx
	jne .L374
	movl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L374:
	cmpl $1,%edx
	jne .L376
	cmpl $3,%eax
	ja .L377
	movzbl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L377:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L376:
	cmpl $2,%edx
	jne .L379
	movzwl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L379:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	jmp .L383
	.p2align 4,,7
.L373:
	addl $-8,%esp
	pushl %eax
	pushl %edx
	call getIMem
.L383:
	addl $16,%esp
.L372:
	addl $12,%esp
	ret
.Lfe4:
	.size	 getIRoM,.Lfe4-getIRoM
.section	.rodata
.LC49:
	.string	"calcBSF(1)"
.LC50:
	.string	"calcBSF(2)"
.text
	.align 4
	.type	 calcBSF,@function
calcBSF:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 36(%esp),%ebx
	cmpl $4,%eax
	je .L385
	cmpl $2,%eax
	je .L385
	cmpl $1,%eax
	je .L385
	addl $-12,%esp
	pushl $.LC49
	call panic
	addl $16,%esp
.L385:
	testl %ebx,%ebx
	jne .L386
	addl $-12,%esp
	pushl $.LC50
	call panic
	addl $16,%esp
.L386:
	xorl %eax,%eax
	testb $1,%bl
	jne .L388
	.p2align 4,,7
.L389:
	incl %eax
	btl %eax,%ebx
	jnc .L389
.L388:
	popl %ebx
	addl $24,%esp
	ret
.Lfe5:
	.size	 calcBSF,.Lfe5-calcBSF
.section	.rodata
.LC51:
	.string	"calcBSR(1)"
.LC52:
	.string	"calcBSR(2)"
.text
	.align 4
	.type	 calcBSR,@function
calcBSR:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	cmpl $4,%ebx
	je .L392
	cmpl $2,%ebx
	je .L392
	cmpl $1,%ebx
	je .L392
	addl $-12,%esp
	pushl $.LC51
	call panic
	addl $16,%esp
.L392:
	testl %esi,%esi
	jne .L393
	addl $-12,%esp
	pushl $.LC52
	call panic
	addl $16,%esp
.L393:
	leal -1(,%ebx,8),%eax
	btl %eax,%esi
	jc .L395
	.p2align 4,,7
.L396:
	decl %eax
	btl %eax,%esi
	jnc .L396
.L395:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe6:
	.size	 calcBSR,.Lfe6-calcBSR
.section	.rodata
.LC53:
	.string	"calcSHLD"
.text
	.align 4
	.type	 calcSHLD,@function
calcSHLD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L401
	ja .L405
	cmpl $1,%edx
	je .L402
	jmp .L403
	.p2align 4,,7
.L405:
	cmpl $4,%edx
	jne .L403
	movl %eax,%edx
	sall %cl,%edx
	movl $32,%eax
	jmp .L407
	.p2align 4,,7
.L401:
	movl %eax,%edx
	sall %cl,%edx
	movl $16,%eax
	jmp .L407
	.p2align 4,,7
.L402:
	movl %eax,%edx
	sall %cl,%edx
	movl $8,%eax
.L407:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	shrl %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L406
	.p2align 4,,7
.L403:
	addl $-12,%esp
	pushl $.LC53
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L406:
	popl %ebx
	addl $24,%esp
	ret
.Lfe7:
	.size	 calcSHLD,.Lfe7-calcSHLD
.section	.rodata
.LC54:
	.string	"calcSHRD"
.text
	.align 4
	.type	 calcSHRD,@function
calcSHRD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L411
	ja .L415
	cmpl $1,%edx
	je .L412
	jmp .L413
	.p2align 4,,7
.L415:
	cmpl $4,%edx
	jne .L413
	movl %eax,%edx
	shrl %cl,%edx
	movl $32,%eax
	jmp .L417
	.p2align 4,,7
.L411:
	movl %eax,%edx
	shrl %cl,%edx
	movl $16,%eax
	jmp .L417
	.p2align 4,,7
.L412:
	movl %eax,%edx
	shrl %cl,%edx
	movl $8,%eax
.L417:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	sall %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L416
	.p2align 4,,7
.L413:
	addl $-12,%esp
	pushl $.LC54
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L416:
	popl %ebx
	addl $24,%esp
	ret
.Lfe8:
	.size	 calcSHRD,.Lfe8-calcSHRD
.section	.rodata
.LC55:
	.string	"calcROL"
.text
	.align 4
	.type	 calcROL,@function
calcROL:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L420
	.p2align 4,,7
.L421:
	cmpl $2,%edi
	je .L424
	ja .L428
	cmpl $1,%edi
	je .L425
	jmp .L426
	.p2align 4,,7
.L428:
	cmpl $4,%edi
	jne .L426
	movl %ebx,%eax
	shrl $31,%eax
	jmp .L430
	.p2align 4,,7
.L424:
	movl %ebx,%eax
	shrl $15,%eax
	jmp .L431
	.p2align 4,,7
.L425:
	movl %ebx,%eax
	shrl $7,%eax
.L431:
	andl $1,%eax
.L430:
	addl %ebx,%ebx
	orl %eax,%ebx
	jmp .L422
	.p2align 4,,7
.L426:
	addl $-12,%esp
	pushl $.LC55
	call panic
	addl $16,%esp
.L422:
	decl %esi
	jnz .L421
.L420:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe9:
	.size	 calcROL,.Lfe9-calcROL
.section	.rodata
.LC56:
	.string	"calcROR"
.text
	.align 4
	.type	 calcROR,@function
calcROR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L434
	.p2align 4,,7
.L435:
	cmpl $2,%edi
	je .L438
	ja .L442
	cmpl $1,%edi
	je .L439
	jmp .L440
	.p2align 4,,7
.L442:
	cmpl $4,%edi
	jne .L440
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $31,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L438:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $15,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L439:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $7,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L440:
	addl $-12,%esp
	pushl $.LC56
	call panic
	addl $16,%esp
.L436:
	decl %esi
	jnz .L435
.L434:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe10:
	.size	 calcROR,.Lfe10-calcROR
.section	.rodata
.LC57:
	.string	"calcSAR"
.text
	.align 4
	.type	 calcSAR,@function
calcSAR:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 40(%esp),%ebx
	cmpl $2,%eax
	je .L447
	ja .L451
	cmpl $1,%eax
	je .L448
	jmp .L449
	.p2align 4,,7
.L451:
	cmpl $4,%eax
	jne .L449
	jmp .L445
	.p2align 4,,7
.L447:
	movswl %bx,%ebx
	jmp .L445
	.p2align 4,,7
.L448:
	movsbl %bl,%ebx
	jmp .L445
	.p2align 4,,7
.L449:
	addl $-12,%esp
	pushl $.LC57
	call panic
	addl $16,%esp
.L445:
	movl 36(%esp),%ecx
	sarl %cl,%ebx
	movl %ebx,%eax
	popl %ebx
	addl $24,%esp
	ret
.Lfe11:
	.size	 calcSAR,.Lfe11-calcSAR
.section	.rodata
.LC58:
	.string	"topBitOf"
.text
	.align 4
	.type	 topBitOf,@function
topBitOf:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	cmpl $4,%esi
	je .L453
	cmpl $2,%esi
	je .L453
	cmpl $1,%esi
	je .L453
	addl $-12,%esp
	pushl $.LC58
	call panic
	addl $16,%esp
.L453:
	leal -1(,%esi,8),%ecx
	shrl %cl,%ebx
	movl %ebx,%eax
	andl $1,%eax
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe12:
	.size	 topBitOf,.Lfe12-topBitOf
	.align 4
	.type	 extend_s_8to16,@function
extend_s_8to16:
	movswl 4(%esp),%eax
	ret
.Lfe13:
	.size	 extend_s_8to16,.Lfe13-extend_s_8to16
	.align 4
	.type	 extend_s_16to32,@function
extend_s_16to32:
	movswl 4(%esp),%eax
	ret
.Lfe14:
	.size	 extend_s_16to32,.Lfe14-extend_s_16to32
.section	.rodata
.LC59:
	.string	"extend_s_to32"
.LC60:
	.string	"extend_s_8to"
.text
	.align 4
	.type	 extend_s_8to,@function
extend_s_8to:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $2,%edx
	je .L470
	ja .L474
	cmpl $1,%edx
	je .L466
	jmp .L472
	.p2align 4,,7
.L474:
	cmpl $4,%edx
	jne .L472
	movsbl %al,%eax
	jmp .L476
	.p2align 4,,7
.L470:
	addl $-12,%esp
	pushl %eax
	call extend_s_8to16
	addl $16,%esp
	jmp .L466
	.p2align 4,,7
.L472:
	addl $-12,%esp
	pushl $.LC60
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L476:
.L466:
	addl $12,%esp
	ret
.Lfe15:
	.size	 extend_s_8to,.Lfe15-extend_s_8to
.section	.rodata
.LC61:
	.string	"extend_s_16to"
.text
	.align 4
	.type	 extend_s_16to,@function
extend_s_16to:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $2,%edx
	je .L477
	cmpl $4,%edx
	jne .L481
	addl $-12,%esp
	pushl %eax
	call extend_s_16to32
	addl $16,%esp
	jmp .L477
	.p2align 4,,7
.L481:
	addl $-12,%esp
	pushl $.LC61
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L477:
	addl $12,%esp
	ret
.Lfe16:
	.size	 extend_s_16to,.Lfe16-extend_s_16to
	.align 4
	.type	 getSDisp16,@function
getSDisp16:
	subl $12,%esp
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	addl $-12,%esp
	pushl %edx
	call extend_s_16to32
	addl $16,%esp
	addl $12,%esp
	ret
.Lfe17:
	.size	 getSDisp16,.Lfe17-getSDisp16
	.align 4
	.type	 getUDisp16,@function
getUDisp16:
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	ret
.Lfe18:
	.size	 getUDisp16,.Lfe18-getUDisp16
.section	.rodata
.LC62:
	.string	"getSDisp"
.text
	.align 4
	.type	 getSDisp,@function
getSDisp:
	subl $12,%esp
	movl 16(%esp),%eax
	cmpl $2,%eax
	je .L494
	jg .L500
	cmpl $1,%eax
	je .L495
	jmp .L498
	.p2align 4,,7
.L500:
	cmpl $4,%eax
	jne .L498
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	jmp .L501
	.p2align 4,,7
.L494:
	call getSDisp16
	jmp .L501
	.p2align 4,,7
.L495:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	movsbl %al,%eax
	jmp .L501
	.p2align 4,,7
.L498:
	addl $-12,%esp
	pushl $.LC62
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L501:
	addl $12,%esp
	ret
.Lfe19:
	.size	 getSDisp,.Lfe19-getSDisp
.section	.rodata
.LC63:
	.string	"getUDisp"
.text
	.align 4
	.type	 getUDisp,@function
getUDisp:
	subl $12,%esp
	movl 16(%esp),%eax
	cmpl $2,%eax
	je .L506
	jg .L511
	cmpl $1,%eax
	je .L507
	jmp .L509
	.p2align 4,,7
.L511:
	cmpl $4,%eax
	jne .L509
	movl m_eip,%eax
	movzbl (%eax),%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $8,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $16,%eax
	orl %eax,%edx
	incl m_eip
	movl m_eip,%eax
	movzbl (%eax),%eax
	sall $24,%eax
	orl %eax,%edx
	incl m_eip
	movl %edx,%eax
	jmp .L512
	.p2align 4,,7
.L506:
	call getUDisp16
	jmp .L512
	.p2align 4,,7
.L507:
	movl m_eip,%eax
	movzbl (%eax),%eax
	incl m_eip
	jmp .L512
	.p2align 4,,7
.L509:
	addl $-12,%esp
	pushl $.LC63
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L512:
	addl $12,%esp
	ret
.Lfe20:
	.size	 getUDisp,.Lfe20-getUDisp
.data
	.align 32
	.type	 cases.468,@object
	.size	 cases.468,128
cases.468:
	.long .L515
	.long .L516
	.long .L517
	.long .L518
	.long .L519
	.long .L520
	.long .L521
	.long .L522
	.long .L523
	.long .L524
	.long .L525
	.long .L526
	.long .L527
	.long .L528
	.long .L529
	.long .L530
	.long .L531
	.long .L532
	.long .L533
	.long .L534
	.long .L535
	.long .L536
	.long .L537
	.long .L538
	.long .L539
	.long .L540
	.long .L541
	.long .L542
	.long .L543
	.long .L544
	.long .L545
	.long .L546
	.align 32
	.type	 parityIsEven,@object
	.size	 parityIsEven,256
parityIsEven:
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.byte 0
.byte 1
.byte 1
.byte 0
.byte 0
.byte 1
.byte 1
.byte 0
.byte 1
.byte 0
.byte 0
.byte 1
.section	.rodata
	.align 32
.LC64:
	.string	"SET_OCSZ_ARITH: src=%u(%d)   dst=%u(%d)   sres=%lld   ures=%llu\n"
.LC67:
	.string	"update_flags_ADD"
	.align 4
.LC65:
	.long -2147483648
	.long 0
	.align 4
.LC66:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_ADD
	.type	 update_flags_ADD,@function
update_flags_ADD:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 36(%esp),%ebp
	movl 40(%esp),%ecx
	movl %ebp,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	addl %ecx,%esi
	adcl $0,%edi
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	addl %eax,%ecx
	adcl %edx,%ebx
	cmpl $2,32(%esp)
	je .L624
	jg .L646
	cmpl $1,32(%esp)
	je .L634
	jmp .L644
	.p2align 4,,7
.L646:
	cmpl $4,32(%esp)
	jne .L644
	movl %ecx,%eax
	movl %ebx,%edx
	addl .LC65,%eax
	adcl .LC65+4,%edx
	je .L615
	orb $8,m_eflags+1
	jmp .L617
	.p2align 4,,7
.L615:
	andb $247,m_eflags+1
.L617:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L618
	orb $1,m_eflags
	jmp .L619
	.p2align 4,,7
.L618:
	andb $254,m_eflags
.L619:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC66,%edx
	andl .LC66+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L620
	orb $64,m_eflags
	jmp .L621
	.p2align 4,,7
.L620:
	andb $191,m_eflags
.L621:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC65,%edx
	andl .LC65+4,%ecx
.L648:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L647
	jmp .L642
	.p2align 4,,7
.L624:
	movl %ecx,%eax
	movl %ebx,%edx
	addl $32768,%eax
	adcl $0,%edx
	jne .L626
	jne .L625
	cmpl $65535,%eax
	jbe .L625
.L626:
	orb $8,m_eflags+1
	jmp .L627
	.p2align 4,,7
.L625:
	andb $247,m_eflags+1
.L627:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L628
	orb $1,m_eflags
	jmp .L629
	.p2align 4,,7
.L628:
	andb $254,m_eflags
.L629:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L630
	orb $64,m_eflags
	jmp .L631
	.p2align 4,,7
.L630:
	andb $191,m_eflags
.L631:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L648
	.p2align 4,,7
.L634:
	movl %ecx,%eax
	movl %ebx,%edx
	addl $128,%eax
	adcl $0,%edx
	jne .L636
	jne .L635
	cmpl $255,%eax
	jbe .L635
.L636:
	orb $8,m_eflags+1
	jmp .L637
	.p2align 4,,7
.L635:
	andb $247,m_eflags+1
.L637:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L638
	orb $1,m_eflags
	jmp .L639
	.p2align 4,,7
.L638:
	andb $254,m_eflags
.L639:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L640
	orb $64,m_eflags
	jmp .L641
	.p2align 4,,7
.L640:
	andb $191,m_eflags
.L641:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L642
.L647:
	orb $128,m_eflags
	jmp .L613
	.p2align 4,,7
.L642:
	andb $127,m_eflags
	jmp .L613
	.p2align 4,,7
.L644:
	addl $-12,%esp
	pushl $.LC67
	call panic
	addl $16,%esp
.L613:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe21:
	.size	 update_flags_ADD,.Lfe21-update_flags_ADD
.section	.rodata
.LC68:
	.string	"update_flags_SUB"
.text
	.align 4
	.type	 update_flags_SUB,@function
update_flags_SUB:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 36(%esp),%ebx
	movl 40(%esp),%ecx
	cmpl $1,%eax
	jne .L650
	movzbl %cl,%ecx
	movzbl %bl,%ebx
	movl %ecx,%edx
	subl %ebx,%edx
	movzbl %dl,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L651
	orb $4,m_eflags
	jmp .L652
	.p2align 4,,7
.L651:
	andb $251,m_eflags
.L652:
	testl %edx,%edx
	jne .L653
	orb $64,m_eflags
	jmp .L654
	.p2align 4,,7
.L653:
	andb $191,m_eflags
.L654:
	cmpl $127,%edx
	jbe .L655
	orb $128,m_eflags
	jmp .L656
	.p2align 4,,7
.L655:
	andb $127,m_eflags
.L656:
	cmpl %ecx,%ebx
	jbe .L657
	orb $1,m_eflags
	jmp .L658
	.p2align 4,,7
.L657:
	andb $254,m_eflags
.L658:
	andl $128,%ecx
	movl %ebx,%eax
	andl $128,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $128,%eax
.L687:
	cmpl %eax,%ecx
	jne .L686
	jmp .L683
	.p2align 4,,7
.L650:
	cmpl $4,%eax
	jne .L662
	movl %ecx,%edx
	subl %ebx,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L663
	orb $4,m_eflags
	jmp .L664
	.p2align 4,,7
.L663:
	andb $251,m_eflags
.L664:
	testl %edx,%edx
	jne .L665
	orb $64,m_eflags
	jmp .L666
	.p2align 4,,7
.L665:
	andb $191,m_eflags
.L666:
	testl %edx,%edx
	jge .L667
	orb $128,m_eflags
	jmp .L668
	.p2align 4,,7
.L667:
	andb $127,m_eflags
.L668:
	cmpl %ecx,%ebx
	jbe .L669
	orb $1,m_eflags
	jmp .L670
	.p2align 4,,7
.L669:
	andb $254,m_eflags
.L670:
	andl $-2147483648,%ecx
	movl %ebx,%eax
	andl $-2147483648,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $-2147483648,%eax
	jmp .L687
	.p2align 4,,7
.L662:
	cmpl $2,%eax
	jne .L674
	movzwl %cx,%ecx
	movzwl %bx,%ebx
	movl %ecx,%edx
	subl %ebx,%edx
	movzwl %dx,%edx
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L675
	orb $4,m_eflags
	jmp .L676
	.p2align 4,,7
.L675:
	andb $251,m_eflags
.L676:
	testl %edx,%edx
	jne .L677
	orb $64,m_eflags
	jmp .L678
	.p2align 4,,7
.L677:
	andb $191,m_eflags
.L678:
	cmpl $32767,%edx
	jbe .L679
	orb $128,m_eflags
	jmp .L680
	.p2align 4,,7
.L679:
	andb $127,m_eflags
.L680:
	cmpl %ecx,%ebx
	jbe .L681
	orb $1,m_eflags
	jmp .L682
	.p2align 4,,7
.L681:
	andb $254,m_eflags
.L682:
	andl $32768,%ecx
	movl %ebx,%eax
	andl $32768,%eax
	cmpl %eax,%ecx
	je .L683
	movl %edx,%eax
	andl $32768,%eax
	cmpl %eax,%ecx
	je .L683
.L686:
	orb $8,m_eflags+1
	jmp .L661
	.p2align 4,,7
.L683:
	andb $247,m_eflags+1
	jmp .L661
	.p2align 4,,7
.L674:
	addl $-12,%esp
	pushl $.LC68
	call panic
	addl $16,%esp
.L661:
	popl %ebx
	addl $24,%esp
	ret
.Lfe22:
	.size	 update_flags_SUB,.Lfe22-update_flags_SUB
.section	.rodata
.LC71:
	.string	"update_flags_ADC"
	.align 4
.LC69:
	.long -2147483648
	.long 0
	.align 4
.LC70:
	.long -1
	.long 0
.text
	.align 4
	.type	 update_flags_ADC,@function
update_flags_ADC:
	subl $28,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 52(%esp),%ebp
	movl 56(%esp),%ecx
	movb m_eflags,%al
	andb $1,%al
	movb %al,31(%esp)
	movl %ebp,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	addl %ecx,%esi
	adcl $0,%edi
	cmpb $0,31(%esp)
	je .L689
	addl $1,%esi
	adcl $0,%edi
.L689:
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	addl %ecx,%eax
	adcl %ebx,%edx
	cmpb $0,31(%esp)
	je .L691
	addl $1,%eax
	adcl $0,%edx
.L691:
	cmpl $2,48(%esp)
	je .L703
	jg .L725
	cmpl $1,48(%esp)
	je .L713
	jmp .L723
	.p2align 4,,7
.L725:
	cmpl $4,48(%esp)
	jne .L723
	addl .LC69,%eax
	adcl .LC69+4,%edx
	je .L694
	orb $8,m_eflags+1
	jmp .L696
	.p2align 4,,7
.L694:
	andb $247,m_eflags+1
.L696:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L697
	orb $1,m_eflags
	jmp .L698
	.p2align 4,,7
.L697:
	andb $254,m_eflags
.L698:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC70,%edx
	andl .LC70+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L699
	orb $64,m_eflags
	jmp .L700
	.p2align 4,,7
.L699:
	andb $191,m_eflags
.L700:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC69,%edx
	andl .LC69+4,%ecx
.L727:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L726
	jmp .L721
	.p2align 4,,7
.L703:
	addl $32768,%eax
	adcl $0,%edx
	jne .L705
	jne .L704
	cmpl $65535,%eax
	jbe .L704
.L705:
	orb $8,m_eflags+1
	jmp .L706
	.p2align 4,,7
.L704:
	andb $247,m_eflags+1
.L706:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L707
	orb $1,m_eflags
	jmp .L708
	.p2align 4,,7
.L707:
	andb $254,m_eflags
.L708:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L709
	orb $64,m_eflags
	jmp .L710
	.p2align 4,,7
.L709:
	andb $191,m_eflags
.L710:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L727
	.p2align 4,,7
.L713:
	addl $128,%eax
	adcl $0,%edx
	jne .L715
	jne .L714
	cmpl $255,%eax
	jbe .L714
.L715:
	orb $8,m_eflags+1
	jmp .L716
	.p2align 4,,7
.L714:
	andb $247,m_eflags+1
.L716:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L717
	orb $1,m_eflags
	jmp .L718
	.p2align 4,,7
.L717:
	andb $254,m_eflags
.L718:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L719
	orb $64,m_eflags
	jmp .L720
	.p2align 4,,7
.L719:
	andb $191,m_eflags
.L720:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L721
.L726:
	orb $128,m_eflags
	jmp .L692
	.p2align 4,,7
.L721:
	andb $127,m_eflags
	jmp .L692
	.p2align 4,,7
.L723:
	addl $-12,%esp
	pushl $.LC71
	call panic
	addl $16,%esp
.L692:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $28,%esp
	ret
.Lfe23:
	.size	 update_flags_ADC,.Lfe23-update_flags_ADC
.section	.rodata
.LC74:
	.string	"update_flags_SBB"
	.align 4
.LC72:
	.long -2147483648
	.long 0
	.align 4
.LC73:
	.long -1
	.long 0
.text
	.align 4
	.type	 update_flags_SBB,@function
update_flags_SBB:
	subl $28,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 52(%esp),%ebp
	movl 56(%esp),%ecx
	movb m_eflags,%al
	andb $1,%al
	movb %al,31(%esp)
	movl %ecx,%eax
	xorl %edx,%edx
	movl %eax,%esi
	movl %edx,%edi
	subl %ebp,%esi
	sbbl $0,%edi
	cmpb $0,31(%esp)
	je .L729
	addl $-1,%esi
	adcl $-1,%edi
.L729:
	movl %ecx,%ebx
	sarl $31,%ebx
	movl %ebp,%eax
	cltd
	subl %eax,%ecx
	sbbl %edx,%ebx
	movl %ecx,%eax
	movl %ebx,%edx
	cmpb $0,31(%esp)
	je .L731
	addl $-1,%eax
	adcl $-1,%edx
.L731:
	cmpl $2,48(%esp)
	je .L743
	jg .L765
	cmpl $1,48(%esp)
	je .L753
	jmp .L763
	.p2align 4,,7
.L765:
	cmpl $4,48(%esp)
	jne .L763
	addl .LC72,%eax
	adcl .LC72+4,%edx
	je .L734
	orb $8,m_eflags+1
	jmp .L736
	.p2align 4,,7
.L734:
	andb $247,m_eflags+1
.L736:
	movl %edi,%edx
	xorl %ecx,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L737
	orb $1,m_eflags
	jmp .L738
	.p2align 4,,7
.L737:
	andb $254,m_eflags
.L738:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC73,%edx
	andl .LC73+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L739
	orb $64,m_eflags
	jmp .L740
	.p2align 4,,7
.L739:
	andb $191,m_eflags
.L740:
	movl %esi,%edx
	movl %edi,%ecx
	andl .LC72,%edx
	andl .LC72+4,%ecx
.L767:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L766
	jmp .L761
	.p2align 4,,7
.L743:
	addl $32768,%eax
	adcl $0,%edx
	jne .L745
	jne .L744
	cmpl $65535,%eax
	jbe .L744
.L745:
	orb $8,m_eflags+1
	jmp .L746
	.p2align 4,,7
.L744:
	andb $247,m_eflags+1
.L746:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $16,%ecx,%edx
	shrl $16,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L747
	orb $1,m_eflags
	jmp .L748
	.p2align 4,,7
.L747:
	andb $254,m_eflags
.L748:
	movl %esi,%edx
	movl %edi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L749
	orb $64,m_eflags
	jmp .L750
	.p2align 4,,7
.L749:
	andb $191,m_eflags
.L750:
	movl %esi,%edx
	movl %edi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L767
	.p2align 4,,7
.L753:
	addl $128,%eax
	adcl $0,%edx
	jne .L755
	jne .L754
	cmpl $255,%eax
	jbe .L754
.L755:
	orb $8,m_eflags+1
	jmp .L756
	.p2align 4,,7
.L754:
	andb $247,m_eflags+1
.L756:
	movl %esi,%edx
	movl %edi,%ecx
	shrdl $8,%ecx,%edx
	shrl $8,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L757
	orb $1,m_eflags
	jmp .L758
	.p2align 4,,7
.L757:
	andb $254,m_eflags
.L758:
	movl %esi,%edx
	movl %edi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L759
	orb $64,m_eflags
	jmp .L760
	.p2align 4,,7
.L759:
	andb $191,m_eflags
.L760:
	movl %esi,%edx
	movl %edi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L761
.L766:
	orb $128,m_eflags
	jmp .L732
	.p2align 4,,7
.L761:
	andb $127,m_eflags
	jmp .L732
	.p2align 4,,7
.L763:
	addl $-12,%esp
	pushl $.LC74
	call panic
	addl $16,%esp
.L732:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $28,%esp
	ret
.Lfe24:
	.size	 update_flags_SBB,.Lfe24-update_flags_SBB
	.align 4
	.type	 update_flags_NEG,@function
update_flags_NEG:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	addl $-4,%esp
	pushl $0
	cmpl $2,%ebx
	je .L769
	ja .L770
	cmpl $1,%ebx
	je .L771
	jmp .L772
	.p2align 4,,7
.L770:
	cmpl $4,%ebx
	jne .L772
	movl %esi,%eax
	jmp .L774
	.p2align 4,,7
.L769:
	addl $-12,%esp
	pushl %esi
	call extend_s_16to32
	addl $16,%esp
	jmp .L774
	.p2align 4,,7
.L771:
	movl %esi,%edx
	movsbl %dl,%eax
	jmp .L774
	.p2align 4,,7
.L772:
	addl $-12,%esp
	pushl $.LC59
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L774:
	pushl %eax
	pushl %ebx
	call update_flags_SUB
	addl $16,%esp
	testl %esi,%esi
	je .L777
	orb $1,m_eflags
	jmp .L778
	.p2align 4,,7
.L777:
	andb $254,m_eflags
.L778:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe25:
	.size	 update_flags_NEG,.Lfe25-update_flags_NEG
.section	.rodata
	.align 32
.LC75:
	.string	"SET_OSZ_INCDEC: dst=%u(%d)   sres=%lld   ures=%llu\n"
.LC78:
	.string	"update_flags_INC"
	.align 4
.LC76:
	.long -2147483648
	.long 0
	.align 4
.LC77:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_INC
	.type	 update_flags_INC,@function
update_flags_INC:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	movl $1,%ebx
	movl $0,%esi
	addl %eax,%ebx
	adcl $0,%esi
	cltd
	addl $1,%eax
	adcl $0,%edx
	cmpl $2,%ecx
	je .L790
	jg .L808
	cmpl $1,%ecx
	je .L798
	jmp .L806
	.p2align 4,,7
.L808:
	cmpl $4,%ecx
	jne .L806
	addl .LC76,%eax
	adcl .LC76+4,%edx
	je .L783
	orb $8,m_eflags+1
	jmp .L785
	.p2align 4,,7
.L783:
	andb $247,m_eflags+1
.L785:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC77,%edx
	andl .LC77+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L786
	orb $64,m_eflags
	jmp .L787
	.p2align 4,,7
.L786:
	andb $191,m_eflags
.L787:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC76,%edx
	andl .LC76+4,%ecx
.L810:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L809
	jmp .L804
	.p2align 4,,7
.L790:
	addl $32768,%eax
	adcl $0,%edx
	jne .L792
	jne .L791
	cmpl $65535,%eax
	jbe .L791
.L792:
	orb $8,m_eflags+1
	jmp .L793
	.p2align 4,,7
.L791:
	andb $247,m_eflags+1
.L793:
	movl %ebx,%edx
	movl %esi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L794
	orb $64,m_eflags
	jmp .L795
	.p2align 4,,7
.L794:
	andb $191,m_eflags
.L795:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L810
	.p2align 4,,7
.L798:
	addl $128,%eax
	adcl $0,%edx
	jne .L800
	jne .L799
	cmpl $255,%eax
	jbe .L799
.L800:
	orb $8,m_eflags+1
	jmp .L801
	.p2align 4,,7
.L799:
	andb $247,m_eflags+1
.L801:
	movl %ebx,%edx
	movl %esi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L802
	orb $64,m_eflags
	jmp .L803
	.p2align 4,,7
.L802:
	andb $191,m_eflags
.L803:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L804
.L809:
	orb $128,m_eflags
	jmp .L781
	.p2align 4,,7
.L804:
	andb $127,m_eflags
	jmp .L781
	.p2align 4,,7
.L806:
	addl $-12,%esp
	pushl $.LC78
	call panic
	addl $16,%esp
.L781:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe26:
	.size	 update_flags_INC,.Lfe26-update_flags_INC
.section	.rodata
.LC81:
	.string	"update_flags_DEC"
	.align 4
.LC79:
	.long -2147483648
	.long 0
	.align 4
.LC80:
	.long -1
	.long 0
.text
	.align 4
.globl update_flags_DEC
	.type	 update_flags_DEC,@function
update_flags_DEC:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	movl $-1,%ebx
	movl $-1,%esi
	addl %eax,%ebx
	adcl $0,%esi
	cltd
	addl $-1,%eax
	adcl $-1,%edx
	cmpl $2,%ecx
	je .L822
	jg .L840
	cmpl $1,%ecx
	je .L830
	jmp .L838
	.p2align 4,,7
.L840:
	cmpl $4,%ecx
	jne .L838
	addl .LC79,%eax
	adcl .LC79+4,%edx
	je .L815
	orb $8,m_eflags+1
	jmp .L817
	.p2align 4,,7
.L815:
	andb $247,m_eflags+1
.L817:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC80,%edx
	andl .LC80+4,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L818
	orb $64,m_eflags
	jmp .L819
	.p2align 4,,7
.L818:
	andb $191,m_eflags
.L819:
	movl %ebx,%edx
	movl %esi,%ecx
	andl .LC79,%edx
	andl .LC79+4,%ecx
.L842:
	movl %edx,%eax
	orl %ecx,%eax
	jne .L841
	jmp .L836
	.p2align 4,,7
.L822:
	addl $32768,%eax
	adcl $0,%edx
	jne .L824
	jne .L823
	cmpl $65535,%eax
	jbe .L823
.L824:
	orb $8,m_eflags+1
	jmp .L825
	.p2align 4,,7
.L823:
	andb $247,m_eflags+1
.L825:
	movl %ebx,%edx
	movl %esi,%ecx
	movzwl %dx,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L826
	orb $64,m_eflags
	jmp .L827
	.p2align 4,,7
.L826:
	andb $191,m_eflags
.L827:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $32768,%edx
	andl $0,%ecx
	jmp .L842
	.p2align 4,,7
.L830:
	addl $128,%eax
	adcl $0,%edx
	jne .L832
	jne .L831
	cmpl $255,%eax
	jbe .L831
.L832:
	orb $8,m_eflags+1
	jmp .L833
	.p2align 4,,7
.L831:
	andb $247,m_eflags+1
.L833:
	movl %ebx,%edx
	movl %esi,%ecx
	movzbl %dl,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	jne .L834
	orb $64,m_eflags
	jmp .L835
	.p2align 4,,7
.L834:
	andb $191,m_eflags
.L835:
	movl %ebx,%edx
	movl %esi,%ecx
	andl $128,%edx
	andl $0,%ecx
	movl %edx,%eax
	orl %ecx,%eax
	je .L836
.L841:
	orb $128,m_eflags
	jmp .L813
	.p2align 4,,7
.L836:
	andb $127,m_eflags
	jmp .L813
	.p2align 4,,7
.L838:
	addl $-12,%esp
	pushl $.LC81
	call panic
	addl $16,%esp
.L813:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe27:
	.size	 update_flags_DEC,.Lfe27-update_flags_DEC
.section	.rodata
	.align 32
.LC82:
	.string	"SET_OCSZP_LOGICAL: src=0x%x   dst=0x%x   ures=0x%x\n"
.LC83:
	.string	"SET_SIGN_FLAG_FROM"
.text
	.align 4
.globl update_flags_AND
	.type	 update_flags_AND,@function
update_flags_AND:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	andl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L845
	orb $4,m_eflags
	jmp .L846
	.p2align 4,,7
.L845:
	andb $251,m_eflags
.L846:
	testl %edx,%edx
	jne .L847
	orb $64,m_eflags
	jmp .L848
	.p2align 4,,7
.L847:
	andb $191,m_eflags
.L848:
	cmpl $2,%ecx
	je .L853
	jg .L861
	cmpl $1,%ecx
	je .L856
	jmp .L859
	.p2align 4,,7
.L861:
	cmpl $4,%ecx
	jne .L859
	testl %edx,%edx
	jl .L862
	jmp .L857
	.p2align 4,,7
.L853:
	testw %dx,%dx
	jl .L862
	jmp .L857
	.p2align 4,,7
.L856:
	testb %dl,%dl
	jge .L857
.L862:
	orb $128,m_eflags
	jmp .L849
	.p2align 4,,7
.L857:
	andb $127,m_eflags
	jmp .L849
	.p2align 4,,7
.L859:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L849:
	addl $12,%esp
	ret
.Lfe28:
	.size	 update_flags_AND,.Lfe28-update_flags_AND
	.align 4
.globl update_flags_OR
	.type	 update_flags_OR,@function
update_flags_OR:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	orl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L865
	orb $4,m_eflags
	jmp .L866
	.p2align 4,,7
.L865:
	andb $251,m_eflags
.L866:
	testl %edx,%edx
	jne .L867
	orb $64,m_eflags
	jmp .L868
	.p2align 4,,7
.L867:
	andb $191,m_eflags
.L868:
	cmpl $2,%ecx
	je .L873
	jg .L881
	cmpl $1,%ecx
	je .L876
	jmp .L879
	.p2align 4,,7
.L881:
	cmpl $4,%ecx
	jne .L879
	testl %edx,%edx
	jl .L882
	jmp .L877
	.p2align 4,,7
.L873:
	testw %dx,%dx
	jl .L882
	jmp .L877
	.p2align 4,,7
.L876:
	testb %dl,%dl
	jge .L877
.L882:
	orb $128,m_eflags
	jmp .L869
	.p2align 4,,7
.L877:
	andb $127,m_eflags
	jmp .L869
	.p2align 4,,7
.L879:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L869:
	addl $12,%esp
	ret
.Lfe29:
	.size	 update_flags_OR,.Lfe29-update_flags_OR
	.align 4
.globl update_flags_XOR
	.type	 update_flags_XOR,@function
update_flags_XOR:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	xorl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L885
	orb $4,m_eflags
	jmp .L886
	.p2align 4,,7
.L885:
	andb $251,m_eflags
.L886:
	testl %edx,%edx
	jne .L887
	orb $64,m_eflags
	jmp .L888
	.p2align 4,,7
.L887:
	andb $191,m_eflags
.L888:
	cmpl $2,%ecx
	je .L893
	jg .L901
	cmpl $1,%ecx
	je .L896
	jmp .L899
	.p2align 4,,7
.L901:
	cmpl $4,%ecx
	jne .L899
	testl %edx,%edx
	jl .L902
	jmp .L897
	.p2align 4,,7
.L893:
	testw %dx,%dx
	jl .L902
	jmp .L897
	.p2align 4,,7
.L896:
	testb %dl,%dl
	jge .L897
.L902:
	orb $128,m_eflags
	jmp .L889
	.p2align 4,,7
.L897:
	andb $127,m_eflags
	jmp .L889
	.p2align 4,,7
.L899:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L889:
	addl $12,%esp
	ret
.Lfe30:
	.size	 update_flags_XOR,.Lfe30-update_flags_XOR
	.align 4
.globl update_flags_TEST
	.type	 update_flags_TEST,@function
update_flags_TEST:
	subl $12,%esp
	movl 16(%esp),%ecx
	movl 24(%esp),%edx
	andl 20(%esp),%edx
	andb $247,m_eflags+1
	andb $254,m_eflags
	movl %edx,%eax
	andl $255,%eax
	cmpb $0,parityIsEven(%eax)
	je .L905
	orb $4,m_eflags
	jmp .L906
	.p2align 4,,7
.L905:
	andb $251,m_eflags
.L906:
	testl %edx,%edx
	jne .L907
	orb $64,m_eflags
	jmp .L908
	.p2align 4,,7
.L907:
	andb $191,m_eflags
.L908:
	cmpl $2,%ecx
	je .L913
	jg .L921
	cmpl $1,%ecx
	je .L916
	jmp .L919
	.p2align 4,,7
.L921:
	cmpl $4,%ecx
	jne .L919
	testl %edx,%edx
	jl .L922
	jmp .L917
	.p2align 4,,7
.L913:
	testw %dx,%dx
	jl .L922
	jmp .L917
	.p2align 4,,7
.L916:
	testb %dl,%dl
	jge .L917
.L922:
	orb $128,m_eflags
	jmp .L909
	.p2align 4,,7
.L917:
	andb $127,m_eflags
	jmp .L909
	.p2align 4,,7
.L919:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L909:
	addl $12,%esp
	ret
.Lfe31:
	.size	 update_flags_TEST,.Lfe31-update_flags_TEST
.section	.rodata
.LC84:
	.string	"update_flags_IMUL"
.text
	.align 4
.globl update_flags_IMUL
	.type	 update_flags_IMUL,@function
update_flags_IMUL:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	imull 40(%esp)
	movb $1,%bl
	cmpl $2,%ecx
	je .L928
	jg .L936
	cmpl $1,%ecx
	je .L931
	jmp .L934
	.p2align 4,,7
.L936:
	cmpl $4,%ecx
	jne .L934
	movl %edx,%eax
	xorl %edx,%edx
	movl %eax,%ecx
	movb $0,%bl
	testl %eax,%eax
	je .L933
	cmpl $-1,%ecx
	jne .L924
	jmp .L933
	.p2align 4,,7
.L928:
	shrdl $16,%edx,%eax
	shrl $16,%edx
	movl %eax,%ecx
	movzwl %cx,%ecx
	movb $0,%bl
	testl %ecx,%ecx
	je .L933
	cmpl $65535,%ecx
	jne .L924
	jmp .L933
	.p2align 4,,7
.L931:
	shrdl $8,%edx,%eax
	shrl $8,%edx
	movl %eax,%ecx
	movzbl %cl,%ecx
	movb $0,%bl
	testl %ecx,%ecx
	je .L933
	cmpl $255,%ecx
	jne .L924
.L933:
	movb $1,%bl
	jmp .L937
	.p2align 4,,7
.L934:
	addl $-12,%esp
	pushl $.LC84
	call panic
	addl $16,%esp
.L924:
	testb %bl,%bl
	jne .L937
	orb $1,m_eflags
	jmp .L938
	.p2align 4,,7
.L937:
	andb $254,m_eflags
.L938:
	testb %bl,%bl
	jne .L939
	orb $8,m_eflags+1
	jmp .L940
	.p2align 4,,7
.L939:
	andb $247,m_eflags+1
.L940:
	popl %ebx
	addl $24,%esp
	ret
.Lfe32:
	.size	 update_flags_IMUL,.Lfe32-update_flags_IMUL
.section	.rodata
.LC85:
	.string	"update_flags_MUL"
.text
	.align 4
.globl update_flags_MUL
	.type	 update_flags_MUL,@function
update_flags_MUL:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ecx
	movl 36(%esp),%eax
	mull 40(%esp)
	movl %eax,%ebx
	movl %edx,%esi
	cmpl $2,%ecx
	je .L944
	jg .L948
	cmpl $1,%ecx
	je .L945
	jmp .L946
	.p2align 4,,7
.L948:
	cmpl $4,%ecx
	jne .L946
	movl %esi,%ebx
	xorl %esi,%esi
	jmp .L942
	.p2align 4,,7
.L944:
	shrdl $16,%esi,%ebx
	shrl $16,%esi
	jmp .L942
	.p2align 4,,7
.L945:
	shrdl $8,%esi,%ebx
	shrl $8,%esi
	jmp .L942
	.p2align 4,,7
.L946:
	addl $-12,%esp
	pushl $.LC85
	call panic
	addl $16,%esp
.L942:
	movl %ebx,%eax
	orl %esi,%eax
	je .L949
	orb $1,m_eflags
	jmp .L950
	.p2align 4,,7
.L949:
	andb $254,m_eflags
.L950:
	movl %ebx,%eax
	orl %esi,%eax
	je .L951
	orb $8,m_eflags+1
	jmp .L952
	.p2align 4,,7
.L951:
	andb $247,m_eflags+1
.L952:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe33:
	.size	 update_flags_MUL,.Lfe33-update_flags_MUL
	.align 4
.globl update_flags_SHL
	.type	 update_flags_SHL,@function
update_flags_SHL:
	subl $12,%esp
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%edi
	movl 40(%esp),%ebp
	movl %ebp,%ebx
	movl %edi,%ecx
	sall %cl,%ebx
	testl %edi,%edi
	je .L954
	addl $-8,%esp
	leal -1(%edi),%ecx
	movl %ebp,%eax
	sall %cl,%eax
	pushl %eax
	pushl %esi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L955
	orb $1,m_eflags
	jmp .L956
	.p2align 4,,7
.L955:
	andb $254,m_eflags
.L956:
	testl %ebx,%ebx
	jne .L957
	orb $64,m_eflags
	jmp .L958
	.p2align 4,,7
.L957:
	andb $191,m_eflags
.L958:
	cmpl $2,%esi
	je .L963
	jg .L971
	cmpl $1,%esi
	je .L966
	jmp .L969
	.p2align 4,,7
.L971:
	cmpl $4,%esi
	jne .L969
	testl %ebx,%ebx
	jl .L975
	jmp .L967
	.p2align 4,,7
.L963:
	testw %bx,%bx
	jl .L975
	jmp .L967
	.p2align 4,,7
.L966:
	testb %bl,%bl
	jge .L967
.L975:
	orb $128,m_eflags
	jmp .L959
	.p2align 4,,7
.L967:
	andb $127,m_eflags
	jmp .L959
	.p2align 4,,7
.L969:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L959:
	cmpl $1,%edi
	jne .L954
	addl $-8,%esp
	pushl %ebp
	pushl %esi
	call topBitOf
	movl %eax,%ebx
	addl $-8,%esp
	leal (%ebp,%ebp),%eax
	pushl %eax
	pushl %esi
	call topBitOf
	addl $32,%esp
	cmpl %eax,%ebx
	je .L973
	orb $8,m_eflags+1
	jmp .L954
	.p2align 4,,7
.L973:
	andb $247,m_eflags+1
.L954:
	popl %ebx
	popl %esi
	popl %edi
	popl %ebp
	addl $12,%esp
	ret
.Lfe34:
	.size	 update_flags_SHL,.Lfe34-update_flags_SHL
	.align 4
.globl update_flags_SHR
	.type	 update_flags_SHR,@function
update_flags_SHR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	movl 40(%esp),%edi
	movl %edi,%edx
	movl %ebx,%ecx
	shrl %cl,%edx
	testl %ebx,%ebx
	je .L977
	leal -1(%ebx),%eax
	btl %eax,%edi
	jnc .L978
	orb $1,m_eflags
	jmp .L979
	.p2align 4,,7
.L978:
	andb $254,m_eflags
.L979:
	testl %edx,%edx
	jne .L980
	orb $64,m_eflags
	jmp .L981
	.p2align 4,,7
.L980:
	andb $191,m_eflags
.L981:
	cmpl $2,%esi
	je .L986
	jg .L994
	cmpl $1,%esi
	je .L989
	jmp .L992
	.p2align 4,,7
.L994:
	cmpl $4,%esi
	jne .L992
	testl %edx,%edx
	jl .L998
	jmp .L990
	.p2align 4,,7
.L986:
	testw %dx,%dx
	jl .L998
	jmp .L990
	.p2align 4,,7
.L989:
	testb %dl,%dl
	jge .L990
.L998:
	orb $128,m_eflags
	jmp .L982
	.p2align 4,,7
.L990:
	andb $127,m_eflags
	jmp .L982
	.p2align 4,,7
.L992:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L982:
	cmpl $1,%ebx
	jne .L977
	addl $-8,%esp
	pushl %edi
	pushl %esi
	call topBitOf
	addl $16,%esp
	testl %eax,%eax
	je .L996
	orb $8,m_eflags+1
	jmp .L977
	.p2align 4,,7
.L996:
	andb $247,m_eflags+1
.L977:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe35:
	.size	 update_flags_SHR,.Lfe35-update_flags_SHR
	.align 4
	.type	 update_flags_SAR,@function
update_flags_SAR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	movl 40(%esp),%edi
	movl %edi,%edx
	movl %esi,%ecx
	shrl %cl,%edx
	testl %esi,%esi
	je .L1000
	leal -1(%esi),%eax
	btl %eax,%edi
	jnc .L1001
	orb $1,m_eflags
	jmp .L1002
	.p2align 4,,7
.L1001:
	andb $254,m_eflags
.L1002:
	testl %edx,%edx
	jne .L1003
	orb $64,m_eflags
	jmp .L1004
	.p2align 4,,7
.L1003:
	andb $191,m_eflags
.L1004:
	cmpl $2,%ebx
	je .L1009
	jg .L1017
	cmpl $1,%ebx
	je .L1012
	jmp .L1015
	.p2align 4,,7
.L1017:
	cmpl $4,%ebx
	jne .L1015
	testl %edx,%edx
	jl .L1021
	jmp .L1013
	.p2align 4,,7
.L1009:
	testw %dx,%dx
	jl .L1021
	jmp .L1013
	.p2align 4,,7
.L1012:
	testb %dl,%dl
	jge .L1013
.L1021:
	orb $128,m_eflags
	jmp .L1005
	.p2align 4,,7
.L1013:
	andb $127,m_eflags
	jmp .L1005
	.p2align 4,,7
.L1015:
	addl $-12,%esp
	pushl $.LC83
	call panic
	addl $16,%esp
.L1005:
	cmpl $1,%esi
	jne .L1000
	andb $247,m_eflags+1
.L1000:
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe36:
	.size	 update_flags_SAR,.Lfe36-update_flags_SAR
	.align 4
	.type	 update_flags_ROL,@function
