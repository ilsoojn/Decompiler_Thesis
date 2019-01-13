
testfiles/simpleblock:     file format elf64-x86-64


Disassembly of section .interp:

0000000000400238 <.interp>:
  400238:	2f                   	(bad)
  400239:	6c                   	insb   (%dx),%es:(%rdi)
  40023a:	69 62 36 34 2f 6c 64 	imul   $0x646c2f34,0x36(%rdx),%esp
  400241:	2d 6c 69 6e 75       	sub    $0x756e696c,%eax
  400246:	78 2d                	js     400275 <_init-0xfb>
  400248:	78 38                	js     400282 <_init-0xee>
  40024a:	36 2d 36 34 2e 73    	ss sub $0x732e3436,%eax
  400250:	6f                   	outsl  %ds:(%rsi),(%dx)
  400251:	2e 32 00             	xor    %cs:(%rax),%al

Disassembly of section .note.ABI-tag:

0000000000400254 <.note.ABI-tag>:
  400254:	04 00                	add    $0x0,%al
  400256:	00 00                	add    %al,(%rax)
  400258:	10 00                	adc    %al,(%rax)
  40025a:	00 00                	add    %al,(%rax)
  40025c:	01 00                	add    %eax,(%rax)
  40025e:	00 00                	add    %al,(%rax)
  400260:	47                   	rex.RXB
  400261:	4e 55                	rex.WRX push %rbp
  400263:	00 00                	add    %al,(%rax)
  400265:	00 00                	add    %al,(%rax)
  400267:	00 03                	add    %al,(%rbx)
  400269:	00 00                	add    %al,(%rax)
  40026b:	00 02                	add    %al,(%rdx)
  40026d:	00 00                	add    %al,(%rax)
  40026f:	00 00                	add    %al,(%rax)
  400271:	00 00                	add    %al,(%rax)
	...

Disassembly of section .gnu.hash:

0000000000400278 <.gnu.hash>:
  400278:	01 00                	add    %eax,(%rax)
  40027a:	00 00                	add    %al,(%rax)
  40027c:	01 00                	add    %eax,(%rax)
  40027e:	00 00                	add    %al,(%rax)
  400280:	01 00                	add    %eax,(%rax)
	...

Disassembly of section .dynsym:

0000000000400298 <.dynsym>:
	...
  4002b0:	0b 00                	or     (%rax),%eax
  4002b2:	00 00                	add    %al,(%rax)
  4002b4:	12 00                	adc    (%rax),%al
	...
  4002c6:	00 00                	add    %al,(%rax)
  4002c8:	29 00                	sub    %eax,(%rax)
  4002ca:	00 00                	add    %al,(%rax)
  4002cc:	20 00                	and    %al,(%rax)
	...

Disassembly of section .dynstr:

00000000004002e0 <.dynstr>:
  4002e0:	00 6c 69 62          	add    %ch,0x62(%rcx,%rbp,2)
  4002e4:	63 2e                	movslq (%rsi),%ebp
  4002e6:	73 6f                	jae    400357 <_init-0x19>
  4002e8:	2e 36 00 5f 5f       	cs add %bl,%ss:0x5f(%rdi)
  4002ed:	6c                   	insb   (%dx),%es:(%rdi)
  4002ee:	69 62 63 5f 73 74 61 	imul   $0x6174735f,0x63(%rdx),%esp
  4002f5:	72 74                	jb     40036b <_init-0x5>
  4002f7:	5f                   	pop    %rdi
  4002f8:	6d                   	insl   (%dx),%es:(%rdi)
  4002f9:	61                   	(bad)
  4002fa:	69 6e 00 47 4c 49 42 	imul   $0x42494c47,0x0(%rsi),%ebp
  400301:	43 5f                	rex.XB pop %r15
  400303:	32 2e                	xor    (%rsi),%ch
  400305:	32 2e                	xor    (%rsi),%ch
  400307:	35 00 5f 5f 67       	xor    $0x675f5f00,%eax
  40030c:	6d                   	insl   (%dx),%es:(%rdi)
  40030d:	6f                   	outsl  %ds:(%rsi),(%dx)
  40030e:	6e                   	outsb  %ds:(%rsi),(%dx)
  40030f:	5f                   	pop    %rdi
  400310:	73 74                	jae    400386 <_init+0x16>
  400312:	61                   	(bad)
  400313:	72 74                	jb     400389 <_init+0x19>
  400315:	5f                   	pop    %rdi
  400316:	5f                   	pop    %rdi
	...

Disassembly of section .gnu.version:

0000000000400318 <.gnu.version>:
  400318:	00 00                	add    %al,(%rax)
  40031a:	02 00                	add    (%rax),%al
	...

Disassembly of section .gnu.version_r:

0000000000400320 <.gnu.version_r>:
  400320:	01 00                	add    %eax,(%rax)
  400322:	01 00                	add    %eax,(%rax)
  400324:	01 00                	add    %eax,(%rax)
  400326:	00 00                	add    %al,(%rax)
  400328:	10 00                	adc    %al,(%rax)
  40032a:	00 00                	add    %al,(%rax)
  40032c:	00 00                	add    %al,(%rax)
  40032e:	00 00                	add    %al,(%rax)
  400330:	75 1a                	jne    40034c <_init-0x24>
  400332:	69 09 00 00 02 00    	imul   $0x20000,(%rcx),%ecx
  400338:	1d 00 00 00 00       	sbb    $0x0,%eax
  40033d:	00 00                	add    %al,(%rax)
	...

Disassembly of section .rela.dyn:

0000000000400340 <.rela.dyn>:
  400340:	f0 0f 60 00          	lock punpcklbw (%rax),%mm0
  400344:	00 00                	add    %al,(%rax)
  400346:	00 00                	add    %al,(%rax)
  400348:	06                   	(bad)
  400349:	00 00                	add    %al,(%rax)
  40034b:	00 01                	add    %al,(%rcx)
	...
  400355:	00 00                	add    %al,(%rax)
  400357:	00 f8                	add    %bh,%al
  400359:	0f 60 00             	punpcklbw (%rax),%mm0
  40035c:	00 00                	add    %al,(%rax)
  40035e:	00 00                	add    %al,(%rax)
  400360:	06                   	(bad)
  400361:	00 00                	add    %al,(%rax)
  400363:	00 02                	add    %al,(%rdx)
	...

Disassembly of section .init:

0000000000400370 <_init>:
  400370:	48 83 ec 08          	sub    $0x8,%rsp
  400374:	48 8b 05 7d 0c 20 00 	mov    0x200c7d(%rip),%rax        # 600ff8 <__gmon_start__>
  40037b:	48 85 c0             	test   %rax,%rax
  40037e:	74 02                	je     400382 <_init+0x12>
  400380:	ff d0                	callq  *%rax
  400382:	48 83 c4 08          	add    $0x8,%rsp
  400386:	c3                   	retq

Disassembly of section .text:

0000000000400390 <_start>:
# rsp = n
  400390:	31 ed                	xor    %ebp,%ebp
  400392:	49 89 d1             	mov    %rdx,%r9         # data?
  400395:	5e                   	pop    %rsi             # rsi := argc & rsp := argv (rsp <- n+(sizeof RSI))
  400396:	48 89 e2             	mov    %rsp,%rdx        # rdx := rsp
  400399:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp # move stackpointer? why? (rsp <- n)
  40039d:	50                   	push   %rax
  40039e:	54                   	push   %rsp
  40039f:	49 c7 c0 40 05 40 00 	mov    $0x400540,%r8    # %r8 := <__libc_csu_fini>
  4003a6:	48 c7 c1 d0 04 40 00 	mov    $0x4004d0,%rcx   # %rcx := <__libc_csu_init>
  4003ad:	48 c7 c7 80 04 40 00 	mov    $0x400480,%rdi   # %rdi := <main>
  4003b4:	ff 15 36 0c 20 00    	callq  *0x200c36(%rip)        # 600ff0 <__libc_start_main@GLIBC_2.2.5>
  4003ba:	f4                   	hlt
  4003bb:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

00000000004003c0 <_dl_relocate_static_pie>:
  4003c0:	f3 c3                	repz retq
  4003c2:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4003c9:	00 00 00
  4003cc:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004003d0 <deregister_tm_clones>:
  4003d0:	55                   	push   %rbp
  4003d1:	b8 28 10 60 00       	mov    $0x601028,%eax
  4003d6:	48 3d 28 10 60 00    	cmp    $0x601028,%rax
  4003dc:	48 89 e5             	mov    %rsp,%rbp
  4003df:	74 17                	je     4003f8 <deregister_tm_clones+0x28>
  4003e1:	b8 00 00 00 00       	mov    $0x0,%eax
  4003e6:	48 85 c0             	test   %rax,%rax
  4003e9:	74 0d                	je     4003f8 <deregister_tm_clones+0x28>
  4003eb:	5d                   	pop    %rbp
  4003ec:	bf 28 10 60 00       	mov    $0x601028,%edi
  4003f1:	ff e0                	jmpq   *%rax
  4003f3:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  4003f8:	5d                   	pop    %rbp
  4003f9:	c3                   	retq
  4003fa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400400 <register_tm_clones>:
  400400:	be 28 10 60 00       	mov    $0x601028,%esi
  400405:	55                   	push   %rbp
  400406:	48 81 ee 28 10 60 00 	sub    $0x601028,%rsi
  40040d:	48 89 e5             	mov    %rsp,%rbp
  400410:	48 c1 fe 03          	sar    $0x3,%rsi
  400414:	48 89 f0             	mov    %rsi,%rax
  400417:	48 c1 e8 3f          	shr    $0x3f,%rax
  40041b:	48 01 c6             	add    %rax,%rsi
  40041e:	48 d1 fe             	sar    %rsi
  400421:	74 15                	je     400438 <register_tm_clones+0x38>
  400423:	b8 00 00 00 00       	mov    $0x0,%eax
  400428:	48 85 c0             	test   %rax,%rax
  40042b:	74 0b                	je     400438 <register_tm_clones+0x38>
  40042d:	5d                   	pop    %rbp
  40042e:	bf 28 10 60 00       	mov    $0x601028,%edi
  400433:	ff e0                	jmpq   *%rax
  400435:	0f 1f 00             	nopl   (%rax)
  400438:	5d                   	pop    %rbp
  400439:	c3                   	retq
  40043a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400440 <__do_global_dtors_aux>:
  400440:	80 3d e1 0b 20 00 00 	cmpb   $0x0,0x200be1(%rip)        # 601028 <__TMC_END__>
  400447:	75 17                	jne    400460 <__do_global_dtors_aux+0x20>
  400449:	55                   	push   %rbp
  40044a:	48 89 e5             	mov    %rsp,%rbp
  40044d:	e8 7e ff ff ff       	callq  4003d0 <deregister_tm_clones>
  400452:	c6 05 cf 0b 20 00 01 	movb   $0x1,0x200bcf(%rip)        # 601028 <__TMC_END__>
  400459:	5d                   	pop    %rbp
  40045a:	c3                   	retq
  40045b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400460:	f3 c3                	repz retq
  400462:	0f 1f 40 00          	nopl   0x0(%rax)
  400466:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40046d:	00 00 00

0000000000400470 <frame_dummy>:
  400470:	55                   	push   %rbp
  400471:	48 89 e5             	mov    %rsp,%rbp
  400474:	5d                   	pop    %rbp
  400475:	eb 89                	jmp    400400 <register_tm_clones>
  400477:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40047e:	00 00

0000000000400480 <main>:
  400480:	55                   	push   %rbp
  400481:	48 89 e5             	mov    %rsp,%rbp
  400484:	31 c0                	xor    %eax,%eax
  400486:	f2 0f 10 05 ca 00 00 	movsd  0xca(%rip),%xmm0        # 400558 <_IO_stdin_used+0x8>
  40048d:	00
  40048e:	f2 0f 10 0d ca 00 00 	movsd  0xca(%rip),%xmm1        # 400560 <_IO_stdin_used+0x10>
  400495:	00
  400496:	c7 45 fc 00 00 00 00 	movl   $0x0,-0x4(%rbp)
  40049d:	c7 45 f8 0a 00 00 00 	movl   $0xa,-0x8(%rbp)
  4004a4:	f2 0f 11 4d f0       	movsd  %xmm1,-0x10(%rbp)
  4004a9:	f2 0f 11 45 e8       	movsd  %xmm0,-0x18(%rbp)
  4004ae:	8b 4d f8             	mov    -0x8(%rbp),%ecx
  4004b1:	f2 0f 2a c1          	cvtsi2sd %ecx,%xmm0
  4004b5:	f2 0f 59 45 e8       	mulsd  -0x18(%rbp),%xmm0
  4004ba:	f2 0f 58 45 f0       	addsd  -0x10(%rbp),%xmm0
  4004bf:	f2 0f 11 45 e0       	movsd  %xmm0,-0x20(%rbp)
  4004c4:	5d                   	pop    %rbp
  4004c5:	c3                   	retq
  4004c6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4004cd:	00 00 00

00000000004004d0 <__libc_csu_init>:
  4004d0:	41 57                	push   %r15
  4004d2:	41 56                	push   %r14
  4004d4:	49 89 d7             	mov    %rdx,%r15
  4004d7:	41 55                	push   %r13
  4004d9:	41 54                	push   %r12
  4004db:	4c 8d 25 6e 09 20 00 	lea    0x20096e(%rip),%r12        # 600e50 <__frame_dummy_init_array_entry>
  4004e2:	55                   	push   %rbp
  4004e3:	48 8d 2d 6e 09 20 00 	lea    0x20096e(%rip),%rbp        # 600e58 <__init_array_end>
  4004ea:	53                   	push   %rbx
  4004eb:	41 89 fd             	mov    %edi,%r13d
  4004ee:	49 89 f6             	mov    %rsi,%r14
  4004f1:	4c 29 e5             	sub    %r12,%rbp
  4004f4:	48 83 ec 08          	sub    $0x8,%rsp
  4004f8:	48 c1 fd 03          	sar    $0x3,%rbp
  4004fc:	e8 6f fe ff ff       	callq  400370 <_init>
  400501:	48 85 ed             	test   %rbp,%rbp
  400504:	74 20                	je     400526 <__libc_csu_init+0x56>
  400506:	31 db                	xor    %ebx,%ebx
  400508:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40050f:	00
  400510:	4c 89 fa             	mov    %r15,%rdx
  400513:	4c 89 f6             	mov    %r14,%rsi
  400516:	44 89 ef             	mov    %r13d,%edi
  400519:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  40051d:	48 83 c3 01          	add    $0x1,%rbx
  400521:	48 39 dd             	cmp    %rbx,%rbp
  400524:	75 ea                	jne    400510 <__libc_csu_init+0x40>
  400526:	48 83 c4 08          	add    $0x8,%rsp
  40052a:	5b                   	pop    %rbx
  40052b:	5d                   	pop    %rbp
  40052c:	41 5c                	pop    %r12
  40052e:	41 5d                	pop    %r13
  400530:	41 5e                	pop    %r14
  400532:	41 5f                	pop    %r15
  400534:	c3                   	retq
  400535:	90                   	nop
  400536:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40053d:	00 00 00

0000000000400540 <__libc_csu_fini>:
  400540:	f3 c3                	repz retq

Disassembly of section .fini:

0000000000400544 <_fini>:
  400544:	48 83 ec 08          	sub    $0x8,%rsp
  400548:	48 83 c4 08          	add    $0x8,%rsp
  40054c:	c3                   	retq

Disassembly of section .rodata:

0000000000400550 <_IO_stdin_used>:
  400550:	01 00                	add    %eax,(%rax)
  400552:	02 00                	add    (%rax),%al
	...
  40055c:	00 00                	add    %al,(%rax)
  40055e:	24 40                	and    $0x40,%al
  400560:	a4                   	movsb  %ds:(%rsi),%es:(%rdi)
  400561:	70 3d                	jo     4005a0 <__GNU_EH_FRAME_HDR+0x38>
  400563:	0a d7                	or     %bh,%dl
  400565:	93                   	xchg   %eax,%ebx
  400566:	73 40                	jae    4005a8 <__GNU_EH_FRAME_HDR+0x40>

Disassembly of section .eh_frame_hdr:

0000000000400568 <__GNU_EH_FRAME_HDR>:
  400568:	01 1b                	add    %ebx,(%rbx)
  40056a:	03 3b                	add    (%rbx),%edi
  40056c:	34 00                	xor    $0x0,%al
  40056e:	00 00                	add    %al,(%rax)
  400570:	05 00 00 00 28       	add    $0x28000000,%eax
  400575:	fe                   	(bad)
  400576:	ff                   	(bad)
  400577:	ff 50 00             	callq  *0x0(%rax)
  40057a:	00 00                	add    %al,(%rax)
  40057c:	58                   	pop    %rax
  40057d:	fe                   	(bad)
  40057e:	ff                   	(bad)
  40057f:	ff                   	(bad)
  400580:	7c 00                	jl     400582 <__GNU_EH_FRAME_HDR+0x1a>
  400582:	00 00                	add    %al,(%rax)
  400584:	18 ff                	sbb    %bh,%bh
  400586:	ff                   	(bad)
  400587:	ff 90 00 00 00 68    	callq  *0x68000000(%rax)
  40058d:	ff                   	(bad)
  40058e:	ff                   	(bad)
  40058f:	ff b0 00 00 00 d8    	pushq  -0x28000000(%rax)
  400595:	ff                   	(bad)
  400596:	ff                   	(bad)
  400597:	ff                   	(bad)
  400598:	f8                   	clc
  400599:	00 00                	add    %al,(%rax)
	...

Disassembly of section .eh_frame:

00000000004005a0 <__FRAME_END__-0xd4>:
  4005a0:	14 00                	adc    $0x0,%al
  4005a2:	00 00                	add    %al,(%rax)
  4005a4:	00 00                	add    %al,(%rax)
  4005a6:	00 00                	add    %al,(%rax)
  4005a8:	01 7a 52             	add    %edi,0x52(%rdx)
  4005ab:	00 01                	add    %al,(%rcx)
  4005ad:	78 10                	js     4005bf <__GNU_EH_FRAME_HDR+0x57>
  4005af:	01 1b                	add    %ebx,(%rbx)
  4005b1:	0c 07                	or     $0x7,%al
  4005b3:	08 90 01 07 10 10    	or     %dl,0x10100701(%rax)
  4005b9:	00 00                	add    %al,(%rax)
  4005bb:	00 1c 00             	add    %bl,(%rax,%rax,1)
  4005be:	00 00                	add    %al,(%rax)
  4005c0:	d0 fd                	sar    %ch
  4005c2:	ff                   	(bad)
  4005c3:	ff 2b                	ljmp   *(%rbx)
  4005c5:	00 00                	add    %al,(%rax)
  4005c7:	00 00                	add    %al,(%rax)
  4005c9:	00 00                	add    %al,(%rax)
  4005cb:	00 14 00             	add    %dl,(%rax,%rax,1)
  4005ce:	00 00                	add    %al,(%rax)
  4005d0:	00 00                	add    %al,(%rax)
  4005d2:	00 00                	add    %al,(%rax)
  4005d4:	01 7a 52             	add    %edi,0x52(%rdx)
  4005d7:	00 01                	add    %al,(%rcx)
  4005d9:	78 10                	js     4005eb <__GNU_EH_FRAME_HDR+0x83>
  4005db:	01 1b                	add    %ebx,(%rbx)
  4005dd:	0c 07                	or     $0x7,%al
  4005df:	08 90 01 00 00 10    	or     %dl,0x10000001(%rax)
  4005e5:	00 00                	add    %al,(%rax)
  4005e7:	00 1c 00             	add    %bl,(%rax,%rax,1)
  4005ea:	00 00                	add    %al,(%rax)
  4005ec:	d4                   	(bad)
  4005ed:	fd                   	std
  4005ee:	ff                   	(bad)
  4005ef:	ff 02                	incl   (%rdx)
  4005f1:	00 00                	add    %al,(%rax)
  4005f3:	00 00                	add    %al,(%rax)
  4005f5:	00 00                	add    %al,(%rax)
  4005f7:	00 1c 00             	add    %bl,(%rax,%rax,1)
  4005fa:	00 00                	add    %al,(%rax)
  4005fc:	30 00                	xor    %al,(%rax)
  4005fe:	00 00                	add    %al,(%rax)
  400600:	80 fe ff             	cmp    $0xff,%dh
  400603:	ff 46 00             	incl   0x0(%rsi)
  400606:	00 00                	add    %al,(%rax)
  400608:	00 41 0e             	add    %al,0xe(%rcx)
  40060b:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  400611:	00 00                	add    %al,(%rax)
  400613:	00 00                	add    %al,(%rax)
  400615:	00 00                	add    %al,(%rax)
  400617:	00 44 00 00          	add    %al,0x0(%rax,%rax,1)
  40061b:	00 50 00             	add    %dl,0x0(%rax)
  40061e:	00 00                	add    %al,(%rax)
  400620:	b0 fe                	mov    $0xfe,%al
  400622:	ff                   	(bad)
  400623:	ff 65 00             	jmpq   *0x0(%rbp)
  400626:	00 00                	add    %al,(%rax)
  400628:	00 42 0e             	add    %al,0xe(%rdx)
  40062b:	10 8f 02 42 0e 18    	adc    %cl,0x180e4202(%rdi)
  400631:	8e 03                	mov    (%rbx),%es
  400633:	45 0e                	rex.RB (bad)
  400635:	20 8d 04 42 0e 28    	and    %cl,0x280e4204(%rbp)
  40063b:	8c 05 48 0e 30 86    	mov    %es,-0x79cff1b8(%rip)        # ffffffff86701489 <_end+0xffffffff86100459>
  400641:	06                   	(bad)
  400642:	48 0e                	rex.W (bad)
  400644:	38 83 07 4d 0e 40    	cmp    %al,0x400e4d07(%rbx)
  40064a:	72 0e                	jb     40065a <__GNU_EH_FRAME_HDR+0xf2>
  40064c:	38 41 0e             	cmp    %al,0xe(%rcx)
  40064f:	30 41 0e             	xor    %al,0xe(%rcx)
  400652:	28 42 0e             	sub    %al,0xe(%rdx)
  400655:	20 42 0e             	and    %al,0xe(%rdx)
  400658:	18 42 0e             	sbb    %al,0xe(%rdx)
  40065b:	10 42 0e             	adc    %al,0xe(%rdx)
  40065e:	08 00                	or     %al,(%rax)
  400660:	10 00                	adc    %al,(%rax)
  400662:	00 00                	add    %al,(%rax)
  400664:	98                   	cwtl
  400665:	00 00                	add    %al,(%rax)
  400667:	00 d8                	add    %bl,%al
  400669:	fe                   	(bad)
  40066a:	ff                   	(bad)
  40066b:	ff 02                	incl   (%rdx)
  40066d:	00 00                	add    %al,(%rax)
  40066f:	00 00                	add    %al,(%rax)
  400671:	00 00                	add    %al,(%rax)
	...

0000000000400674 <__FRAME_END__>:
  400674:	00 00                	add    %al,(%rax)
	...

Disassembly of section .init_array:

0000000000600e50 <__frame_dummy_init_array_entry>:
  600e50:	70 04                	jo     600e56 <__frame_dummy_init_array_entry+0x6>
  600e52:	40 00 00             	add    %al,(%rax)
  600e55:	00 00                	add    %al,(%rax)
	...

Disassembly of section .fini_array:

0000000000600e58 <__do_global_dtors_aux_fini_array_entry>:
  600e58:	40 04 40             	add    $0x40,%al
  600e5b:	00 00                	add    %al,(%rax)
  600e5d:	00 00                	add    %al,(%rax)
	...

Disassembly of section .dynamic:

0000000000600e60 <_DYNAMIC>:
  600e60:	01 00                	add    %eax,(%rax)
  600e62:	00 00                	add    %al,(%rax)
  600e64:	00 00                	add    %al,(%rax)
  600e66:	00 00                	add    %al,(%rax)
  600e68:	01 00                	add    %eax,(%rax)
  600e6a:	00 00                	add    %al,(%rax)
  600e6c:	00 00                	add    %al,(%rax)
  600e6e:	00 00                	add    %al,(%rax)
  600e70:	0c 00                	or     $0x0,%al
  600e72:	00 00                	add    %al,(%rax)
  600e74:	00 00                	add    %al,(%rax)
  600e76:	00 00                	add    %al,(%rax)
  600e78:	70 03                	jo     600e7d <_DYNAMIC+0x1d>
  600e7a:	40 00 00             	add    %al,(%rax)
  600e7d:	00 00                	add    %al,(%rax)
  600e7f:	00 0d 00 00 00 00    	add    %cl,0x0(%rip)        # 600e85 <_DYNAMIC+0x25>
  600e85:	00 00                	add    %al,(%rax)
  600e87:	00 44 05 40          	add    %al,0x40(%rbp,%rax,1)
  600e8b:	00 00                	add    %al,(%rax)
  600e8d:	00 00                	add    %al,(%rax)
  600e8f:	00 19                	add    %bl,(%rcx)
  600e91:	00 00                	add    %al,(%rax)
  600e93:	00 00                	add    %al,(%rax)
  600e95:	00 00                	add    %al,(%rax)
  600e97:	00 50 0e             	add    %dl,0xe(%rax)
  600e9a:	60                   	(bad)
  600e9b:	00 00                	add    %al,(%rax)
  600e9d:	00 00                	add    %al,(%rax)
  600e9f:	00 1b                	add    %bl,(%rbx)
  600ea1:	00 00                	add    %al,(%rax)
  600ea3:	00 00                	add    %al,(%rax)
  600ea5:	00 00                	add    %al,(%rax)
  600ea7:	00 08                	add    %cl,(%rax)
  600ea9:	00 00                	add    %al,(%rax)
  600eab:	00 00                	add    %al,(%rax)
  600ead:	00 00                	add    %al,(%rax)
  600eaf:	00 1a                	add    %bl,(%rdx)
  600eb1:	00 00                	add    %al,(%rax)
  600eb3:	00 00                	add    %al,(%rax)
  600eb5:	00 00                	add    %al,(%rax)
  600eb7:	00 58 0e             	add    %bl,0xe(%rax)
  600eba:	60                   	(bad)
  600ebb:	00 00                	add    %al,(%rax)
  600ebd:	00 00                	add    %al,(%rax)
  600ebf:	00 1c 00             	add    %bl,(%rax,%rax,1)
  600ec2:	00 00                	add    %al,(%rax)
  600ec4:	00 00                	add    %al,(%rax)
  600ec6:	00 00                	add    %al,(%rax)
  600ec8:	08 00                	or     %al,(%rax)
  600eca:	00 00                	add    %al,(%rax)
  600ecc:	00 00                	add    %al,(%rax)
  600ece:	00 00                	add    %al,(%rax)
  600ed0:	f5                   	cmc
  600ed1:	fe                   	(bad)
  600ed2:	ff 6f 00             	ljmp   *0x0(%rdi)
  600ed5:	00 00                	add    %al,(%rax)
  600ed7:	00 78 02             	add    %bh,0x2(%rax)
  600eda:	40 00 00             	add    %al,(%rax)
  600edd:	00 00                	add    %al,(%rax)
  600edf:	00 05 00 00 00 00    	add    %al,0x0(%rip)        # 600ee5 <_DYNAMIC+0x85>
  600ee5:	00 00                	add    %al,(%rax)
  600ee7:	00 e0                	add    %ah,%al
  600ee9:	02 40 00             	add    0x0(%rax),%al
  600eec:	00 00                	add    %al,(%rax)
  600eee:	00 00                	add    %al,(%rax)
  600ef0:	06                   	(bad)
  600ef1:	00 00                	add    %al,(%rax)
  600ef3:	00 00                	add    %al,(%rax)
  600ef5:	00 00                	add    %al,(%rax)
  600ef7:	00 98 02 40 00 00    	add    %bl,0x4002(%rax)
  600efd:	00 00                	add    %al,(%rax)
  600eff:	00 0a                	add    %cl,(%rdx)
  600f01:	00 00                	add    %al,(%rax)
  600f03:	00 00                	add    %al,(%rax)
  600f05:	00 00                	add    %al,(%rax)
  600f07:	00 38                	add    %bh,(%rax)
  600f09:	00 00                	add    %al,(%rax)
  600f0b:	00 00                	add    %al,(%rax)
  600f0d:	00 00                	add    %al,(%rax)
  600f0f:	00 0b                	add    %cl,(%rbx)
  600f11:	00 00                	add    %al,(%rax)
  600f13:	00 00                	add    %al,(%rax)
  600f15:	00 00                	add    %al,(%rax)
  600f17:	00 18                	add    %bl,(%rax)
  600f19:	00 00                	add    %al,(%rax)
  600f1b:	00 00                	add    %al,(%rax)
  600f1d:	00 00                	add    %al,(%rax)
  600f1f:	00 15 00 00 00 00    	add    %dl,0x0(%rip)        # 600f25 <_DYNAMIC+0xc5>
	...
  600f2d:	00 00                	add    %al,(%rax)
  600f2f:	00 07                	add    %al,(%rdi)
  600f31:	00 00                	add    %al,(%rax)
  600f33:	00 00                	add    %al,(%rax)
  600f35:	00 00                	add    %al,(%rax)
  600f37:	00 40 03             	add    %al,0x3(%rax)
  600f3a:	40 00 00             	add    %al,(%rax)
  600f3d:	00 00                	add    %al,(%rax)
  600f3f:	00 08                	add    %cl,(%rax)
  600f41:	00 00                	add    %al,(%rax)
  600f43:	00 00                	add    %al,(%rax)
  600f45:	00 00                	add    %al,(%rax)
  600f47:	00 30                	add    %dh,(%rax)
  600f49:	00 00                	add    %al,(%rax)
  600f4b:	00 00                	add    %al,(%rax)
  600f4d:	00 00                	add    %al,(%rax)
  600f4f:	00 09                	add    %cl,(%rcx)
  600f51:	00 00                	add    %al,(%rax)
  600f53:	00 00                	add    %al,(%rax)
  600f55:	00 00                	add    %al,(%rax)
  600f57:	00 18                	add    %bl,(%rax)
  600f59:	00 00                	add    %al,(%rax)
  600f5b:	00 00                	add    %al,(%rax)
  600f5d:	00 00                	add    %al,(%rax)
  600f5f:	00 fe                	add    %bh,%dh
  600f61:	ff                   	(bad)
  600f62:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f65:	00 00                	add    %al,(%rax)
  600f67:	00 20                	add    %ah,(%rax)
  600f69:	03 40 00             	add    0x0(%rax),%eax
  600f6c:	00 00                	add    %al,(%rax)
  600f6e:	00 00                	add    %al,(%rax)
  600f70:	ff                   	(bad)
  600f71:	ff                   	(bad)
  600f72:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f75:	00 00                	add    %al,(%rax)
  600f77:	00 01                	add    %al,(%rcx)
  600f79:	00 00                	add    %al,(%rax)
  600f7b:	00 00                	add    %al,(%rax)
  600f7d:	00 00                	add    %al,(%rax)
  600f7f:	00 f0                	add    %dh,%al
  600f81:	ff                   	(bad)
  600f82:	ff 6f 00             	ljmp   *0x0(%rdi)
  600f85:	00 00                	add    %al,(%rax)
  600f87:	00 18                	add    %bl,(%rax)
  600f89:	03 40 00             	add    0x0(%rax),%eax
	...

Disassembly of section .got:

0000000000600ff0 <.got>:
	...

Disassembly of section .got.plt:

0000000000601000 <_GLOBAL_OFFSET_TABLE_>:
  601000:	60                   	(bad)
  601001:	0e                   	(bad)
  601002:	60                   	(bad)
	...

Disassembly of section .data:

0000000000601018 <__data_start>:
	...

0000000000601020 <__dso_handle>:
	...

Disassembly of section .bss:

0000000000601028 <__bss_start>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp (%r8),%spl
   5:	28 55 62             	sub    %dl,0x62(%rbp)
   8:	75 6e                	jne    78 <_init-0x4002f8>
   a:	74 75                	je     81 <_init-0x4002ef>
   c:	20 37                	and    %dh,(%rdi)
   e:	2e 33 2e             	xor    %cs:(%rsi),%ebp
  11:	30 2d 31 36 75 62    	xor    %ch,0x62753631(%rip)        # 62753648 <_end+0x62152618>
  17:	75 6e                	jne    87 <_init-0x4002e9>
  19:	74 75                	je     90 <_init-0x4002e0>
  1b:	33 29                	xor    (%rcx),%ebp
  1d:	20 37                	and    %dh,(%rdi)
  1f:	2e 33 2e             	xor    %cs:(%rsi),%ebp
  22:	30 00                	xor    %al,(%rax)
  24:	63 6c 61 6e          	movslq 0x6e(%rcx,%riz,2),%ebp
  28:	67 20 76 65          	and    %dh,0x65(%esi)
  2c:	72 73                	jb     a1 <_init-0x4002cf>
  2e:	69 6f 6e 20 36 2e 30 	imul   $0x302e3620,0x6e(%rdi),%ebp
  35:	2e 30 2d 31 75 62 75 	xor    %ch,%cs:0x75627531(%rip)        # 7562756d <_end+0x7502653d>
  3c:	6e                   	outsb  %ds:(%rsi),(%dx)
  3d:	74 75                	je     b4 <_init-0x4002bc>
  3f:	32 20                	xor    (%rax),%ah
  41:	28 74 61 67          	sub    %dh,0x67(%rcx,%riz,2)
  45:	73 2f                	jae    76 <_init-0x4002fa>
  47:	52                   	push   %rdx
  48:	45                   	rex.RB
  49:	4c                   	rex.WR
  4a:	45                   	rex.RB
  4b:	41 53                	push   %r11
  4d:	45 5f                	rex.RB pop %r15
  4f:	36 30 30             	xor    %dh,%ss:(%rax)
  52:	2f                   	(bad)
  53:	66 69 6e 61 6c 29    	imul   $0x296c,0x61(%rsi),%bp
	...
