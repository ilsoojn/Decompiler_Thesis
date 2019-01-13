
link:     file format elf64-x86-64


Disassembly of section .interp:

0000000000400238 <.interp>:
  400238:	2f                   	(bad)  
  400239:	6c                   	insb   (%dx),%es:(%rdi)
  40023a:	69 62 36 34 2f 6c 64 	imul   $0x646c2f34,0x36(%rdx),%esp
  400241:	2d 6c 69 6e 75       	sub    $0x756e696c,%eax
  400246:	78 2d                	js     400275 <_init-0x253>
  400248:	78 38                	js     400282 <_init-0x246>
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
  400278:	03 00                	add    (%rax),%eax
  40027a:	00 00                	add    %al,(%rax)
  40027c:	05 00 00 00 01       	add    $0x1000000,%eax
  400281:	00 00                	add    %al,(%rax)
  400283:	00 06                	add    %al,(%rsi)
  400285:	00 00                	add    %al,(%rax)
  400287:	00 88 c0 20 01 00    	add    %cl,0x120c0(%rax)
  40028d:	04 40                	add    $0x40,%al
  40028f:	09 05 00 00 00 07    	or     %eax,0x7000000(%rip)        # 7400295 <_end+0x6dff255>
  400295:	00 00                	add    %al,(%rax)
  400297:	00 09                	add    %cl,(%rcx)
  400299:	00 00                	add    %al,(%rax)
  40029b:	00 42 45             	add    %al,0x45(%rdx)
  40029e:	d5                   	(bad)  
  40029f:	ec                   	in     (%dx),%al
  4002a0:	bb e3 92 7c d8       	mov    $0xd87c92e3,%ebx
  4002a5:	71 58                	jno    4002ff <_init-0x1c9>
  4002a7:	1c b9                	sbb    $0xb9,%al
  4002a9:	8d                   	(bad)  
  4002aa:	f1                   	icebp  
  4002ab:	0e                   	(bad)  
  4002ac:	eb d3                	jmp    400281 <_init-0x247>
  4002ae:	ef                   	out    %eax,(%dx)
  4002af:	0e                   	(bad)  

Disassembly of section .dynsym:

00000000004002b0 <.dynsym>:
	...
  4002c8:	3d 00 00 00 12       	cmp    $0x12000000,%eax
	...
  4002dd:	00 00                	add    %al,(%rax)
  4002df:	00 57 00             	add    %dl,0x0(%rdi)
  4002e2:	00 00                	add    %al,(%rax)
  4002e4:	12 00                	adc    (%rax),%al
	...
  4002f6:	00 00                	add    %al,(%rax)
  4002f8:	5e                   	pop    %rsi
  4002f9:	00 00                	add    %al,(%rax)
  4002fb:	00 12                	add    %dl,(%rdx)
	...
  40030d:	00 00                	add    %al,(%rax)
  40030f:	00 2e                	add    %ch,(%rsi)
  400311:	00 00                	add    %al,(%rax)
  400313:	00 20                	add    %ah,(%rax)
	...
  400325:	00 00                	add    %al,(%rax)
  400327:	00 70 00             	add    %dh,0x0(%rax)
  40032a:	00 00                	add    %al,(%rax)
  40032c:	10 00                	adc    %al,(%rax)
  40032e:	16                   	(bad)  
  40032f:	00 38                	add    %bh,(%rax)
  400331:	10 60 00             	adc    %ah,0x0(%rax)
	...
  400340:	83 00 00             	addl   $0x0,(%rax)
  400343:	00 10                	add    %dl,(%rax)
  400345:	00 17                	add    %dl,(%rdi)
  400347:	00 40 10             	add    %al,0x10(%rax)
  40034a:	60                   	(bad)  
	...
  400357:	00 77 00             	add    %dh,0x0(%rdi)
  40035a:	00 00                	add    %al,(%rax)
  40035c:	10 00                	adc    %al,(%rax)
  40035e:	17                   	(bad)  
  40035f:	00 38                	add    %bh,(%rax)
  400361:	10 60 00             	adc    %ah,0x0(%rax)
	...
  400370:	41 00 00             	add    %al,(%r8)
  400373:	00 12                	add    %dl,(%rdx)
  400375:	00 0a                	add    %cl,(%rdx)
  400377:	00 c8                	add    %cl,%al
  400379:	04 40                	add    $0x40,%al
	...
  400387:	00 47 00             	add    %al,0x0(%rdi)
  40038a:	00 00                	add    %al,(%rax)
  40038c:	12 00                	adc    (%rax),%al
  40038e:	0d 00 f4 06 40       	or     $0x4006f400,%eax
	...

Disassembly of section .dynstr:

00000000004003a0 <.dynstr>:
  4003a0:	00 2f                	add    %ch,(%rdi)
  4003a2:	68 6f 6d 65 2f       	pushq  $0x2f656d6f
  4003a7:	69 6c 73 6f 6f 2f 64 	imul   $0x65642f6f,0x6f(%rbx,%rsi,2),%ebp
  4003ae:	65 
  4003af:	63 2f                	movslq (%rdi),%ebp
  4003b1:	74 65                	je     400418 <_init-0xb0>
  4003b3:	73 74                	jae    400429 <_init-0x9f>
  4003b5:	66 69 6c 65 73 2f 6c 	imul   $0x6c2f,0x73(%rbp,%riz,2),%bp
  4003bc:	69 6e 6b 2f 6c 69 62 	imul   $0x62696c2f,0x6b(%rsi),%ebp
  4003c3:	44 79 6e             	rex.R jns 400434 <_init-0x94>
  4003c6:	61                   	(bad)  
  4003c7:	6d                   	insl   (%dx),%es:(%rdi)
  4003c8:	69 63 2e 73 6f 00 5f 	imul   $0x5f006f73,0x2e(%rbx),%esp
  4003cf:	5f                   	pop    %rdi
  4003d0:	67 6d                	insl   (%dx),%es:(%edi)
  4003d2:	6f                   	outsl  %ds:(%rsi),(%dx)
  4003d3:	6e                   	outsb  %ds:(%rsi),(%dx)
  4003d4:	5f                   	pop    %rdi
  4003d5:	73 74                	jae    40044b <_init-0x7d>
  4003d7:	61                   	(bad)  
  4003d8:	72 74                	jb     40044e <_init-0x7a>
  4003da:	5f                   	pop    %rdi
  4003db:	5f                   	pop    %rdi
  4003dc:	00 62 61             	add    %ah,0x61(%rdx)
  4003df:	72 00                	jb     4003e1 <_init-0xe7>
  4003e1:	5f                   	pop    %rdi
  4003e2:	69 6e 69 74 00 5f 66 	imul   $0x665f0074,0x69(%rsi),%ebp
  4003e9:	69 6e 69 00 6c 69 62 	imul   $0x62696c00,0x69(%rsi),%ebp
  4003f0:	63 2e                	movslq (%rsi),%ebp
  4003f2:	73 6f                	jae    400463 <_init-0x65>
  4003f4:	2e 36 00 70 72       	cs add %dh,%ss:0x72(%rax)
  4003f9:	69 6e 74 66 00 5f 5f 	imul   $0x5f5f0066,0x74(%rsi),%ebp
  400400:	6c                   	insb   (%dx),%es:(%rdi)
  400401:	69 62 63 5f 73 74 61 	imul   $0x6174735f,0x63(%rdx),%esp
  400408:	72 74                	jb     40047e <_init-0x4a>
  40040a:	5f                   	pop    %rdi
  40040b:	6d                   	insl   (%dx),%es:(%rdi)
  40040c:	61                   	(bad)  
  40040d:	69 6e 00 5f 65 64 61 	imul   $0x6164655f,0x0(%rsi),%ebp
  400414:	74 61                	je     400477 <_init-0x51>
  400416:	00 5f 5f             	add    %bl,0x5f(%rdi)
  400419:	62 73                	(bad)  
  40041b:	73 5f                	jae    40047c <_init-0x4c>
  40041d:	73 74                	jae    400493 <_init-0x35>
  40041f:	61                   	(bad)  
  400420:	72 74                	jb     400496 <_init-0x32>
  400422:	00 5f 65             	add    %bl,0x65(%rdi)
  400425:	6e                   	outsb  %ds:(%rsi),(%dx)
  400426:	64 00 47 4c          	add    %al,%fs:0x4c(%rdi)
  40042a:	49                   	rex.WB
  40042b:	42                   	rex.X
  40042c:	43 5f                	rex.XB pop %r15
  40042e:	32 2e                	xor    (%rsi),%ch
  400430:	32 2e                	xor    (%rsi),%ch
  400432:	35                   	.byte 0x35
	...

Disassembly of section .gnu.version:

0000000000400434 <.gnu.version>:
  400434:	00 00                	add    %al,(%rax)
  400436:	00 00                	add    %al,(%rax)
  400438:	02 00                	add    (%rax),%al
  40043a:	02 00                	add    (%rax),%al
  40043c:	00 00                	add    %al,(%rax)
  40043e:	01 00                	add    %eax,(%rax)
  400440:	01 00                	add    %eax,(%rax)
  400442:	01 00                	add    %eax,(%rax)
  400444:	01 00                	add    %eax,(%rax)
  400446:	01 00                	add    %eax,(%rax)

Disassembly of section .gnu.version_r:

0000000000400448 <.gnu.version_r>:
  400448:	01 00                	add    %eax,(%rax)
  40044a:	01 00                	add    %eax,(%rax)
  40044c:	4d 00 00             	rex.WRB add %r8b,(%r8)
  40044f:	00 10                	add    %dl,(%rax)
  400451:	00 00                	add    %al,(%rax)
  400453:	00 00                	add    %al,(%rax)
  400455:	00 00                	add    %al,(%rax)
  400457:	00 75 1a             	add    %dh,0x1a(%rbp)
  40045a:	69 09 00 00 02 00    	imul   $0x20000,(%rcx),%ecx
  400460:	88 00                	mov    %al,(%rax)
  400462:	00 00                	add    %al,(%rax)
  400464:	00 00                	add    %al,(%rax)
	...

Disassembly of section .rela.dyn:

0000000000400468 <.rela.dyn>:
  400468:	f0 0f 60 00          	lock punpcklbw (%rax),%mm0
  40046c:	00 00                	add    %al,(%rax)
  40046e:	00 00                	add    %al,(%rax)
  400470:	06                   	(bad)  
  400471:	00 00                	add    %al,(%rax)
  400473:	00 03                	add    %al,(%rbx)
	...
  40047d:	00 00                	add    %al,(%rax)
  40047f:	00 f8                	add    %bh,%al
  400481:	0f 60 00             	punpcklbw (%rax),%mm0
  400484:	00 00                	add    %al,(%rax)
  400486:	00 00                	add    %al,(%rax)
  400488:	06                   	(bad)  
  400489:	00 00                	add    %al,(%rax)
  40048b:	00 04 00             	add    %al,(%rax,%rax,1)
	...

Disassembly of section .rela.plt:

0000000000400498 <.rela.plt>:
  400498:	18 10                	sbb    %dl,(%rax)
  40049a:	60                   	(bad)  
  40049b:	00 00                	add    %al,(%rax)
  40049d:	00 00                	add    %al,(%rax)
  40049f:	00 07                	add    %al,(%rdi)
  4004a1:	00 00                	add    %al,(%rax)
  4004a3:	00 01                	add    %al,(%rcx)
	...
  4004ad:	00 00                	add    %al,(%rax)
  4004af:	00 20                	add    %ah,(%rax)
  4004b1:	10 60 00             	adc    %ah,0x0(%rax)
  4004b4:	00 00                	add    %al,(%rax)
  4004b6:	00 00                	add    %al,(%rax)
  4004b8:	07                   	(bad)  
  4004b9:	00 00                	add    %al,(%rax)
  4004bb:	00 02                	add    %al,(%rdx)
	...

Disassembly of section .init:

00000000004004c8 <_init>:
  4004c8:	48 83 ec 08          	sub    $0x8,%rsp
  4004cc:	48 8b 05 25 0b 20 00 	mov    0x200b25(%rip),%rax        # 600ff8 <__gmon_start__>
  4004d3:	48 85 c0             	test   %rax,%rax
  4004d6:	74 02                	je     4004da <_init+0x12>
  4004d8:	ff d0                	callq  *%rax
  4004da:	48 83 c4 08          	add    $0x8,%rsp
  4004de:	c3                   	retq   

Disassembly of section .plt:

00000000004004e0 <.plt>:
  4004e0:	ff 35 22 0b 20 00    	pushq  0x200b22(%rip)        # 601008 <_GLOBAL_OFFSET_TABLE_+0x8>
  4004e6:	ff 25 24 0b 20 00    	jmpq   *0x200b24(%rip)        # 601010 <_GLOBAL_OFFSET_TABLE_+0x10>
  4004ec:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004004f0 <bar@plt>:
  4004f0:	ff 25 22 0b 20 00    	jmpq   *0x200b22(%rip)        # 601018 <bar>
  4004f6:	68 00 00 00 00       	pushq  $0x0
  4004fb:	e9 e0 ff ff ff       	jmpq   4004e0 <.plt>

0000000000400500 <printf@plt>:
  400500:	ff 25 1a 0b 20 00    	jmpq   *0x200b1a(%rip)        # 601020 <printf@GLIBC_2.2.5>
  400506:	68 01 00 00 00       	pushq  $0x1
  40050b:	e9 d0 ff ff ff       	jmpq   4004e0 <.plt>

Disassembly of section .text:

0000000000400510 <_start>:
  400510:	31 ed                	xor    %ebp,%ebp
  400512:	49 89 d1             	mov    %rdx,%r9
  400515:	5e                   	pop    %rsi
  400516:	48 89 e2             	mov    %rsp,%rdx
  400519:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  40051d:	50                   	push   %rax
  40051e:	54                   	push   %rsp
  40051f:	49 c7 c0 f0 06 40 00 	mov    $0x4006f0,%r8
  400526:	48 c7 c1 80 06 40 00 	mov    $0x400680,%rcx
  40052d:	48 c7 c7 00 06 40 00 	mov    $0x400600,%rdi
  400534:	ff 15 b6 0a 20 00    	callq  *0x200ab6(%rip)        # 600ff0 <__libc_start_main@GLIBC_2.2.5>
  40053a:	f4                   	hlt    
  40053b:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)

0000000000400540 <_dl_relocate_static_pie>:
  400540:	f3 c3                	repz retq 
  400542:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  400549:	00 00 00 
  40054c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400550 <deregister_tm_clones>:
  400550:	55                   	push   %rbp
  400551:	b8 38 10 60 00       	mov    $0x601038,%eax
  400556:	48 3d 38 10 60 00    	cmp    $0x601038,%rax
  40055c:	48 89 e5             	mov    %rsp,%rbp
  40055f:	74 17                	je     400578 <deregister_tm_clones+0x28>
  400561:	b8 00 00 00 00       	mov    $0x0,%eax
  400566:	48 85 c0             	test   %rax,%rax
  400569:	74 0d                	je     400578 <deregister_tm_clones+0x28>
  40056b:	5d                   	pop    %rbp
  40056c:	bf 38 10 60 00       	mov    $0x601038,%edi
  400571:	ff e0                	jmpq   *%rax
  400573:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  400578:	5d                   	pop    %rbp
  400579:	c3                   	retq   
  40057a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400580 <register_tm_clones>:
  400580:	be 38 10 60 00       	mov    $0x601038,%esi
  400585:	55                   	push   %rbp
  400586:	48 81 ee 38 10 60 00 	sub    $0x601038,%rsi
  40058d:	48 89 e5             	mov    %rsp,%rbp
  400590:	48 c1 fe 03          	sar    $0x3,%rsi
  400594:	48 89 f0             	mov    %rsi,%rax
  400597:	48 c1 e8 3f          	shr    $0x3f,%rax
  40059b:	48 01 c6             	add    %rax,%rsi
  40059e:	48 d1 fe             	sar    %rsi
  4005a1:	74 15                	je     4005b8 <register_tm_clones+0x38>
  4005a3:	b8 00 00 00 00       	mov    $0x0,%eax
  4005a8:	48 85 c0             	test   %rax,%rax
  4005ab:	74 0b                	je     4005b8 <register_tm_clones+0x38>
  4005ad:	5d                   	pop    %rbp
  4005ae:	bf 38 10 60 00       	mov    $0x601038,%edi
  4005b3:	ff e0                	jmpq   *%rax
  4005b5:	0f 1f 00             	nopl   (%rax)
  4005b8:	5d                   	pop    %rbp
  4005b9:	c3                   	retq   
  4005ba:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

00000000004005c0 <__do_global_dtors_aux>:
  4005c0:	80 3d 71 0a 20 00 00 	cmpb   $0x0,0x200a71(%rip)        # 601038 <__TMC_END__>
  4005c7:	75 17                	jne    4005e0 <__do_global_dtors_aux+0x20>
  4005c9:	55                   	push   %rbp
  4005ca:	48 89 e5             	mov    %rsp,%rbp
  4005cd:	e8 7e ff ff ff       	callq  400550 <deregister_tm_clones>
  4005d2:	c6 05 5f 0a 20 00 01 	movb   $0x1,0x200a5f(%rip)        # 601038 <__TMC_END__>
  4005d9:	5d                   	pop    %rbp
  4005da:	c3                   	retq   
  4005db:	0f 1f 44 00 00       	nopl   0x0(%rax,%rax,1)
  4005e0:	f3 c3                	repz retq 
  4005e2:	0f 1f 40 00          	nopl   0x0(%rax)
  4005e6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4005ed:	00 00 00 

00000000004005f0 <frame_dummy>:
  4005f0:	55                   	push   %rbp
  4005f1:	48 89 e5             	mov    %rsp,%rbp
  4005f4:	5d                   	pop    %rbp
  4005f5:	eb 89                	jmp    400580 <register_tm_clones>
  4005f7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4005fe:	00 00 

0000000000400600 <main>:
  400600:	55                   	push   %rbp
  400601:	48 89 e5             	mov    %rsp,%rbp
  400604:	48 83 ec 10          	sub    $0x10,%rsp
  400608:	48 bf 04 07 40 00 00 	movabs $0x400704,%rdi
  40060f:	00 00 00 
  400612:	c7 45 fc 00 00 00 00 	movl   $0x0,-0x4(%rbp)
  400619:	b0 00                	mov    $0x0,%al
  40061b:	e8 e0 fe ff ff       	callq  400500 <printf@plt>
  400620:	89 45 f8             	mov    %eax,-0x8(%rbp)
  400623:	b0 00                	mov    $0x0,%al
  400625:	e8 26 00 00 00       	callq  400650 <foo>
  40062a:	b0 00                	mov    $0x0,%al
  40062c:	e8 bf fe ff ff       	callq  4004f0 <bar@plt>
  400631:	48 bf 10 07 40 00 00 	movabs $0x400710,%rdi
  400638:	00 00 00 
  40063b:	b0 00                	mov    $0x0,%al
  40063d:	e8 be fe ff ff       	callq  400500 <printf@plt>
  400642:	31 c9                	xor    %ecx,%ecx
  400644:	89 45 f4             	mov    %eax,-0xc(%rbp)
  400647:	89 c8                	mov    %ecx,%eax
  400649:	48 83 c4 10          	add    $0x10,%rsp
  40064d:	5d                   	pop    %rbp
  40064e:	c3                   	retq   
  40064f:	90                   	nop

0000000000400650 <foo>:
  400650:	55                   	push   %rbp
  400651:	48 89 e5             	mov    %rsp,%rbp
  400654:	48 83 ec 10          	sub    $0x10,%rsp
  400658:	48 bf 1a 07 40 00 00 	movabs $0x40071a,%rdi
  40065f:	00 00 00 
  400662:	b0 00                	mov    $0x0,%al
  400664:	e8 97 fe ff ff       	callq  400500 <printf@plt>
  400669:	89 45 fc             	mov    %eax,-0x4(%rbp)
  40066c:	48 83 c4 10          	add    $0x10,%rsp
  400670:	5d                   	pop    %rbp
  400671:	c3                   	retq   
  400672:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  400679:	00 00 00 
  40067c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400680 <__libc_csu_init>:
  400680:	41 57                	push   %r15
  400682:	41 56                	push   %r14
  400684:	49 89 d7             	mov    %rdx,%r15
  400687:	41 55                	push   %r13
  400689:	41 54                	push   %r12
  40068b:	4c 8d 25 6e 07 20 00 	lea    0x20076e(%rip),%r12        # 600e00 <__frame_dummy_init_array_entry>
  400692:	55                   	push   %rbp
  400693:	48 8d 2d 6e 07 20 00 	lea    0x20076e(%rip),%rbp        # 600e08 <__init_array_end>
  40069a:	53                   	push   %rbx
  40069b:	41 89 fd             	mov    %edi,%r13d
  40069e:	49 89 f6             	mov    %rsi,%r14
  4006a1:	4c 29 e5             	sub    %r12,%rbp
  4006a4:	48 83 ec 08          	sub    $0x8,%rsp
  4006a8:	48 c1 fd 03          	sar    $0x3,%rbp
  4006ac:	e8 17 fe ff ff       	callq  4004c8 <_init>
  4006b1:	48 85 ed             	test   %rbp,%rbp
  4006b4:	74 20                	je     4006d6 <__libc_csu_init+0x56>
  4006b6:	31 db                	xor    %ebx,%ebx
  4006b8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4006bf:	00 
  4006c0:	4c 89 fa             	mov    %r15,%rdx
  4006c3:	4c 89 f6             	mov    %r14,%rsi
  4006c6:	44 89 ef             	mov    %r13d,%edi
  4006c9:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  4006cd:	48 83 c3 01          	add    $0x1,%rbx
  4006d1:	48 39 dd             	cmp    %rbx,%rbp
  4006d4:	75 ea                	jne    4006c0 <__libc_csu_init+0x40>
  4006d6:	48 83 c4 08          	add    $0x8,%rsp
  4006da:	5b                   	pop    %rbx
  4006db:	5d                   	pop    %rbp
  4006dc:	41 5c                	pop    %r12
  4006de:	41 5d                	pop    %r13
  4006e0:	41 5e                	pop    %r14
  4006e2:	41 5f                	pop    %r15
  4006e4:	c3                   	retq   
  4006e5:	90                   	nop
  4006e6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4006ed:	00 00 00 

00000000004006f0 <__libc_csu_fini>:
  4006f0:	f3 c3                	repz retq 

Disassembly of section .fini:

00000000004006f4 <_fini>:
  4006f4:	48 83 ec 08          	sub    $0x8,%rsp
  4006f8:	48 83 c4 08          	add    $0x8,%rsp
  4006fc:	c3                   	retq   

Disassembly of section .rodata:

0000000000400700 <_IO_stdin_used>:
  400700:	01 00                	add    %eax,(%rax)
  400702:	02 00                	add    (%rax),%al
  400704:	42                   	rex.X
  400705:	65 67 69 6e 20 54 65 	imul   $0x74736554,%gs:0x20(%esi),%ebp
  40070c:	73 74 
  40070e:	0a 00                	or     (%rax),%al
  400710:	45 6e                	rex.RB outsb %ds:(%rsi),(%dx)
  400712:	64 20 54 65 73       	and    %dl,%fs:0x73(%rbp,%riz,2)
  400717:	74 0a                	je     400723 <_IO_stdin_used+0x23>
  400719:	00 46 4f             	add    %al,0x4f(%rsi)
  40071c:	4f 28 29             	rex.WRXB sub %r13b,(%r9)
  40071f:	20 2d 20 45 78 61    	and    %ch,0x61784520(%rip)        # 61b84c45 <_end+0x61583c05>
  400725:	6d                   	insl   (%dx),%es:(%rdi)
  400726:	70 6c                	jo     400794 <__GNU_EH_FRAME_HDR+0x54>
  400728:	65 20 6f 66          	and    %ch,%gs:0x66(%rdi)
  40072c:	20 53 74             	and    %dl,0x74(%rbx)
  40072f:	61                   	(bad)  
  400730:	74 69                	je     40079b <__GNU_EH_FRAME_HDR+0x5b>
  400732:	63 20                	movslq (%rax),%esp
  400734:	4c 69 62 72 61 72 79 	imul   $0xa797261,0x72(%rdx),%r12
  40073b:	0a 
	...

Disassembly of section .eh_frame_hdr:

0000000000400740 <__GNU_EH_FRAME_HDR>:
  400740:	01 1b                	add    %ebx,(%rbx)
  400742:	03 3b                	add    (%rbx),%edi
  400744:	44 00 00             	add    %r8b,(%rax)
  400747:	00 07                	add    %al,(%rdi)
  400749:	00 00                	add    %al,(%rax)
  40074b:	00 a0 fd ff ff a0    	add    %ah,-0x5f000003(%rax)
  400751:	00 00                	add    %al,(%rax)
  400753:	00 d0                	add    %dl,%al
  400755:	fd                   	std    
  400756:	ff                   	(bad)  
  400757:	ff 60 00             	jmpq   *0x0(%rax)
  40075a:	00 00                	add    %al,(%rax)
  40075c:	00 fe                	add    %bh,%dh
  40075e:	ff                   	(bad)  
  40075f:	ff 8c 00 00 00 c0 fe 	decl   -0x1400000(%rax,%rax,1)
  400766:	ff                   	(bad)  
  400767:	ff c8                	dec    %eax
  400769:	00 00                	add    %al,(%rax)
  40076b:	00 10                	add    %dl,(%rax)
  40076d:	ff                   	(bad)  
  40076e:	ff                   	(bad)  
  40076f:	ff                   	(bad)  
  400770:	e8 00 00 00 40       	callq  40400775 <_end+0x3fdff735>
  400775:	ff                   	(bad)  
  400776:	ff                   	(bad)  
  400777:	ff 08                	decl   (%rax)
  400779:	01 00                	add    %eax,(%rax)
  40077b:	00 b0 ff ff ff 50    	add    %dh,0x50ffffff(%rax)
  400781:	01 00                	add    %eax,(%rax)
	...

Disassembly of section .eh_frame:

0000000000400788 <__FRAME_END__-0x11c>:
  400788:	14 00                	adc    $0x0,%al
  40078a:	00 00                	add    %al,(%rax)
  40078c:	00 00                	add    %al,(%rax)
  40078e:	00 00                	add    %al,(%rax)
  400790:	01 7a 52             	add    %edi,0x52(%rdx)
  400793:	00 01                	add    %al,(%rcx)
  400795:	78 10                	js     4007a7 <__GNU_EH_FRAME_HDR+0x67>
  400797:	01 1b                	add    %ebx,(%rbx)
  400799:	0c 07                	or     $0x7,%al
  40079b:	08 90 01 07 10 10    	or     %dl,0x10100701(%rax)
  4007a1:	00 00                	add    %al,(%rax)
  4007a3:	00 1c 00             	add    %bl,(%rax,%rax,1)
  4007a6:	00 00                	add    %al,(%rax)
  4007a8:	68 fd ff ff 2b       	pushq  $0x2bfffffd
  4007ad:	00 00                	add    %al,(%rax)
  4007af:	00 00                	add    %al,(%rax)
  4007b1:	00 00                	add    %al,(%rax)
  4007b3:	00 14 00             	add    %dl,(%rax,%rax,1)
  4007b6:	00 00                	add    %al,(%rax)
  4007b8:	00 00                	add    %al,(%rax)
  4007ba:	00 00                	add    %al,(%rax)
  4007bc:	01 7a 52             	add    %edi,0x52(%rdx)
  4007bf:	00 01                	add    %al,(%rcx)
  4007c1:	78 10                	js     4007d3 <__GNU_EH_FRAME_HDR+0x93>
  4007c3:	01 1b                	add    %ebx,(%rbx)
  4007c5:	0c 07                	or     $0x7,%al
  4007c7:	08 90 01 00 00 10    	or     %dl,0x10000001(%rax)
  4007cd:	00 00                	add    %al,(%rax)
  4007cf:	00 1c 00             	add    %bl,(%rax,%rax,1)
  4007d2:	00 00                	add    %al,(%rax)
  4007d4:	6c                   	insb   (%dx),%es:(%rdi)
  4007d5:	fd                   	std    
  4007d6:	ff                   	(bad)  
  4007d7:	ff 02                	incl   (%rdx)
  4007d9:	00 00                	add    %al,(%rax)
  4007db:	00 00                	add    %al,(%rax)
  4007dd:	00 00                	add    %al,(%rax)
  4007df:	00 24 00             	add    %ah,(%rax,%rax,1)
  4007e2:	00 00                	add    %al,(%rax)
  4007e4:	30 00                	xor    %al,(%rax)
  4007e6:	00 00                	add    %al,(%rax)
  4007e8:	f8                   	clc    
  4007e9:	fc                   	cld    
  4007ea:	ff                   	(bad)  
  4007eb:	ff 30                	pushq  (%rax)
  4007ed:	00 00                	add    %al,(%rax)
  4007ef:	00 00                	add    %al,(%rax)
  4007f1:	0e                   	(bad)  
  4007f2:	10 46 0e             	adc    %al,0xe(%rsi)
  4007f5:	18 4a 0f             	sbb    %cl,0xf(%rdx)
  4007f8:	0b 77 08             	or     0x8(%rdi),%esi
  4007fb:	80 00 3f             	addb   $0x3f,(%rax)
  4007fe:	1a 3b                	sbb    (%rbx),%bh
  400800:	2a 33                	sub    (%rbx),%dh
  400802:	24 22                	and    $0x22,%al
  400804:	00 00                	add    %al,(%rax)
  400806:	00 00                	add    %al,(%rax)
  400808:	1c 00                	sbb    $0x0,%al
  40080a:	00 00                	add    %al,(%rax)
  40080c:	58                   	pop    %rax
  40080d:	00 00                	add    %al,(%rax)
  40080f:	00 f0                	add    %dh,%al
  400811:	fd                   	std    
  400812:	ff                   	(bad)  
  400813:	ff 4f 00             	decl   0x0(%rdi)
  400816:	00 00                	add    %al,(%rax)
  400818:	00 41 0e             	add    %al,0xe(%rcx)
  40081b:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  400821:	00 00                	add    %al,(%rax)
  400823:	00 00                	add    %al,(%rax)
  400825:	00 00                	add    %al,(%rax)
  400827:	00 1c 00             	add    %bl,(%rax,%rax,1)
  40082a:	00 00                	add    %al,(%rax)
  40082c:	78 00                	js     40082e <__GNU_EH_FRAME_HDR+0xee>
  40082e:	00 00                	add    %al,(%rax)
  400830:	20 fe                	and    %bh,%dh
  400832:	ff                   	(bad)  
  400833:	ff 22                	jmpq   *(%rdx)
  400835:	00 00                	add    %al,(%rax)
  400837:	00 00                	add    %al,(%rax)
  400839:	41 0e                	rex.B (bad) 
  40083b:	10 86 02 43 0d 06    	adc    %al,0x60d4302(%rsi)
  400841:	00 00                	add    %al,(%rax)
  400843:	00 00                	add    %al,(%rax)
  400845:	00 00                	add    %al,(%rax)
  400847:	00 44 00 00          	add    %al,0x0(%rax,%rax,1)
  40084b:	00 98 00 00 00 30    	add    %bl,0x30000000(%rax)
  400851:	fe                   	(bad)  
  400852:	ff                   	(bad)  
  400853:	ff 65 00             	jmpq   *0x0(%rbp)
  400856:	00 00                	add    %al,(%rax)
  400858:	00 42 0e             	add    %al,0xe(%rdx)
  40085b:	10 8f 02 42 0e 18    	adc    %cl,0x180e4202(%rdi)
  400861:	8e 03                	mov    (%rbx),%es
  400863:	45 0e                	rex.RB (bad) 
  400865:	20 8d 04 42 0e 28    	and    %cl,0x280e4204(%rbp)
  40086b:	8c 05 48 0e 30 86    	mov    %es,-0x79cff1b8(%rip)        # ffffffff867016b9 <_end+0xffffffff86100679>
  400871:	06                   	(bad)  
  400872:	48 0e                	rex.W (bad) 
  400874:	38 83 07 4d 0e 40    	cmp    %al,0x400e4d07(%rbx)
  40087a:	72 0e                	jb     40088a <__GNU_EH_FRAME_HDR+0x14a>
  40087c:	38 41 0e             	cmp    %al,0xe(%rcx)
  40087f:	30 41 0e             	xor    %al,0xe(%rcx)
  400882:	28 42 0e             	sub    %al,0xe(%rdx)
  400885:	20 42 0e             	and    %al,0xe(%rdx)
  400888:	18 42 0e             	sbb    %al,0xe(%rdx)
  40088b:	10 42 0e             	adc    %al,0xe(%rdx)
  40088e:	08 00                	or     %al,(%rax)
  400890:	10 00                	adc    %al,(%rax)
  400892:	00 00                	add    %al,(%rax)
  400894:	e0 00                	loopne 400896 <__GNU_EH_FRAME_HDR+0x156>
  400896:	00 00                	add    %al,(%rax)
  400898:	58                   	pop    %rax
  400899:	fe                   	(bad)  
  40089a:	ff                   	(bad)  
  40089b:	ff 02                	incl   (%rdx)
  40089d:	00 00                	add    %al,(%rax)
  40089f:	00 00                	add    %al,(%rax)
  4008a1:	00 00                	add    %al,(%rax)
	...

00000000004008a4 <__FRAME_END__>:
  4008a4:	00 00                	add    %al,(%rax)
	...

Disassembly of section .init_array:

0000000000600e00 <__frame_dummy_init_array_entry>:
  600e00:	f0 05 40 00 00 00    	lock add $0x40,%eax
	...

Disassembly of section .fini_array:

0000000000600e08 <__do_global_dtors_aux_fini_array_entry>:
  600e08:	c0 05 40 00 00 00 00 	rolb   $0x0,0x40(%rip)        # 600e4f <_DYNAMIC+0x3f>
	...

Disassembly of section .dynamic:

0000000000600e10 <_DYNAMIC>:
  600e10:	01 00                	add    %eax,(%rax)
  600e12:	00 00                	add    %al,(%rax)
  600e14:	00 00                	add    %al,(%rax)
  600e16:	00 00                	add    %al,(%rax)
  600e18:	01 00                	add    %eax,(%rax)
  600e1a:	00 00                	add    %al,(%rax)
  600e1c:	00 00                	add    %al,(%rax)
  600e1e:	00 00                	add    %al,(%rax)
  600e20:	01 00                	add    %eax,(%rax)
  600e22:	00 00                	add    %al,(%rax)
  600e24:	00 00                	add    %al,(%rax)
  600e26:	00 00                	add    %al,(%rax)
  600e28:	4d 00 00             	rex.WRB add %r8b,(%r8)
  600e2b:	00 00                	add    %al,(%rax)
  600e2d:	00 00                	add    %al,(%rax)
  600e2f:	00 0c 00             	add    %cl,(%rax,%rax,1)
  600e32:	00 00                	add    %al,(%rax)
  600e34:	00 00                	add    %al,(%rax)
  600e36:	00 00                	add    %al,(%rax)
  600e38:	c8 04 40 00          	enterq $0x4004,$0x0
  600e3c:	00 00                	add    %al,(%rax)
  600e3e:	00 00                	add    %al,(%rax)
  600e40:	0d 00 00 00 00       	or     $0x0,%eax
  600e45:	00 00                	add    %al,(%rax)
  600e47:	00 f4                	add    %dh,%ah
  600e49:	06                   	(bad)  
  600e4a:	40 00 00             	add    %al,(%rax)
  600e4d:	00 00                	add    %al,(%rax)
  600e4f:	00 19                	add    %bl,(%rcx)
	...
  600e59:	0e                   	(bad)  
  600e5a:	60                   	(bad)  
  600e5b:	00 00                	add    %al,(%rax)
  600e5d:	00 00                	add    %al,(%rax)
  600e5f:	00 1b                	add    %bl,(%rbx)
  600e61:	00 00                	add    %al,(%rax)
  600e63:	00 00                	add    %al,(%rax)
  600e65:	00 00                	add    %al,(%rax)
  600e67:	00 08                	add    %cl,(%rax)
  600e69:	00 00                	add    %al,(%rax)
  600e6b:	00 00                	add    %al,(%rax)
  600e6d:	00 00                	add    %al,(%rax)
  600e6f:	00 1a                	add    %bl,(%rdx)
  600e71:	00 00                	add    %al,(%rax)
  600e73:	00 00                	add    %al,(%rax)
  600e75:	00 00                	add    %al,(%rax)
  600e77:	00 08                	add    %cl,(%rax)
  600e79:	0e                   	(bad)  
  600e7a:	60                   	(bad)  
  600e7b:	00 00                	add    %al,(%rax)
  600e7d:	00 00                	add    %al,(%rax)
  600e7f:	00 1c 00             	add    %bl,(%rax,%rax,1)
  600e82:	00 00                	add    %al,(%rax)
  600e84:	00 00                	add    %al,(%rax)
  600e86:	00 00                	add    %al,(%rax)
  600e88:	08 00                	or     %al,(%rax)
  600e8a:	00 00                	add    %al,(%rax)
  600e8c:	00 00                	add    %al,(%rax)
  600e8e:	00 00                	add    %al,(%rax)
  600e90:	f5                   	cmc    
  600e91:	fe                   	(bad)  
  600e92:	ff 6f 00             	ljmp   *0x0(%rdi)
  600e95:	00 00                	add    %al,(%rax)
  600e97:	00 78 02             	add    %bh,0x2(%rax)
  600e9a:	40 00 00             	add    %al,(%rax)
  600e9d:	00 00                	add    %al,(%rax)
  600e9f:	00 05 00 00 00 00    	add    %al,0x0(%rip)        # 600ea5 <_DYNAMIC+0x95>
  600ea5:	00 00                	add    %al,(%rax)
  600ea7:	00 a0 03 40 00 00    	add    %ah,0x4003(%rax)
  600ead:	00 00                	add    %al,(%rax)
  600eaf:	00 06                	add    %al,(%rsi)
  600eb1:	00 00                	add    %al,(%rax)
  600eb3:	00 00                	add    %al,(%rax)
  600eb5:	00 00                	add    %al,(%rax)
  600eb7:	00 b0 02 40 00 00    	add    %dh,0x4002(%rax)
  600ebd:	00 00                	add    %al,(%rax)
  600ebf:	00 0a                	add    %cl,(%rdx)
  600ec1:	00 00                	add    %al,(%rax)
  600ec3:	00 00                	add    %al,(%rax)
  600ec5:	00 00                	add    %al,(%rax)
  600ec7:	00 94 00 00 00 00 00 	add    %dl,0x0(%rax,%rax,1)
  600ece:	00 00                	add    %al,(%rax)
  600ed0:	0b 00                	or     (%rax),%eax
  600ed2:	00 00                	add    %al,(%rax)
  600ed4:	00 00                	add    %al,(%rax)
  600ed6:	00 00                	add    %al,(%rax)
  600ed8:	18 00                	sbb    %al,(%rax)
  600eda:	00 00                	add    %al,(%rax)
  600edc:	00 00                	add    %al,(%rax)
  600ede:	00 00                	add    %al,(%rax)
  600ee0:	15 00 00 00 00       	adc    $0x0,%eax
	...
  600eed:	00 00                	add    %al,(%rax)
  600eef:	00 03                	add    %al,(%rbx)
	...
  600ef9:	10 60 00             	adc    %ah,0x0(%rax)
  600efc:	00 00                	add    %al,(%rax)
  600efe:	00 00                	add    %al,(%rax)
  600f00:	02 00                	add    (%rax),%al
  600f02:	00 00                	add    %al,(%rax)
  600f04:	00 00                	add    %al,(%rax)
  600f06:	00 00                	add    %al,(%rax)
  600f08:	30 00                	xor    %al,(%rax)
  600f0a:	00 00                	add    %al,(%rax)
  600f0c:	00 00                	add    %al,(%rax)
  600f0e:	00 00                	add    %al,(%rax)
  600f10:	14 00                	adc    $0x0,%al
  600f12:	00 00                	add    %al,(%rax)
  600f14:	00 00                	add    %al,(%rax)
  600f16:	00 00                	add    %al,(%rax)
  600f18:	07                   	(bad)  
  600f19:	00 00                	add    %al,(%rax)
  600f1b:	00 00                	add    %al,(%rax)
  600f1d:	00 00                	add    %al,(%rax)
  600f1f:	00 17                	add    %dl,(%rdi)
  600f21:	00 00                	add    %al,(%rax)
  600f23:	00 00                	add    %al,(%rax)
  600f25:	00 00                	add    %al,(%rax)
  600f27:	00 98 04 40 00 00    	add    %bl,0x4004(%rax)
  600f2d:	00 00                	add    %al,(%rax)
  600f2f:	00 07                	add    %al,(%rdi)
  600f31:	00 00                	add    %al,(%rax)
  600f33:	00 00                	add    %al,(%rax)
  600f35:	00 00                	add    %al,(%rax)
  600f37:	00 68 04             	add    %ch,0x4(%rax)
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
  600f67:	00 48 04             	add    %cl,0x4(%rax)
  600f6a:	40 00 00             	add    %al,(%rax)
  600f6d:	00 00                	add    %al,(%rax)
  600f6f:	00 ff                	add    %bh,%bh
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
  600f87:	00 34 04             	add    %dh,(%rsp,%rax,1)
  600f8a:	40 00 00             	add    %al,(%rax)
	...

Disassembly of section .got:

0000000000600ff0 <.got>:
	...

Disassembly of section .got.plt:

0000000000601000 <_GLOBAL_OFFSET_TABLE_>:
  601000:	10 0e                	adc    %cl,(%rsi)
  601002:	60                   	(bad)  
	...
  601017:	00 f6                	add    %dh,%dh
  601019:	04 40                	add    $0x40,%al
  60101b:	00 00                	add    %al,(%rax)
  60101d:	00 00                	add    %al,(%rax)
  60101f:	00 06                	add    %al,(%rsi)
  601021:	05 40 00 00 00       	add    $0x40,%eax
	...

Disassembly of section .data:

0000000000601028 <__data_start>:
	...

0000000000601030 <__dso_handle>:
	...

Disassembly of section .bss:

0000000000601038 <__bss_start>:
	...

Disassembly of section .comment:

0000000000000000 <.comment>:
   0:	47                   	rex.RXB
   1:	43                   	rex.XB
   2:	43 3a 20             	rex.XB cmp (%r8),%spl
   5:	28 55 62             	sub    %dl,0x62(%rbp)
   8:	75 6e                	jne    78 <_init-0x400450>
   a:	74 75                	je     81 <_init-0x400447>
   c:	20 37                	and    %dh,(%rdi)
   e:	2e 33 2e             	xor    %cs:(%rsi),%ebp
  11:	30 2d 31 36 75 62    	xor    %ch,0x62753631(%rip)        # 62753648 <_end+0x62152608>
  17:	75 6e                	jne    87 <_init-0x400441>
  19:	74 75                	je     90 <_init-0x400438>
  1b:	33 29                	xor    (%rcx),%ebp
  1d:	20 37                	and    %dh,(%rdi)
  1f:	2e 33 2e             	xor    %cs:(%rsi),%ebp
  22:	30 00                	xor    %al,(%rax)
  24:	63 6c 61 6e          	movslq 0x6e(%rcx,%riz,2),%ebp
  28:	67 20 76 65          	and    %dh,0x65(%esi)
  2c:	72 73                	jb     a1 <_init-0x400427>
  2e:	69 6f 6e 20 36 2e 30 	imul   $0x302e3620,0x6e(%rdi),%ebp
  35:	2e 30 2d 31 75 62 75 	xor    %ch,%cs:0x75627531(%rip)        # 7562756d <_end+0x7502652d>
  3c:	6e                   	outsb  %ds:(%rsi),(%dx)
  3d:	74 75                	je     b4 <_init-0x400414>
  3f:	32 20                	xor    (%rax),%ah
  41:	28 74 61 67          	sub    %dh,0x67(%rcx,%riz,2)
  45:	73 2f                	jae    76 <_init-0x400452>
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
