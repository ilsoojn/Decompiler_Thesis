
./sample/sbb:     file format elf64-x86-64


Disassembly of section .interp:

0000000000400238 <.interp>:
  400238:	2f                   	(bad)  
  400239:	6c                   	ins    BYTE PTR es:[rdi],dx
  40023a:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
  400241:	2d 6c 69 6e 75       	sub    eax,0x756e696c
  400246:	78 2d                	js     400275 <_init-0xfb>
  400248:	78 38                	js     400282 <_init-0xee>
  40024a:	36 2d 36 34 2e 73    	ss sub eax,0x732e3436
  400250:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400251:	2e 32 00             	xor    al,BYTE PTR cs:[rax]

Disassembly of section .note.ABI-tag:

0000000000400254 <.note.ABI-tag>:
  400254:	04 00                	add    al,0x0
  400256:	00 00                	add    BYTE PTR [rax],al
  400258:	10 00                	adc    BYTE PTR [rax],al
  40025a:	00 00                	add    BYTE PTR [rax],al
  40025c:	01 00                	add    DWORD PTR [rax],eax
  40025e:	00 00                	add    BYTE PTR [rax],al
  400260:	47                   	rex.RXB
  400261:	4e 55                	rex.WRX push rbp
  400263:	00 00                	add    BYTE PTR [rax],al
  400265:	00 00                	add    BYTE PTR [rax],al
  400267:	00 03                	add    BYTE PTR [rbx],al
  400269:	00 00                	add    BYTE PTR [rax],al
  40026b:	00 02                	add    BYTE PTR [rdx],al
  40026d:	00 00                	add    BYTE PTR [rax],al
  40026f:	00 00                	add    BYTE PTR [rax],al
  400271:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .gnu.hash:

0000000000400278 <.gnu.hash>:
  400278:	01 00                	add    DWORD PTR [rax],eax
  40027a:	00 00                	add    BYTE PTR [rax],al
  40027c:	01 00                	add    DWORD PTR [rax],eax
  40027e:	00 00                	add    BYTE PTR [rax],al
  400280:	01 00                	add    DWORD PTR [rax],eax
	...

Disassembly of section .dynsym:

0000000000400298 <.dynsym>:
	...
  4002b0:	0b 00                	or     eax,DWORD PTR [rax]
  4002b2:	00 00                	add    BYTE PTR [rax],al
  4002b4:	12 00                	adc    al,BYTE PTR [rax]
	...
  4002c6:	00 00                	add    BYTE PTR [rax],al
  4002c8:	29 00                	sub    DWORD PTR [rax],eax
  4002ca:	00 00                	add    BYTE PTR [rax],al
  4002cc:	20 00                	and    BYTE PTR [rax],al
	...

Disassembly of section .dynstr:

00000000004002e0 <.dynstr>:
  4002e0:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  4002e4:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  4002e6:	73 6f                	jae    400357 <_init-0x19>
  4002e8:	2e 36 00 5f 5f       	cs add BYTE PTR ss:[rdi+0x5f],bl
  4002ed:	6c                   	ins    BYTE PTR es:[rdi],dx
  4002ee:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  4002f5:	72 74                	jb     40036b <_init-0x5>
  4002f7:	5f                   	pop    rdi
  4002f8:	6d                   	ins    DWORD PTR es:[rdi],dx
  4002f9:	61                   	(bad)  
  4002fa:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  400301:	43 5f                	rex.XB pop r15
  400303:	32 2e                	xor    ch,BYTE PTR [rsi]
  400305:	32 2e                	xor    ch,BYTE PTR [rsi]
  400307:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  40030c:	6d                   	ins    DWORD PTR es:[rdi],dx
  40030d:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  40030e:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40030f:	5f                   	pop    rdi
  400310:	73 74                	jae    400386 <_init+0x16>
  400312:	61                   	(bad)  
  400313:	72 74                	jb     400389 <_init+0x19>
  400315:	5f                   	pop    rdi
  400316:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

0000000000400318 <.gnu.version>:
  400318:	00 00                	add    BYTE PTR [rax],al
  40031a:	02 00                	add    al,BYTE PTR [rax]
	...

Disassembly of section .gnu.version_r:

0000000000400320 <.gnu.version_r>:
  400320:	01 00                	add    DWORD PTR [rax],eax
  400322:	01 00                	add    DWORD PTR [rax],eax
  400324:	01 00                	add    DWORD PTR [rax],eax
  400326:	00 00                	add    BYTE PTR [rax],al
  400328:	10 00                	adc    BYTE PTR [rax],al
  40032a:	00 00                	add    BYTE PTR [rax],al
  40032c:	00 00                	add    BYTE PTR [rax],al
  40032e:	00 00                	add    BYTE PTR [rax],al
  400330:	75 1a                	jne    40034c <_init-0x24>
  400332:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
  400338:	1d 00 00 00 00       	sbb    eax,0x0
  40033d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .rela.dyn:

0000000000400340 <.rela.dyn>:
  400340:	f0 0f 60 00          	lock punpcklbw mm0,DWORD PTR [rax]
  400344:	00 00                	add    BYTE PTR [rax],al
  400346:	00 00                	add    BYTE PTR [rax],al
  400348:	06                   	(bad)  
  400349:	00 00                	add    BYTE PTR [rax],al
  40034b:	00 01                	add    BYTE PTR [rcx],al
	...
  400355:	00 00                	add    BYTE PTR [rax],al
  400357:	00 f8                	add    al,bh
  400359:	0f 60 00             	punpcklbw mm0,DWORD PTR [rax]
  40035c:	00 00                	add    BYTE PTR [rax],al
  40035e:	00 00                	add    BYTE PTR [rax],al
  400360:	06                   	(bad)  
  400361:	00 00                	add    BYTE PTR [rax],al
  400363:	00 02                	add    BYTE PTR [rdx],al
	...

Disassembly of section .init:

0000000000400370 <_init>:
  400370:	48 83 ec 08          	sub    rsp,0x8
  400374:	48 8b 05 7d 0c 20 00 	mov    rax,QWORD PTR [rip+0x200c7d]        # 600ff8 <__gmon_start__>
  40037b:	48 85 c0             	test   rax,rax
  40037e:	74 02                	je     400382 <_init+0x12>
  400380:	ff d0                	call   rax
  400382:	48 83 c4 08          	add    rsp,0x8
  400386:	c3                   	ret    

Disassembly of section .text:

0000000000400390 <_start>:
  400390:	31 ed                	xor    ebp,ebp
  400392:	49 89 d1             	mov    r9,rdx
  400395:	5e                   	pop    rsi
  400396:	48 89 e2             	mov    rdx,rsp
  400399:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
  40039d:	50                   	push   rax
  40039e:	54                   	push   rsp
  40039f:	49 c7 c0 40 05 40 00 	mov    r8,0x400540
  4003a6:	48 c7 c1 d0 04 40 00 	mov    rcx,0x4004d0
  4003ad:	48 c7 c7 80 04 40 00 	mov    rdi,0x400480
  4003b4:	ff 15 36 0c 20 00    	call   QWORD PTR [rip+0x200c36]        # 600ff0 <__libc_start_main@GLIBC_2.2.5>
  4003ba:	f4                   	hlt    
  4003bb:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

00000000004003c0 <_dl_relocate_static_pie>:
  4003c0:	f3 c3                	repz ret 
  4003c2:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  4003c9:	00 00 00 
  4003cc:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

00000000004003d0 <deregister_tm_clones>:
  4003d0:	55                   	push   rbp
  4003d1:	b8 28 10 60 00       	mov    eax,0x601028
  4003d6:	48 3d 28 10 60 00    	cmp    rax,0x601028
  4003dc:	48 89 e5             	mov    rbp,rsp
  4003df:	74 17                	je     4003f8 <deregister_tm_clones+0x28>
  4003e1:	b8 00 00 00 00       	mov    eax,0x0
  4003e6:	48 85 c0             	test   rax,rax
  4003e9:	74 0d                	je     4003f8 <deregister_tm_clones+0x28>
  4003eb:	5d                   	pop    rbp
  4003ec:	bf 28 10 60 00       	mov    edi,0x601028
  4003f1:	ff e0                	jmp    rax
  4003f3:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]
  4003f8:	5d                   	pop    rbp
  4003f9:	c3                   	ret    
  4003fa:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]

0000000000400400 <register_tm_clones>:
  400400:	be 28 10 60 00       	mov    esi,0x601028
  400405:	55                   	push   rbp
  400406:	48 81 ee 28 10 60 00 	sub    rsi,0x601028
  40040d:	48 89 e5             	mov    rbp,rsp
  400410:	48 c1 fe 03          	sar    rsi,0x3
  400414:	48 89 f0             	mov    rax,rsi
  400417:	48 c1 e8 3f          	shr    rax,0x3f
  40041b:	48 01 c6             	add    rsi,rax
  40041e:	48 d1 fe             	sar    rsi,1
  400421:	74 15                	je     400438 <register_tm_clones+0x38>
  400423:	b8 00 00 00 00       	mov    eax,0x0
  400428:	48 85 c0             	test   rax,rax
  40042b:	74 0b                	je     400438 <register_tm_clones+0x38>
  40042d:	5d                   	pop    rbp
  40042e:	bf 28 10 60 00       	mov    edi,0x601028
  400433:	ff e0                	jmp    rax
  400435:	0f 1f 00             	nop    DWORD PTR [rax]
  400438:	5d                   	pop    rbp
  400439:	c3                   	ret    
  40043a:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]

0000000000400440 <__do_global_dtors_aux>:
  400440:	80 3d e1 0b 20 00 00 	cmp    BYTE PTR [rip+0x200be1],0x0        # 601028 <__TMC_END__>
  400447:	75 17                	jne    400460 <__do_global_dtors_aux+0x20>
  400449:	55                   	push   rbp
  40044a:	48 89 e5             	mov    rbp,rsp
  40044d:	e8 7e ff ff ff       	call   4003d0 <deregister_tm_clones>
  400452:	c6 05 cf 0b 20 00 01 	mov    BYTE PTR [rip+0x200bcf],0x1        # 601028 <__TMC_END__>
  400459:	5d                   	pop    rbp
  40045a:	c3                   	ret    
  40045b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]
  400460:	f3 c3                	repz ret 
  400462:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]
  400466:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  40046d:	00 00 00 

0000000000400470 <frame_dummy>:
  400470:	55                   	push   rbp
  400471:	48 89 e5             	mov    rbp,rsp
  400474:	5d                   	pop    rbp
  400475:	eb 89                	jmp    400400 <register_tm_clones>
  400477:	66 0f 1f 84 00 00 00 	nop    WORD PTR [rax+rax*1+0x0]
  40047e:	00 00 

0000000000400480 <main>:
  400480:	55                   	push   rbp
  400481:	48 89 e5             	mov    rbp,rsp
  400484:	31 c0                	xor    eax,eax
  400486:	f2 0f 10 05 ca 00 00 	movsd  xmm0,QWORD PTR [rip+0xca]        # 400558 <_IO_stdin_used+0x8>
  40048d:	00 
  40048e:	f2 0f 10 0d ca 00 00 	movsd  xmm1,QWORD PTR [rip+0xca]        # 400560 <_IO_stdin_used+0x10>
  400495:	00 
  400496:	c7 45 fc 00 00 00 00 	mov    DWORD PTR [rbp-0x4],0x0
  40049d:	c7 45 f8 0a 00 00 00 	mov    DWORD PTR [rbp-0x8],0xa
  4004a4:	f2 0f 11 4d f0       	movsd  QWORD PTR [rbp-0x10],xmm1
  4004a9:	f2 0f 11 45 e8       	movsd  QWORD PTR [rbp-0x18],xmm0
  4004ae:	8b 4d f8             	mov    ecx,DWORD PTR [rbp-0x8]
  4004b1:	f2 0f 2a c1          	cvtsi2sd xmm0,ecx
  4004b5:	f2 0f 59 45 e8       	mulsd  xmm0,QWORD PTR [rbp-0x18]
  4004ba:	f2 0f 58 45 f0       	addsd  xmm0,QWORD PTR [rbp-0x10]
  4004bf:	f2 0f 11 45 e0       	movsd  QWORD PTR [rbp-0x20],xmm0
  4004c4:	5d                   	pop    rbp
  4004c5:	c3                   	ret    
  4004c6:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  4004cd:	00 00 00 

00000000004004d0 <__libc_csu_init>:
  4004d0:	41 57                	push   r15
  4004d2:	41 56                	push   r14
  4004d4:	49 89 d7             	mov    r15,rdx
  4004d7:	41 55                	push   r13
  4004d9:	41 54                	push   r12
  4004db:	4c 8d 25 6e 09 20 00 	lea    r12,[rip+0x20096e]        # 600e50 <__frame_dummy_init_array_entry>
  4004e2:	55                   	push   rbp
  4004e3:	48 8d 2d 6e 09 20 00 	lea    rbp,[rip+0x20096e]        # 600e58 <__init_array_end>
  4004ea:	53                   	push   rbx
  4004eb:	41 89 fd             	mov    r13d,edi
  4004ee:	49 89 f6             	mov    r14,rsi
  4004f1:	4c 29 e5             	sub    rbp,r12
  4004f4:	48 83 ec 08          	sub    rsp,0x8
  4004f8:	48 c1 fd 03          	sar    rbp,0x3
  4004fc:	e8 6f fe ff ff       	call   400370 <_init>
  400501:	48 85 ed             	test   rbp,rbp
  400504:	74 20                	je     400526 <__libc_csu_init+0x56>
  400506:	31 db                	xor    ebx,ebx
  400508:	0f 1f 84 00 00 00 00 	nop    DWORD PTR [rax+rax*1+0x0]
  40050f:	00 
  400510:	4c 89 fa             	mov    rdx,r15
  400513:	4c 89 f6             	mov    rsi,r14
  400516:	44 89 ef             	mov    edi,r13d
  400519:	41 ff 14 dc          	call   QWORD PTR [r12+rbx*8]
  40051d:	48 83 c3 01          	add    rbx,0x1
  400521:	48 39 dd             	cmp    rbp,rbx
  400524:	75 ea                	jne    400510 <__libc_csu_init+0x40>
  400526:	48 83 c4 08          	add    rsp,0x8
  40052a:	5b                   	pop    rbx
  40052b:	5d                   	pop    rbp
  40052c:	41 5c                	pop    r12
  40052e:	41 5d                	pop    r13
  400530:	41 5e                	pop    r14
  400532:	41 5f                	pop    r15
  400534:	c3                   	ret    
  400535:	90                   	nop
  400536:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
  40053d:	00 00 00 

0000000000400540 <__libc_csu_fini>:
  400540:	f3 c3                	repz ret 

Disassembly of section .fini:

0000000000400544 <_fini>:
  400544:	48 83 ec 08          	sub    rsp,0x8
  400548:	48 83 c4 08          	add    rsp,0x8
  40054c:	c3                   	ret    

Disassembly of section .rodata:

0000000000400550 <_IO_stdin_used>:
  400550:	01 00                	add    DWORD PTR [rax],eax
  400552:	02 00                	add    al,BYTE PTR [rax]
	...
  40055c:	00 00                	add    BYTE PTR [rax],al
  40055e:	24 40                	and    al,0x40
  400560:	a4                   	movs   BYTE PTR es:[rdi],BYTE PTR ds:[rsi]
  400561:	70 3d                	jo     4005a0 <__GNU_EH_FRAME_HDR+0x38>
  400563:	0a d7                	or     dl,bh
  400565:	93                   	xchg   ebx,eax
  400566:	73 40                	jae    4005a8 <__GNU_EH_FRAME_HDR+0x40>

Disassembly of section .eh_frame_hdr:

0000000000400568 <__GNU_EH_FRAME_HDR>:
  400568:	01 1b                	add    DWORD PTR [rbx],ebx
  40056a:	03 3b                	add    edi,DWORD PTR [rbx]
  40056c:	34 00                	xor    al,0x0
  40056e:	00 00                	add    BYTE PTR [rax],al
  400570:	05 00 00 00 28       	add    eax,0x28000000
  400575:	fe                   	(bad)  
  400576:	ff                   	(bad)  
  400577:	ff 50 00             	call   QWORD PTR [rax+0x0]
  40057a:	00 00                	add    BYTE PTR [rax],al
  40057c:	58                   	pop    rax
  40057d:	fe                   	(bad)  
  40057e:	ff                   	(bad)  
  40057f:	ff                   	(bad)  
  400580:	7c 00                	jl     400582 <__GNU_EH_FRAME_HDR+0x1a>
  400582:	00 00                	add    BYTE PTR [rax],al
  400584:	18 ff                	sbb    bh,bh
  400586:	ff                   	(bad)  
  400587:	ff 90 00 00 00 68    	call   QWORD PTR [rax+0x68000000]
  40058d:	ff                   	(bad)  
  40058e:	ff                   	(bad)  
  40058f:	ff b0 00 00 00 d8    	push   QWORD PTR [rax-0x28000000]
  400595:	ff                   	(bad)  
  400596:	ff                   	(bad)  
  400597:	ff                   	(bad)  
  400598:	f8                   	clc    
  400599:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .eh_frame:

00000000004005a0 <__FRAME_END__-0xd4>:
  4005a0:	14 00                	adc    al,0x0
  4005a2:	00 00                	add    BYTE PTR [rax],al
  4005a4:	00 00                	add    BYTE PTR [rax],al
  4005a6:	00 00                	add    BYTE PTR [rax],al
  4005a8:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  4005ab:	00 01                	add    BYTE PTR [rcx],al
  4005ad:	78 10                	js     4005bf <__GNU_EH_FRAME_HDR+0x57>
  4005af:	01 1b                	add    DWORD PTR [rbx],ebx
  4005b1:	0c 07                	or     al,0x7
  4005b3:	08 90 01 07 10 10    	or     BYTE PTR [rax+0x10100701],dl
  4005b9:	00 00                	add    BYTE PTR [rax],al
  4005bb:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4005be:	00 00                	add    BYTE PTR [rax],al
  4005c0:	d0 fd                	sar    ch,1
  4005c2:	ff                   	(bad)  
  4005c3:	ff 2b                	jmp    FWORD PTR [rbx]
  4005c5:	00 00                	add    BYTE PTR [rax],al
  4005c7:	00 00                	add    BYTE PTR [rax],al
  4005c9:	00 00                	add    BYTE PTR [rax],al
  4005cb:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
  4005ce:	00 00                	add    BYTE PTR [rax],al
  4005d0:	00 00                	add    BYTE PTR [rax],al
  4005d2:	00 00                	add    BYTE PTR [rax],al
  4005d4:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
  4005d7:	00 01                	add    BYTE PTR [rcx],al
  4005d9:	78 10                	js     4005eb <__GNU_EH_FRAME_HDR+0x83>
  4005db:	01 1b                	add    DWORD PTR [rbx],ebx
  4005dd:	0c 07                	or     al,0x7
  4005df:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
  4005e5:	00 00                	add    BYTE PTR [rax],al
  4005e7:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4005ea:	00 00                	add    BYTE PTR [rax],al
  4005ec:	d4                   	(bad)  
  4005ed:	fd                   	std    
  4005ee:	ff                   	(bad)  
  4005ef:	ff 02                	inc    DWORD PTR [rdx]
  4005f1:	00 00                	add    BYTE PTR [rax],al
  4005f3:	00 00                	add    BYTE PTR [rax],al
  4005f5:	00 00                	add    BYTE PTR [rax],al
  4005f7:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  4005fa:	00 00                	add    BYTE PTR [rax],al
  4005fc:	30 00                	xor    BYTE PTR [rax],al
  4005fe:	00 00                	add    BYTE PTR [rax],al
  400600:	80 fe ff             	cmp    dh,0xff
  400603:	ff 46 00             	inc    DWORD PTR [rsi+0x0]
  400606:	00 00                	add    BYTE PTR [rax],al
  400608:	00 41 0e             	add    BYTE PTR [rcx+0xe],al
  40060b:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
  400611:	00 00                	add    BYTE PTR [rax],al
  400613:	00 00                	add    BYTE PTR [rax],al
  400615:	00 00                	add    BYTE PTR [rax],al
  400617:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
  40061b:	00 50 00             	add    BYTE PTR [rax+0x0],dl
  40061e:	00 00                	add    BYTE PTR [rax],al
  400620:	b0 fe                	mov    al,0xfe
  400622:	ff                   	(bad)  
  400623:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
  400626:	00 00                	add    BYTE PTR [rax],al
  400628:	00 42 0e             	add    BYTE PTR [rdx+0xe],al
  40062b:	10 8f 02 42 0e 18    	adc    BYTE PTR [rdi+0x180e4202],cl
  400631:	8e 03                	mov    es,WORD PTR [rbx]
  400633:	45 0e                	rex.RB (bad) 
  400635:	20 8d 04 42 0e 28    	and    BYTE PTR [rbp+0x280e4204],cl
  40063b:	8c 05 48 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e48],es        # ffffffff86701489 <_end+0xffffffff86100459>
  400641:	06                   	(bad)  
  400642:	48 0e                	rex.W (bad) 
  400644:	38 83 07 4d 0e 40    	cmp    BYTE PTR [rbx+0x400e4d07],al
  40064a:	72 0e                	jb     40065a <__GNU_EH_FRAME_HDR+0xf2>
  40064c:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
  40064f:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
  400652:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
  400655:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
  400658:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
  40065b:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
  40065e:	08 00                	or     BYTE PTR [rax],al
  400660:	10 00                	adc    BYTE PTR [rax],al
  400662:	00 00                	add    BYTE PTR [rax],al
  400664:	98                   	cwde   
  400665:	00 00                	add    BYTE PTR [rax],al
  400667:	00 d8                	add    al,bl
  400669:	fe                   	(bad)  
  40066a:	ff                   	(bad)  
  40066b:	ff 02                	inc    DWORD PTR [rdx]
  40066d:	00 00                	add    BYTE PTR [rax],al
  40066f:	00 00                	add    BYTE PTR [rax],al
  400671:	00 00                	add    BYTE PTR [rax],al
	...

0000000000400674 <__FRAME_END__>:
  400674:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .init_array:

0000000000600e50 <__frame_dummy_init_array_entry>:
  600e50:	70 04                	jo     600e56 <__frame_dummy_init_array_entry+0x6>
  600e52:	40 00 00             	add    BYTE PTR [rax],al
  600e55:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .fini_array:

0000000000600e58 <__do_global_dtors_aux_fini_array_entry>:
  600e58:	40 04 40             	add    al,0x40
  600e5b:	00 00                	add    BYTE PTR [rax],al
  600e5d:	00 00                	add    BYTE PTR [rax],al
	...

Disassembly of section .dynamic:

0000000000600e60 <_DYNAMIC>:
  600e60:	01 00                	add    DWORD PTR [rax],eax
  600e62:	00 00                	add    BYTE PTR [rax],al
  600e64:	00 00                	add    BYTE PTR [rax],al
  600e66:	00 00                	add    BYTE PTR [rax],al
  600e68:	01 00                	add    DWORD PTR [rax],eax
  600e6a:	00 00                	add    BYTE PTR [rax],al
  600e6c:	00 00                	add    BYTE PTR [rax],al
  600e6e:	00 00                	add    BYTE PTR [rax],al
  600e70:	0c 00                	or     al,0x0
  600e72:	00 00                	add    BYTE PTR [rax],al
  600e74:	00 00                	add    BYTE PTR [rax],al
  600e76:	00 00                	add    BYTE PTR [rax],al
  600e78:	70 03                	jo     600e7d <_DYNAMIC+0x1d>
  600e7a:	40 00 00             	add    BYTE PTR [rax],al
  600e7d:	00 00                	add    BYTE PTR [rax],al
  600e7f:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 600e85 <_DYNAMIC+0x25>
  600e85:	00 00                	add    BYTE PTR [rax],al
  600e87:	00 44 05 40          	add    BYTE PTR [rbp+rax*1+0x40],al
  600e8b:	00 00                	add    BYTE PTR [rax],al
  600e8d:	00 00                	add    BYTE PTR [rax],al
  600e8f:	00 19                	add    BYTE PTR [rcx],bl
  600e91:	00 00                	add    BYTE PTR [rax],al
  600e93:	00 00                	add    BYTE PTR [rax],al
  600e95:	00 00                	add    BYTE PTR [rax],al
  600e97:	00 50 0e             	add    BYTE PTR [rax+0xe],dl
  600e9a:	60                   	(bad)  
  600e9b:	00 00                	add    BYTE PTR [rax],al
  600e9d:	00 00                	add    BYTE PTR [rax],al
  600e9f:	00 1b                	add    BYTE PTR [rbx],bl
  600ea1:	00 00                	add    BYTE PTR [rax],al
  600ea3:	00 00                	add    BYTE PTR [rax],al
  600ea5:	00 00                	add    BYTE PTR [rax],al
  600ea7:	00 08                	add    BYTE PTR [rax],cl
  600ea9:	00 00                	add    BYTE PTR [rax],al
  600eab:	00 00                	add    BYTE PTR [rax],al
  600ead:	00 00                	add    BYTE PTR [rax],al
  600eaf:	00 1a                	add    BYTE PTR [rdx],bl
  600eb1:	00 00                	add    BYTE PTR [rax],al
  600eb3:	00 00                	add    BYTE PTR [rax],al
  600eb5:	00 00                	add    BYTE PTR [rax],al
  600eb7:	00 58 0e             	add    BYTE PTR [rax+0xe],bl
  600eba:	60                   	(bad)  
  600ebb:	00 00                	add    BYTE PTR [rax],al
  600ebd:	00 00                	add    BYTE PTR [rax],al
  600ebf:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
  600ec2:	00 00                	add    BYTE PTR [rax],al
  600ec4:	00 00                	add    BYTE PTR [rax],al
  600ec6:	00 00                	add    BYTE PTR [rax],al
  600ec8:	08 00                	or     BYTE PTR [rax],al
  600eca:	00 00                	add    BYTE PTR [rax],al
  600ecc:	00 00                	add    BYTE PTR [rax],al
  600ece:	00 00                	add    BYTE PTR [rax],al
  600ed0:	f5                   	cmc    
  600ed1:	fe                   	(bad)  
  600ed2:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  600ed5:	00 00                	add    BYTE PTR [rax],al
  600ed7:	00 78 02             	add    BYTE PTR [rax+0x2],bh
  600eda:	40 00 00             	add    BYTE PTR [rax],al
  600edd:	00 00                	add    BYTE PTR [rax],al
  600edf:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 600ee5 <_DYNAMIC+0x85>
  600ee5:	00 00                	add    BYTE PTR [rax],al
  600ee7:	00 e0                	add    al,ah
  600ee9:	02 40 00             	add    al,BYTE PTR [rax+0x0]
  600eec:	00 00                	add    BYTE PTR [rax],al
  600eee:	00 00                	add    BYTE PTR [rax],al
  600ef0:	06                   	(bad)  
  600ef1:	00 00                	add    BYTE PTR [rax],al
  600ef3:	00 00                	add    BYTE PTR [rax],al
  600ef5:	00 00                	add    BYTE PTR [rax],al
  600ef7:	00 98 02 40 00 00    	add    BYTE PTR [rax+0x4002],bl
  600efd:	00 00                	add    BYTE PTR [rax],al
  600eff:	00 0a                	add    BYTE PTR [rdx],cl
  600f01:	00 00                	add    BYTE PTR [rax],al
  600f03:	00 00                	add    BYTE PTR [rax],al
  600f05:	00 00                	add    BYTE PTR [rax],al
  600f07:	00 38                	add    BYTE PTR [rax],bh
  600f09:	00 00                	add    BYTE PTR [rax],al
  600f0b:	00 00                	add    BYTE PTR [rax],al
  600f0d:	00 00                	add    BYTE PTR [rax],al
  600f0f:	00 0b                	add    BYTE PTR [rbx],cl
  600f11:	00 00                	add    BYTE PTR [rax],al
  600f13:	00 00                	add    BYTE PTR [rax],al
  600f15:	00 00                	add    BYTE PTR [rax],al
  600f17:	00 18                	add    BYTE PTR [rax],bl
  600f19:	00 00                	add    BYTE PTR [rax],al
  600f1b:	00 00                	add    BYTE PTR [rax],al
  600f1d:	00 00                	add    BYTE PTR [rax],al
  600f1f:	00 15 00 00 00 00    	add    BYTE PTR [rip+0x0],dl        # 600f25 <_DYNAMIC+0xc5>
	...
  600f2d:	00 00                	add    BYTE PTR [rax],al
  600f2f:	00 07                	add    BYTE PTR [rdi],al
  600f31:	00 00                	add    BYTE PTR [rax],al
  600f33:	00 00                	add    BYTE PTR [rax],al
  600f35:	00 00                	add    BYTE PTR [rax],al
  600f37:	00 40 03             	add    BYTE PTR [rax+0x3],al
  600f3a:	40 00 00             	add    BYTE PTR [rax],al
  600f3d:	00 00                	add    BYTE PTR [rax],al
  600f3f:	00 08                	add    BYTE PTR [rax],cl
  600f41:	00 00                	add    BYTE PTR [rax],al
  600f43:	00 00                	add    BYTE PTR [rax],al
  600f45:	00 00                	add    BYTE PTR [rax],al
  600f47:	00 30                	add    BYTE PTR [rax],dh
  600f49:	00 00                	add    BYTE PTR [rax],al
  600f4b:	00 00                	add    BYTE PTR [rax],al
  600f4d:	00 00                	add    BYTE PTR [rax],al
  600f4f:	00 09                	add    BYTE PTR [rcx],cl
  600f51:	00 00                	add    BYTE PTR [rax],al
  600f53:	00 00                	add    BYTE PTR [rax],al
  600f55:	00 00                	add    BYTE PTR [rax],al
  600f57:	00 18                	add    BYTE PTR [rax],bl
  600f59:	00 00                	add    BYTE PTR [rax],al
  600f5b:	00 00                	add    BYTE PTR [rax],al
  600f5d:	00 00                	add    BYTE PTR [rax],al
  600f5f:	00 fe                	add    dh,bh
  600f61:	ff                   	(bad)  
  600f62:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  600f65:	00 00                	add    BYTE PTR [rax],al
  600f67:	00 20                	add    BYTE PTR [rax],ah
  600f69:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
  600f6c:	00 00                	add    BYTE PTR [rax],al
  600f6e:	00 00                	add    BYTE PTR [rax],al
  600f70:	ff                   	(bad)  
  600f71:	ff                   	(bad)  
  600f72:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  600f75:	00 00                	add    BYTE PTR [rax],al
  600f77:	00 01                	add    BYTE PTR [rcx],al
  600f79:	00 00                	add    BYTE PTR [rax],al
  600f7b:	00 00                	add    BYTE PTR [rax],al
  600f7d:	00 00                	add    BYTE PTR [rax],al
  600f7f:	00 f0                	add    al,dh
  600f81:	ff                   	(bad)  
  600f82:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
  600f85:	00 00                	add    BYTE PTR [rax],al
  600f87:	00 18                	add    BYTE PTR [rax],bl
  600f89:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
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
   2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
   5:	28 55 62             	sub    BYTE PTR [rbp+0x62],dl
   8:	75 6e                	jne    78 <_init-0x4002f8>
   a:	74 75                	je     81 <_init-0x4002ef>
   c:	20 37                	and    BYTE PTR [rdi],dh
   e:	2e 33 2e             	xor    ebp,DWORD PTR cs:[rsi]
  11:	30 2d 32 37 75 62    	xor    BYTE PTR [rip+0x62753732],ch        # 62753749 <_end+0x62152719>
  17:	75 6e                	jne    87 <_init-0x4002e9>
  19:	74 75                	je     90 <_init-0x4002e0>
  1b:	31 7e 31             	xor    DWORD PTR [rsi+0x31],edi
  1e:	38 2e                	cmp    BYTE PTR [rsi],ch
  20:	30 34 29             	xor    BYTE PTR [rcx+rbp*1],dh
  23:	20 37                	and    BYTE PTR [rdi],dh
  25:	2e 33 2e             	xor    ebp,DWORD PTR cs:[rsi]
  28:	30 00                	xor    BYTE PTR [rax],al
  2a:	63 6c 61 6e          	movsxd ebp,DWORD PTR [rcx+riz*2+0x6e]
  2e:	67 20 76 65          	and    BYTE PTR [esi+0x65],dh
  32:	72 73                	jb     a7 <_init-0x4002c9>
  34:	69 6f 6e 20 36 2e 30 	imul   ebp,DWORD PTR [rdi+0x6e],0x302e3620
  3b:	2e 30 2d 31 75 62 75 	xor    BYTE PTR cs:[rip+0x75627531],ch        # 75627573 <_end+0x75026543>
  42:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  43:	74 75                	je     ba <_init-0x4002b6>
  45:	32 20                	xor    ah,BYTE PTR [rax]
  47:	28 74 61 67          	sub    BYTE PTR [rcx+riz*2+0x67],dh
  4b:	73 2f                	jae    7c <_init-0x4002f4>
  4d:	52                   	push   rdx
  4e:	45                   	rex.RB
  4f:	4c                   	rex.WR
  50:	45                   	rex.RB
  51:	41 53                	push   r11
  53:	45 5f                	rex.RB pop r15
  55:	36 30 30             	xor    BYTE PTR ss:[rax],dh
  58:	2f                   	(bad)  
  59:	66 69 6e 61 6c 29    	imul   bp,WORD PTR [rsi+0x61],0x296c
	...
