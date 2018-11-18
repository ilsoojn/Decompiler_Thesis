
io:     file format elf64-x86-64

Disassembly of section .interp:

  0000000000400238 <.interp>:
  400238:	2f                   	(bad)
  400239:	6c                   	ins    BYTE PTR es:[rdi],dx
  40023a:	69 62 36 34 2f 6c 64 	imul   esp,DWORD PTR [rdx+0x36],0x646c2f34
  400241:	2d 6c 69 6e 75       	sub    eax,0x756e696c
  400246:	78 2d                	js     400275 <_init-0x1fb>
  400248:	78 38                	js     400282 <_init-0x1ee>
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
  4002b0:	1f                   	(bad)
  4002b1:	00 00                	add    BYTE PTR [rax],al
  4002b3:	00 12                	add    BYTE PTR [rdx],dl
	...
  4002c5:	00 00                	add    BYTE PTR [rax],al
  4002c7:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
  4002ca:	00 00                	add    BYTE PTR [rax],al
  4002cc:	12 00                	adc    al,BYTE PTR [rax]
	...
  4002de:	00 00                	add    BYTE PTR [rax],al
  4002e0:	2b 00                	sub    eax,DWORD PTR [rax]
  4002e2:	00 00                	add    BYTE PTR [rax],al
  4002e4:	12 00                	adc    al,BYTE PTR [rax]
	...
  4002f6:	00 00                	add    BYTE PTR [rax],al
  4002f8:	53                   	push   rbx
  4002f9:	00 00                	add    BYTE PTR [rax],al
  4002fb:	00 20                	add    BYTE PTR [rax],ah
	...
  40030d:	00 00                	add    BYTE PTR [rax],al
  40030f:	00 0b                	add    BYTE PTR [rbx],cl
  400311:	00 00                	add    BYTE PTR [rax],al
  400313:	00 12                	add    BYTE PTR [rdx],dl
	...
  400325:	00 00                	add    BYTE PTR [rax],al
  400327:	00 10                	add    BYTE PTR [rax],dl
  400329:	00 00                	add    BYTE PTR [rax],al
  40032b:	00 12                	add    BYTE PTR [rdx],dl
	...

Disassembly of section .dynstr:

  0000000000400340 <.dynstr>:
  400340:	00 6c 69 62          	add    BYTE PTR [rcx+rbp*2+0x62],ch
  400344:	63 2e                	movsxd ebp,DWORD PTR [rsi]
  400346:	73 6f                	jae    4003b7 <_init-0xb9>
  400348:	2e 36 00 67 65       	cs add BYTE PTR ss:[rdi+0x65],ah
  40034d:	74 73                	je     4003c2 <_init-0xae>
  40034f:	00 5f 5f             	add    BYTE PTR [rdi+0x5f],bl
  400352:	69 73 6f 63 39 39 5f 	imul   esi,DWORD PTR [rbx+0x6f],0x5f393963
  400359:	73 63                	jae    4003be <_init-0xb2>
  40035b:	61                   	(bad)
  40035c:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  40035d:	66 00 70 75          	data16 add BYTE PTR [rax+0x75],dh
  400361:	74 73                	je     4003d6 <_init-0x9a>
  400363:	00 70 72             	add    BYTE PTR [rax+0x72],dh
  400366:	69 6e 74 66 00 5f 5f 	imul   ebp,DWORD PTR [rsi+0x74],0x5f5f0066
  40036d:	6c                   	ins    BYTE PTR es:[rdi],dx
  40036e:	69 62 63 5f 73 74 61 	imul   esp,DWORD PTR [rdx+0x63],0x6174735f
  400375:	72 74                	jb     4003eb <_init-0x85>
  400377:	5f                   	pop    rdi
  400378:	6d                   	ins    DWORD PTR es:[rdi],dx
  400379:	61                   	(bad)
  40037a:	69 6e 00 47 4c 49 42 	imul   ebp,DWORD PTR [rsi+0x0],0x42494c47
  400381:	43 5f                	rex.XB pop r15
  400383:	32 2e                	xor    ch,BYTE PTR [rsi]
  400385:	37                   	(bad)
  400386:	00 47 4c             	add    BYTE PTR [rdi+0x4c],al
  400389:	49                   	rex.WB
  40038a:	42                   	rex.X
  40038b:	43 5f                	rex.XB pop r15
  40038d:	32 2e                	xor    ch,BYTE PTR [rsi]
  40038f:	32 2e                	xor    ch,BYTE PTR [rsi]
  400391:	35 00 5f 5f 67       	xor    eax,0x675f5f00
  400396:	6d                   	ins    DWORD PTR es:[rdi],dx
  400397:	6f                   	outs   dx,DWORD PTR ds:[rsi]
  400398:	6e                   	outs   dx,BYTE PTR ds:[rsi]
  400399:	5f                   	pop    rdi
  40039a:	73 74                	jae    400410 <_init-0x60>
  40039c:	61                   	(bad)
  40039d:	72 74                	jb     400413 <_init-0x5d>
  40039f:	5f                   	pop    rdi
  4003a0:	5f                   	pop    rdi
	...

Disassembly of section .gnu.version:

  00000000004003a2 <.gnu.version>:
  4003a2:	00 00                	add    BYTE PTR [rax],al
  4003a4:	02 00                	add    al,BYTE PTR [rax]
  4003a6:	02 00                	add    al,BYTE PTR [rax]
  4003a8:	02 00                	add    al,BYTE PTR [rax]
  4003aa:	00 00                	add    BYTE PTR [rax],al
  4003ac:	02 00                	add    al,BYTE PTR [rax]
  4003ae:	03 00                	add    eax,DWORD PTR [rax]

Disassembly of section .gnu.version_r:

  00000000004003b0 <.gnu.version_r>:
    4003b0:	01 00                	add    DWORD PTR [rax],eax
    4003b2:	02 00                	add    al,BYTE PTR [rax]
    4003b4:	01 00                	add    DWORD PTR [rax],eax
    4003b6:	00 00                	add    BYTE PTR [rax],al
    4003b8:	10 00                	adc    BYTE PTR [rax],al
    4003ba:	00 00                	add    BYTE PTR [rax],al
    4003bc:	00 00                	add    BYTE PTR [rax],al
    4003be:	00 00                	add    BYTE PTR [rax],al
    4003c0:	17                   	(bad)
    4003c1:	69 69 0d 00 00 03 00 	imul   ebp,DWORD PTR [rcx+0xd],0x30000
    4003c8:	3d 00 00 00 10       	cmp    eax,0x10000000
    4003cd:	00 00                	add    BYTE PTR [rax],al
    4003cf:	00 75 1a             	add    BYTE PTR [rbp+0x1a],dh
    4003d2:	69 09 00 00 02 00    	imul   ecx,DWORD PTR [rcx],0x20000
    4003d8:	47 00 00             	rex.RXB add BYTE PTR [r8],r8b
    4003db:	00 00                	add    BYTE PTR [rax],al
    4003dd:	00 00                	add    BYTE PTR [rax],al
  	...

Disassembly of section .rela.dyn:

  00000000004003e0 <.rela.dyn>:
    4003e0:	f0 0f 60 00          	lock punpcklbw mm0,DWORD PTR [rax]
    4003e4:	00 00                	add    BYTE PTR [rax],al
    4003e6:	00 00                	add    BYTE PTR [rax],al
    4003e8:	06                   	(bad)
    4003e9:	00 00                	add    BYTE PTR [rax],al
    4003eb:	00 03                	add    BYTE PTR [rbx],al
  	...
    4003f5:	00 00                	add    BYTE PTR [rax],al
    4003f7:	00 f8                	add    al,bh
    4003f9:	0f 60 00             	punpcklbw mm0,DWORD PTR [rax]
    4003fc:	00 00                	add    BYTE PTR [rax],al
    4003fe:	00 00                	add    BYTE PTR [rax],al
    400400:	06                   	(bad)
    400401:	00 00                	add    BYTE PTR [rax],al
    400403:	00 04 00             	add    BYTE PTR [rax+rax*1],al
  	...

Disassembly of section .rela.plt:

  0000000000400410 <.rela.plt>:
    400410:	18 10                	sbb    BYTE PTR [rax],dl
    400412:	60                   	(bad)
    400413:	00 00                	add    BYTE PTR [rax],al
    400415:	00 00                	add    BYTE PTR [rax],al
    400417:	00 07                	add    BYTE PTR [rdi],al
    400419:	00 00                	add    BYTE PTR [rax],al
    40041b:	00 01                	add    BYTE PTR [rcx],al
  	...
    400425:	00 00                	add    BYTE PTR [rax],al
    400427:	00 20                	add    BYTE PTR [rax],ah
    400429:	10 60 00             	adc    BYTE PTR [rax+0x0],ah
    40042c:	00 00                	add    BYTE PTR [rax],al
    40042e:	00 00                	add    BYTE PTR [rax],al
    400430:	07                   	(bad)
    400431:	00 00                	add    BYTE PTR [rax],al
    400433:	00 02                	add    BYTE PTR [rdx],al
  	...
    40043d:	00 00                	add    BYTE PTR [rax],al
    40043f:	00 28                	add    BYTE PTR [rax],ch
    400441:	10 60 00             	adc    BYTE PTR [rax+0x0],ah
    400444:	00 00                	add    BYTE PTR [rax],al
    400446:	00 00                	add    BYTE PTR [rax],al
    400448:	07                   	(bad)
    400449:	00 00                	add    BYTE PTR [rax],al
    40044b:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 400451 <_init-0x1f>
    400451:	00 00                	add    BYTE PTR [rax],al
    400453:	00 00                	add    BYTE PTR [rax],al
    400455:	00 00                	add    BYTE PTR [rax],al
    400457:	00 30                	add    BYTE PTR [rax],dh
    400459:	10 60 00             	adc    BYTE PTR [rax+0x0],ah
    40045c:	00 00                	add    BYTE PTR [rax],al
    40045e:	00 00                	add    BYTE PTR [rax],al
    400460:	07                   	(bad)
    400461:	00 00                	add    BYTE PTR [rax],al
    400463:	00 06                	add    BYTE PTR [rsi],al
  	...

Disassembly of section .init:

  0000000000400470 <_init>:
    400470:	48 83 ec 08          	sub    rsp,0x8
    400474:	48 8b 05 7d 0b 20 00 	mov    rax,QWORD PTR [rip+0x200b7d]        # 600ff8 <__gmon_start__>
    40047b:	48 85 c0             	test   rax,rax
    40047e:	74 02                	je     400482 <_init+0x12>
    400480:	ff d0                	call   rax
    400482:	48 83 c4 08          	add    rsp,0x8
    400486:	c3                   	ret

Disassembly of section .plt:

  0000000000400490 <.plt>:
    400490:	ff 35 72 0b 20 00    	push   QWORD PTR [rip+0x200b72]        # 601008 <_GLOBAL_OFFSET_TABLE_+0x8>
    400496:	ff 25 74 0b 20 00    	jmp    QWORD PTR [rip+0x200b74]        # 601010 <_GLOBAL_OFFSET_TABLE_+0x10>
    40049c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

  00000000004004a0 <puts@plt>:
    4004a0:	ff 25 72 0b 20 00    	jmp    QWORD PTR [rip+0x200b72]        # 601018 <puts@GLIBC_2.2.5>
    4004a6:	68 00 00 00 00       	push   0x0
    4004ab:	e9 e0 ff ff ff       	jmp    400490 <.plt>

  00000000004004b0 <printf@plt>:
    4004b0:	ff 25 6a 0b 20 00    	jmp    QWORD PTR [rip+0x200b6a]        # 601020 <printf@GLIBC_2.2.5>
    4004b6:	68 01 00 00 00       	push   0x1
    4004bb:	e9 d0 ff ff ff       	jmp    400490 <.plt>

  00000000004004c0 <gets@plt>:
    4004c0:	ff 25 62 0b 20 00    	jmp    QWORD PTR [rip+0x200b62]        # 601028 <gets@GLIBC_2.2.5>
    4004c6:	68 02 00 00 00       	push   0x2
    4004cb:	e9 c0 ff ff ff       	jmp    400490 <.plt>

  00000000004004d0 <__isoc99_scanf@plt>:
  4004d0:	ff 25 5a 0b 20 00    	jmp    QWORD PTR [rip+0x200b5a]        # 601030 <__isoc99_scanf@GLIBC_2.7>
  4004d6:	68 03 00 00 00       	push   0x3
  4004db:	e9 b0 ff ff ff       	jmp    400490 <.plt>

Disassembly of section .text:

  00000000004004e0 <_start>:
    4004e0:	31 ed                	xor    ebp,ebp
    4004e2:	49 89 d1             	mov    r9,rdx
    4004e5:	5e                   	pop    rsi
    4004e6:	48 89 e2             	mov    rdx,rsp
    4004e9:	48 83 e4 f0          	and    rsp,0xfffffffffffffff0
    4004ed:	50                   	push   rax
    4004ee:	54                   	push   rsp
    4004ef:	49 c7 c0 b0 06 40 00 	mov    r8,0x4006b0
    4004f6:	48 c7 c1 40 06 40 00 	mov    rcx,0x400640
    4004fd:	48 c7 c7 d0 05 40 00 	mov    rdi,0x4005d0
    400504:	ff 15 e6 0a 20 00    	call   QWORD PTR [rip+0x200ae6]        # 600ff0 <__libc_start_main@GLIBC_2.2.5>
    40050a:	f4                   	hlt
    40050b:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]

  0000000000400510 <_dl_relocate_static_pie>:
    400510:	f3 c3                	repz ret
    400512:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
    400519:	00 00 00
    40051c:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]

  0000000000400520 <deregister_tm_clones>:
    400520:	55                   	push   rbp
    400521:	b8 48 10 60 00       	mov    eax,0x601048
    400526:	48 3d 48 10 60 00    	cmp    rax,0x601048
    40052c:	48 89 e5             	mov    rbp,rsp
    40052f:	74 17                	je     400548 <deregister_tm_clones+0x28>
    400531:	b8 00 00 00 00       	mov    eax,0x0
    400536:	48 85 c0             	test   rax,rax
    400539:	74 0d                	je     400548 <deregister_tm_clones+0x28>
    40053b:	5d                   	pop    rbp
    40053c:	bf 48 10 60 00       	mov    edi,0x601048
    400541:	ff e0                	jmp    rax
    400543:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]
    400548:	5d                   	pop    rbp
    400549:	c3                   	ret
    40054a:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]

  0000000000400550 <register_tm_clones>:
    400550:	be 48 10 60 00       	mov    esi,0x601048
    400555:	55                   	push   rbp
    400556:	48 81 ee 48 10 60 00 	sub    rsi,0x601048
    40055d:	48 89 e5             	mov    rbp,rsp
    400560:	48 c1 fe 03          	sar    rsi,0x3
    400564:	48 89 f0             	mov    rax,rsi
    400567:	48 c1 e8 3f          	shr    rax,0x3f
    40056b:	48 01 c6             	add    rsi,rax
    40056e:	48 d1 fe             	sar    rsi,1
    400571:	74 15                	je     400588 <register_tm_clones+0x38>
    400573:	b8 00 00 00 00       	mov    eax,0x0
    400578:	48 85 c0             	test   rax,rax
    40057b:	74 0b                	je     400588 <register_tm_clones+0x38>
    40057d:	5d                   	pop    rbp
    40057e:	bf 48 10 60 00       	mov    edi,0x601048
    400583:	ff e0                	jmp    rax
    400585:	0f 1f 00             	nop    DWORD PTR [rax]
    400588:	5d                   	pop    rbp
    400589:	c3                   	ret
    40058a:	66 0f 1f 44 00 00    	nop    WORD PTR [rax+rax*1+0x0]

  0000000000400590 <__do_global_dtors_aux>:
    400590:	80 3d b1 0a 20 00 00 	cmp    BYTE PTR [rip+0x200ab1],0x0        # 601048 <__TMC_END__>
    400597:	75 17                	jne    4005b0 <__do_global_dtors_aux+0x20>
    400599:	55                   	push   rbp
    40059a:	48 89 e5             	mov    rbp,rsp
    40059d:	e8 7e ff ff ff       	call   400520 <deregister_tm_clones>
    4005a2:	c6 05 9f 0a 20 00 01 	mov    BYTE PTR [rip+0x200a9f],0x1        # 601048 <__TMC_END__>
    4005a9:	5d                   	pop    rbp
    4005aa:	c3                   	ret
    4005ab:	0f 1f 44 00 00       	nop    DWORD PTR [rax+rax*1+0x0]
    4005b0:	f3 c3                	repz ret
    4005b2:	0f 1f 40 00          	nop    DWORD PTR [rax+0x0]
    4005b6:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
    4005bd:	00 00 00

  00000000004005c0 <frame_dummy>:
    4005c0:	55                   	push   rbp
    4005c1:	48 89 e5             	mov    rbp,rsp
    4005c4:	5d                   	pop    rbp
    4005c5:	eb 89                	jmp    400550 <register_tm_clones>
    4005c7:	66 0f 1f 84 00 00 00 	nop    WORD PTR [rax+rax*1+0x0]
    4005ce:	00 00

  00000000004005d0 <main>:
    4005d0:	55                   	push   rbp
    4005d1:	48 89 e5             	mov    rbp,rsp
    4005d4:	48 83 ec 40          	sub    rsp,0x40
    4005d8:	48 bf c4 06 40 00 00 	movabs rdi,0x4006c4
    4005df:	00 00 00
    4005e2:	c7 45 fc 00 00 00 00 	mov    DWORD PTR [rbp-0x4],0x0
    4005e9:	e8 b2 fe ff ff       	call   4004a0 <puts@plt>
    4005ee:	48 8d 7d d0          	lea    rdi,[rbp-0x30]
    4005f2:	89 45 cc             	mov    DWORD PTR [rbp-0x34],eax
    4005f5:	b0 00                	mov    al,0x0
    4005f7:	e8 c4 fe ff ff       	call   4004c0 <gets@plt>
    4005fc:	48 bf e3 06 40 00 00 	movabs rdi,0x4006e3
    400603:	00 00 00
    400606:	89 45 c8             	mov    DWORD PTR [rbp-0x38],eax
    400609:	b0 00                	mov    al,0x0
    40060b:	e8 a0 fe ff ff       	call   4004b0 <printf@plt>
    400610:	48 bf f9 06 40 00 00 	movabs rdi,0x4006f9
    400617:	00 00 00
    40061a:	48 8d 75 f8          	lea    rsi,[rbp-0x8]
    40061e:	89 45 c4             	mov    DWORD PTR [rbp-0x3c],eax
    400621:	b0 00                	mov    al,0x0
    400623:	e8 a8 fe ff ff       	call   4004d0 <__isoc99_scanf@plt>
    400628:	31 c9                	xor    ecx,ecx
    40062a:	89 45 c0             	mov    DWORD PTR [rbp-0x40],eax
    40062d:	89 c8                	mov    eax,ecx
    40062f:	48 83 c4 40          	add    rsp,0x40
    400633:	5d                   	pop    rbp
    400634:	c3                   	ret
    400635:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
    40063c:	00 00 00
    40063f:	90                   	nop

  0000000000400640 <__libc_csu_init>:
    400640:	41 57                	push   r15
    400642:	41 56                	push   r14
    400644:	49 89 d7             	mov    r15,rdx
    400647:	41 55                	push   r13
    400649:	41 54                	push   r12
    40064b:	4c 8d 25 be 07 20 00 	lea    r12,[rip+0x2007be]        # 600e10 <__frame_dummy_init_array_entry>
    400652:	55                   	push   rbp
    400653:	48 8d 2d be 07 20 00 	lea    rbp,[rip+0x2007be]        # 600e18 <__init_array_end>
    40065a:	53                   	push   rbx
    40065b:	41 89 fd             	mov    r13d,edi
    40065e:	49 89 f6             	mov    r14,rsi
    400661:	4c 29 e5             	sub    rbp,r12
    400664:	48 83 ec 08          	sub    rsp,0x8
    400668:	48 c1 fd 03          	sar    rbp,0x3
    40066c:	e8 ff fd ff ff       	call   400470 <_init>
    400671:	48 85 ed             	test   rbp,rbp
    400674:	74 20                	je     400696 <__libc_csu_init+0x56>
    400676:	31 db                	xor    ebx,ebx
    400678:	0f 1f 84 00 00 00 00 	nop    DWORD PTR [rax+rax*1+0x0]
    40067f:	00
    400680:	4c 89 fa             	mov    rdx,r15
    400683:	4c 89 f6             	mov    rsi,r14
    400686:	44 89 ef             	mov    edi,r13d
    400689:	41 ff 14 dc          	call   QWORD PTR [r12+rbx*8]
    40068d:	48 83 c3 01          	add    rbx,0x1
    400691:	48 39 dd             	cmp    rbp,rbx
    400694:	75 ea                	jne    400680 <__libc_csu_init+0x40>
    400696:	48 83 c4 08          	add    rsp,0x8
    40069a:	5b                   	pop    rbx
    40069b:	5d                   	pop    rbp
    40069c:	41 5c                	pop    r12
    40069e:	41 5d                	pop    r13
    4006a0:	41 5e                	pop    r14
    4006a2:	41 5f                	pop    r15
    4006a4:	c3                   	ret
    4006a5:	90                   	nop
    4006a6:	66 2e 0f 1f 84 00 00 	nop    WORD PTR cs:[rax+rax*1+0x0]
    4006ad:	00 00 00

  00000000004006b0 <__libc_csu_fini>:
    4006b0:	f3 c3                	repz ret

Disassembly of section .fini:

  00000000004006b4 <_fini>:
    4006b4:	48 83 ec 08          	sub    rsp,0x8
    4006b8:	48 83 c4 08          	add    rsp,0x8
    4006bc:	c3                   	ret

Disassembly of section .rodata:

  00000000004006c0 <_IO_stdin_used>:
    4006c0:	01 00                	add    DWORD PTR [rax],eax
    4006c2:	02 00                	add    al,BYTE PTR [rax]
    4006c4:	45 6e                	rex.RB outs dx,BYTE PTR ds:[rsi]
    4006c6:	74 65                	je     40072d <__GNU_EH_FRAME_HDR+0x31>
    4006c8:	72 20                	jb     4006ea <_IO_stdin_used+0x2a>
    4006ca:	65 69 74 68 65 72 20 	imul   esi,DWORD PTR gs:[rax+rbp*2+0x65],0x7a272072
    4006d1:	27 7a
    4006d3:	65 72 6f             	gs jb  400745 <__GNU_EH_FRAME_HDR+0x49>
    4006d6:	27                   	(bad)
    4006d7:	20 6f 72             	and    BYTE PTR [rdi+0x72],ch
    4006da:	20 27                	and    BYTE PTR [rdi],ah
    4006dc:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    4006dd:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    4006de:	65 27                	gs (bad)
    4006e0:	3a 20                	cmp    ah,BYTE PTR [rax]
    4006e2:	00 45 6e             	add    BYTE PTR [rbp+0x6e],al
    4006e5:	74 65                	je     40074c <__GNU_EH_FRAME_HDR+0x50>
    4006e7:	72 20                	jb     400709 <__GNU_EH_FRAME_HDR+0xd>
    4006e9:	65 69 74 68 65 72 20 	imul   esi,DWORD PTR gs:[rax+rbp*2+0x65],0x20302072
    4006f0:	30 20
    4006f2:	6f                   	outs   dx,DWORD PTR ds:[rsi]
    4006f3:	72 20                	jb     400715 <__GNU_EH_FRAME_HDR+0x19>
    4006f5:	31 3a                	xor    DWORD PTR [rdx],edi
    4006f7:	20 00                	and    BYTE PTR [rax],al
    4006f9:	25                   	.byte 0x25
    4006fa:	64                   	fs
  	...

Disassembly of section .eh_frame_hdr:

  00000000004006fc <__GNU_EH_FRAME_HDR>:
    4006fc:	01 1b                	add    DWORD PTR [rbx],ebx
    4006fe:	03 3b                	add    edi,DWORD PTR [rbx]
    400700:	38 00                	cmp    BYTE PTR [rax],al
    400702:	00 00                	add    BYTE PTR [rax],al
    400704:	06                   	(bad)
    400705:	00 00                	add    BYTE PTR [rax],al
    400707:	00 94 fd ff ff 94 00 	add    BYTE PTR [rbp+rdi*8+0x94ffff],dl
    40070e:	00 00                	add    BYTE PTR [rax],al
    400710:	e4 fd                	in     al,0xfd
    400712:	ff                   	(bad)
    400713:	ff 54 00 00          	call   QWORD PTR [rax+rax*1+0x0]
    400717:	00 14 fe             	add    BYTE PTR [rsi+rdi*8],dl
    40071a:	ff                   	(bad)
    40071b:	ff 80 00 00 00 d4    	inc    DWORD PTR [rax-0x2c000000]
    400721:	fe                   	(bad)
    400722:	ff                   	(bad)
    400723:	ff                   	(bad)
    400724:	bc 00 00 00 44       	mov    esp,0x44000000
    400729:	ff                   	(bad)
    40072a:	ff                   	(bad)
    40072b:	ff                   	(bad)
    40072c:	dc 00                	fadd   QWORD PTR [rax]
    40072e:	00 00                	add    BYTE PTR [rax],al
    400730:	b4 ff                	mov    ah,0xff
    400732:	ff                   	(bad)
    400733:	ff 24 01             	jmp    QWORD PTR [rcx+rax*1]
  	...

Disassembly of section .eh_frame:

  0000000000400738 <__FRAME_END__-0xfc>:
    400738:	14 00                	adc    al,0x0
    40073a:	00 00                	add    BYTE PTR [rax],al
    40073c:	00 00                	add    BYTE PTR [rax],al
    40073e:	00 00                	add    BYTE PTR [rax],al
    400740:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    400743:	00 01                	add    BYTE PTR [rcx],al
    400745:	78 10                	js     400757 <__GNU_EH_FRAME_HDR+0x5b>
    400747:	01 1b                	add    DWORD PTR [rbx],ebx
    400749:	0c 07                	or     al,0x7
    40074b:	08 90 01 07 10 10    	or     BYTE PTR [rax+0x10100701],dl
    400751:	00 00                	add    BYTE PTR [rax],al
    400753:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    400756:	00 00                	add    BYTE PTR [rax],al
    400758:	88 fd                	mov    ch,bh
    40075a:	ff                   	(bad)
    40075b:	ff 2b                	jmp    FWORD PTR [rbx]
    40075d:	00 00                	add    BYTE PTR [rax],al
    40075f:	00 00                	add    BYTE PTR [rax],al
    400761:	00 00                	add    BYTE PTR [rax],al
    400763:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    400766:	00 00                	add    BYTE PTR [rax],al
    400768:	00 00                	add    BYTE PTR [rax],al
    40076a:	00 00                	add    BYTE PTR [rax],al
    40076c:	01 7a 52             	add    DWORD PTR [rdx+0x52],edi
    40076f:	00 01                	add    BYTE PTR [rcx],al
    400771:	78 10                	js     400783 <__GNU_EH_FRAME_HDR+0x87>
    400773:	01 1b                	add    DWORD PTR [rbx],ebx
    400775:	0c 07                	or     al,0x7
    400777:	08 90 01 00 00 10    	or     BYTE PTR [rax+0x10000001],dl
    40077d:	00 00                	add    BYTE PTR [rax],al
    40077f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    400782:	00 00                	add    BYTE PTR [rax],al
    400784:	8c fd                	mov    ebp,?
    400786:	ff                   	(bad)
    400787:	ff 02                	inc    DWORD PTR [rdx]
    400789:	00 00                	add    BYTE PTR [rax],al
    40078b:	00 00                	add    BYTE PTR [rax],al
    40078d:	00 00                	add    BYTE PTR [rax],al
    40078f:	00 24 00             	add    BYTE PTR [rax+rax*1],ah
    400792:	00 00                	add    BYTE PTR [rax],al
    400794:	30 00                	xor    BYTE PTR [rax],al
    400796:	00 00                	add    BYTE PTR [rax],al
    400798:	f8                   	clc
    400799:	fc                   	cld
    40079a:	ff                   	(bad)
    40079b:	ff 50 00             	call   QWORD PTR [rax+0x0]
    40079e:	00 00                	add    BYTE PTR [rax],al
    4007a0:	00 0e                	add    BYTE PTR [rsi],cl
    4007a2:	10 46 0e             	adc    BYTE PTR [rsi+0xe],al
    4007a5:	18 4a 0f             	sbb    BYTE PTR [rdx+0xf],cl
    4007a8:	0b 77 08             	or     esi,DWORD PTR [rdi+0x8]
    4007ab:	80 00 3f             	add    BYTE PTR [rax],0x3f
    4007ae:	1a 3b                	sbb    bh,BYTE PTR [rbx]
    4007b0:	2a 33                	sub    dh,BYTE PTR [rbx]
    4007b2:	24 22                	and    al,0x22
    4007b4:	00 00                	add    BYTE PTR [rax],al
    4007b6:	00 00                	add    BYTE PTR [rax],al
    4007b8:	1c 00                	sbb    al,0x0
    4007ba:	00 00                	add    BYTE PTR [rax],al
    4007bc:	58                   	pop    rax
    4007bd:	00 00                	add    BYTE PTR [rax],al
    4007bf:	00 10                	add    BYTE PTR [rax],dl
    4007c1:	fe                   	(bad)
    4007c2:	ff                   	(bad)
    4007c3:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
    4007c6:	00 00                	add    BYTE PTR [rax],al
    4007c8:	00 41 0e             	add    BYTE PTR [rcx+0xe],al
    4007cb:	10 86 02 43 0d 06    	adc    BYTE PTR [rsi+0x60d4302],al
    4007d1:	00 00                	add    BYTE PTR [rax],al
    4007d3:	00 00                	add    BYTE PTR [rax],al
    4007d5:	00 00                	add    BYTE PTR [rax],al
    4007d7:	00 44 00 00          	add    BYTE PTR [rax+rax*1+0x0],al
    4007db:	00 78 00             	add    BYTE PTR [rax+0x0],bh
    4007de:	00 00                	add    BYTE PTR [rax],al
    4007e0:	60                   	(bad)
    4007e1:	fe                   	(bad)
    4007e2:	ff                   	(bad)
    4007e3:	ff 65 00             	jmp    QWORD PTR [rbp+0x0]
    4007e6:	00 00                	add    BYTE PTR [rax],al
    4007e8:	00 42 0e             	add    BYTE PTR [rdx+0xe],al
    4007eb:	10 8f 02 42 0e 18    	adc    BYTE PTR [rdi+0x180e4202],cl
    4007f1:	8e 03                	mov    es,WORD PTR [rbx]
    4007f3:	45 0e                	rex.RB (bad)
    4007f5:	20 8d 04 42 0e 28    	and    BYTE PTR [rbp+0x280e4204],cl
    4007fb:	8c 05 48 0e 30 86    	mov    WORD PTR [rip+0xffffffff86300e48],es        # ffffffff86701649 <_end+0xffffffff861005f9>
    400801:	06                   	(bad)
    400802:	48 0e                	rex.W (bad)
    400804:	38 83 07 4d 0e 40    	cmp    BYTE PTR [rbx+0x400e4d07],al
    40080a:	72 0e                	jb     40081a <__GNU_EH_FRAME_HDR+0x11e>
    40080c:	38 41 0e             	cmp    BYTE PTR [rcx+0xe],al
    40080f:	30 41 0e             	xor    BYTE PTR [rcx+0xe],al
    400812:	28 42 0e             	sub    BYTE PTR [rdx+0xe],al
    400815:	20 42 0e             	and    BYTE PTR [rdx+0xe],al
    400818:	18 42 0e             	sbb    BYTE PTR [rdx+0xe],al
    40081b:	10 42 0e             	adc    BYTE PTR [rdx+0xe],al
    40081e:	08 00                	or     BYTE PTR [rax],al
    400820:	10 00                	adc    BYTE PTR [rax],al
    400822:	00 00                	add    BYTE PTR [rax],al
    400824:	c0 00 00             	rol    BYTE PTR [rax],0x0
    400827:	00 88 fe ff ff 02    	add    BYTE PTR [rax+0x2fffffe],cl
    40082d:	00 00                	add    BYTE PTR [rax],al
    40082f:	00 00                	add    BYTE PTR [rax],al
    400831:	00 00                	add    BYTE PTR [rax],al
  	...

  0000000000400834 <__FRAME_END__>:
    400834:	00 00                	add    BYTE PTR [rax],al
  	...

Disassembly of section .init_array:

  0000000000600e10 <__frame_dummy_init_array_entry>:
    600e10:	c0 05 40 00 00 00 00 	rol    BYTE PTR [rip+0x40],0x0        # 600e57 <_DYNAMIC+0x37>
  	...

Disassembly of section .fini_array:

  0000000000600e18 <__do_global_dtors_aux_fini_array_entry>:
    600e18:	90                   	nop
    600e19:	05 40 00 00 00       	add    eax,0x40
  	...

Disassembly of section .dynamic:

  0000000000600e20 <_DYNAMIC>:
    600e20:	01 00                	add    DWORD PTR [rax],eax
    600e22:	00 00                	add    BYTE PTR [rax],al
    600e24:	00 00                	add    BYTE PTR [rax],al
    600e26:	00 00                	add    BYTE PTR [rax],al
    600e28:	01 00                	add    DWORD PTR [rax],eax
    600e2a:	00 00                	add    BYTE PTR [rax],al
    600e2c:	00 00                	add    BYTE PTR [rax],al
    600e2e:	00 00                	add    BYTE PTR [rax],al
    600e30:	0c 00                	or     al,0x0
    600e32:	00 00                	add    BYTE PTR [rax],al
    600e34:	00 00                	add    BYTE PTR [rax],al
    600e36:	00 00                	add    BYTE PTR [rax],al
    600e38:	70 04                	jo     600e3e <_DYNAMIC+0x1e>
    600e3a:	40 00 00             	add    BYTE PTR [rax],al
    600e3d:	00 00                	add    BYTE PTR [rax],al
    600e3f:	00 0d 00 00 00 00    	add    BYTE PTR [rip+0x0],cl        # 600e45 <_DYNAMIC+0x25>
    600e45:	00 00                	add    BYTE PTR [rax],al
    600e47:	00 b4 06 40 00 00 00 	add    BYTE PTR [rsi+rax*1+0x40],dh
    600e4e:	00 00                	add    BYTE PTR [rax],al
    600e50:	19 00                	sbb    DWORD PTR [rax],eax
    600e52:	00 00                	add    BYTE PTR [rax],al
    600e54:	00 00                	add    BYTE PTR [rax],al
    600e56:	00 00                	add    BYTE PTR [rax],al
    600e58:	10 0e                	adc    BYTE PTR [rsi],cl
    600e5a:	60                   	(bad)
    600e5b:	00 00                	add    BYTE PTR [rax],al
    600e5d:	00 00                	add    BYTE PTR [rax],al
    600e5f:	00 1b                	add    BYTE PTR [rbx],bl
    600e61:	00 00                	add    BYTE PTR [rax],al
    600e63:	00 00                	add    BYTE PTR [rax],al
    600e65:	00 00                	add    BYTE PTR [rax],al
    600e67:	00 08                	add    BYTE PTR [rax],cl
    600e69:	00 00                	add    BYTE PTR [rax],al
    600e6b:	00 00                	add    BYTE PTR [rax],al
    600e6d:	00 00                	add    BYTE PTR [rax],al
    600e6f:	00 1a                	add    BYTE PTR [rdx],bl
    600e71:	00 00                	add    BYTE PTR [rax],al
    600e73:	00 00                	add    BYTE PTR [rax],al
    600e75:	00 00                	add    BYTE PTR [rax],al
    600e77:	00 18                	add    BYTE PTR [rax],bl
    600e79:	0e                   	(bad)
    600e7a:	60                   	(bad)
    600e7b:	00 00                	add    BYTE PTR [rax],al
    600e7d:	00 00                	add    BYTE PTR [rax],al
    600e7f:	00 1c 00             	add    BYTE PTR [rax+rax*1],bl
    600e82:	00 00                	add    BYTE PTR [rax],al
    600e84:	00 00                	add    BYTE PTR [rax],al
    600e86:	00 00                	add    BYTE PTR [rax],al
    600e88:	08 00                	or     BYTE PTR [rax],al
    600e8a:	00 00                	add    BYTE PTR [rax],al
    600e8c:	00 00                	add    BYTE PTR [rax],al
    600e8e:	00 00                	add    BYTE PTR [rax],al
    600e90:	f5                   	cmc
    600e91:	fe                   	(bad)
    600e92:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    600e95:	00 00                	add    BYTE PTR [rax],al
    600e97:	00 78 02             	add    BYTE PTR [rax+0x2],bh
    600e9a:	40 00 00             	add    BYTE PTR [rax],al
    600e9d:	00 00                	add    BYTE PTR [rax],al
    600e9f:	00 05 00 00 00 00    	add    BYTE PTR [rip+0x0],al        # 600ea5 <_DYNAMIC+0x85>
    600ea5:	00 00                	add    BYTE PTR [rax],al
    600ea7:	00 40 03             	add    BYTE PTR [rax+0x3],al
    600eaa:	40 00 00             	add    BYTE PTR [rax],al
    600ead:	00 00                	add    BYTE PTR [rax],al
    600eaf:	00 06                	add    BYTE PTR [rsi],al
    600eb1:	00 00                	add    BYTE PTR [rax],al
    600eb3:	00 00                	add    BYTE PTR [rax],al
    600eb5:	00 00                	add    BYTE PTR [rax],al
    600eb7:	00 98 02 40 00 00    	add    BYTE PTR [rax+0x4002],bl
    600ebd:	00 00                	add    BYTE PTR [rax],al
    600ebf:	00 0a                	add    BYTE PTR [rdx],cl
    600ec1:	00 00                	add    BYTE PTR [rax],al
    600ec3:	00 00                	add    BYTE PTR [rax],al
    600ec5:	00 00                	add    BYTE PTR [rax],al
    600ec7:	00 62 00             	add    BYTE PTR [rdx+0x0],ah
    600eca:	00 00                	add    BYTE PTR [rax],al
    600ecc:	00 00                	add    BYTE PTR [rax],al
    600ece:	00 00                	add    BYTE PTR [rax],al
    600ed0:	0b 00                	or     eax,DWORD PTR [rax]
    600ed2:	00 00                	add    BYTE PTR [rax],al
    600ed4:	00 00                	add    BYTE PTR [rax],al
    600ed6:	00 00                	add    BYTE PTR [rax],al
    600ed8:	18 00                	sbb    BYTE PTR [rax],al
    600eda:	00 00                	add    BYTE PTR [rax],al
    600edc:	00 00                	add    BYTE PTR [rax],al
    600ede:	00 00                	add    BYTE PTR [rax],al
    600ee0:	15 00 00 00 00       	adc    eax,0x0
  	...
    600eed:	00 00                	add    BYTE PTR [rax],al
    600eef:	00 03                	add    BYTE PTR [rbx],al
  	...
    600ef9:	10 60 00             	adc    BYTE PTR [rax+0x0],ah
    600efc:	00 00                	add    BYTE PTR [rax],al
    600efe:	00 00                	add    BYTE PTR [rax],al
    600f00:	02 00                	add    al,BYTE PTR [rax]
    600f02:	00 00                	add    BYTE PTR [rax],al
    600f04:	00 00                	add    BYTE PTR [rax],al
    600f06:	00 00                	add    BYTE PTR [rax],al
    600f08:	60                   	(bad)
    600f09:	00 00                	add    BYTE PTR [rax],al
    600f0b:	00 00                	add    BYTE PTR [rax],al
    600f0d:	00 00                	add    BYTE PTR [rax],al
    600f0f:	00 14 00             	add    BYTE PTR [rax+rax*1],dl
    600f12:	00 00                	add    BYTE PTR [rax],al
    600f14:	00 00                	add    BYTE PTR [rax],al
    600f16:	00 00                	add    BYTE PTR [rax],al
    600f18:	07                   	(bad)
    600f19:	00 00                	add    BYTE PTR [rax],al
    600f1b:	00 00                	add    BYTE PTR [rax],al
    600f1d:	00 00                	add    BYTE PTR [rax],al
    600f1f:	00 17                	add    BYTE PTR [rdi],dl
    600f21:	00 00                	add    BYTE PTR [rax],al
    600f23:	00 00                	add    BYTE PTR [rax],al
    600f25:	00 00                	add    BYTE PTR [rax],al
    600f27:	00 10                	add    BYTE PTR [rax],dl
    600f29:	04 40                	add    al,0x40
    600f2b:	00 00                	add    BYTE PTR [rax],al
    600f2d:	00 00                	add    BYTE PTR [rax],al
    600f2f:	00 07                	add    BYTE PTR [rdi],al
    600f31:	00 00                	add    BYTE PTR [rax],al
    600f33:	00 00                	add    BYTE PTR [rax],al
    600f35:	00 00                	add    BYTE PTR [rax],al
    600f37:	00 e0                	add    al,ah
    600f39:	03 40 00             	add    eax,DWORD PTR [rax+0x0]
    600f3c:	00 00                	add    BYTE PTR [rax],al
    600f3e:	00 00                	add    BYTE PTR [rax],al
    600f40:	08 00                	or     BYTE PTR [rax],al
    600f42:	00 00                	add    BYTE PTR [rax],al
    600f44:	00 00                	add    BYTE PTR [rax],al
    600f46:	00 00                	add    BYTE PTR [rax],al
    600f48:	30 00                	xor    BYTE PTR [rax],al
    600f4a:	00 00                	add    BYTE PTR [rax],al
    600f4c:	00 00                	add    BYTE PTR [rax],al
    600f4e:	00 00                	add    BYTE PTR [rax],al
    600f50:	09 00                	or     DWORD PTR [rax],eax
    600f52:	00 00                	add    BYTE PTR [rax],al
    600f54:	00 00                	add    BYTE PTR [rax],al
    600f56:	00 00                	add    BYTE PTR [rax],al
    600f58:	18 00                	sbb    BYTE PTR [rax],al
    600f5a:	00 00                	add    BYTE PTR [rax],al
    600f5c:	00 00                	add    BYTE PTR [rax],al
    600f5e:	00 00                	add    BYTE PTR [rax],al
    600f60:	fe                   	(bad)
    600f61:	ff                   	(bad)
    600f62:	ff 6f 00             	jmp    FWORD PTR [rdi+0x0]
    600f65:	00 00                	add    BYTE PTR [rax],al
    600f67:	00 b0 03 40 00 00    	add    BYTE PTR [rax+0x4003],dh
    600f6d:	00 00                	add    BYTE PTR [rax],al
    600f6f:	00 ff                	add    bh,bh
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
    600f87:	00 a2 03 40 00 00    	add    BYTE PTR [rdx+0x4003],ah
  	...

Disassembly of section .got:

  0000000000600ff0 <.got>:
  	...

Disassembly of section .got.plt:

  0000000000601000 <_GLOBAL_OFFSET_TABLE_>:
    601000:	20 0e                	and    BYTE PTR [rsi],cl
    601002:	60                   	(bad)
  	...
    601017:	00 a6 04 40 00 00    	add    BYTE PTR [rsi+0x4004],ah
    60101d:	00 00                	add    BYTE PTR [rax],al
    60101f:	00 b6 04 40 00 00    	add    BYTE PTR [rsi+0x4004],dh
    601025:	00 00                	add    BYTE PTR [rax],al
    601027:	00 c6                	add    dh,al
    601029:	04 40                	add    al,0x40
    60102b:	00 00                	add    BYTE PTR [rax],al
    60102d:	00 00                	add    BYTE PTR [rax],al
    60102f:	00 d6                	add    dh,dl
    601031:	04 40                	add    al,0x40
    601033:	00 00                	add    BYTE PTR [rax],al
    601035:	00 00                	add    BYTE PTR [rax],al
  	...

Disassembly of section .data:

  0000000000601038 <__data_start>:
  	...

  0000000000601040 <__dso_handle>:
  	...

Disassembly of section .bss:

  0000000000601048 <__bss_start>:
  	...

Disassembly of section .comment:

  0000000000000000 <.comment>:
     0:	47                   	rex.RXB
     1:	43                   	rex.XB
     2:	43 3a 20             	rex.XB cmp spl,BYTE PTR [r8]
     5:	28 55 62             	sub    BYTE PTR [rbp+0x62],dl
     8:	75 6e                	jne    78 <_init-0x4003f8>
     a:	74 75                	je     81 <_init-0x4003ef>
     c:	20 37                	and    BYTE PTR [rdi],dh
     e:	2e 33 2e             	xor    ebp,DWORD PTR cs:[rsi]
    11:	30 2d 31 36 75 62    	xor    BYTE PTR [rip+0x62753631],ch        # 62753648 <_end+0x621525f8>
    17:	75 6e                	jne    87 <_init-0x4003e9>
    19:	74 75                	je     90 <_init-0x4003e0>
    1b:	33 29                	xor    ebp,DWORD PTR [rcx]
    1d:	20 37                	and    BYTE PTR [rdi],dh
    1f:	2e 33 2e             	xor    ebp,DWORD PTR cs:[rsi]
    22:	30 00                	xor    BYTE PTR [rax],al
    24:	63 6c 61 6e          	movsxd ebp,DWORD PTR [rcx+riz*2+0x6e]
    28:	67 20 76 65          	and    BYTE PTR [esi+0x65],dh
    2c:	72 73                	jb     a1 <_init-0x4003cf>
    2e:	69 6f 6e 20 36 2e 30 	imul   ebp,DWORD PTR [rdi+0x6e],0x302e3620
    35:	2e 30 2d 31 75 62 75 	xor    BYTE PTR cs:[rip+0x75627531],ch        # 7562756d <_end+0x7502651d>
    3c:	6e                   	outs   dx,BYTE PTR ds:[rsi]
    3d:	74 75                	je     b4 <_init-0x4003bc>
    3f:	32 20                	xor    ah,BYTE PTR [rax]
    41:	28 74 61 67          	sub    BYTE PTR [rcx+riz*2+0x67],dh
    45:	73 2f                	jae    76 <_init-0x4003fa>
    47:	52                   	push   rdx
    48:	45                   	rex.RB
    49:	4c                   	rex.WR
    4a:	45                   	rex.RB
    4b:	41 53                	push   r11
    4d:	45 5f                	rex.RB pop r15
    4f:	36 30 30             	xor    BYTE PTR ss:[rax],dh
    52:	2f                   	(bad)
    53:	66 69 6e 61 6c 29    	imul   bp,WORD PTR [rsi+0x61],0x296c
  	...
