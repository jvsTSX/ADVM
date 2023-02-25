	.org 0   ; entry point
  jmpf Start
	.org $03 ; External int. (INTO)                 - I01CR
  reti
	.org $0B ; External int. (INT1)                 - I01CR
  reti
	.org $13 ; External int. (INT2) and Timer 0 low - I23CR and T0CNT
  reti
	.org $1B ; External int. (INT3) and base timer  - I23CR and BTCR
  reti
	.org $23 ; Timer 0 high                         - T0CNT
  reti
	.org $2B ; Timer 1 Low and High                 - T1CNT
  reti
	.org $33 ; Serial IO 1                          - SCON0
  reti
	.org $3B ; Serial IO 2                          - SCON1
  reti
	.org $43 ; MAPLE?                               - MAPLE?????? (160h/161h) (MAPLE?????????)
  reti
	.org $4B ; Port 3 interrupt                     - P3INT
  reti


 	.org $1F0 ; exit app mode
goodbye:	
	not1 EXT, 0
  jmpf goodbye



	.org $200
	.byte "audio drive test" ; ................... 16-byte Title
	.byte "https://github.com/jvsTSX/ADVM  " ; ... 32-byte Description
	.org $240 ; >>> ICON HEADER
	.org $260 ; >>> PALETTE TABLE
	.org $280 ; >>> ICON DATA

;    /////////////////////////////////////////////////////////////
;   ///                       GAME CODE                       ///
;  /////////////////////////////////////////////////////////////

	.include "sfr.i" 
	
; ////// START 
Start:

;	not1 PSW, 1
	mov #0, IE    ; disable ints to configure them first
	mov #%10000000, VCCR
	mov #%00001001, MCR
	mov #%10000001, OCR
	mov #0, P3INT ; i don't want joypad ints
	
	; setup T0
	mov #%01000001, T0CON ; Low running, int enabled
	mov #0, T0PRR
	mov #1, T0L
	mov #$DF, T0LR
	
	; i don't know what these do but they enable T1 to output audio to P1
	mov #$80, P1FCR
	clr1 P1, 7
	mov #$80, P1DDR
	mov #%11010000, T1CNT ; except this one
	
	mov #<SONGDATA_0, TRL
	mov #>SONGDATA_0, TRH
	callf ADVM_SETUP
	mov #$80, IE
	
	mov #<TESTSFXLIST, ADVMSFX_ListLocalLow ; setup SFX local
	mov #>TESTSFXLIST, ADVMSFX_ListLocalHigh
	
	mov #$FF, LastKeys
	
	; copy the example picture
	mov #<ADVMexampleScreen, TRL
	mov #>ADVMexampleScreen, TRH
	xor ACC
	st XBNK
	st C
	mov #6, B
	mov #$80, 2
.CopyLoop
	ld C
	ldc
	st @r2
	inc 2
	inc C
	ld C
	ldc
	st @r2
	inc 2
	inc C
	
	dec B
	ld B
  bnz .CopyLoop
	
	ld 2
	add #4
	st 2
	mov #6, B

  bn PSW, 7, .CopyLoop
  bp XBNK, 0, .ExitCopyLoop	
	inc XBNK
	set1 2, 7
  br .CopyLoop
.ExitCopyLoop:



; ////// MAIN
Main:
LastKeys = $2F
	
	; SFX dakjthlkdjgtadgjkfsabkjgasbfg
	ld P3 ; get keys from port 3
	st B
	ld LastKeys
  be B, .SkipInputs
	
	
	bp ACC, 0, .NoKeyUp
	mov #0, ADVMRAM_SFXoverlay
.NoKeyUp:

	bp ACC, 1, .NoKeyDown
	mov #1, ADVMRAM_SFXoverlay
.NoKeyDown:

	bp ACC, 2, .NoKeyLeft
	mov #2, ADVMRAM_SFXoverlay
.NoKeyLeft:

	bp ACC, 3, .NoKeyRight
	mov #3, ADVMRAM_SFXoverlay
.NoKeyRight:

	bp ACC, 6, .ModeNotPressed
	jmpf goodbye
.ModeNotPressed

	ld B
	st LastKeys
.SkipInputs:
	
	
	callf ADVM_RUN
	callf ADVMSFX_RUN
	
	
	mov #1, PCON
  br Main

	.include "ADVM1_1.s"



SONGDATA_0:
; index list header
	.word TimeLineBuzzer
	.word PhrasesIndex
	.word GmacroIndex
	.word GrooveTable
;	.byte 4 ; your song's tick speed
	.byte 0 ; groove table start pos

GrooveTable:
	.byte 04
	.byte $FF, $FF
	.byte 02
	.byte $FF, $FF

TimeLineBuzzer:
;  phrase no | transpose
;	.byte 03
	.byte 00, 0
	.byte 01, 0
	.byte 00, 0
	.byte 02, 0
	.byte 03

PhrasesIndex:
	.word SONGDATA0_Phrase0
	.word SONGDATA0_Phrase1
	.word SONGDATA0_Phrase2
	.word SONGDATA0_Phrase3

SONGDATA0_Phrase0:
	.byte 02, $E3, $0C, $FF,    %10100000, 0
	
	
	.byte 00, $0B
	.byte 02, $17,    %10100000, 0
	
	
	.byte 00, $0B
	.byte %01000001,    %10100000, 0
	
	.byte 01, $21
	
	.byte 01, $20,    %10100000, 0
	
	.byte 00, $21
	.byte 00, $0B
	.byte $21


SONGDATA0_Phrase1:
	.byte 00, $20,    %10100000, 0
	.byte 00, $21
	.byte 01, $20
	
	.byte 01, $20,    %10100000, 0
	
	.byte 00, $1C
	.byte 00, $17
	.byte %01000000,    %10100000, 0
	.byte 00, $0B
	.byte 01, $14
	
	.byte 00, $0B,    %10100000, 0
	.byte 01, $17
	
	.byte 00, $0B
	.byte 01, $1F,    %10100000, 0
	
	.byte 00, $13
	.byte 00, $21
	.byte %01000000,    %10100000, 0
	.byte 00, $13
	.byte 01, $1E
	
	.byte 00, $13,    %10100000, 0
	.byte 01, $1C
	
	.byte 00, $13
	.byte 01, $1A,    %10100000, 0

	.byte 01, $13
	
	.byte 01, $21,    %10100000, 0
	
	.byte 00, $15
	.byte 00, $23
	.byte %01000000,    %10100000, 0
	.byte 00, $15
	.byte 01, $20
	
	.byte 00, $15,    %10100000, 0
	.byte 01, $1C
	
	.byte 00, $15
	.byte 01, $17,    %10100000, 0
	
	.byte 01, $09
	
	.byte $21


SONGDATA0_Phrase2:
	.byte 00, $20,    %10100000, 0
	.byte 00, $21
	.byte 00, $20
	.byte 00, $0B
	.byte 01, $20,    %10100000, 0
	
	.byte 00, $23
	.byte 00, $28
	.byte %01000000,    %10100000, 0
	.byte 01, $0B
	
	.byte 00, $2A
	.byte 01, $28,    %10100000, 0
	
	.byte 01, $23
	
	.byte 01, $13,    %10100000, 0
	
	.byte 01, $1F
	
	.byte 01, $1F,    %10100000, 0
	
	.byte 01, $13
	
	.byte 01, $1E,    %10100000, 0
	
	.byte 00, $13
	.byte 00, $1C
	.byte %01000000,    %10100000, 0
	.byte 00, $13
	.byte 01, $1A
	
	.byte 01, $1A,    %10100000, 0
	
	.byte 00, $15
	.byte 00, $1C
	.byte %01000000,    %10100000, 0
	.byte 00, $15
	.byte 01, $19
	
	.byte 01, $15,    %10100000, 0
	
	.byte 01, $1A
	
	.byte 01, $19,    %10100000, 0
	
	.byte 01, $15
	
	.byte $21



SONGDATA0_Phrase3:
;	.byte 00, $E3, $0C, $FF
;	.byte 00, $F3, $08, $FF
	.byte $20, $00



GmacroIndex:
	.word SONGDATA0_Gmacro0

SONGDATA0_Gmacro0:
	.byte %11010000, $A0
	.byte %11011110, $90
	.byte %11011110, $00
	.byte %01000000
	
	
	
	
;	SFX TESTS
TESTSFXLIST:
	.word .SFX0
	.word .SFX1
	.word .SFX2
	.word .SFX3
	
.SFX0:
	.byte %11010000, $FE
	.byte %11010000, $FD
	.byte %11010000, $FC
	.byte %11010000, $FB
	.byte %11010000, $FA
	.byte 0
	
.SFX1:
	.byte %11010000, $E9
	.byte %10010010
	.byte %10110100, 2
	.byte %10011000
	.byte 0
	
.SFX2:
	.byte %11101000, $FB, 2
	.byte %11101000, $FC, 2
	.byte %11101000, $FD, 2
	.byte %11101000, $FA, 2
	.byte 0
	
.SFX3:
	.byte %11010100, $EC
	.byte %11010100, $F0
	.byte %11010100, $F8
	.byte %11010100, $EC
	.byte %11010100, $F0
	.byte %11010100, $F8
	.byte %11010100, $EC
	.byte %11010100, $F0
	.byte %11010100, $F8
	.byte 0
	
	
	
ADVMexampleScreen:
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000011, %00110000, %00000001, %11011101, %11011100
	.byte %00000000, %00101001, %00010000, %00000000, %01010100, %01001100
	.byte %00000000, %00101001, %00010000, %00000001, %10010101, %10000100
	.byte %00000000, %00010001, %01010000, %00000001, %11011101, %11011100
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %00000001, %11111111, %11111111, %11110011, %11111100, %11111100
	.byte %00000001, %00000100, %00000011, %00010010, %00100100, %10000100
	.byte %00000010, %00000100, %00000001, %00010100, %01000011, %00000100
	.byte %00000010, %01000100, %01110001, %00010100, %01000011, %00000100
	.byte %00000100, %01000100, %01010000, %10011000, %10000000, %00000100
	.byte %00000100, %11000100, %01001000, %10011000, %10000000, %00000100
	.byte %00001000, %11000100, %01001000, %10010001, %10001000, %01000100
	.byte %00001001, %11000100, %01001000, %10010001, %10001000, %01000100
	.byte %00010000, %00000100, %01001000, %10000010, %10001100, %11000100
	.byte %00010000, %00000100, %01010000, %10000010, %10001100, %11000100
	.byte %00100011, %11000100, %01110001, %00000100, %10001011, %01000100
	.byte %00100010, %01000100, %00000001, %00000100, %10001011, %01000100
	.byte %01000100, %01000100, %00000011, %00001000, %10001000, %01000100
	.byte %01111100, %01111111, %11111111, %11111000, %11111000, %01111100
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
	.byte %10011111, %11111111, %11111111, %11111111, %01111001, %00010101
	.byte %10101111, %11111111, %11111110, %01111101, %01110111, %01110101
	.byte %10101101, %10011001, %10011110, %01111011, %01110111, %01110101
	.byte %10001011, %01010111, %01111000, %00011000, %01111011, %00111011
	.byte %10111011, %00011011, %10111000, %00011111, %01111101, %01110101
	.byte %10111011, %01111101, %11011110, %01111111, %01111101, %01110101
	.byte %10111011, %10010011, %00111110, %01111111, %01110011, %01110101
	.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111

	
;	.byte %00000000, %00000001, %00000000, %00000000, %00000000, %00000000
;	.byte %00000000, %00000000, %00000000, %00100000, %00000000, %00000000
;	.byte %00000000, %00000001, %01001001, %10100000, %00000000, %00000000
;	.byte %00000000, %00000001, %01001011, %00000000, %00000000, %00000000
;	.byte %00000000, %00000101, %01010000, %10000000, %00000000, %00000000
;	.byte %00000000, %00000110, %00100011, %10000000, %00000000, %00000000
;	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
;	.byte %00000000, %00011111, %11111111, %11100011, %11110111, %11000000
;	.byte %00000000, %00010001, %00000011, %00100010, %01010100, %01000000
;	.byte %00000000, %00100001, %00000001, %00100100, %10001000, %01000000
;	.byte %00000000, %00100001, %00111001, %00100100, %10001000, %01000000
;	.byte %00000000, %01000001, %00101000, %10101001, %00000000, %01000000
;	.byte %00000000, %01001001, %00100100, %10101001, %00000000, %01000000
;	.byte %00000000, %10001001, %00100100, %10110011, %00100010, %01000000
;	.byte %00000000, %10000001, %00100100, %10110011, %00100010, %01000000
;	.byte %00000001, %00000001, %00100100, %10000101, %00110110, %01000000
;	.byte %00000001, %00111001, %00101000, %10000101, %00110110, %01000000
;	.byte %00000010, %01001001, %00111001, %00001001, %00101010, %01000000
;	.byte %00000010, %01001001, %00000001, %00001001, %00101010, %01000000
;	.byte %00000100, %10001001, %00000011, %00010001, %00100010, %01000000
;	.byte %00000111, %10001111, %11111111, %11110001, %11100011, %11000000
;	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
;	.byte %00000100, %10010001, %11000000, %00011101, %11011101, %11000000
;	.byte %00000100, %10010001, %01000000, %00000101, %01000100, %11000000
;	.byte %00000101, %00010001, %01000000, %00001001, %01001000, %01000000
;	.byte %00000010, %00010101, %11000000, %00011101, %11011101, %11000000
;	.byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
;	.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
;	.byte %11000110, %10001100, %11001111, %01111010, %11110010, %00101011
;	.byte %11000101, %10011101, %11011110, %00111000, %11110110, %01110111
;	.byte %11011101, %10001001, %10011111, %01111110, %11100110, %11101011
;	.byte %11111111, %11111111, %11111111, %11111111, %11111111, %11111111
	
	
	
	.cnop 0, $200
