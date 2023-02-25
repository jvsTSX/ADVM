; ///////////////////////////////////////////////////////////////
;          ____________  __    ______    ____    __++++++__
;        /     |   __  \|  |  /  /   \  /    |  |  @       |
;       /  /|  |  |  \  \  | /  /     \/     |  |  ------  |
;      /  /_|  |  |   |  | |/  /|  |\    /|  |  | |      | |
;     /  ___   |  |   |  |    / |  | \  / |  |  | |      | |
;    /  /   |  |  |__/  /    /  |  |  \/  |  |  |  ------  |
;   /__/    |__|_______/|___/   |__|      |__|  | _+_ o o  |
;        Audio Driver for (dreamcast) VMu       |  +   O O |
;                         jvsTSX  /  2023        --______--
; ///////////////////////////////////////////////////////////////
;	
;	special thanks to Tildearrow for the duty formula
;	and the dreamcast community for incentivating this project
;
;	port of ADPM for dreamcast VMU - cut down to fit for 133KHz apps
;		features:
;
;		version 1.0
;		- timeline and phrase format like trackers
;		- length encoded notes for better space usage
;		- General purpose Macro (Gmacro)
;		- delayed "off-grid" note support
;		- F-0 to C#4 frequency range
;		- fixed notes on Gmacro for drums
;		- 5-bit pulse widths ranging from super thin to square
;		- Sound effects sub-driver included (you can optionally replace it)
;
;		version 1.1 
;		- groove table
;
;	performance:
;		- RAM usage				: 34 bytes music, 6 bytes SFX
;		- CPU usage				: ~550 cycles worst ever case*
;		- Flash usage (driver)  : 1187 bytes
;		- Flash usage (song)    : 2-4 bytes per note, most other commands are 2 bytes
;
;	*worst ever = 7 commands on Block B at once, everything enabled on Block A (and firing)



;    /////////////////////////////////////////////////////////////
;   ///                      SETUP CODE                       ///
;  /////////////////////////////////////////////////////////////
ADVM_SETUP:
	xor ACC
	st ADVMRAM_OffsetNote
	st ADVMRAM_Flags
	ldc
	st ADVMRAM_TmLineLocalLow
	st ADVMRAM_TmLinePosLow
	mov #1, ACC
	st ADVMRAM_TickCount
	st ADVMRAM_WaitCount
	ldc
	st ADVMRAM_TmLineLocalHigh
	st ADVMRAM_TmLinePosHigh
	mov #2, ACC
	ldc
	st ADVMRAM_PhraseLocalLow
	mov #3, ACC
	ldc
	st ADVMRAM_PhraseLocalHigh	
	mov #4, ACC
	ldc
	st ADVMRAM_GmacroLocalLow
	mov #5, ACC
	ldc
	st ADVMRAM_GmacroLocalHigh
	mov #6, ACC
	ldc
	st ADVMRAM_GrooveTableLocalLow
	mov #7, ACC
	ldc
	st ADVMRAM_GrooveTableLocalHigh
	mov #8, ACC
	ldc
	st ADVMRAM_GroovePos
	
	mov #$FF, ADVMRAM_SFXoverlay
	mov #$FF, ADVMRAM_FixedFreq

	; reference and get new transpose
	ld ADVMRAM_TmLinePosLow
	st TRL
	ld ADVMRAM_TmLinePosHigh
	st TRH
	mov #1, ACC
	ldc
	st ADVMRAM_TransposePend
	
	; add 2 to timeline pos
	ld ADVMRAM_TmLinePosLow
	add #2
	st ADVMRAM_TmLinePosLow
	xor ACC
	addc ADVMRAM_TmLinePosHigh
	st ADVMRAM_TmLinePosHigh
	
	; get new phrase index
	xor ACC
	ldc
	st B
	
	; reference index local
	ld ADVMRAM_PhraseLocalLow
	st TRL
	ld ADVMRAM_PhraseLocalHigh
	st TRH
	
	; add the index local with offset*2 value, then reference it and store into phrase pos
	ld B
	add ACC ; *2 the offset
  bn PSW, 7, .NoCarry
	inc TRH ; in case the 8th bit becomes 9th in the *2 process
.NoCarry:
	st B
	ldc 
	st ADVMRAM_PhrasePosLow
	ld B
	inc ACC
	ldc 
	st ADVMRAM_PhrasePosHigh	
  ret



;    /////////////////////////////////////////////////////////////
;   ///                  ADVM SFX SUB-Engine                  ///
;  /////////////////////////////////////////////////////////////
	
ADVMSFX_RUN:
	ld ADVMRAM_SFXoverlay
  bne #$FF, .CheckRun
.NoSFX:
  ret
.CheckRun:
  be #$FE, .SFXisRunning

	; fall condition: SFX to be setup
	st B
	ld ADVMSFX_ListLocalLow
	st TRL
	ld ADVMSFX_ListLocalHigh
	st TRH
	
	ld B
	add ACC
  bn PSW, 7, .NoCarry
	inc TRH
.NoCarry:
	st B
	ldc
	st ADVMSFX_PosLow
	ld B
	inc ACC
	ldc
	st ADVMSFX_PosHigh
	mov #1, ADVMSFX_Wait ; avoid locking the song out for 255 ticks
	mov #$FE, ADVMRAM_SFXoverlay ; signal it's running
	
.SFXisRunning:
	dec ADVMSFX_Wait ; check wait
	ld ADVMSFX_Wait
  bnz .NoSFX
	ld ADVMSFX_PosLow ; reference position
	st TRL
	ld ADVMSFX_PosHigh
	st TRH
	
	; first parameter: header and duty
	xor ACC
	ldc
  bn ACC, 7, .DisableSFX
	; continue SFX
	st B
	xor ACC
	st C
  bn B, 6, .DutyOnly
	; otherwise get Pitch field
	inc C
	ld C
	ldc
	st T1LR
.DutyOnly:
	ld B
	and #%00011111
  be #$1E, .NoDuty
	st ADVMSFX_Duty
.NoDuty:

	xor ACC ; check for wait field
  bn B, 5, .WaitIsOne
	inc C
	ld C
	ldc
.WaitIsOne:
	inc ACC
	st ADVMSFX_Wait
	
	inc C ; step SFX position
	ld C
	add TRL
  bn PSW, 7, .NoCarry2
	inc TRH
.NoCarry2:
	st ADVMSFX_PosLow
	ld TRH
	st ADVMSFX_PosHigh

	ld ADVMSFX_Duty ; calculate duty
  be #$1F, .Mute
	st B
	ld T1LR
	xor #$FF
	st C
	xor ACC
	mul
	mov #32, B
	div
	ld C
	xor #$FF
	st T1LC 
  ret

.DisableSFX:
	mov #$FF, ADVMRAM_SFXoverlay
  ret

.Mute:
	mov #$FF, T1LC
	mov #$FF, T1LR
  ret





; hello
;     there
;         this
;              is
;                 pro
;                     crastination
;
;                        |\
;                        | \
;       ___              |  |\              ___
;     -    -             /  | \            -   -
;    /      \         _-=      |__        /      \
;   |    _   \    _ /          /  \ _    /   _    |
;   |   / \   | _/                   \_ |   / \   |
;   |   |  |  |/   _--           --_   \|  |  |   |
;   |   |  |  /   /   |         |   \   \  |  |   |
;    \   \_| |  /  =_  \       /  _=  \  | |_/   /
;     \     |  |  || \  |     |  / ||  | |      /
;        ---|  |  || v|  \___/  |v ||  |  |---
;  ---_____/   |  \ --|   "="   |-- /  |   \____---
;   \_          \  ---     _     ---  /         _/
;     -__         \_      / \      _/        __-
;        --_____    ---_________---   _____--
;         --_______/\             /\______--
;                     \  \     /
;                       ||__--
;
;      and also a space between code to not let my dumb ass get lost in the source
;
;	... if you're looking for SFX subdriver docs, it's moved to the DOCUMENTATION section





;    /////////////////////////////////////////////////////////////
;   ///                    DRIVER BLOCK A                     ///
;  /////////////////////////////////////////////////////////////
ADVM_RUN:

; ////////////////// Kill count ///////
  bn ADVMRAM_Flags, 0, .SkipKill
	dec ADVMRAM_KillCount
	ld ADVMRAM_KillCount
  bnz .SkipKill
	clr1 ADVMRAM_Flags, 0 ; disable KillCount
	clr1 ADVMRAM_Flags, 2 ; disable Gmacro
	mov #$FF, ADVMRAM_FixedFreq
	mov #$1F, ADVMRAM_CurrDuty ; mute channel
.SkipKill: ; 12 cycles


; ////////////////// Delay count //////
  bn ADVMRAM_Flags, 1, .SkipDelay
	dec ADVMRAM_DelayCount
	ld ADVMRAM_DelayCount
  bnz .SkipDelay
	clr1 ADVMRAM_Flags, 1 ; disable DelayCount
	
	ld ADVMRAM_PendingNote ; grab note
	and #%00111111
	st ADVMRAM_CurrentNote
  bp ADVMRAM_PendingDuty, 5, .SkipDelay ; legato check
  
  ; apply duty
  bn ADVMRAM_PendingNote, 6, .UseLastDuty
	ld ADVMRAM_PendingDuty
	and #%00001111
	inc ACC
	st ADVMRAM_LastDuty
  br .SkipDuty
.UseLastDuty:
	ld ADVMRAM_LastDuty
.SkipDuty:
	st ADVMRAM_CurrDuty

	; apply Gmacro
  bp ADVMRAM_PendingNote, 7, .NewGmacro
  br .UseLastGmacro
.NewGmacro:
	ld ADVMRAM_PendingGmacro
	st ADVMRAM_LastGmacro
.UseLastGmacro:
; GmacroPos = word[ byte[PhrasePos]*2 + word[GmacroLocal] ]
	ld ADVMRAM_GmacroLocalLow
	st TRL
	ld ADVMRAM_GmacroLocalHigh
	st TRH
	
	ld ADVMRAM_LastGmacro ; check Gmacro number, FF = off
	clr1 ADVMRAM_Flags, 2
	be #$FF, .SkipDelay
	set1 ADVMRAM_Flags, 2
	
	add ACC
  bn PSW, 7, .NoCarry
	inc TRH
.NoCarry:
	st B
	ldc 
	st ADVMRAM_GmacroPosLow
	ld B
	inc ACC
	ldc
	st ADVMRAM_GmacroPosHigh
.SkipDelay: ; 47 cycles



;    /////////////////////////////////////////////////////////////
;   ///                    GMACRO HANDLER                     ///
;  /////////////////////////////////////////////////////////////
ADVM_Gmacro:
  bn ADVMRAM_Flags, 2, .SkipGmacro
	
	dec ADVMRAM_GmacroWait
	ld ADVMRAM_GmacroWait
  bnz .SkipGmacro
	
	ld ADVMRAM_GmacroPosLow
	st TRL
	ld ADVMRAM_GmacroPosHigh
	st TRH
	
	; first byte: action or end
	xor ACC
	st C
	ldc
  bn ACC, 7, .EndOrLoopMacro
	; otherwise action macro
	
	; pitch and duty
	st B
  bn B, 6, .RelModeDutyCheck
	inc C
	ld C
	ldc
  bpc ACC, 7, .FixedMode
	
	mov #$FF, ADVMRAM_FixedFreq
  bn ACC, 6, .NoEx
	set1 ACC, 7
.NoEx:
	st ADVMRAM_OffsetNote
	ld B
.RelModeDutyCheck:
	and #%00011111
  be #$1E, .NoFreq
	st ADVMRAM_CurrDuty
  br .NoFreq
	
.FixedMode:
	st ADVMRAM_FixedFreq
	ld B
	and #%00011111
  be #$1E, .NoFreq
	st ADVMRAM_DutyOverlay
.NoFreq:

	; wait field
	xor ACC
  bn B, 5, .WaitIsOne
	inc C
	ld C
	ldc
.WaitIsOne:
	inc ACC
	st ADVMRAM_GmacroWait
	inc C
  br .Step
	
.EndOrLoopMacro:
  bn ACC, 6, .BlockIsLoop
	clr1 ADVMRAM_Flags, 2 ; disable Gmacro
  br .SkipGmacro
	
.BlockIsLoop:
	inc ACC ; previous ACC = 0
	st ADVMRAM_GmacroWait ; reload wait so the macro don't softlock
	ldc ; grab offset
	st B ; important: the subtraction order DOES matter, TRL - B = good, B - TRL = bad
	ld TRL
	sub B
  bn PSW, 7, .NoCarry ; ^ really dumb dev note
	dec TRH           ; imagine not knowing basic maths
.NoBorrow
  br .NoCarry         ; right???

.Step:
	ld C
	add TRL
  bn PSW, 7, .NoCarry
	inc TRH
.NoCarry:
	st ADVMRAM_GmacroPosLow
	ld TRH
	st ADVMRAM_GmacroPosHigh
.SkipGmacro: ; 58 cycles worst



;    /////////////////////////////////////////////////////////////
;   ///                      PITCH PIPE                       ///
;  /////////////////////////////////////////////////////////////
ADVM_PitchPipe:
	; check SFX status
	ld ADVMRAM_SFXoverlay
  be #$FE, ADVM_TickCounter

	mov #<ADVM_NoteLut, TRL
	mov #>ADVM_NoteLut, TRH
	ld ADVMRAM_FixedFreq
  be #$FF, .RunPipe
	; otherwise run a fixed note
	ldc
	st T1LR
	xor #$FF
	st C
	ld ADVMRAM_DutyOverlay
  br .DutyCalc
	
.Mute:
	mov #$FF, T1LR
	mov #$FF, T1LC
  br ADVM_TickCounter
	
.RunPipe:
	; get note index
	ld ADVMRAM_CurrentNote
	add ADVMRAM_TransposeCurr
	add ADVMRAM_OffsetNote
	ldc
	st T1LR
	
	; duty calculation procedure: 
	; Compare = byte( word( byte(pitch) * bit5(duty) ) / 32 )
	xor #$FF ; invert to make calculation easier
	st C
	ld ADVMRAM_CurrDuty
.DutyCalc:
  be #$1F, .Mute
	
	st B
	xor ACC
	mul
	mov #32, B
	div
	ld C
	xor #$FF ; flip it back and store at T1 Compare data
	st T1LC 
	; 42 cycles full run



;    /////////////////////////////////////////////////////////////
;   ///                    DRIVER BLOCK B                     ///
;  /////////////////////////////////////////////////////////////
ADVM_TickCounter:
	dec ADVMRAM_TickCount
	ld ADVMRAM_TickCount
  bnz ADVM_EXIT
	; groove step
	ld ADVMRAM_GrooveTableLocalLow
	st TRL
	ld ADVMRAM_GrooveTableLocalHigh
	st TRH
	ld ADVMRAM_GroovePos
	ldc
  be #$FF, .endgroove
  br .end
.endgroove: ; groove reset
	ld ADVMRAM_GroovePos
	inc ACC ; get next byte
	ldc
	add ADVMRAM_GroovePos
	st ADVMRAM_GroovePos
	ldc 
.end:
	inc ADVMRAM_GroovePos
	st ADVMRAM_TickCount

	dec ADVMRAM_WaitCount ; check rest count
	ld ADVMRAM_WaitCount
  bnz ADVM_EXIT
	
	clr1 ADVMRAM_Flags, 7 ; clear continuity flag
	; 11 cycles
	
	; reference current phrase position
ADVM_NextCMDfull:
	ld ADVMRAM_PhrasePosLow
	st TRL
	ld ADVMRAM_PhrasePosHigh
ADVM_NextCMD:
	st TRH
	xor ACC
	ldc
	st C ; store it for later, low 5 bits might be useful
	mov #1, ACC
	ldc

  bp C, 7, .LowerHalf
  bp C, 6, .TopLowQuarter
  bp C, 5, .CmdIsEnd		; 00-
  jmpf ADVMCMD_PlayNote
.CmdIsEnd:
  jmpf ADVMCMD_EndEvent

.TopLowQuarter:				; 01-
  bp C, 5, .CmdIsKill
  jmpf ADVMCMD_Wait
.CmdIsKill:
  jmpf ADVMCMD_KillNote

.LowerHalf:
  bp C, 6, .LowerLowQuarter
  bp C, 5, .CommandIsGmacro
  jmpf ADVMCMD_DelayNote 	; 10-
.CommandIsGmacro:
  jmpf ADVMCMD_RunGmacro

.LowerLowQuarter: 			; 11-
  bp C, 5, .CmdIsSpeed
  jmpf ADVMCMD_SetDuty
.CmdIsSpeed:
  jmpf ADVMCMD_SetSpeed

	; 31 cycles any case (     full, first run     )
	; 20 cycles any case (  from end event command )
	; 25 cycles any case ( when returning from CMD )

;	PlayNote 		000 2-4 bytes
;	EndEvent		001 1-2 bytes
;	Wait			010 1-2 bytes
;	KillNote		011  2  bytes
;	DelayNote		100 3-4 bytes
;	RunGmacro		101  2  bytes
;	SetDuty			110  1  byte
;	SetSpeed		111  2  bytes


ADVM_EXIT:
  ret

ADVM_CMDreturn: ; return the byte skip amount in ACC	
	; offset phrase position by last command's size to fetch next command
	add ADVMRAM_PhrasePosLow
	st ADVMRAM_PhrasePosLow
	st TRL
	xor ACC
	addc ADVMRAM_PhrasePosHigh
	st ADVMRAM_PhrasePosHigh
  br ADVM_NextCMD


;    /////////////////////////////////////////////////////////////
;   ///                    LIBRARY SPACE                      ///
;  /////////////////////////////////////////////////////////////
ADVM_NoteLut:
	.byte $06 ; F-0   00
	.byte $14 ; F#0   01
	.byte $21 ; G-0   02
	.byte $2E ; G#0   03
	.byte $39 ; A-0   04
	.byte $45 ; A#0   05
	.byte $4F ; B-0   06

	.byte $59 ; C-1   07
	.byte $62 ; C#1   08
	.byte $6B ; D-1   09
	.byte $73 ; D#1   0A
	.byte $7C ; E-1   0B
	.byte $83 ; F-1   0C
	.byte $8A ; F#1   0D
	.byte $90 ; G-1   0E
	.byte $97 ; G#1   0F
	.byte $9D ; A-1   10
	.byte $A2 ; A#1   11
	.byte $A8 ; B-1   12

	.byte $AC ; C-2   13
	.byte $B1 ; C#2   14
	.byte $B6 ; D-2   15
	.byte $BA ; D#2   16
	.byte $BE ; E-2   17
	.byte $C1 ; F-2   18
	.byte $C5 ; F#2   19
	.byte $C8 ; G-2   1A
	.byte $CB ; G#2   1B
	.byte $CE ; A-2   1C
	.byte $D1 ; A#2   1D
	.byte $D4 ; B-2   1E

	.byte $D6 ; C-3   1F
	.byte $D9 ; C#3   20
	.byte $DB ; D-3   21
	.byte $DD ; D#3   22
	.byte $DF ; E-3   23
	.byte $E1 ; F-3   24
	.byte $E2 ; F#3   25
	.byte $E4 ; G-3   26
	.byte $E6 ; G#3   27
	.byte $E7 ; A-3   28
	.byte $E8 ; A#3   29
	.byte $EA ; B-3   2A

	.byte $EB ; C-4   2B
	.byte $EC ; C#4   2C


;    /////////////////////////////////////////////////////////////
;   ///                    COMMAND'S CODE                     ///
;  /////////////////////////////////////////////////////////////

ADVMCMD_PlayNote: ; ///////////////////////////////////// PLAY NOTE ///
	not1 ADVMRAM_Flags, 7
  bn ADVMRAM_Flags, 7, .ContinuityZero

	st B ; note index
	and #%00111111
	st ADVMRAM_CurrentNote
	ld ADVMRAM_TransposePend	; update transpose
	st ADVMRAM_TransposeCurr
	
	; get wait count
	ld C
	and #%00001111
	inc ACC
	st ADVMRAM_WaitCount
	
  bp C, 4, .NoteIsLegato ; if the note is a legato note, it will ignore the other two params
	mov #1, C
	ld ADVMRAM_PhrasePosLow
	st TRL
	ld ADVMRAM_PhrasePosHigh
	st TRH
	
	; grab duty parameter
  bn B, 6, .ReuseDuty
	inc C
	ld C
	ldc
	and #%00011111
	st ADVMRAM_LastDuty
  br .SkipDuty
.ReuseDuty:
	ld ADVMRAM_LastDuty
.SkipDuty:
	st ADVMRAM_CurrDuty
	
	; grab Gmacro parameter
	mov #1, ADVMRAM_GmacroWait
  bp B, 7, .NewGmacro
	br .UseLastGmacro

.NewGmacro:
	inc C
	ld C
	ldc
	st ADVMRAM_LastGmacro
	
.UseLastGmacro:
; GmacroPos = word[ byte[PhrasePos]*2 + word[GmacroLocal] ]
	ld ADVMRAM_GmacroLocalLow
	st TRL
	ld ADVMRAM_GmacroLocalHigh
	st TRH
	
	ld ADVMRAM_LastGmacro ; check Gmacro number, FF = off
	clr1 ADVMRAM_Flags, 2
  be #$FF, .SkipGmacro
	set1 ADVMRAM_Flags, 2
	
	add ACC
  bn PSW, 7, .NoCarry
	inc TRH
.NoCarry:
	st B
	ldc 
	st ADVMRAM_GmacroPosLow
	ld B
	inc ACC
	ldc
	st ADVMRAM_GmacroPosHigh
.SkipGmacro
	
	inc C
	ld C
  jmpf ADVM_CMDreturn ; 62 cycles worst

.ContinuityZero:
  ret

.NoteIsLegato:
	mov #2, ACC
  jmpf ADVM_CMDreturn



ADVMCMD_EndEvent: ; ///////////////////////////////////// END EVENT ///
; END SONG PROCEDURE:    TimeLinePos  = offset*2 + [TimeLineLocal] ; then do below
; END PHRASE PROCEDURE:  NewTranspose = byte[TimeLinePos + 1]
;                        TimeLinePos  + 2
;                        PhrasePos    = word[ byte[TimeLinePos + 0]*2 + word[PhraseLocal] ]

  bp C, 0, ADVM_CommandIsEndPhrase
	
;	; if it comes down here it's END SONG
	add ACC ; *2 the offset value
	mov #0, B ; use B to store the *2 carry if it happens (offset greater than 127)
  bn PSW, 7, ADVM_EndSongNoCarry
	inc B ; increment high if a carry occurs at *2
ADVM_EndSongNoCarry:
	add ADVMRAM_TmLineLocalLow
	st ADVMRAM_TmLinePosLow	
	ld B
	addc ADVMRAM_TmLineLocalHigh
	st ADVMRAM_TmLinePosHigh



ADVM_CommandIsEndPhrase: ; ////////////////////////////// END PHRASE ///
	; reference and get new transpose
	ld ADVMRAM_TmLinePosLow
	st TRL
	ld ADVMRAM_TmLinePosHigh
	st TRH
	mov #1, ACC
	ldc
	st ADVMRAM_TransposePend
	
	; add 2 to timeline pos
	ld ADVMRAM_TmLinePosLow
	add #2
	st ADVMRAM_TmLinePosLow
	xor ACC
	addc ADVMRAM_TmLinePosHigh
	st ADVMRAM_TmLinePosHigh
	
	; get new phrase index
	xor ACC
	ldc
	st B
	
	; reference index local
	ld ADVMRAM_PhraseLocalLow
	st TRL
	ld ADVMRAM_PhraseLocalHigh
	st TRH
	
	; add the index local with offset*2 value, then reference it and store into phrase pos
	ld B
	add ACC ; *2 the offset
  bn PSW, 7, ADVM_EndEvntNoCarry
	inc TRH ; in case the 8th bit becomes 9th in the *2 process
ADVM_EndEvntNoCarry:
	st B
	ldc 
	st ADVMRAM_PhrasePosLow
	ld B
	inc ACC
	ldc 
	st ADVMRAM_PhrasePosHigh	
  jmpf ADVM_NextCMDfull ; end phrase = 41 cycles, end song = 52 cycles 



ADVMCMD_Wait: ; ///////////////////////////////////////// WAIT STEPS ///
	not1 ADVMRAM_Flags, 7
  bn ADVMRAM_Flags, 7, .ContinuityZero

	st B
	ld C
  be #%01011111, .TwoBytes
	
	; otherwise 1 byte
	and #%00011111
	inc ACC
	st ADVMRAM_WaitCount
	
	mov #1, ACC
  jmpf ADVM_CMDreturn ; 14 cycles

.TwoBytes:
	ld B
	inc ACC
	st ADVMRAM_WaitCount
	
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 14 cycles

.ContinuityZero:
  ret



ADVMCMD_KillNote: ; ///////////////////////////////////// KILL NOTE ///
	inc ACC
	st ADVMRAM_KillCount
	set1 ADVMRAM_Flags, 0
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 7 cycles



ADVMCMD_DelayNote: ; //////////////////////////////////// DELAYED NOTE ///
	st ADVMRAM_PendingNote ; store note index
	ld C
	st ADVMRAM_PendingDuty ; store duty
	
	ld ADVMRAM_PhrasePosLow ; setup TR to grab wait
	st TRL
	ld ADVMRAM_PhrasePosHigh
	st TRH

	mov #2, ACC ; grab wait
	ldc
	inc ACC
	st ADVMRAM_DelayCount
	
	mov #3, C
  bn ADVMRAM_PendingNote, 7, .NoGmacro ; grab Gmacro or else 3-byte command
	ld C
	inc C
	ldc
	st ADVMRAM_PendingGmacro
	
.NoGmacro:
	set1 ADVMRAM_Flags, 1
	ld C
  jmpf ADVM_CMDreturn ; 24 cycles (worst)



ADVMCMD_RunGmacro: ; //////////////////////////////////// RUN GMACRO ///
	mov #1, ADVMRAM_GmacroWait
	st B
	
	ld ADVMRAM_GmacroLocalLow
	st TRL
	ld ADVMRAM_GmacroLocalHigh
	st TRH
	
	ld B
	clr1 ADVMRAM_Flags, 2
	be #$FF, .SkipGmacro
	set1 ADVMRAM_Flags, 2
	
	add ACC
  bn PSW, 7, .NoCarry
	inc TRH
.NoCarry:
	st B
	ldc 
	st ADVMRAM_GmacroPosLow
	ld B
	inc ACC
	ldc
	st ADVMRAM_GmacroPosHigh
.SkipGmacro

	mov #2, ACC
  jmpf ADVM_CMDreturn ; 29 cycles worst



ADVMCMD_SetDuty: ; ////////////////////////////////////// SET DUTY ///
	ld C
	and #%00011111
	st ADVMRAM_CurrDuty
	mov #1, ACC
  jmpf ADVM_CMDreturn ; 7 cycles



ADVMCMD_SetSpeed: ; ///////////////////////////////////// SET SPEED ///
  bp C, 2, .SpeedIsTempo
	st ADVMRAM_GroovePos
	
	ld ADVMRAM_GrooveTableLocalLow
	st TRL
	ld ADVMRAM_GrooveTableLocalHigh
	st TRH
	ld ADVMRAM_GroovePos
	ldc
  be #$FF, .endgroove
  br .end
	
.endgroove:
	ld ADVMRAM_GroovePos
	inc ACC ; get next byte
	ldc
	add ADVMRAM_GroovePos
	st ADVMRAM_GroovePos
	ldc 
.end:
	inc ADVMRAM_GroovePos
	st ADVMRAM_TickCount
	
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 25 cycles

.SpeedIsTempo:
  bp C, 1, .Timer1
  bp C, 0, .T0High
	st T0LR
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 11 cycles

.T0High:
	st T0HR
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 11 cycles

.Timer1:
	st T1HR
	mov #2, ACC
  jmpf ADVM_CMDreturn ; 9 cycles


;    /////////////////////////////////////////////////////////////
;   ///                    RAM DEFINITIONS                    ///
;  /////////////////////////////////////////////////////////////

; ADVM_MUSIC RAM USAGE: 34 bytes

; Block A
ADVMRAM_KillCount =		$04
ADVMRAM_DelayCount =	$05
ADVMRAM_PendingNote =   $06
ADVMRAM_PendingGmacro = $07
ADVMRAM_PendingDuty =   $08
ADVMRAM_GmacroPosLow =	$09
ADVMRAM_GmacroPosHigh =	$0A
ADVMRAM_GmacroWait = 	$0B
ADVMRAM_CurrentNote =	$0C
ADVMRAM_OffsetNote = 	$0D
ADVMRAM_TransposePend = $0E
ADVMRAM_TransposeCurr = $0F
ADVMRAM_FixedFreq =     $10
ADVMRAM_DutyOverlay =   $11
ADVMRAM_SFXoverlay =    $12
ADVMRAM_TickCount =     $13
ADVMRAM_GroovePos =		$14

; Block B
ADVMRAM_WaitCount =		$15
ADVMRAM_Flags = 		$16
ADVMRAM_PhrasePosLow =	$17
ADVMRAM_PhrasePosHigh =	$18
ADVMRAM_CurrDuty =      $19
ADVMRAM_LastDuty =		$1A
ADVMRAM_LastGmacro =    $1B
ADVMRAM_TmLinePosLow =	$1C
ADVMRAM_TmLinePosHigh =	$1D

; Position Block
ADVMRAM_TmLineLocalLow =		$1E
ADVMRAM_TmLineLocalHigh	=		$1F
ADVMRAM_PhraseLocalLow =		$20
ADVMRAM_PhraseLocalHigh	=		$21
ADVMRAM_GmacroLocalLow =		$22
ADVMRAM_GmacroLocalHigh	=		$23
ADVMRAM_GrooveTableLocalLow =   $24
ADVMRAM_GrooveTableLocalHigh =  $25

; SFX SUB-Engine block
; ADVM_SFX RAM USAGE: 6 bytes
ADVMSFX_ListLocalLow =  $26
ADVMSFX_ListLocalHigh = $27
ADVMSFX_Wait =			$28
ADVMSFX_PosLow = 		$29
ADVMSFX_PosHigh = 		$2A
ADVMSFX_Duty =			$2B

; TOTAL: 40 bytes or a little over 1/8 of the user arythmetic RAM





;    /////////////////////////////////////////////////////////////
;   ///                    DOCUMENTATION                      ///
;  /////////////////////////////////////////////////////////////
;
; ////////////////////// COMMANDS' FORMAT ///
;
;  0: PLAY NOTE - 000LWWWW GDNNNNNN ---DDDDD GGGGGGGG
;		L: Legato bit, will basically just change the note and ignore the last parameter fields and their request bits
; 		0001WWWW --NNNNNN
;	
;		W: Wait paremeter, literal WaitCount value+1 because 0 would mean waiting 255 steps (oops)
;	
;		G: request new Gmacro bit - will request the GGGGGGGG field
;			bit on    0000WWWW 1DNNNNNN ... GGGGGGGG
;			bit off   0000WWWW 0DNNNNNN ...
;	
;		D: request new Duty value - requrests new 5-bit duty value at the ---DDDDD field
;			bit on    0000WWWW G1NNNNNN ---DDDDD ...
;			bit off   0000WWWW G0NNNNNN ...
;
;		N: Note table index - grabs a note value in the Note table, you can see it in the //LIBRARY// section
;
;		if the Legato is 0 and N or D are also zero, this command will re-use the last specified duty and Gmacro
;       stored at Block B -> LastDuty and LastGmacro
;
;
;
;  1: END EVENT - 001----1  or  001-----0 SSSSSSSS
;		if the 0th bit of this command is 1, it ends the phrase to grab the next entry in the time line data
;		or else, if this bit is 0, it will reset the time line position added with the SSSSSSSS field
;
;		S: Song Start offset
;
;
;
;  2: WAIT STEPS - 010WWWWW  or  001111111 WWWWWWWW
;		sets a wait value+1 to WaitCount variable
;		if the wait value is below 1F, the command is a single-byte command, otherwise it's two-byte
;
;
;
;  3: KILL NOTE - 011----- KKKKKKKK
;		a two-byte command that sets the note kill counter at Block A (+1 like wait steps)
;		one tick of this command is the base refresh rate you define before the tick counter
;
;
;
;  4: DELAYED NOTE - 100LDDDD GDNNNNNN WWWWWWWW GGGGGGGG
;		similar to the PLAY NOTE command but it will wait in Block A ticks (the base refresh before the tick counter)
;		however, Wait and Duty are swapped to preserve one byte
;		as a side effect of Duty being reduced to 4-bits, you can only define duties ranging from 1/32 to 16/32
;
;		the parameter fields' meaning are the same as PLAY NOTE, see command 0 for details
;
;
;
;  5: RUN GMACRO - 101----- GGGGGGGG
;		a two-byte command that specifies a new Gmacro index, used for mid-note Gmacro changes like drums
;		if a non-legato PLAY NOTE happens afterwards, this value is discarded
;
;
;
;  6: SET PWM - 110DDDDD
;		an always single-byte command that specifies a new duty for mid-note changes
;		D is the 5-bit duty value for the duty formula, it's saved under CurrentDuty variable
;		if a non-legato PLAY NOTE happens afterwards, this value is discarded
;
;
;
;  7: SET SPEED - 111----- TTTTTTTT
;		a new value for the Tick counter (+1), used for changing the song's Block B run speed
;		NOTE FOR SELF: possibly make a mode where it reloads one of the T0 halves for tempo control?
;
;
;
; ////////////////////// GMACRO FORMAT ///
;
;	APWDDDDD MFFFFFFF WWWWWWWW
;   |||||||| |||||||| ||||||||
;   |||||||| |||||||| ++++++++---- Wait parameter
;   |||||||| |+++++++------------- note LUT index
;   |||||||| +-------------------- pitch Mode
;   |||+++++---------------------- Duty number
;   ||+--------------------------- request Wait field
;   |+---------------------------- request Freq field
;   +----------------------------- Action bit
;
;	Action bit: tells whether the macro ends (0) or acts on the sound registers (1)
;   req Pitch: requests Pitch field byte (PPPPPPPP), if it's 0 it won't update the pitch
;	req Wait:  requests Wait field byte (WWWWWWWW), if it's 0 it will wait only 1 tick
;	Duty: literal duty value EXCEPT when it's
;		$1E: ignore duty change and use last duty
;		$1F: mute channel
;	pitch Mode: tells whether it's a fixed note (1) or an arpeggio offset (0)
;		when on fixed mode (1) it redirects the pulse width value to DutyOverride variable
;
;	possible sizes:
;		100DDDDD
;		110DDDDD PPPPPPPP
;		101DDDDD MFFFFFFF
;		111DDDDD MFFFFFFF PPPPPPPP
;
;
;
;	0S------ BBBBBBBB
;    |       ||||||||
;    |       ++++++++---- go Back
;    +------------------- Stop bit
;
;	Stop bit: don't look for a Loop parameter and disable the Gmacro
;	go Back: offset current Gmacro pos by bytes amount backwards (GmacroPos - BBBBBBBB)
;
;	possible sizes:
;		01000000
;		00000000 BBBBBBBB
;
;
;	TODO: rewrite Gmacro and pitch pipe fixed part
;
;
; ////////////////////// SONG HEADER FORMAT ///
;
;	SONG_NAME label, it's used to know the start of the header, you feed its address to the setup code
;		.word	time line location, tells where the time line data starts at 
;		.word	phrase index table location, tells where the list of Phrase locations is on Flash
;		.word	Gmacro index table location, tells where the list of Gmacro locations is on Flash
;		.word	Groove table location, tells where the groove table is on Flash
;		.byte	Groove table start position
;
;
;	the driver setup code copies these words into the Position block of RAM (and the last byte into TickPreset)
;	access patterns for each local are:
;		TmLinePos <- TmLineLocal
;		PhrasePos <- Phrase data location <- Phrase index list
;		GmacroPos <- Gmacro data location <- Gmacro index list
;
;
;  Time line format:
;		PP TT (byte) - first byte (PP) contains a Phrase Index, second byte (TT) is a note transpose value
;
;
;
; ////////////////////// OTHER DOCUMENTATION ///
;
;		FLAGS variable: C----GDK
;						|    |||
;						|    ||+--- Kill note counter enable
;						|    |+---- Delay note is pending
;						|    +----- Gmacro enable
;						+---------- Continuity check
;
;			Continuity check: used for telling if a command that modifies step counter have been read already
;
;
;		SFXoverlay variable: 00-FC: new SFX pending, $FE: SFX being processed, $FF: no SFX running
;		FixedFreq variable:  00-FE: fixed T1 reload to play and ignore pitch pipe, FF: no pitch, run normally
;			if this variable is 00-FE the value at DutyOverlay variable is used, otherwise it's ignored
;
;
;
; ////////////////////// GROOVE TABLE ///
;	
;	the groove is a sequence of numbers to be used with the Block B tick counter, it affects the speed of the song
;	for example, a groove table that alternates between waiting 4 and 6 ticks will create a swing tempo
;
;	in order to use the groove table just specify its start position in the song's header
;	you can change the position within the groove table using comand 7 (SetSpeed) with the lower bits cleared
;
;	to tell the Tick counter to go back to a previous point you have to write $FF to mark an offset event
;	then the Tick counter will grab the next byte and add to its 8-bit position
;	this also means it overflows, so if you want to go back, all you have to do is to type a reaaally big number
;	like $FF to return one entry, $FE to return two, $FD to return three and so on...
;
;	maybe add an example song that shows this better... todo?
;
;
;
; /////////////////////// SFX SUBDRIVER ///
;
;	HOW TO SETUP LIST LOCAL
;	just write this
;		mov #<yoursfxlist, ADVMSFX_ListLocalLow
;		mov #>yoursfxlist, ADVMSFX_ListLocalHigh
;	MAKING SURE THE LABEL IS THE LIST, NOT INDIVIDUAL SFX TRACKS
;
;
;
;	ADVM SFX SUBDRIVER DIRECTORY:
;	Flash(data) <- Flash(index list) 
;
;
;
;	ADVM SFX SUBDRIVER FORMAT:
;	very similar to Gmacro but M bit doesn't exist, it's T1LR value directly
;		APWDDDDD PPPPPPPP WWWWWWWW
;		|||||||| |||||||| ||||||||
;		|||||||| |||||||| ++++++++--- wait
;		|||||||| ++++++++------------ T1 pitch
;		|||+++++--------------------- duty (1E = ignore; 1F = mute)
;		||+-------------------------- Wait field request
;		|+--------------------------- Pitch field request
;		+---------------------------- Action bit
;
;	if A is 0
;		end SFX and re-enable main sound, all other bits disregarded
