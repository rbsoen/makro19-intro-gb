;;; Includes ;;;
INCLUDE "./inc/hw.asm"
INCLUDE "./inc/ram.asm"

GBS_LOAD equ $3FE0
GBS_INIT equ $3FE0
GBS_PLAY equ $4000

;;; Makro ;;;
waitframe equs "rst $00"

waitvram: MACRO
    ld a, [rSTAT]
    bit 1, a
    db $20, $FA
    ENDM

checkline: MACRO
    ld a, [rLY]
    cp d
    db $20, $FA
    ENDM

loadgfx: MACRO
    ld de, \1
    ld bc, \1_END - \1
    ld hl, vChars0 + (\2 * $10)
    call CopyVRAM
    ENDM

loadgfx_off: MACRO
    ld de, \1
    ld bc, \1_END - \1
    ld hl, vChars0 + (\2 * $10)
    call CopyGeneric
    ENDM

copy2x2: MACRO
    ld de, \1
    IF \2 == 0
        xor a
    ELSE
        ld a, \2
    ENDC
    call Copyobject22
    ENDM

;;; Rst vectors ;;;

SECTION "rst00", ROM0 [$0]
WaitFrame::
; rst $08 can't be used
    ld a, 1
    ld [VBlankFlag], a
.wait
    ld a,[VBlankFlag]
    and a
    jr nz, .wait
    reti
;SECTION "rst08", ROM0 [$8]
;	reti
SECTION "rst10", ROM0 [$10]
	reti
SECTION "rst18", ROM0 [$18]
	reti
SECTION "rst20", ROM0 [$20]
	reti
SECTION "rst28", ROM0 [$28]
	reti
SECTION "rst30", ROM0 [$30]
	reti
SECTION "rst38", ROM0 [$38]
	reti

;;; Hardware ints ;;;
SECTION "Vblank", ROM0 [$40]
	jp Vblank

SECTION "Stat", ROM0 [$48]
	jp StatInt ; lyc

SECTION "Timer", ROM0 [$50]
	reti
SECTION "Serial", ROM0 [$58]
	reti
SECTION "Joypad", ROM0 [$60]
	reti

;;; PROGRAM START ;;;
SECTION "Entrypoint", ROM0[$100]
Entry::
	nop
	jp Start

SECTION "Start", ROM0[$150]
Start::
; init
	di
	ld sp, $DFFF

; wait for vblank, then shut off the screen
.waitvb
    ld a, [rLY]
    cp LY_VBLANK
    jr nz, .waitvb
	ld a, %00000000
	ld [rLCDC], a

; set palettes
	ld a, %11100100
	ld [rBGP], a
	ld [rOBP0], a

; reset hw vars
	xor a           ; fast set a=0
	ld [rSCX], a
	ld [rSCY], a

; clear wram
	ld hl, $C000
	ld bc, $DFFF - $C000
.clear
	xor a
	ld [hl+], a
	dec bc
	ld a, c
	or b
	jr nz, .clear

; a = 0, load 1st song (title screen)
    call GBS_INIT

; bootstrap oam code
	ld hl, CopyOAM
	ld b, CopyOAM_end - CopyOAM
	ld c, (OAMCode % $100)
.bootstrap
    ld a, [hl+]
	ldh [c], a
	inc c
	dec b
	jr nz, .bootstrap

; load graphics
    loadgfx_off ASCII, $20 + $11E
    loadgfx_off MAKROTEXT, $7C
    loadgfx_off MAKROLOGO, $88

; init oam positions
    ld de, InitPositions
    ld hl, wObject0
    ld c, InitPositions_END - InitPositions
.loadpos
    ld a, [de]
    inc de
    ld [hl+], a
    dec c
    jr nz, .loadpos

; make "MAKRO" text
    copy2x2 Letter_M, 0
    copy2x2 Letter_A, 1
    copy2x2 Letter_K, 2
    copy2x2 Letter_R, 3
    copy2x2 Letter_O, 4

; load logo tile map
LoadLogo:
    ld hl, vBGMap0 + $A6 + $20
    ld a, $88
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $40
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $60
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $80
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $a0
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $c0
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld hl, vBGMap0 + $A6 + $e0
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a
    ld [hl+], a
    inc a

; turn on the screen
    ld a, %10000111
	ld [rLCDC], a

; set compare line
    ld a, 120
    ld [rLYC], a
    ld a, [rSTAT]
    set 6, a        ; enable stat interrupt
    ld [rSTAT], a   ;

; enable vblank and stat interrupt
    ld a, (1 << VBLANK) + (1 << LCD_STAT)
	ld [rIE], a
	ei

;;; Main Loop ;;;
MainLoop::
; reset frame counters
    xor a
    ld [FrameCounter], a
    ld [FrameCounter+1], a
    ld [ScrollVar2], a
; init text
    ld hl, ScrollingText
    ld a, h
    ld [TextPointer], a
    ld a, l
    ld [TextPointer+1], a
.begin
    ld bc, $01D0
    call TimedWait
.loop
    ld a, [ScrollVar2]
    inc a
    ld [ScrollVar2], a
    ld a, [FrameCounter+1]
    and %000000011
    jr nz, .animate
	;ld a, [rOBP0]
	;cpl
    ;ld [rOBP0], a
.animate
    ;call AnimateTextMoving
    call DoScroll

    ld a, [FrameCounter+1]

    call AnimateSineIndex_y

    ld a, [FrameCounter+1]
    rlca
    call AnimateSineIndex_x

    waitframe
	jr .loop

AnimateTextMoving:
    ld hl, wOAMBuffer+1
    rept 5
    rept 4
    inc [hl]
    inc hl
    inc hl
    inc hl
    inc hl
    endr
    endr
    ret

DoScroll:
	ld a, [ScrollVar2]		; current position
	and a
	jr nz, .skiploadingtexttile	; if we're still scrolling
					; skip loading text
; load text tile
	waitvram
	ld a, [vBGMap0 + $1e0]		; move the first visible tile
	ld [vBGMap0 + $1ff], a		; to the end of the row
	ld b, 20			; tiles to move
	ld hl, vBGMap0 + $1e1		; begin offset
.move
	waitvram			; wait till it's safe to tinker
					; with VRAM
	ld a, [hl]
	dec hl
	ld [hli], a			; move tile backwards
	inc hl				; next tile
	dec b
	jr nz, .move
; get character from pointer
	ld a, [TextPointer]
	ld h, a
	ld a, [TextPointer + 1]
	ld l, a
	ld a, [hl]
	push af
	waitvram
	pop af
; add new character
	cp "@"				; end of text?
	jr z, .finishtext
	ld [vBGMap0 + $1f3], a
; update the current character pointer
	inc hl
	ld a, h
	ld [TextPointer], a
	ld a, l
	ld [TextPointer + 1], a
; reset counter
	ld a, -8
	ld [ScrollVar2], a
.skiploadingtexttile
; update scx
	ld d, 120-4
	checkline
	ld a, [ScrollVar2]	; the actual scrolling
	ld [rSCX], a
	ret
.finishtext
	ld hl, ScrollingText		; reset scroller text address
	ld a, h
	ld [TextPointer], a
	ld a, l
	ld [TextPointer+1], a
	jp DoScroll

CopyVRAM:
;   (de:Source, hl:Dest, bc:Length)
    waitvram
    ld a, [de]
    ld [hl+], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, CopyVRAM
    ret

AnimateSineIndex_y:
    ld c, 5
    ld de, Sines
    ld hl, wOAMBuffer
; de + a
    adc a, e
    ld e, a
    jr nc, .cont
    inc d
.cont
    ld a, [de]
    ;ld [rLYC], a
    ld [hli], a
    inc l
    inc l
    inc l
    ld [hli], a
    inc l
    inc l
    inc l
    rept 13
    inc de
    endr
    dec c
    jr nz, .cont
    ret

AnimateSineIndex_x:
    ld c, 5
    ld de, Sines
    ld hl, wOAMBuffer+1
; de + a
    adc a, e
    ld e, a
    jr nc, .cont
    inc d
.cont
    ld a, [de]
    ld [rLYC], a
    rlca
    rlca
    add a, 80
    ld [hli], a
    inc l
    inc l
    inc l
    add a, 8
    ld [hli], a
    inc l
    inc l
    inc l
    rept 16
    inc de
    endr
    dec c
    jr nz, .cont
    ret

CopyGeneric:
;   (de:Source, hl:Dest, bc:Length)
    ld a, [de]
    ld [hl+], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, CopyGeneric
    ret

CopyOAM::
    ld a, wOAMBuffer / $100
	ld [rDMA], a
	ld a, 40
.wait
	dec a
	jr nz, .wait
	ret
CopyOAM_end::

Vblank::
    push af
    push de
    push bc
    push hl
    xor a
    ld [VBlankFlag], a
; reset scroll
    ld [rSCY], a
    ld [rSCX], a
; reset palette
    ld a, %11100100
    ld [rBGP], a
    ld [rOBP0], a
; run sound engine
    call GBS_PLAY
    call OAMCode
; add 1 to frame counter
    ld a, [FrameCounter+1]
    inc a
    ld [FrameCounter+1], a
    jr nz, .nomajor
    ld a, [FrameCounter]
    inc a
    ld [FrameCounter], a
.nomajor
    pop hl
    pop bc
    pop de
    pop af
    reti

StatInt::
    push af
; insert scroller code here...
    ld a, %00011011
    ld [rBGP], a
    ld [rOBP0], a
    ld a, [FrameCounter+1]
    cpl
    ld [rSCX], a
    ;call DoScroll
    pop af
    reti

TimedWait::
    waitframe
    ld a, [FrameCounter]
    cp b
    jr nz, TimedWait
    ld a, [FrameCounter+1]
    cp c
    jr nz, TimedWait
    reti

Copyobject22:
    ; (de: Source, a: Slot)
    push de

    ; slot * 2
    rlca
    ld [wObjectIndex], a

    ; slot * $10
    rlca
    rlca
    ld [wObjectOffset], a

    ld hl, wObjectCur
    ld de, wObject0

    ; wObject0 + (slot*2)
    ld a, [wObjectIndex]
    add a, e
    ld e, a
    jr nc, .a2
    inc d
.a2

    ; wObject -> wObjectCur
    ld a, [de]
    ld [hl+], a
    inc de
    ld a, [de]
    ld [hl], a

    ; wOAMBuffer + (slot * $10)
    ld hl, wOAMBuffer
    ld a, [wObjectOffset]
    add a, l
    ld l, a
    jr nc, .a1
    inc h
.a1

    ; hl = OAMBuffer
    pop de
    ; tile 1 y
    ld a, [wObjectCur]
    ld [hl+], a
    ; tile 1 x
    ld a, [wObjectCur+1]
    ld [hl+], a
    add a, 8
    ld [wObjectCur+1], a
    ; tile 1
    ld a, [de]
    ld [hl+], a
    inc de
    ld a, [de]
    ld [hl+], a
    inc de

    ; tile 2 y
    ld a, [wObjectCur]
    ld [hl+], a
    add a, 8
    ld [wObjectCur], a
    ; tile 2 x
    ld a, [wObjectCur+1]
    ld [hl+], a
    sbc a, 8
    ld [wObjectCur+1], a
    ; tile 2
    ld a, [de]
    ld [hl+], a
    inc de
    ld a, [de]
    ld [hl+], a
    inc de
    ret



SECTION "Graphics", ROM0

ASCII:: INCBIN "./art/ascii.2bpp"
ASCII_END::

MAKROTEXT:: INCBIN "./art/lettering.2bpp"
MAKROTEXT_END::

MAKROLOGO:: INCBIN "./art/logo.2bpp"
MAKROLOGO_END::

OAM_Objects::

Letter_M::
    db $7C, %00000000 ; top row
    db $7C, %00100000
Letter_A::
    db $7E, %00000000 ; top row
    db $7E, %00100000
Letter_K::
    db $80, %00000000 ; top row
    db $82, %00000000
Letter_R::
    db $7E, %00000000 ; top row
    db $84, %00000000
Letter_O::
    db $86, %00000000 ; top row
    db $86, %00100000

InitPositions::
    dw $2C30    ; M
    dw $3C30    ; A
    dw $4C30    ; K
    dw $5C30    ; R
    dw $6C30    ; O
InitPositions_END::

Sines::
angle set 0.0
rept 512
    db (mul(16.0, sin(mul(1.0, angle)))+64.0)>>16
angle set angle+256.0
endr
Sines_END::

ScrollingText::
    db "Pagi Elektro!!! Kami dari DTE 19 dengan "
    db "bangga mempersembahkan: MAKRO 2019!!! "
    db "Wah!! Apa tuh?? MAKRO tuh Malam Keakraban "
    db "Elektro, kayaknya seru tuh! Serunya kayak "
    db "gimana?? Seru deh, pokoknya! Kita bakal kumpul "
    db "bersama, bermain games, dan lain lain! Kali ini "
    db "MAKRO kita bertema SMURF! Kenapa SMURF?? Soalnya "
    db "SMURF itu saling bekerja sama, bergotong-royong "
    db "dalam melakukan pekerjaan. Harapan kita tuh biar "
    db "DTE itu juga sama!! Yuk semarakkan rangkaian "
    db "MAKRO 2019 !!!                               "
    db "@"
ScrollingText_END::

SECTION "GBS_Init", ROM0[GBS_LOAD]

INCBIN "./inc/smurfs.gbs", $70, $20

SECTION "GBS_Play", ROMX[GBS_PLAY]

INCBIN "./inc/smurfs.gbs", $90, $4000

SECTION "HRAM", HRAM

OAMCode::	ds CopyOAM_end - CopyOAM
VBlankFlag:: ds 1
FrameCounter:: ds 2
SceneCounter:: ds 1
TextPointer:: ds 2
ScrollVar2:: ds 1
