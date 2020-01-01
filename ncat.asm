a_punch			=	$01
b_punch			=	$02
select_punch	=	$04
start_punch		=	$08
up_punch		=	$10
down_punch		=	$20
left_punch		=	$40
right_punch		=	$80

pal_address		=	$100

title_spr		=	$200
menu_spr		=	$204
cat00_spr		=	$208
cat01_spr		=	$20c
cat02_spr		=	$210
cat03_spr		=	$214
cat04_spr		=	$218
cat05_spr		=	$21c
cat06_spr		=	$220
cat07_spr		=	$224
cat08_spr		=	$228
cat09_spr		=	$22c
cat10_spr		=	$230
cat11_spr		=	$234
cat12_spr		=	$238
cat13_spr		=	$23c
cat14_spr		=	$240
cat15_spr		=	$244
cat16_spr		=	$248
cat17_spr		=	$24c
ha00_spr		=	$250
ha01_spr		=	$254
ha02_spr		=	$258
ha03_spr		=	$25c
ha04_spr		=	$260
ha05_spr		=	$264
ha06_spr		=	$268
ha07_spr		=	$26c
ha08_spr		=	$270
ha09_spr		=	$274
ha10_spr		=	$278
ha11_spr		=	$27c
ha12_spr		=	$280
ha13_spr		=	$284
ha14_spr		=	$288
toy_spr			=	$28c
feed0_spr		=	$290
feed1_spr		=	$294
water0_spr		=	$298
water1_spr		=	$29c

sprite        	=	$200
sprite2			=	$204
sprite3        	=	$208
sprite4        	=	$20c
sprite5        	=	$210
sprite6        	=	$214
sprite7        	=	$218
sprite8        	=	$21c
sprite9        	=	$220
sprite10        =	$224
sprite11		=	$228
init			=	$898a
play			=	$8977

.segment "ZEROPAGE"

nmi_num:	 .res 1
control_pad: .res 1
control_old: .res 1
save2000:	 .res 1
save2001:	 .res 1
nametable:	 .res 1
delay:		 .res 1
anim_ticker: .res 1
temp:		 .res 1
temp16bit:	 .res 2

.segment "CODE"

.incbin "s\ncat.nsf"

reset:
    sei
    cld
	ldx #$00
	stx $4015
    ldx #$40
    stx $4017
    ldx #$ff
    txs
    inx
    stx $2000
    stx $2001

    txa
clrmem:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne clrmem

	ldx #$ff
	txs

	ldx #$02
:	bit $2002
	bpl :-
:	bit $2002
	bmi :-
	dex
	bne :--

	ldy #$00
	ldx #$04
	lda #<title
	sta $10
	lda #>title
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-

	ldy #$00
	ldx #$04
	lda #<instr
	sta $10
	lda #>instr
	sta $11
	lda #$24
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-

	lda #$3f
	ldx #$00
	sta $2006
	stx $2006
	lda #$0f
:	sta $2007
	inx
	cpx #$20
	bne :-

	lda #$8f
	sta sprite
	lda #$01
	sta sprite+1
	lda #$00
	sta sprite+2
	lda #$4d
	sta sprite+3

	lda #$0
	ldx #$00
	jsr init

	lda #$20
	sta nametable
	sta $2006
	lda #$00
	sta $2006
	sta $2005
	sta $2005

	jsr PPU_with_sprites


loop_title:
	lda #$20
	sta nametable
	lda #%00011110
	sta save2001
	jsr control_update
								; Start button
	lda control_pad
	eor control_old
	and control_pad
	and #start_punch
	beq no_start_punch
		lda sprite
		cmp #$8f
		bne :++
			ldx #$00
			lda #$0f
:			sta pal_address, x
			inx
			cpx #$20
			bne :-
			lda #$ff
			sta sprite+0
			jsr wait_for_nmi
			jmp load_game_area
:			jmp loop_instr

no_start_punch:					; Select button
	lda control_pad
	eor control_old
	and control_pad
	and #select_punch
	beq no_select_punch
	lda sprite
	cmp #$8f
	beq :+
		lda #$8f
		sta sprite
		bne :++
:	lda #$9f
	sta sprite
:
no_select_punch:

	ldx #$00
:	lda palette, x
	sta pal_address, x
;	sta $2007
	inx
	cpx #$20
	bne :-
	jsr wait_for_nmi
	jmp loop_title

loop_instr:
	lda #$24
	sta nametable
	lda #%00001110
	sta save2001
	jsr control_update

	lda control_pad
	eor control_old
	and control_pad
	and #b_punch
	beq no_b_punch
		jmp loop_title
no_b_punch:	
	jsr wait_for_nmi
	jmp loop_instr

loop_play:
	lda #$20
	sta nametable
	lda #%00011110
	sta save2001
	jsr control_update
	ldx #$00
	ldy #$00
:	lda regular, x
	sta cat00_spr+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #18
	bne :-

	ldx #$00
	ldy #$00
:	lda #$00
	sta ha00_spr+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #15
	bne :-


		lda #$30
		sta feed0_spr+1
		lda #$31
		sta feed1_spr+1

	lda #$f0
	sta toy_spr
	sta feed0_spr
	sta feed1_spr
	sta water0_spr
	sta water1_spr
	lda #$b8
	sta toy_spr+3
	lda #$02
	sta menu_spr+1

	lda control_pad				; Down button
	eor control_old
	and control_pad
	and #down_punch
	beq no_down_punch
	lda menu_spr
	cmp #$d7
	beq at_bottom
		clc
		adc #$0a
		sta menu_spr
at_bottom:

no_down_punch:					; Up button
	lda control_pad
	eor control_old
	and control_pad
	and #up_punch
	beq no_up_punch
	lda menu_spr
	cmp #$af
	beq at_top
		sec
		sbc #$0a
		sta menu_spr
at_top:
	jmp no_up_punch


;$72d and $735 edited in NT2 file to $1f from $0f to try and get dmc to work
no_up_punch:					; A button
	lda control_pad
	eor control_old
	and control_pad
	and #a_punch
	bne :+
			jmp really_done_a
:		lda menu_spr
		cmp #$af; call
		bne :+
			lda #$0f
			sta $4010
			lda #$80
			sta $4012
			lda #$de
			sta $4013
			lda #$10
			sta $4015
			lda #$b4
			sta anim_ticker
			lda #$12
			sta menu_spr+1
			jmp loop_hey
:		cmp #$b9; toy
		bne :+
			lda #$0d
			sta $4010
			lda #$c0
			sta $4012
			lda #$ff
			sta $4013
			lda #$10
			sta $4015
			lda #$b4
			sta anim_ticker
			lda #$12
			sta menu_spr+1
			jmp loop_toy
:		cmp #$c3; feed
		bne :+
			lda #$98
			sta feed0_spr
			sta feed1_spr
			lda #$12
			sta menu_spr+1
			jmp loop_feed
:		cmp #$cd; water
		bne :+
			lda #$98
			sta water0_spr
			sta water1_spr
			lda #$12
			sta menu_spr+1
			jmp loop_water
:		cmp #$d7; save
		bne :+
			lda #$04
			sta delay
			lda #$12
			sta menu_spr+1
			lda #$00
			sta anim_ticker
			jmp loop_save
:
really_done_a:
	jsr wait_for_nmi

	ldx #$00
:	lda palette, x
	sta pal_address, x
;	sta $2007
	inx
	cpx #$20
	bne :-
	jmp loop_play


loop_feed:
	lda anim_ticker
	beq :++
		lda temp
		cmp #27
		bne :+
			lda #$00
			sta temp
			jmp loop_play
:
		dec anim_ticker
		jmp feed_wait
:	ldx temp
	cpx #$06
	bcc :+
		lda #$20
		sta feed0_spr+1
		lda #$21
		sta feed1_spr+1
:
	lda feed_lo, x
	sta temp16bit+0
	lda feed_hi, x
	sta temp16bit+1
	ldy #$00
	ldx #$00
:	lda (temp16bit), y
	sta cat00_spr+1, x
	inx
	inx
	inx
	inx
	iny
	cpy #18
	bne :-
	ldx #$00
	lda (temp16bit), y
:	sta cat00_spr+2, x
	inx
	inx
	inx
	inx
	cpx #72
	bne :-
	ldx temp
	lda feed_tmr, x
	sta anim_ticker
	lda feed_y, x
	sta cat00_spr+0
	lda feed_x, x
	sta cat00_spr+3
	inx
	stx temp
feed_wait:
	jsr tie_cat
	jsr wait_for_nmi
	jmp loop_feed
feed_lo:
	.byte <headlt, <walklt1, <walklt2, <walklt1, <walklt2, <headlt, <walkrt1, <walkrt2, <walkrt1, <walkrt2
	.byte <walkrt1, <walkrt2, <walkrt1, <walkrt2, <walkrt1, <walkrt2, <shitnow, <shitnow, <shitnow, <shitnow
	.byte <walklt1, <walklt2, <walklt1, <walklt2, <walklt1, <walklt2, <regular
feed_hi:
	.byte >headlt, >walklt1, >walklt2, >walklt1, >walklt2, >headlt, >walkrt1, >walkrt2, >walkrt1, >walkrt2
	.byte >walkrt1, >walkrt2, >walkrt1, >walkrt2, >walkrt1, >walkrt2, >shitnow, >shitnow, >shitnow, >shitnow
	.byte >walklt1, >walklt2, >walklt1, >walklt2, >walklt1, >walklt2, >regular
feed_tmr:
	.byte     $40,     $20,      $20,      $20,       $20,     $f0,      $20,      $20,      $20,      $20
	.byte     $20,      $20,     $20,      $20,       $20,     $20,      $f0,      $f0,      $f0,      $f0
	.byte     $20,      $20,     $20,      $20,       $20,     $20,      $01
feed_y:
	.byte     $88,     $88,      $88,      $88,       $88,     $88,      $88,      $88,      $88,      $88
	.byte     $88,     $88,      $88,      $80,       $78,     $70,      $4a,      $4a,      $4a,      $4a
	.byte     $70,     $78,      $80,      $88,       $88,     $88,      $88
feed_x:
	.byte     $78,      $78,      $70,     $68,       $60,     $58,      $58,      $60,      $68,      $70
	.byte     $78,      $80,      $88,     $90,       $98,     $a0,      $a8,      $a8,      $a8,      $a8
	.byte     $a0,      $98,      $90,     $88,       $80,     $78,      $78


wtr_lo:
	.byte <headlt, <walklt1, <walklt2, <walklt1, <walklt2, <headlt, <walkrt1, <walkrt2, <walkrt1, <walkrt2, <regular
wtr_hi:
	.byte >headlt, >walklt1, >walklt2, >walklt1, >walklt2, >headlt, >walkrt1, >walkrt2, >walkrt1, >walkrt2, >regular
wtr_tmr:
	.byte     $40,     $20,      $20,      $20,       $20,     $f0,      $20,      $20,      $20,      $20,      $01
wtr_x:
	.byte     $78,      $78,      $70,     $68,       $60,     $58,      $58,      $60,      $68,      $70,      $78
loop_water:
	lda anim_ticker
	beq :++
		lda temp
		cmp #11
		bne :+
			lda #$00
			sta temp
			jmp loop_play
:
		dec anim_ticker
		jmp wtr_wait
:	ldx temp
	lda wtr_lo, x
	sta temp16bit+0
	lda wtr_hi, x
	sta temp16bit+1
	ldy #$00
	ldx #$00
:	lda (temp16bit), y
	sta cat00_spr+1, x
	inx
	inx
	inx
	inx
	iny
	cpy #18
	bne :-
	ldx #$00
	lda (temp16bit), y
:	sta cat00_spr+2, x
	inx
	inx
	inx
	inx
	cpx #72
	bne :-
	ldx temp
	lda wtr_tmr, x
	sta anim_ticker
	lda wtr_x, x
	sta cat00_spr+3
	inx
	stx temp
wtr_wait:
	jsr tie_cat
	jsr wait_for_nmi
	jmp loop_water






loop_hey:
	lda $4015
	and #$10
	beq :+
		jmp hey_wait
:	ldx #$00
	ldy #$00
:	lda headup, x
	sta cat00_spr+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #18
	bne :-
	dec anim_ticker
	bne :+
		jmp loop_play
:
hey_wait:
	jsr wait_for_nmi
	jmp loop_hey


loop_toy:
	lda toy_spr
	cmp #$98
	beq :+
		dec toy_spr
		dec toy_spr+3
:
	lda $4015
	and #$10
	beq :+
		jmp toy_wait
:	ldx #$00
	ldy #$00
:	lda headlt, x
	sta cat00_spr+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #18
	bne :-
	dec anim_ticker
	bne :+
		jmp loop_play
:
toy_wait:
	jsr wait_for_nmi
	jmp loop_toy

loop_save:
	lda #$24
	sta nametable
	lda #%00011110
	sta save2001

	lda delay
	beq :+
	lda $4015
	and #$10
	bne :+
		dec delay
		beq :+
			lda #$0b
			sta $4010
			lda #$80
			sta $4012
			lda #$18
			sta $4013
			lda #$10
			sta $4015
:

	dec anim_ticker
	bne :+
		jmp loop_play
:
	ldx #$00
	ldy #$00
:	lda ha, x
	sta ha00_spr+1, y
	iny
	iny
	iny
	iny
	inx
	cpx #15
	bne :-

	jsr wait_for_nmi
	jmp loop_save

load_game_area:
	jsr PPU_off
	ldy #$00
	ldx #$04
	lda #<room
	sta $10
	lda #>room
	sta $11
	lda #$20
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-

	ldy #$00
	ldx #$04
	lda #<room2
	sta $10
	lda #>room2
	sta $11
	lda #$24
	sta $2006
	lda #$00
	sta $2006
:	lda ($10),y
	sta $2007
	iny
	bne :-
	inc $11
	dex
	bne :-

	ldx #$00					; Pull in bytes for sprites and their
:	lda orig_sprite_data,x		; attributes which are stored in
	sta sprite,x				; the_sprites. Use X as an index
	inx							; to load and store each byte, which
    cpx #160					; get stored starting in $200, where
    bne :-						; 'sprite' is located at.

	lda #$1
	ldx #$00
	jsr init

	jsr PPU_with_sprites
	lda #$00
;	sta $2006
;	sta $2006
	sta $2005
	sta $2005
	jmp loop_play



nmi:
	pha
	txa
	pha
	tya
	pha

	lda $2002

	inc nmi_num

	lda #$02
	sta $4014

	lda save2000
	sta $2000
	lda save2001
	sta $2001

	lda #$3f
	sta $2006
	lda #$00
	sta $2006
	ldx #$00
:	lda pal_address, x
	sta $2007
	inx
	cpx #$20
	bne :-

	lda nametable
	sta $2006
	lda #$00
	sta $2006
	sta $2005
	sta $2005
	lda $4015
	and #$10
	beq :+
		lda #$10
		sta $4015
		jmp end_nmi
:	jsr play
end_nmi:	
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	rti

palette:
.incbin "g\ncat.pal"

ha:
.byte $00,$0c,$0d,$0e,$0f
.byte $1b,$1c,$1d,$1e,$1f
.byte $2b,$2c,$2d,$2e,$2f

regular:
.byte $00,$00,$00,$00,$00,$00
.byte $00,$03,$04,$05,$06,$00
.byte $00,$13,$14,$15,$16,$17
.byte $00

headup:
.byte $00,$23,$24,$00,$00,$00
.byte $00,$33,$34,$35,$36,$00
.byte $00,$43,$44,$45,$46,$47
.byte $00

headlt:
.byte $00,$00,$00,$00,$00,$00
.byte $50,$51,$52,$53,$54,$00
.byte $60,$61,$62,$63,$64,$65
.byte $00

walklt1:
.byte $3f,$3e,$3d,$3c,$3b,$00
.byte $4f,$4e,$4d,$4c,$4b,$00
.byte $5f,$5e,$5d,$5c,$5b,$00
.byte $40
walklt2:
.byte $6f,$6e,$6d,$6c,$6b,$00
.byte $7f,$7e,$7d,$7c,$7b,$00
.byte $8f,$8e,$8d,$8c,$8b,$00
.byte $40

walkrt1:
.byte $00,$3b,$3c,$3d,$3e,$3f
.byte $00,$4b,$4c,$4d,$4e,$4f
.byte $00,$5b,$5c,$5d,$5e,$5f
.byte $00
walkrt2:
.byte $00,$6b,$6c,$6d,$6e,$6f
.byte $00,$7b,$7c,$7d,$7e,$7f
.byte $00,$8b,$8c,$8d,$8e,$8f
.byte $00

shitnow:
.byte $00,$00,$00,$08,$09,$0a
.byte $00,$00,$00,$18,$19,$1a
.byte $00,$00,$00,$28,$29,$2a
.byte $00

; *********************************************************
; The .bytes below are setup for the sprites to be used   *
; .byte (Y-Pos),(Tile Number),(Attributes),(X-Pos)        *
; *********************************************************
orig_sprite_data:
	.byte $f0,$01,$00,$f0			; title sprite
	.byte $af,$02,$02,$09;$37			; menu sprite
	.byte $88,$00,$00,$78
	.byte $88,$00,$00,$80
	.byte $88,$00,$00,$88
	.byte $88,$00,$00,$90
	.byte $88,$00,$00,$98
	.byte $88,$00,$00,$a0
	.byte $90,$00,$00,$78
	.byte $90,$03,$00,$80
	.byte $90,$04,$00,$88
	.byte $90,$05,$00,$90
	.byte $90,$06,$00,$98
	.byte $90,$00,$00,$a0
	.byte $98,$00,$00,$78
	.byte $98,$13,$00,$80
	.byte $98,$14,$00,$88
	.byte $98,$15,$00,$90
	.byte $98,$16,$00,$98
	.byte $98,$17,$00,$a0

	.byte $46,$00,$02,$68
	.byte $46,$00,$02,$70
	.byte $46,$00,$02,$78
	.byte $46,$00,$02,$80
	.byte $46,$00,$02,$88
	.byte $4e,$00,$02,$68
	.byte $4e,$00,$02,$70
	.byte $4e,$00,$02,$78
	.byte $4e,$00,$02,$80
	.byte $4e,$00,$02,$88
	.byte $56,$00,$02,$68
	.byte $56,$00,$02,$70
	.byte $56,$00,$02,$78
	.byte $56,$00,$02,$80
	.byte $56,$00,$02,$88

	.byte $f0,$10,$01,$b8

	.byte $98,$30,$01,$50
	.byte $98,$31,$01,$58

	.byte $f0,$20,$01,$50
	.byte $f0,$21,$01,$58

control_update:
	lda #$01
	sta $4016
	lda #$00
	sta $4016
	lda control_pad
	sta control_old
	ldx #$08
:
	lda $4016
	lsr A
	ror control_pad
	dex
	bne :-
	rts


PPU_off:
	lda #$00
	sta save2000
	sta save2001
	sta $2000
	sta $2001
	rts

PPU_with_sprites:
	lda #%10010000
	sta save2000
	sta $2000
	lda #%00011110
	sta save2001
	sta $2001
	rts

PPU_no_sprites:
	lda #%10010000
	sta save2000
	sta $2000
	lda #%00001110
	sta save2001
	sta $2001
	rts

wait_for_nmi:
	lda nmi_num
@check:
	cmp nmi_num
	beq @check
	rts

tie_cat:
	lda cat00_spr+0
	sta cat00_spr+4
	sta cat00_spr+8
	sta cat00_spr+12
	sta cat00_spr+16
	sta cat00_spr+20
	clc
	adc #$08
	sta cat00_spr+24
	sta cat00_spr+28
	sta cat00_spr+32
	sta cat00_spr+36
	sta cat00_spr+40
	sta cat00_spr+44
	clc
	adc #$08
	sta cat00_spr+48
	sta cat00_spr+52
	sta cat00_spr+56
	sta cat00_spr+60
	sta cat00_spr+64
	sta cat00_spr+68

	lda cat00_spr+3
	sta cat00_spr+27
	sta cat00_spr+51
	clc
	adc #$08
	sta cat00_spr+7
	sta cat00_spr+31
	sta cat00_spr+55
	clc
	adc #$08
	sta cat00_spr+11
	sta cat00_spr+35
	sta cat00_spr+59
	clc
	adc #$08
	sta cat00_spr+15
	sta cat00_spr+39
	sta cat00_spr+63
	clc
	adc #$08
	sta cat00_spr+19
	sta cat00_spr+43
	sta cat00_spr+67
	clc
	adc #$08
	sta cat00_spr+23
	sta cat00_spr+47
	sta cat00_spr+71
	rts

; *********************************************************
; Include all of the nametables for the game below        *
; *********************************************************
title:
.incbin "g\ncat_title.nam"
instr:
.incbin "g\ncat_instr.nam"
room:
.incbin "g\ncat_room.nam"
room2:
.incbin "g\ncat_room2.nam"


.segment "SAMPLE1"
.incbin "s\call.dmc"
.segment "SAMPLE2"
.incbin "s\toy.dmc"

.segment "VECTORS"
	.addr nmi
	.addr reset
	.addr irq
