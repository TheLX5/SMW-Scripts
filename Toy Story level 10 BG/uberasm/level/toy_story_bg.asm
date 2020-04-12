;##################################################################################################
;# SNES Toy Story's level 10 BG
;# Ported by lx5
;# 
;# This code recreates the pseudo extra layers effect from Toy Story's level 10 in SMW.
;# It's REALLY slow and may cause performance issues.
;#

!timer				= $7F9C80		; Timer for the gears' animation.
							; 1 byte.

!gfx_buffer			= $7F0000		; RAM buffer for the graphics that are being
							; uploaded to VRAM.
							; 0x800 bytes.
							; NOTE: It has to be in the same bank as !beam_buffer

!beam_buffer			= $7F0800		; RAM buffer to store the uncompressed graphics
							; for the diagonal beams. Needed for performance
							; purposes
							; 0x1000 bytes.
							; NOTE: It has to be in the same bank as !gfx_buffer

;#######################
;# SA-1 defines

!timer_sa1			= $405800		; Timer for the gears' animation.
							; 1 byte.

!bg_dp				= $36A0			; Free IRAM for the diagonal beams gfx
							; 0x60 bytes.

!gfx_buffer_sa1			= $404000		; RAM buffer for the graphics that are being
							; uploaded to VRAM.
							; 0x800 bytes.
							; NOTE: It has to be in the same bank as !beam_buffer

!beam_buffer_sa1		= $404800		; RAM buffer to store the uncompressed graphics
							; for the diagonal beams. Needed for performance
							; purposes
							; 0x1000 bytes.
							; NOTE: It has to be in the same bank as !gfx_buffer

;################################################
;# Misc defines.

!initial_pos			= $1400			; When do the gears GFX start in the .bin file.

!layer_1_y_pos			= $1468|!addr		; Reference layer for the Y coordinates of the BG
!layer_1_x_pos			= $1466|!addr		; Reference layer for the X coordinates of the BG

!draw_horizontal_beams		= 1			; Enables drawing the thin horizontal beams
!draw_diagonal_beams		= 1			; Enables drawing the metal diagonal beams
!draw_animated_gears		= 1			; Enables drawing the animated gears
!animated_gears_frames		= 4			; How many frames the animated gears have

;#######################
;# Below lies a bunch of repurposed scratch rams.
;# Their names probably not correspond to what they actually do.

!layer_1_y_pos_slow		= $00
!layer_1_y_pos_slower		= $02
!layer_1_y_pos_slowish		= $04
!layer_1_y_pos_slowish_2	= $06
!pattern_y_field		= $08
!pattern_y_index		= $0A

!layer_1_x_pos_slow		= $0E
!layer_1_x_pos_slower		= $2A
!layer_1_x_pos_slowish		= $2C
!layer_1_x_pos_slowish_2	= $2E
!pattern_x_field		= $30
!pattern_x_index		= $32
!pattern_x_index_2		= $34

!pattern_diag_index		= $36
!pattern_diag_index_2		= $38

!gfx_pointer			= $8A
!gfx_pointer_2			= $3A

if !sa1 == 1
	!gfx_buffer	= !gfx_buffer_sa1
	!beam_buffer	= !beam_buffer_sa1
	!timer		= !timer_sa1
endif

;################################################
;# Macros
;# You'd be better if you leave these completely intact.

macro build_diagonal_beam(offset, row)
	lda.w !gfx_buffer+<offset>,x
	and.w #(((2**(1+<row>))-1)<<(7-<row>))|((((2**(1+<row>))-1)<<(7-<row>))<<8)
	ora.b $00+((<row>+1)*2)
	sta.w !gfx_buffer+<offset>,x
	lda.w !gfx_buffer+<offset>+$10,x
	and.w #(((2**(1+<row>))-1)<<(7-<row>))|((((2**(1+<row>))-1)<<(7-<row>))<<8)
	ora.b $10+((<row>+1)*2)
	sta.w !gfx_buffer+<offset>+$10,x
	lda.w !gfx_buffer+<offset>,y
	and.w #((((2**(((<row>)^$FF)&7))-1))|((((2**(((<row>)^$FF)&7))-1))<<8))
	ora.b $40+((<row>+1)*2)
	sta.w !gfx_buffer+<offset>,y
	lda.w !gfx_buffer+<offset>+$10,y
	and.w #((((2**(((<row>)^$FF)&7))-1))|((((2**(((<row>)^$FF)&7))-1))<<8))
	ora.b $50+((<row>+1)*2)
	sta.w !gfx_buffer+<offset>+$10,y
endmacro

macro build_diagonal_beam_2(i, offset, offset_2)
	lda.b <offset>
	sta.w !gfx_buffer+<offset_2>,<i>
	lda.b <offset>+$10
	sta.w !gfx_buffer+<offset_2>+$10,<i>
endmacro

macro diagonal_beam_shift(bytes)
	clc
	adc.w #<bytes>
	bit #$00E0
	bne +
	sec
	sbc #$0100
+	
endmacro

macro build_beam_gfx(offset)
	lda.w <offset>,y
	sta.w <offset>,x
endmacro

;################################################
;# NMI Routine
;# Upload the 64x64 texture to VRAM.

nmi:	
	rep #$20
	ldy #$80
	sty $2115
	lda #$1000
	sta $2116
	lda #$1801
	sta $4300
	lda.w #!gfx_buffer
	sta $4302
	ldy.b #!gfx_buffer>>16
	sty $4304
	lda #$0800
	sta $4305
	ldy #$01
	sty $420B
	sep #$20
	rtl

;################################################
;# Init routine
;# Sets up the timer RAM and transfers the beam graphics from ROM to RAM
;# Requires running the main routine at least once after performing the transfer.

init:	
	lda #$00
	sta !timer

if !sa1 == 1
	%invoke_sa1(load_beam_gfx_sa1)
else
	jsr load_beam_gfx
endif
	
;################################################
;# Main routine
;# This routine does all of the magic.
;# It animates the BG and calculates which tiles will be shown depending on the reference layer's position

main:	


.timer	
	lda $14
	and #$03			;timer
	bne ..skip			;this exists for the sole reason that i though the gears had 5
	lda !timer			;frames of animation, which is false
	cmp.b #!animated_gears_frames-1	;i was too lazy to remove this code
	bcs ..reset
	inc
	bra ..end
..reset
	lda #$00
..end
	sta !timer
..skip


	
.build_gfx
	phb
	
	jsr setup_layer_positions	;sets up most of the scratch rams based on the reference layer
	
if !sa1 == 1
	%invoke_sa1(.sa1)
	plb
	rtl
.sa1	
	phb
endif	

if !sa1 == 0
	jsr load_animated		;draws the animated gears in the background
else	
	jsr load_animated_sa1
endif	

if !draw_diagonal_beams == 1
if !sa1 == 0
	jsr load_diagonal_gfx		;writes to ram the diagonal beam gfx
else	
	jsr load_diagonal_gfx_sa1
endif	
endif	

	rep #$30
	pea.w (!gfx_buffer/$10000)|((!gfx_buffer/$100)&$00FF00)
	plb				;sets up data bank to a better location for performance purposes
	plb
	
if !draw_horizontal_beams == 1
	lda !layer_1_y_pos_slowish
	jsr horizontal_beam		;draws the horizontal beams
endif	
	
if !draw_diagonal_beams == 1
	lda !layer_1_y_pos_slowish_2
	jsr diagonal_beam		;draws the diagonal beams
endif	
	
	sep #$30
	plb	
	rtl

;################################################
;# Load beam GFX subroutine
;# Simple ROM-RAM transfer of the beam graphics.

load_beam_gfx:
	rep #$20
	stz $4300
	ldy #$80
	sty $4301
	lda.w #!beam_buffer
	sta $2181
	ldy.b #!beam_buffer>>16
	sty $2183
	lda.w #bg_gfx+$0400
	sta $4302
	ldy.b #bg_gfx>>16
	sty $4304
	lda #$1000
	sta $4305
	ldy #$01
	sty $420B
	sep #$20
	rts

.sa1	
	phb
	phk
	plb
	lda.b #%11000100
	sta $2230
	rep #$20
	lda.w #bg_gfx+$0400
	sta $2232
	ldx.b #bg_gfx>>16
	stx $2234
	lda #$1000
	sta $2238
	lda.w #!beam_buffer
	sta $2235
	ldx.b #!beam_buffer>>16
	stx $2237
..wait	
	ldx $318C
	beq ..wait
	ldx #$00
	stx $318C
	stx $2230
	sep #$20
	plb
	rtl
	
;################################################
;# Setup layer positions subroutine
;# Basically precalculates a bunch of velocities based on a reference.
;# Most of them are used.

setup_layer_positions:
	rep #$20
	lda !layer_1_y_pos
	lsr #2
	sta !layer_1_y_pos_slow
	lsr #1
	sta !layer_1_y_pos_slower
	lda !layer_1_x_pos
	lsr #2
	sta !layer_1_x_pos_slow
	lsr #1
	sta !layer_1_x_pos_slower
	
	lda !layer_1_x_pos
	lsr
	sec
	sbc !layer_1_x_pos_slow
	eor #$FFFF
	sta !layer_1_x_pos_slowish
	lda !layer_1_y_pos
	lsr
	sec
	sbc !layer_1_y_pos_slow
	sta !layer_1_y_pos_slowish
	
	lda !layer_1_x_pos
	asl
	clc
	adc !layer_1_x_pos
	lsr #2
	sec
	sbc !layer_1_x_pos_slow
	sta !layer_1_y_pos_slowish_2
	sta !layer_1_x_pos_slowish_2
	
	lda !layer_1_y_pos
	asl
	clc
	adc !layer_1_y_pos
	lsr #2
	sec
	sbc !layer_1_y_pos_slow
	sec
	sbc !layer_1_y_pos_slowish_2
	sta !layer_1_y_pos_slowish_2
	sep #$20
	rts

;################################################
;# Load animated tiles subroutine
;# Loads to the GFX buffer the gears graphics depending on the timer

load_animated:
	lda !timer
	asl
	tax 
	rep #$20
	lda .animated_frames,x
	clc
	adc.w #bg_gfx
	sta $4302
	sta $00
	stz $4300
	ldy #$80
	sty $4301
	lda.w #!gfx_buffer
	sta $2181
	ldy.b #!gfx_buffer>>16
	sty $2183
	ldy.b #bg_gfx>>16
	sty $4304
	lda #$0800
	sta $4305
	ldy #$01
	sty $420B
	sep #$20
	rts

.animated_frames
	dw !initial_pos+($0800*0)
	dw !initial_pos+($0800*1)
	dw !initial_pos+($0800*2)
	dw !initial_pos+($0800*3)
	dw !initial_pos+($0800*4)

.sa1	
	lda !timer
	asl
	tax
	lda.b #%11000100
	sta $2230
	rep #$20
	lda.l .animated_frames,x
	clc
	adc #bg_gfx
	sta $2232
	ldx.b #bg_gfx/$10000
	stx $2234
	lda #$0800
	sta $2238
	lda.w #!gfx_buffer
	sta $2235
	ldx #!gfx_buffer>>16
	stx $2237
..wait	
	ldx $318C
	beq ..wait
	ldx #$00
	stx $318C
	stx $2230
	sep #$20
	rts

;################################################
;# Load beam tiles subroutine

load_diagonal_gfx:
	rep #$20
	lda !layer_1_x_pos_slowish_2
	and #$0007
	pha
	asl
	clc
	adc $01,s
	tax
	pla 
	lda.l .diagonal_beam_frames,x
	sta $4302
	lda.l .diagonal_beam_frames+1,x
	sta $4303
	lda #$8000 
	sta $4300				; The original game stores the beam GFX in bank $00
	lda #$0060				; to use some DP magic to speed up a LOT the routine
	sta $4305
	lda.w #$0110				; In this case, we can't do that, so I opted for loading
	sta $2181				; the needed graphics directly into the stack reserved RAM
	sep #$20				; which saves the trouble of figuring out a different
	lda.b #$7E				; approach for the DP stuff.
	sta $2183
	lda #$01				; It's completely safe to do this btw.
	sta $420B				; It's highly unlikely that you'd run into issues.
	rts

.diagonal_beam_frames	;$92E665
	dl bg_gfx+($60*0)
	dl bg_gfx+($60*1)
	dl bg_gfx+($60*2)
	dl bg_gfx+($60*3)
	dl bg_gfx+($60*4)
	dl bg_gfx+($60*5)
	dl bg_gfx+($60*6)
	dl bg_gfx+($60*7)

.sa1	
	lda.b #%11000000
	sta $2230
	rep #$20
	lda !layer_1_x_pos_slowish_2
	and #$0007
	pha
	asl
	clc
	adc $01,s
	tax
	pla 
	lda.l .diagonal_beam_frames,x
	sta $2232
	lda.l .diagonal_beam_frames+1,x
	sta $2233
	lda.w #$0060
	sta $2238
	lda.w #!bg_dp
	sta $2235
..wait	
	ldx $318C
	beq ..wait
	ldx #$00
	stx $318C
	stx $2230
	sep #$20
	rts

;################################################
;# Horizontal beam drawing subroutine
;# This one draws the 5 px tall horizontal beams to the GFX buffer
;# It does a bunch of math which surprisingly works fine, but I really don't fully understand it.
;# 
;# There are some 16 bit addresses as comments in some sublabels, which are where the code is located in the
;# original game in bank $92.

horizontal_beam:
	and #$003F
	sta !pattern_x_field
	lda !pattern_x_field
	and #$0038
	xba
	lsr #3
	clc
	adc.w #!gfx_buffer
	sta !pattern_x_index
	clc
	adc #$0100
	cmp.w #!beam_buffer
	bmi .in_range
	sec
	sbc #$0800
.in_range
	sta !pattern_x_index_2
	
	lda !layer_1_x_pos_slowish
	and #$0038
	sta !pattern_y_field
	lda #$0038
	sec
	sbc !pattern_y_field
	asl #2
	sta !pattern_y_index
	lda !layer_1_x_pos_slowish
	xba
	and #$0700
	asl
	clc
	adc !pattern_y_index
	clc
	adc.w #!beam_buffer
	sta !pattern_y_index
	
	lda !pattern_x_field
	and #$0007
	asl
	tax
	jsr (.beam_ptrs,x)
	rts

.beam_ptrs
	dw ..case_0	; $f67f
	dw ..case_1	; $f686
	dw ..case_2	; $f68f
	dw ..case_3	; $f69a
	dw ..case_4	; $f6a5
	dw ..case_5	; $f6bb
	dw ..case_6	; $f6d1
	dw ..case_7	; $f6e7

..case_0
	ldy !pattern_y_index
	ldx !pattern_x_index
	jmp ..build_beam_gfx
	
..case_1
	ldy !pattern_y_index
	ldx !pattern_x_index
	inx #2
	jmp ..build_beam_gfx

..case_2
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$0004
	tax
	jmp ..build_beam_gfx

..case_3	; $f69a
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$0006
	tax
	jmp ..build_beam_gfx

..case_4	; $f6a5
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$0008
	tax
	jsr ..build_beam_gfx_2
	lda !pattern_y_index
	ora #$0008
	tay
	ldx !pattern_x_index_2
	jmp ..build_beam_gfx_4
	
..case_5	; $f6bb
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$000A
	tax
	jsr ..build_beam_gfx_2
	lda !pattern_y_index
	ora #$0006
	tay
	ldx !pattern_x_index_2
	jmp ..build_beam_gfx_3
	
..case_6	; $f6d1
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$000C
	tax
	jsr ..build_beam_gfx_3
	lda !pattern_y_index
	ora #$0004
	tay
	ldx !pattern_x_index_2
	jmp ..build_beam_gfx_2

..case_7	; $f6e7
	ldy !pattern_y_index
	lda !pattern_x_index
	ora #$000E
	tax
	jsr ..build_beam_gfx_4
	ldy !pattern_y_index
	iny #2
	ldx !pattern_x_index_2
	jmp ..build_beam_gfx_1

..build_beam_gfx	; $F6FB
	!i = 0
	while !i < $10
		%build_beam_gfx($0008+(!i*$0010))
		!i #= !i+1
	endif
...1			; $F75B
	!i = 0
	while !i < $10
		%build_beam_gfx($0006+(!i*$0010))
		!i #= !i+1
	endif
...2			; $F7BB
	!i = 0
	while !i < $10
		%build_beam_gfx($0004+(!i*$0010))
		!i #= !i+1
	endif
...3			; $F81B
	!i = 0
	while !i < $10
		%build_beam_gfx($0002+(!i*$0010))
		!i #= !i+1
	endif
...4			; $F87B
	!i = 0
	while !i < $10
		%build_beam_gfx($0000+(!i*$0010))
		!i #= !i+1
	endif
	rts

;################################################
;# Diagonal beam drawing subroutine
;# This draws the diagonal beam on the screen. It's a really long and complex routine to manage 4bpp graphics
;# Don't expect a lot of comments on this.
;# 
;# There are some 16 bit addresses as comments in some sublabels, which are where the code is located in the
;# original game in bank $92.

diagonal_beam:
	and #$003F
	sta !pattern_x_field
	and #$0007
	asl
	pha
	lda #$0008
	sta !pattern_y_index
	lda !pattern_x_field
	and #$0038
	asl #2
	sta !pattern_diag_index
	clc
	adc #$0020
	bit #$00E0
	bne .no_fix
	sec
	sbc #$0100
.no_fix	
	sta !pattern_diag_index_2
	clc
	adc #$0020
	bit #$00E0
	bne .no_fix_2
	sec
	sbc #$0100
.no_fix_2
	tay
	
if !sa1 == 0
	lda.w #$0110
else	
	lda.w #!bg_dp
endif	
	tcd
	plx
	jsr (.diagonal_beam_cases,x)
	lda.w #!dp
	tcd
	rts

.diagonal_beam_cases	; $E675
	dw ..case_0	; $E686
	dw ..case_1	; $E868
	dw ..case_2	; $EA44
	dw ..case_3	; $EC2F
	dw ..case_4	; $EE1A
	dw ..case_5	; $F005
	dw ..case_6	; $F1F0
	dw ..case_7	; $F3DB

;#####################

..case_0	; $E686
	lda.l !pattern_diag_index
	tax
...loop
	phx
	%build_diagonal_beam_2(x, $00, $00)
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $20, $00)
	plx
	
	%build_diagonal_beam($02, 0)
	%build_diagonal_beam($04, 1)
	%build_diagonal_beam($06, 2)
	%build_diagonal_beam($08, 3)
	%build_diagonal_beam($0A, 4)
	%build_diagonal_beam($0C, 5)
	%build_diagonal_beam($0E, 6)
	
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $22, $02)
	%build_diagonal_beam_2(x, $24, $04)
	%build_diagonal_beam_2(x, $26, $06)
	%build_diagonal_beam_2(x, $28, $08)
	%build_diagonal_beam_2(x, $2A, $0A)
	%build_diagonal_beam_2(x, $2C, $0C)
	%build_diagonal_beam_2(x, $2E, $0E)
	pla
	%diagonal_beam_shift($0120)
	tax
	lda.l !pattern_diag_index_2
	%diagonal_beam_shift($0120)
	sta.l !pattern_diag_index_2
	tya
	%diagonal_beam_shift($0120)
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts
	
;#####################

..case_1	; $E868
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 0)
	%build_diagonal_beam($02, 1)
	%build_diagonal_beam($04, 2)
	%build_diagonal_beam($06, 3)
	%build_diagonal_beam($08, 4)
	%build_diagonal_beam($0A, 5)
	%build_diagonal_beam($0C, 6)

	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $22, $00)
	%build_diagonal_beam_2(x, $24, $02)
	%build_diagonal_beam_2(x, $26, $04)
	%build_diagonal_beam_2(x, $28, $06)
	%build_diagonal_beam_2(x, $2A, $08)
	%build_diagonal_beam_2(x, $2C, $0A)
	%build_diagonal_beam_2(x, $2E, $0C)
	%build_diagonal_beam_2(x, $00, $0E)
	plx
	%build_diagonal_beam_2(y, $20, $0E)
	
	txa
	%diagonal_beam_shift($0120)
	tax
	lda.l !pattern_diag_index_2
	%diagonal_beam_shift($0120)
	sta.l !pattern_diag_index_2
	tya
	%diagonal_beam_shift($0120)
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_2	; $EA44
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 1)
	%build_diagonal_beam($02, 2)
	%build_diagonal_beam($04, 3)
	%build_diagonal_beam($06, 4)
	%build_diagonal_beam($08, 5)
	%build_diagonal_beam($0A, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $0C)
	%build_diagonal_beam($0E, 0)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $24, $00)
	%build_diagonal_beam_2(x, $26, $02)
	%build_diagonal_beam_2(x, $28, $04)
	%build_diagonal_beam_2(x, $2A, $06)
	%build_diagonal_beam_2(x, $2C, $08)
	%build_diagonal_beam_2(x, $2E, $0A)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $0C)
	%build_diagonal_beam_2(x, $22, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla 
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_3	; $EC2F
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 2)
	%build_diagonal_beam($02, 3)
	%build_diagonal_beam($04, 4)
	%build_diagonal_beam($06, 5)
	%build_diagonal_beam($08, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $0A)
	%build_diagonal_beam($0C, 0)
	%build_diagonal_beam($0E, 1)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $26, $00)
	%build_diagonal_beam_2(x, $28, $02)
	%build_diagonal_beam_2(x, $2A, $04)
	%build_diagonal_beam_2(x, $2C, $06)
	%build_diagonal_beam_2(x, $2E, $08)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $0A)
	%build_diagonal_beam_2(x, $22, $0C)
	%build_diagonal_beam_2(x, $24, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla 
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_4	; $EE1A
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 3)
	%build_diagonal_beam($02, 4)
	%build_diagonal_beam($04, 5)
	%build_diagonal_beam($06, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $08)
	%build_diagonal_beam($0A, 0)
	%build_diagonal_beam($0C, 1)
	%build_diagonal_beam($0E, 2)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $28, $00)
	%build_diagonal_beam_2(x, $2A, $02)
	%build_diagonal_beam_2(x, $2C, $04)
	%build_diagonal_beam_2(x, $2E, $06)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $08)
	%build_diagonal_beam_2(x, $22, $0A)
	%build_diagonal_beam_2(x, $24, $0C)
	%build_diagonal_beam_2(x, $26, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla 
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_5	; $F005
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 4)
	%build_diagonal_beam($02, 5)
	%build_diagonal_beam($04, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $06)
	%build_diagonal_beam($08, 0)
	%build_diagonal_beam($0A, 1)
	%build_diagonal_beam($0C, 2)
	%build_diagonal_beam($0E, 3)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $2A, $00)
	%build_diagonal_beam_2(x, $2C, $02)
	%build_diagonal_beam_2(x, $2E, $04)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $06)
	%build_diagonal_beam_2(x, $22, $08)
	%build_diagonal_beam_2(x, $24, $0A)
	%build_diagonal_beam_2(x, $26, $0C)
	%build_diagonal_beam_2(x, $28, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla 
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_6	; $F1F0
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 5)
	%build_diagonal_beam($02, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $04)
	%build_diagonal_beam($06, 0)
	%build_diagonal_beam($08, 1)
	%build_diagonal_beam($0A, 2)
	%build_diagonal_beam($0C, 3)
	%build_diagonal_beam($0E, 4)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $2C, $00)
	%build_diagonal_beam_2(x, $2E, $02)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $04)
	%build_diagonal_beam_2(x, $22, $06)
	%build_diagonal_beam_2(x, $24, $08)
	%build_diagonal_beam_2(x, $26, $0A)
	%build_diagonal_beam_2(x, $28, $0C)
	%build_diagonal_beam_2(x, $2A, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla 
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;#####################

..case_7	; $F3DB
	lda.l !pattern_diag_index
	tax
...loop	
	%build_diagonal_beam($00, 6)
	txa
	%diagonal_beam_shift($0020)
	tax
	tya
	%diagonal_beam_shift($0020)
	tay
	%build_diagonal_beam_2(x, $00, $02)
	%build_diagonal_beam($04, 0)
	%build_diagonal_beam($06, 1)
	%build_diagonal_beam($08, 2)
	%build_diagonal_beam($0A, 3)
	%build_diagonal_beam($0C, 4)
	%build_diagonal_beam($0E, 5)
	
	phx
	lda.l !pattern_diag_index_2
	tax
	%build_diagonal_beam_2(x, $2E, $00)
	txa
	%diagonal_beam_shift($0020)
	tax
	%build_diagonal_beam_2(x, $20, $02)
	%build_diagonal_beam_2(x, $22, $04)
	%build_diagonal_beam_2(x, $24, $06)
	%build_diagonal_beam_2(x, $26, $08)
	%build_diagonal_beam_2(x, $28, $0A)
	%build_diagonal_beam_2(x, $2A, $0C)
	%build_diagonal_beam_2(x, $2C, $0E)
	
	txa
	clc
	adc #$0100
	sta.l !pattern_diag_index_2
	pla
	clc
	adc #$0100
	tax
	tya 
	clc
	adc #$0100
	tay
	lda.l !pattern_y_index
	dec
	sta.l !pattern_y_index
	beq ...return
	jmp ...loop
...return
	rts

;################################################
;# GFX file

%prot_file(gfx/toy_story_lvl_10_bg.bin,bg_gfx)