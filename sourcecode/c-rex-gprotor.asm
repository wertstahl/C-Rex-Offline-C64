; 
; CBMprgStudio 3.10.0+ Syntax 
;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; Most graphics where imported 1:1 unchanged from the original game
;
; Idea & Production : Hedning / G*P
; Code, GFX, additonal Production : Wertstahl / G*P
; Music : LFT
;
; 



;------ BASIC - bootloader ------------------------------------------------------------

*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

*=$080e
            jmp   prgstart

;-------------------------------------------------------------------------------------


prgstart    cld                ; anti-fuckup
            sei

            lda   #$35
            sta   $01

            ;------------------------------------------

            jmp   titles      ; show titles
ret_f_ttl   nop               ; welcome back

            ;------------------------------------------

            lda   #$00         ; close all curtains
            sta   $d011

            ;------------------------------------------

            lda   #$01
            sta   $d01a
            sta   $dc0d

            sta   $dc0e        ; Set TOD Clock Frequency to 60Hz
            sta   $dc0f        ; Enable Set-TOD-Clock
            sta   $dc0b        ; Set TOD-Clock to 0 (hours)
            sta   $dc0a        ; - (minutes)
            sta   $dc09        ; - (seconds)
            sta   $dc08        ; - (deciseconds)

            ;jsr   spriteup

            jsr   tools

            lda   #$00
            jsr   $1000

            lda   flashwait
            sec
            sbc   #$30        ; timing adjustment, so that the flash         
            sta   flashbyte   ; is in sync with the music's swoosh

            ;------------------------------------------

            lda   #$c8
            sta   $d016

            lda   #<mainloop
            sta   $FFFE
            ldy   #>mainloop
            sty   $FFFF

            lda   topscan     ; first interrupt line
            sta   $d012
            lda   #$7f
            sta   $dc0d
            lda   $dc0d       

            ;------------------------------------------

            lda   #$60
prevwait    cmp   $d012
            bne   prevwait
            
            ;------------------------------------------
            ;------------------------------------------
            ;------------------------------------------

            cli

idle        lda   $dc01
            cmp   lastkey
            beq   debnc_spc

            sta   lastkey
            cmp   #$ff         ; check space pressed
            bne   destroy

debnc_spc   lda   signal_to_animate
            beq   idle

            ;------------------------------
            ; logo animation

delax       lda   #$02          ;logo animation speed control
            dec   delax+1       ;
            lda   delax+1       ;
            cmp   #$ff          ;
            bne   notxchng      ;
            lda   #$02          ;
            sta   delax+1       ;
            jsr   settxt

notxchng    ;>------ no new animation frame

            lda   #$00
            sta   signal_to_animate
            jmp   idle

            ;------------------------------
            ; end of intro

destroy     jmp   wrapup

signal_to_animate byte 0
lastkey           byte 0
;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

topscan    = #$0f
firstscan  = #$32   
secondscan = #$cc  
bottscan   = #$f2

mainloop    pha
            txa
            pha
            tya
            pha

            inc   $d019
            lda   $dc0d
      
vwait       lda   $d012

            cmp   topscan
            beq   interrupt0

            cmp   firstscan
            beq   interrupt1

            cmp   secondscan
            beq   interrupt2

            cmp   bottscan
            beq   interrupt3

reentry     pla
            tay
            pla
            tax
            pla
            rti

            ;-----------------

interrupt0  jmp   inter0
interrupt1  jmp   inter1
interrupt2  jmp   inter2
interrupt3  jmp   inter3      

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter0      ;------------------------------
            ; top area above logo

            lda   border_col
            sta   $d020
            lda   screen_col
            sta   $d021
            lda   #%11000000
            sta   $d016

            lda   firstscan
            sta   $d012
            jmp   reentry

;--------------------------------------------------------------------------------------------------------------------

inter1      ;------------------------------
            ; Area of logo  : set logo gfx regs + play music

            jsr   setpgfnt1
fontnum     lda   #$00              ;this value is changed by setfont
            sta   $d018

;            inc   $d020
            jsr   $1003 ;musipp
;            dec   $d020


;            inc   $d020
            jsr   desrtscroll
;            dec   $d020


            lda   secondscan
            sta   $d012
            jmp   reentry

;--------------------------------------------------------------------------------------------------------------------

inter2      ;------------------------------
            ; Area below logo : paint logo gfx

            nop
            nop
            nop

            lda   desertsky
            sta   $d020
            lda   desertsky
            sta   $d021

            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            lda   $d016
            and   #%11110000
            ora   softx
            sta   $d016

            lda   #$1b
            sta   $d011

            jsr   setpgfnt2

            ;-------------------------------
            ; signal idle loop to animate logo

            lda   #$01
            sta   signal_to_animate

            ;-------------------------------
            ; animated logo delayed flashing

;            dec   $d020
            jsr   whiteflash    ;animated logo delayed flashing
;            inc   $d020

            ;-------------------------------
            ; end of area below logo

            lda   bottscan
            sta   $d012
            jmp   reentry


;--------------------------------------------------------------------------------------------------------------------


inter3      ;------------------------------
            ; Area below logo 2 - interruption to paint border-horizonline

            ldy   desertsky
            lda   desertcol
            sta   $d020

            ldx   $d012
wvit        cpx   $d012
            beq   wvit

            sty   $d020

            lda   topscan
            sta   $d012
            jmp   reentry


;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================


            ;-----------------------------------------------------
            ; mechanisms

            ;-----------------------------------------------------
            ; animated logo flash

flash_light = #$01      ;lightest color of flash
fshade      = #$0a      ;second, shade color of flash
flashwait   = #$ff      ;delay counter between flashes 255 * 50hz = 5,1 Seconds
flashstart  = #$26      ;x position of first flash blob
flashbyte   byte $ff

whiteflash  lda flashbyte
            beq f_xstate
            dec flashbyte
            rts 

f_xstate    ldy #$26
            ldx flash_light
            jsr flashladder
            dec f_xstate+1
            beq flash_reset
            dec f_xstate+1
            beq flash_reset
            rts

flash_reset lda flashwait
            sta flashbyte
            lda flashstart
            sta f_xstate+1
            rts

incasm      "flashladder.asm"

            ;-----------------------------------------------------
            ; logo animation

logowidth   = #30

whichtx     byte $ff          ; framepointer - starts with out of
                              ; range init value
settxt      ldx whichtx       ;
            inx               ; inc framepointer
            cpx #$07
            bne mktxchoic
            ldx #$00          ; reset framepointer
mktxchoic   ldy logowidth     ; load logowidth for all versions
            stx whichtx       ; STORE framepointer

            cpx #$00
            beq disp0
            cpx #$01
            beq disp1
            cpx #$02
            beq disp2
            cpx #$03
            beq disp3
            cpx #$04
            beq disp4
            cpx #$05
            beq disp5
            cpx #$06
            beq disp6
            brk               ; something is wrong

disp0       jsr dosettx0
            jsr setfont0
            rts
disp1       jsr dosettx1
            jsr setfont1
            rts
disp2       jsr dosettx2
            jsr setfont2
            rts
disp3       jsr dosettx3
            jsr setfont3
            rts
disp4       jsr dosettx4
            jsr setfont4
            rts
disp5       jsr dosettx5
            jsr setfont5
            rts
disp6       jsr dosettx6
            jsr setfont6
            rts

            ;-----------------------------------------------------
            ; workhorse screencopy

incasm      "dosettx.asm"

;-----------------------------------------------------

            ;-----------------------------------------------------
            ; animation states
 
setfont0    lda #%00010010 ;screenmem at $4400, font at $4800
            sta fontnum+1       
            rts   
setfont1    lda #%00010100 ;screenmem at $4400, font at $5000
            sta fontnum+1        
            rts            
setfont2    lda #%00010110 ;screenmem at $4400, font at $5800
            sta fontnum+1       
            rts            
setfont3    lda #%00011000 ;screenmem at $4400, font at $6000
            sta fontnum+1       
            rts            
setfont4    lda #%00011010 ;screenmem at $4400, font at $6800
            sta fontnum+1       
            rts            
setfont5    lda #%00011100 ;screenmem at $4400, font at $7000
            sta fontnum+1        
            rts            
setfont6    lda #%00011110 ;screenmem at $4400, font at $7800
            sta fontnum+1        
            rts            


            ;-----------------------------------------------------
            ; set graphics bank for logo part of screen

setpgfnt1   lda $dd00
            and #%11111100
            ora #%00000010 ;<- your desired VIC bank value $DD00 = %xxxxxx10 -> bank1: $4000-$7fff
            sta $dd00
            rts

            ;-----------------------------------------------------
            ; set graphics bank for logo part of screen

setpgfnt2   lda $dd00
            and #%11111100
            ora #%00000011 ;<- your desired VIC bank value $DD00 = %xxxxxx11 -> bank0: $0000-$3fff
            sta $dd00
            lda #%00011110  ;%xxxx111x -> charmem is at $3800 , %0001xxxx -> screenmem is at $0400       
            sta $d018         

            rts


            ;-----------------------------------------------------
            ; scroll the desert (test)

softx       byte $07
desrtscroll ldx softx
            dex
            dex
            bpl nodesrst
            ldx #$07
            jsr do_desert
nodesrst    stx softx
            rts
do_desert   ldy #$00
            lda $06f8
            pha
            lda $0720
            pha
            lda $0748
            pha
            lda $0770
            pha
            lda $0798
            pha
            lda $07c0
            pha
            
combsand    lda $06f9,y
            sta $06f8,y
            iny
            cpy #$ef
            bne combsand

            pla
            sta $07e7
            pla
            sta $07bf
            pla 
            sta $0797
            pla
            sta $076f
            pla
            sta $0747
            pla 
            sta $071f
            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

            ;-----------------------------------------------------
            ; setup & tools

tools       jsr   cls

            jsr   setscrncol

            jsr   deserttest

            rts



            ;-----------------------------------------------------
            ; deserttest
      
deserttest  ldx #$00
des_torm    lda $42f8,x
            sta $06f8,x
            lda desertcol
            sta $daf8,x
            inx
            cpx #$f0
            bne des_torm

            ldx #$28
des_compensate
            lda desertsky
            sta $daf7,x
            dex
            bne des_compensate
            rts
            
            
            ;-----------------------------------------------------
            ; bordercols

screen_col  = #$00
border_col  = #$00

setscrncol  lda   border_col
            sta   $d020    
            lda   screen_col   
            sta   $d021       
            rts

            ;-----------------------------------------------------
            ; prepare screen

firstcls    = #$00
clscol      = #$02
desertcol   = #$0b
desertsky   = #$01
clschar     = #$00
screenbase  = $4400

cls         ldy   #$00        ; clear screen & color

clr         lda   clschar
            sta   screenbase,y
            sta   screenbase+256,y
            sta   screenbase+512,y
            sta   screenbase+744,y

            iny
            bne   clr

            lda   firstcls
clrcol      sta   $d800,y
            sta   $d900,y
            sta   $da00,y
            sta   $dae8,y

            iny
            bne   clrcol

            rts


            ;------------------------------------------------------------
            ; simpletone

simpletone  lda   #$80             ;freq 1 fine
            sta   $d400
            lda   #$10             ;freq 1 coarse
            sta   $d401
            ;-------------------------
            lda   #$1E             ;AD
            sta   $d405
            lda   #$FB             ;SR
            sta   $d406
            ;-------------------------
            lda   #$41             ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
            sta   $d404            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------
            lda   #$0f             ;Bits #0-#3: Volume.
                                   ;Bit #4: 1 = Low pass filter enabled.
                                   ;Bit #5: 1 = Band pass filter enabled.
                                   ;Bit #6: 1 = High pass filter enabled.
            sta   $d018            ;Bit #7: 1 = Voice #3 disabled.
            ;-------------------------
            lda   #$10             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d404
            rts
            ;=========================



            ;------------------------------------------------------------
            ; exit program after pressing space

wrapup
            sei
            lda #$00
            sta $d011
            lda $0800
            sta $3fff
            jsr cls
            lda #$37
            sta $01
            jmp $fce2   ;reset 

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

            ;---------------------------------------------------------------
            ; music

;*= - $7e
*=$0f82
music
incbin      "tdf2.sid" 

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

            ;---------------------------------------------------------------
            ; gfx


*=$3800
font2
incbin      "c-rex-chars.bin"
*=$4000
rextex      
incasm      "basescreenold.bin"

            ;---------------------------------------------------------------


            ; $4400 - $47ff : main character screen


            ;---------------------------------------------------------------
            ; font derived from 3d anim and each respective text screen map
            ; cropped for size reasons

*=$4800
font
incbin      "font1.bin"
txscreena
incasm      "map1.asm"

*=$5000
incbin      "font2.bin"
txscreenb
incasm      "map2.asm"

*=$5800
incbin      "font3.bin"
txscreenc
incasm      "map3.asm"

*=$6000
incbin      "font4.bin"
txscreend
incasm      "map4.asm"

*=$6800
incbin      "font5.bin"
txscreene
incasm      "map5.asm"

*=$7000
incbin      "font6.bin"
txscreenf
incasm      "map6.asm"

*=$7800
incbin      "font7.bin"
txscreeng
incasm      "map7.asm"

*=$b000
titleintro      
incasm      "titles.asm"

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;EOF
