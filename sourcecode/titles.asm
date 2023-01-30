; 
; CBMprgStudio 3.10.0+ Syntax 
;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; Most graphics where imported 1:1 unchanged from the original game
;
; Conversion-Idea & Production : Hedning / G*P
; Code, GFX, additonal Production : Wertstahl / G*P
; Intro SFX : Wertstahl
;

; Intro / Titles

;-----------------------------------------------------------------------------------

*=$b000
intromus
            incbin  "crexintro_sid_b000.bin"

*=$bb00
titlesmap   
            incbin  "titlesmap.bin"
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;--------------------------------------------------------------------------------

*=$c000
titles      sei

            lda   #$35
            sta   $01

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #$00
            tay
            tax
            sta   $d011

            sta   $dd00       ; $DD00 = %xxxxxx00 -> bank3: $c000-$ffff
            sta   $d020
            sta   $d021
            
            ora   #%00000010  ;$D018 = %xxxx001x -> charmem is at $0800
            ora   #%00010000  ;$D018 = %0001xxxx -> screenmem is at $0400
            sta   $d018

            ;------------------------------------------
            ; add some initial tension (delay)

            ldy   #$20
t_elong_i   jsr   t_a_flicker
            dey
            bne   t_elong_i

            ;------------------------------------------

            lda   #$00
clr_tibas   sta   titlebase+1024,x
            sta   titlebase+1280,x
            sta   titlebase+1536,x
            sta   titlebase+1768,x
            inx
            bne   clr_tibas

            ldy   #$00
            lda   #$0b
            jsr   clrcol

            ldy   #$10
            lda   #$00
spraway     sta   $d000,y
            dey
            bpl   spraway
            sta   $d015
            sta   $d017

            ;------------------------------------------

            lda   #$c8
            sta   $d016

            lda   #$00
            jsr   intromus

            lda   #<introloop
            sta   $FFFE
            ldy   #>introloop
            sty   $FFFF

            lda   #$a0     ; first interrupt line
            sta   $d012
            lda   #$7f
            sta   $dc0d
            lda   $dc0d       
            
            lda   #$1b
            sta   $d011

            cli

;-------------- all set, curtains up -----------------

            lda   #$00
            sta   titl_seq

            jsr   t_area_clr

process_titles

            ldx   titl_seq
            cpx   #$09
            beq   exit_titles


            ;------------------------------------------
            ; last panel timing exeption

            cpx   #$07
            lda   #$08                    ;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            sta   titl_speed

            ;------------------------------------------

            jsr   t_a_analyze

            ldy   titl_seq                ; load flicker lenght per scene
            lda   ti_fl_rep,y             ; 
            tay                           ;

t_a_f_do    jsr   t_a_flicker
            dey
            bne   t_a_f_do

            inc   titl_seq
            jmp   process_titles

;---------------- well done, kevin -------------------

exit_titles 

abortintro  sei
            sta   lastkey

            lda   #$00
            jsr   intromus

            jmp   ret_f_ttl   ;<<<<<<<<<<<<<<<<<<<<<<----------------- return from titles

;=====================================================
;-----------------------------------------------------

introloop   pha
            txa
            pha
            tya
            pha

            inc   $d019
            lda   $dc0d
      
            lda   $dc01
            cmp   #$ff        ; check space pressed
            bne   abortintro

            ldx   titl_seq
            cpx   #$08
            beq   i_skipmus
            cpx   #$09
            beq   i_skipmus
            jsr   intromus+3
reinsert    lda   #$a0
            sta   $d012

            pla
            tay
            pla
            tax
            pla
            rti

            ;------- bend intro sound pitch down ----------------

i_skipmus   lda   #$96
            sta   $d400
i_skipmus2  lda   #$02
            sta   $d401

i_smus      lda   #$90
            sta   $d407
i_smus2     lda   #$02
            sta   $d408

            lda   i_skipmus2+1
            beq   skipi1
            dec   i_skipmus+1
            bne   skipi1
            dec   i_skipmus2+1

skipi1      lda   i_smus2+1
            beq   skipi2
            dec   i_smus+1
            bne   skipi2
            dec   i_smus2+1

skipi2      jmp   reinsert

;=====================================================

            ;      0   1   2   3   4   5   6   7   8
ti_fl_rep   byte $50,$40,$30,$30,$30,$20,$20,$30,$10,$08

titlebase   = $c000       ; base adress of this intro block
colorrom    = $d800       ; color rom base
colorwork   = $d990
workarea    = $0590       ; first byte of screen area to work with
centerchar  = #$51        ; this is a clear character
titl_seq    byte  $0      ; title frame sequencer

            ;-----------------------------------------
            ; phase 1 clear 4 lines at target position

t_area_clr  ldy   #$78    ; three lines
            lda   #$00
t_a_c_l     sta   titlebase+workarea,y
            dey
            bne   t_a_c_l
            rts

            ;------------------------------------------
            ; phase 2 compare each byte with target values from buffer
            ; and work towards target

t_a_analyze ldy   titl_seq          ;which text segment to be shown

            lda   titl_startl,y     ;low byte of text segment in text map
            sta   ti_read+1         ;to character readout low-adress   
            sta   color_coat+1
            sta   finischeck+1

            lda   titl_starth,y     ;hi byte of text segment in text map
            clc
            adc   #>titlesmap       ;calculate actual hibyte
            sta   ti_read+2         ;to character readout hi-adress
            sta   color_coat+2
            sta   finischeck+2

            ;----------------------
            ; real loop begins here
            ;----------------------

ti_again    ldx   #$00

ti_read     lda   $ffff,x                 ; load target value
            cmp   titlebase+workarea,x    ; how far off are we?
            beq   ti_next                 ; already reached, skip
            sec
            sbc   titlebase+workarea,x
            beq   ti_d_chk   
            bcs   ti_inc


ti_dec      lda   titlebase+workarea,x
            sec                           ;
            sbc   #$01                    ; we are above, decrease value
            jmp   ti_d_chk                ; now write value back

ti_inc      lda   titlebase+workarea,x
            clc
            adc   #$01                    ; we are below, increase value

ti_d_chk    sta   titlebase+workarea,x    ; write value back

ti_next     inx
            ldy   titl_seq
            txa
            cmp   titl_seglen,y
            bne   ti_read

            ;----------------------------------------------
            ; each byte in the textpanel has been touched
            ; now it is time to refresh the colors

            ldx   #$00
color_coat  lda   $ffff,x
            sta   ti_cup                  ;targetvalue
            sec   
            sbc   titlebase+workarea,x    ; is
            bcs   color_ladder            ; larger than onscreenvalue

            lda   titlebase+workarea
            sec
            sbc   ti_cup

color_ladder
            tay
            lda   div8,y
            tay

end_ladder  lda   titl_seq
            cmp   #$08
            bne   lad_normell
            lda   titl_fadout,y
            jmp   lad_cheat

lad_normell lda   titl_fadin,y
lad_cheat   sta   colorwork,x
            inx
            ldy   titl_seq
            txa
            cmp   titl_seglen,y
            bne   color_coat

            ;-------------------------------------------------
            ; soo, was this all?

            ldy   titl_seq
            lda   titl_seglen,y
            tay
            dey
finischeck  lda   $ffff,y
            cmp   titlebase+workarea,y
            bne   ti_nextround
fc_skip     dey
            bne   finischeck
            rts

ti_nextround
            jsr   fx_delay
            jmp   ti_again

;-------------------------------------------------------------

            ;-------------------------------------------------
            ; evil delay routine

ti_cup      byte  $0

titl_story  byte  $0
titl_storx  byte  $0
titl_speed  byte  $28
titl_times  byte  $1

fx_delay    sty   titl_story
            stx   titl_storx

            ldy   titl_times  ;how many times to do it

fx_dl_out   ldx   titl_speed  ;how strong is the delay

fx_dl_in    dex
            bne   fx_dl_in

            dey
            bne   fx_dl_out

            ldy   titl_story
            ldx   titl_storx

            rts

;----------------------------------------------------

t_a_flicker
            lda   #$07  ;yellow
            ldx   #$78
cwfill1     sta   colorwork,x
            dex
            bne   cwfill1

            lda   #$20
cvwait1     cmp   $d012
            bne   cvwait1

            lda   #$19
cvwait3     cmp   $d012
            bne   cvwait3

            lda   #$02  ;red

            ;---------- special case----------
            ldx   titl_seq
            cpx   #$07
            bne   cwignor1
            lda   #$06 ;blue
cwignor1
            ;---------------------------------
            ldx   #$78
cwfill2     sta   colorwork,x
            dex
            bne   cwfill2

            lda   #$18
cvwait2     cmp   $d012
            bne   cvwait2

            lda   #$17
cvwait4     cmp   $d012
            bne   cvwait4

            rts

;----------------------------------------------------

div8        byte 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
            byte 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
            byte 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1
            byte 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1
            byte 2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2
            byte 2,2,2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2
            byte 3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
            byte 3,3,3,3, 3,3,3,3, 3,3,3,3, 3,3,3,3
            byte 4,4,4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4
            byte 4,4,4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4
            byte 5,5,5,5, 5,5,5,5, 5,5,5,5, 5,5,5,5
            byte 5,5,5,5, 5,5,5,5, 5,5,5,5, 5,5,5,5
            byte 6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6
            byte 6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6
            byte 7,7,7,7, 7,7,7,7, 7,7,7,7, 7,7,7,7
            byte 7,7,7,7, 7,7,7,7, 7,7,7,7, 7,7,7,7

                ;   0  1   2   3   4   5   6   7
                ;  gp  in  pp  ws  hd  lft gm crex 
titl_startl byte  $00,$a0,$18,$90,$08,$80,$f8,$70,$e8
titl_starth byte  $00,$00,$01,$01,$02,$02,$02,$03,$03
titl_seglen byte  $78,$78,$78,$78,$78,$78,$78,$78,$78
                ;  4!  3   3   3   3   3   3   3

            ;       0  1   2   3   4   5   6   7
titl_fadout byte  $0b,$0b,$06,$06,$06,$06,$0e,$0e
titl_fadin  byte  $02,$0f,$0b,$0b,$0b,$0b,$0b,$0b


*=$c800
incbin      "titlesfont.bin"

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;EOF