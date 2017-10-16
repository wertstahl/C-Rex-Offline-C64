
; main code -- build this -- few page boundary crossings are OK with the given setup



; 
; CBMprgStudio 3.10.0+ Syntax -- Thanks to Arthur Jordison for this great IDE! Everybody please donate!
;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; Most graphics where imported 1:1 unchanged from the original game
;
; Conversion-Idea & Production : Hedning / G*P
; Code, GFX, SFX, additonal Production : Wertstahl / G*P
; Music : LFT
;
;--------------------------------------------------------------------------------

;d_bugging   = 1  ;view debugging timings
d_music     = 1   ;play music
d_intro     = 1  ;link intro
;cheat       = 1  ;turn off collision for testing

            ;
            ;  W A R N I N G !
            ;
            ;  the branch instruction at ntscswitch is being changed by
            ;  the ninjantsc-check to point to "hrz_ntsc" if ntsc is detected
            ;  ADJUST ntsc-branch value at label "ntsc"
            ;  if byte distance of "hrz_short1" to "hrz_ntsc" is changed!!!
            ;


topscan    = #$04
firstscan  = #$38   
secondscan = #$aa
bottscan   = #$ea


desertsoil  = $f7
desertcol   = $f8
desertsky   = $f9

firstcls    = desertcol
clscol      = desertcol
keycol      = desertcol

keyothercol = #$0c
scorcol     = #$02
scorcol2    = #$06


clschar     = #$20
screenbase  = $0400
colorrom    = $d800

connblinkdl = #$18
keyblinkdl  = #$7
scorblinkdl = #$11
birddelay   = #$15

shadval     = #$02 ;after how many kpoints color change

;------ BASIC - bootloader ------------------------------------------------------------

*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

*=$080e     ;<<<<<<<<-------------------------------------------------------------------------------

            jsr   ninjantsc          ; prior to anything check pal/ntsc and patch accordingly            
            jmp   prgstart

;=====================================================================================

            ;----------------------------------------------------------
            ; pal/ntsc check by ninja (codebase64)


ninjantsc   cld
            jsr   palntsc             ; perform check
            sta   $02a6               ; update KERNAL-variable
            beq   ntsc                ; if accu=0, then go to NTSC
            rts

ntsc        lda   #$28                ; adjust branch if ntsc!!!!!!!
            sta   ntscswitch+1
            lda   #$60
            sta   rastersync

            lda   #<ntscsync
            sta   ntsc_rs+1
            lda   #>ntscsync
            sta   ntsc_rs+2

            lda   #$0f
            sta   ntsctop0+1
            sta   ntsctop1+1
            sta   ntsctop2+1

            rts

nmivec      = $0318                   ; NMI-vector

palntsc     sei                       ; disable interrupts
            ldx   nmivec
            ldy   nmivec+1            ; remember old NMI-vector
            lda   #<rrttii
            sta   nmivec
            lda   #>rrttii            ; let NMI-vector point to
            sta   nmivec+1            ; a RTI

nnwait      lda   $d012
            bne   nnwait              ; wait for rasterline 0 or 256
            lda   #$37
            sta   $d012
            lda   #$9b                ; write testline $137 to the
            sta   $d011               ; latch-register
            lda   #$01
            sta   $d019               ; clear IMR-Bit 0

nnwait1     lda   $d011               ; Is rasterbeam in the area
            bpl   nnwait1             ; 0-255? if yes, wait
nnwait2     lda   $d011               ; Is rasterbeam in the area
            bmi   nnwait2             ; 256 to end? if yes, wait
            lda   $d019               ; read IMR
            and   #$01                ; mask Bit 0
            sta   $d019               ; clear IMR-Bit 0
            stx   nmivec
            sty   nmivec+1            ; restore old NMI-vector
            cli                       ; enable interrupts
            rts                       ; return

rrttii      rti                       ; go immediately back after
                                      ; a NMI

;=====================================================================================
;-------------------------------------------------------------------------------------

            ;----------------------------------------------------------
            ; main boot

prgstart    cld

      ifdef d_intro
            jmp   titles      ; quickly re-route to titles here
      endif

ret_f_ttl   sei

            lda   #$35
            sta   $01

            ;------------------------------------------

            lda   #$00         ; close all curtains
            sta   $d011

            ;------------------------------------------

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #$80         ; http://codebase64.org/doku.php?id=base:initialize_tod_clock_on_all_platforms
            sta   $dc0e        ; Set TOD Clock Frequency to 50Hz (we always assume PAL)
            lda   #$00
            sta   $dc0f        ; Enable Set-TOD-Clock
            sta   $dc0b        ; Set TOD-Clock to 0 (hours)
            sta   $dc0a        ; - (minutes)
            sta   $dc09        ; - (seconds)
            sta   $dc08        ; - (deciseconds)

            lda   #$FC
            sta   $0328        ; disable run/stop + restore keys!
            lda   #80
            sta   $0291        ; disable shift

            jsr   firstmus     ; init music + print current play

            lda   #$c8
            sta   $d016

            lda   #<mainloop
            sta   $FFFE
            ldy   #>mainloop
            sty   $FFFF

ntsctop0    lda   topscan     ; first interrupt line
            sta   $d012
            lda   #$7f
            sta   $dc0d
            lda   $dc0d       

            lda   #$01
            jmp   firstrun    ; declare first run

            ;-----------------------------------------------------------
            ; point of restart after collision

restart_le  lda   #$00
            sta   first_run    ; declare 2nd+ run
firstrun    sta   c_collided   ; clear collision flag

            ldx   #$05
            lda   #$00
resetscore  sta   score,x      ; clear current score
            dex
            bpl   resetscore

            sta   speedupdl    ; clear speed counter
            sta   randframe    ; clear obstaclepointer
            sta   shad_base    ; clear shading pointer
            sta   shadecount   ; clear shading counter

            lda   #$07
            sta   dtilestep    ; prepare tile-pointer

            lda   dinoytab     ; load first y value
            sta   dino2y       ; and reset dino position

            lda   #$0b         ;
            sta   desertcol    ;
            lda   #$0f         ;
            sta   desertsky    ;
            lda   #$0c         ;
            sta   desertsoil   ; reset colorscheme

            lda   #$02
            sta   scrollspeed  ; reset scrolling speed

            jsr   spriteup     ; prepare sprites

            jsr   tools        ; clear screen and such

            lda   #$00
            sta   softx        ; reset softscrolling

            cli 

            ;------------------------------------------
            ;------------------------------------------

            jsr   initbird1    ; initialize flapping bird gfx-swap
            jsr   plothud      ; draw texts on screen
            jsr   mussfx       ; refresh sound display

            ;------------------------------------------

idle        lda   trig_space   ; check if space (fire) was pressed
            beq   debnc_spc
            lda   c_collided
            bne   destroy

            ;-------------------------------------
debnc_spc   jmp   idle

            ;------------------------------
            ; 

destroy     jmp   restart_le  ;  warm restart


lastkey     byte 0
first_run   byte 1

;---------------------------------------------------------------------------------------------------
;===================================================================================================
;---------------------------------------------------------------------------------------------------


*=$0980     ;<<<<<<<<-----------------------------------------------------

mainloop    pha
            txa
            pha
            tya
            pha

            inc   $d019
            lda   $dc0d
      
vwait       lda   $d012

ntsctop1    cmp   topscan
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


;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

*=$0a00     ;<<<<<<<<-----------------------------------------------------

inter0      ;-------------------------------------------------------
            ; top area 

            lda   desertsky
            sta   $d020
            lda   desertsky
            sta   $d021

            lda   #%11000000
            sta   $d016

            ;-------------------------------------------------------


      ifdef d_bugging
            dec   $d020
      endif

            jsr   flapbird          ; animate pterodactyl gfx swap

      ifdef d_bugging
            inc   $d020
      endif


            ;-------------------------------------------------------


            lda   firstscan
            sta   $d012
            jmp   reentry




;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter1      ;------------------------------
            ; mid area  : scroll scroll

            ;-------------------------------------------------------
            ; update sprites + position + score


            lda   c_collided        ; stop doing this 
            bne   dino_stalled1     ; when collision occurred
            ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                  jsr   calcscore         ; update score
                  jsr   copyhi            ; and update hiscore if there is

                  jsr   anim_dinos        ; dinosaur animation

                  lda   isfloored
                  bne   a_normldo
                  ldx   #$07
                  stx   dinopointr

a_normldo         ldx   dinopointr        ; 
a_jumpdo          lda   dinoframes,x      ;
                  sta   dinoframe         ; main dino frame
                  lda   dinobfrmes,x      ;
                  sta   dinobframe        ; dino B frame (head when ducked)
                  lda   dinoframe         ;
                  sta   screenbase+1016   ;
                  lda   dinobframe        ;
                  sta   screenbase+1017   ;

                  jsr   cloudmeister

dino_stalled1;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

            lda   c_collided        ; start doing this
            beq   no_dead_pic       ; when collision occurred
                  lda   first_run
                  bne   no_dead_pic
                        lda   iscrouching
                        beq   notcrouching

                              ldx   #$00
                              lda   dincrouchk,x
                              sta   dinoframe
                              sta   screenbase+1016
                              inx
                              lda   dincrouchk,x
                              sta   dinobframe
                              sta   screenbase+1017
                              jmp   no_dead_pic


notcrouching            ldx   #$06
                        lda   dinoframes,x
                        sta   dinoframe
                        sta   screenbase+1016
                        lda   dinobfrmes,x
                        sta   dinobframe
                        sta   screenbase+1017
no_dead_pic

            ;------------------------------------------------------

      ifdef d_bugging
            inc   $d020
      endif

            lda   dino2y            ; if the dino is
            cmp   #$ce              ; in the air,
            bne   d_intheair        ; we need a replacement
            ldx   #$00              ; sprite for 
            jmp   ontheline         ; timing reasons on
                                    ; the horizon raster line
d_intheair  ldx   #$ce              ; this might look weird
ontheline   stx   $d005             ; but helps working around
            sta   $d001             ; a special positioning case
            sta   $d003             ; while keeping all other structures



            ;------------------------------------------------------

            ;----
      ifdef d_bugging
            inc   $d020
      endif

            ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            lda   c_collided
            bne   skipscroll
                  jsr   desrtscroll ;scroll the desert
skipscroll  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ifdef d_bugging
            dec   $d020
      endif
            ;----


            ;---------------------------------------------------
            ; joystick check 

      ifdef d_bugging
            lda   #$02
            sta   $d020
      endif
            jsr   checkjoy    ;joystick check
      ifdef d_bugging
            dec   $d020
      endif

            ;---------------------------------------------------
            ; collision check 

            jsr   collision2


            ;---------------------------------------------------
            ; dino position and movement control
            ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
            lda   c_collided
            bne   skipcontrol
                  jsr   dinocontrol ;maintain dinosaur position and animation
skipcontrol ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ifdef d_bugging
            lda   desertsky
            sta   $d020
      endif


      ifdef d_bugging
            dec   $d020
      endif
            ;---------------------------------------------------
            ; make sure hiscore color is ok when stalled
            lda   c_collided
            beq   skipretouche
            ldy   #$00              ;
            ldx   #$00              ;
            jsr   pre_freshhi       ; reset scorecolor to grey if not hiscoring
skipretouche


            ;-------------------------------------------------------

      ifdef d_bugging
            inc   $d020
      endif


            lda   secondscan
            sta   $d012
            jmp   reentry




;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter2      ;------------------------------
            ; Desert Scroller Area

      ifdef d_bugging
            dec $d020
      endif

            lda   $d016
            and   #%11110000
            ora   softx       ; set softscrolling
            sta   $d016

            lda   #$1b
            sta   $d011

      ifdef d_bugging
            inc   $d020
      endif
            jsr   setpgfnt2   ; set charset and graphics

            ;----
      ifdef d_bugging
            inc   $d020
      endif
      ifdef d_music
            jsr   soundplay       ;music/sfx frame
      endif
      ifdef d_bugging
            dec   $d020
      endif
            ;----

            ;-------------------------------------------------------
            ; blinking elements


      ifdef d_bugging
            dec   $d020
      endif
            jsr   scorblink         ; blining of score in title screen
            jsr   connblink         ; blinking of "no connection"
            jsr   shadecols                     
 ; color theme switchingM
            jsr   keyblink          ; blinking of "fire to play"
            jsr   titleflicker      ; flicker game title

            ;------------------------------------
            ; calm the raster a bit

            ldx   $d012
            inx
            inx
            jsr   rastersync
            ldx   $d012
            inx
            inx
            jsr   rastersync


            ;-------------------------------
            ; end of Desert scroller

            lda   bottscan
            sta   $d012
            jmp   reentry


;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

            ;
            ;  W A R N I N G !
            ;
            ;  the branch instruction at ntscswitch is being changed by
            ;  the ninjantsc-check to point to "hrz_ntsc" if ntsc is detected
            ;  ADJUST ntsc-branch value at label "ntsc"
            ;  if byte distance of "hrz_short1" to "hrz_ntsc" is changed!!!
            ;

inter3      ;------------------------------
            ; desert border-horizonline cheat

            ldx   $d012
            inx
            inx
            inx
            inx

ntsc_rs     jsr   rastersync

            ldy   desertsoil


            ;------------------------------------
            ; commence timing adjustment

            bit   $ea

            ldx   #$ce              ;is the dino on the rasterline
            cpx   $d001
ntscswitch  beq   hrz_short1        ;yes he is. -- -- detected NTSC?!?-------x
            nop                     ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
                                    ;                                        |
                                    ;                                        |
hrz_short1  ;------------------------------------                            |
            nop                     ;yes he is, set color minimally later    |
ntsc_short  bit  $ea                ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
            lda   desertcol         ;                                        |
            sta   $d020             ;                                        |
                                    ;                                        |
            cpx   $d001             ;                                        |
            beq   hrz_short2        ; and if he is, also make it shorter     |
                                    ;                                        |
            nop                     ;                                        |
            nop                     ;                                        |
hrz_short2  ;------------------------------------                            |
            bit   $ea               ; shortbranch                            |
            sty   $d020             ;                                        |
            sty   $d021             ;                                        |
                                    ;                                        |
ntsctop2    lda   topscan           ;                                        |
            sta   $d012             ;                                        |
            jmp   postdesert        ;                                        | 
                                    ;                                        |
                                    ;                                        |
            ;-------------------------------------------------               |
            ; ntsc adjustments. being switched on by ninjantsc               |
                                    ;                                        |
hrz_ntsc    jmp   ntsc_short ;<<--- branch must be modified to point here----x 

ntscsync    ldy   #$f2
wastentsc   cpy   $d012
            bne   wastentsc
            nop
            nop
            nop
            nop
            nop
            nop
            rts


            ;-------------------------------------------------
            ; some appendix

postdesert
            jmp   reentry

;---------------------------------------------------------------------------------------------------
;===================================================================================================
;===================================================================================================
;===================================================================================================


            ;-----------------------------------------------------
            ; mechanisms


            ;-----------------------------------------------------
            ; scroll the desert 

scrollspeed byte $02
softx       byte $07

desrtscroll lda softx
            sec
            sbc scrollspeed
            bcc scrldesrt     ;below zero?
            sta softx
            rts

scrldesrt   adc #$07          ;get above zero again
            sta softx
            jmp do_desert     

*=$0c00     ;<<<<<<<<-----------------------------------------------------

do_desert   ldy #216 ; here: DECIMAL for better documentation
            ;no cmp: saves about 9 Rasterlines

combsand    lda screenbase+425,y;640 - 216 - see above    
            sta screenbase+424,y;640
            lda screenbase+465,y;680 - 216
            sta screenbase+464,y;680
            lda screenbase+505,y;720 - 216
            sta screenbase+504,y;720
            lda screenbase+545,y;760 - 216 
            sta screenbase+544,y;760
            lda screenbase+585,y;800 - 216 
            sta screenbase+584,y;800
            lda screenbase+625,y;840 - 216 
            sta screenbase+624,y;840
            lda screenbase+665,y;880 - 216 
            sta screenbase+664,y;880
            lda screenbase+705,y;920 - 216 
            sta screenbase+704,y;920
            lda screenbase+745,y;960 - 216             
            sta screenbase+744,y;960
            iny
            bne combsand
            jmp   fetchlvl

            ;---------------------------------------------------------------
            ; randomized level plotter
gapband     byte  $02,$02,$02,$02,$03,$03,$04,$04,$04,$03,$02,$01,$01,$01
mean        byte  $0

fetchlvl    lda   $d850       ; totally. random.
            lsr               ;
            lsr               ; 
            lsr               ; 
            lsr               ; 
            sta   randkeep    ; 
            lda   $da31       ; 
            and   #$f0        ; both nybbles
            ora   randkeep    ; 
            eor   $dc08       ; stir it up
            eor   $dc09       ; 
            
            tax               ; 
            lsr               ; 
            lsr               ; 
            lsr               ; 
            lsr               ; 
            sta   randkeep    ; 

            txa
            and   #$0f
            eor   randkeep
            sta   randkeep    ; unpredictable in some way.
            
            lsr
            lsr
            lsr                                             ; boil it down
            and   #$01                    ;
            sta   mean                    ; to be mean (a random #$01 will sometimes cut obstacle space short)

            ;------------------------

            lda   dtilestep
            cmp   #$07
            bne   plotrow     ; if tile is not complete, just plot the row

            lda   #$00
            sta   dtilestep   ; reset tile step pointer

            ;--------------
            
            ldx   shadecount  ; gap lenght pointer is coupled with shading colors. 
            lda   gapband,x
            sec 
            sbc   mean
            sbc   randframe   ; enough space already?
            bcs   noobstacle  ; no, fetch pure desert


            ;--------------------------------------
            ;  fetch new obstacle

            lda   #$00
            sta   randframe   ; reset pure desert counter

            ldx   randkeep    ; load current random value
            cpx   randold     ; compare with last value
            bne   r_nodupl    ; not the same, thats ok

            dex               ; it is the same, change it
            bpl   r_nodupl    ;
            inx               ;

r_nodupl    stx   randold
            lda   randslate,x ; fetch value from obstacle tiles
            sta   dtilecurr
            jmp   plotrow     ; and plot that


            ;--------------------------------------
            ;  fetch pure desert

noobstacle  inc   randframe         ; increase pure desert counter
            ldx   randkeep          ; get randomized pointer
            lda   desertlate,x      ; load tile from pure desert
            sta   dtilecurr         ; desert gfx

            ;--------------------------------------


            ;--------------------------------------
            ; plot the current line of the current tile

plotrow     ldx   dtilecurr
            lda   dtilelow,x
            sta   $fd

            lda   dtilehi,x
            clc
            adc   #>desertile
            sta   $fe


            ldx   #$08        ;9 lines
            lda   #$05        ;<--- skip first 5 bytes, inserted for ease of layouting
            clc   
            adc   dtilestep   ;add state of advancement


            tay
            lda   ($fd),y
            sta   screenbase+679
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+719
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+759
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+799
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+839
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+879
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+919
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+959
            tya
            clc
            adc   #$07        ;next tile row

            tay
            lda   ($fd),y
            sta   screenbase+999
            tya
            clc
            adc   #$07        ;next tile row

            inc   dtilestep
            rts

            ;-----------------------------------------------------
            ; pointer translation tables

randslate   byte  $00,$01,$02,$03,$09,$04,$05,$06,$0a,$07,$08,$0b
            byte  $04,$03,$02,$01

desertlate
            byte  $09,$0a,$0b,$09,$0a,$0b,$09,$0a,$0b,$09,$0a,$0b
            byte  $09,$0a,$0b,$0a

            ;-----------------------------------------------------
            ; tile source adress calculation table

dtilelow    byte  $00,$50,$a0,$f0,$40,$90,$e0,$30,$80,$d0,$20,$70
dtilehi     byte  $00,$00,$00,$00,$01,$01,$01,$02,$02,$02,$03,$03
            

randkeep    byte  $0
randold     byte  $0
randframe   byte  $0
dtilestep   byte  $0
dtilecurr   byte  $a
tileshort   byte  $0


            ;-----------------------------------------------------
            ; set graphics bank for logo part of screen

setpgfnt2   lda $dd00
            and #%11111100
            ora #%00000011 ;<- your desired VIC bank value $DD00 = %xxxxxx11 -> bank0: $0000-$3fff
            sta $dd00
            lda #%00011110  ;%xxxx111x -> charmem is at $3800 , %0001xxxx -> screenmem is at $0400       
            sta $d018         

            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

            ;-----------------------------------------------------
            ; setup & tools

tools       jsr   cls
            jsr   setscrncol
            jsr   desertfirst
            rts

            ;-----------------------------------------------------
            ; deserttest
      
desertfirst ldx #$00
des_torm    lda rextex,x
            sta screenbase+640,x
            lda rextex+120,x
            sta screenbase+760,x
            lda desertcol
            sta colorrom+640,x
            sta colorrom+760,x
            inx
            cpx #$f0
            bne des_torm

            rts
            
            ;-----------------------------------------------------
            ; bordercols

setscrncol  lda   desertsky
            sta   $d020    
            sta   $d021       
            rts

            ;-----------------------------------------------------
            ; prepare screen

cls         ldy   #$00        ; clear screen & color

clr         lda   clschar
            sta   screenbase,y
            sta   screenbase+256,y
            sta   screenbase+512,y
            sta   screenbase+744,y

            iny
            bne   clr

            lda   firstcls
clrcol      sta   colorrom,y
            sta   colorrom+256,y
            sta   colorrom+512,y
            sta   colorrom+744,y

            iny
            bne   clrcol

            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

*=$0e00     ;<<<<<<<<-----------------------------------------------------

trig_space  byte 0
joy1last    byte 0

            ;-----------------------------
            ; CHECK FIRE EXCLUSIVE

checkjoy    lda   $dc00
            and   #%00010000      ;joy2 fire
            beq   joy2_fire  
            lda   #$00        
            sta   trig_space
            jmp   checkjoy2

joy2_fire                          ;fire pressed
            lda #$01
            sta trig_space
            jmp checkjoy2

            ;------------------------------
            ; CHECK JOYSTICK PORT II

joy2_uptime byte 0
joy2_uptmax byte $40
joy2last    byte 0

checkjoy2   lda   c_collided
            bne   chkj2_exit

            lda   $dc00           ;check joystick II
                                    ora   #%00010000      ;firebutton disable

            ldx   joy2_uptime     ;being held?
            cpx   #$00
            beq   chkj2_new

            cpx   joy2_uptmax     ;held how long?
            bne   chkj2_exhld     ;hold still allowed, exit and count!

chkj2_new   
            cmp   joy2last        ;still same as last time?
            beq   chkj2_exit      ;

            sta   joy2last        ;is new. store value.
            tax                   ;store for working

            lsr   stick2_u        ;internally cancel stick position
            lsr   stick2_d

            txa
            and   #%00000010      ;joy2 down
            beq   chkj2_dn  

            lda   dinofalling       ; is falling
            bne   chkj2_exit        ; cannot retrigger up movement

            txa
            and   #%00000001      ;joy2 up
            beq   chkj2_up    

            lsr   stick2_u
            lsr   stick2_d

            lda   #$01
            sta   dinopointr

chkj2_exit  rts


chkj2_exhld inc   joy2_uptime
            rts

chkj2_up    lda   #$01
            sta   stick2_u
            sta   dobing
            lda   #$00
            sta   dinopointr
            lsr   isfloored
            rts

chkj2_dn    lda   #$00
            sta   joy2_uptime
            lda   #$01
            sta   stick2_d
            lsr   stick2_u          ;joy is held down, so no up bit
            lda   isfloored         ;are we on the ground?
            beq   chkj2_exit        ;no - so dont show the long dino

            lda   #$03
            sta   dinopointr
            rts

stick2_d    byte  0
stick2_u    byte  0

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================


dino2yread  byte  0
dinofalling byte  0
dino2y      byte  0

            ;--------------------------------------------------
            ; dinosaur position and momentary interaction 
            ; features control mechanism

dinocontrol jsr   dinopl2
            rts

dinopl2     lda   isfloored
            beq   pr_dinopl2
            rts

pr_dinopl2  lda   dinofalling ; is the dinosaur descending?
            bne   descendino  ; yes, go to descending progress, no matter what

            lda   stick2_d    ; is the stick being held down?
            bne   descendino  ; yes - switch to descent

            lda   stick2_u    ; is the stick being held up?
            beq   descendino  ; no - switch do descent

            ;--------------------------------------
            ; ascending jump animation

ascenddino  ldx   dino2yread  ; check if y position
            lda   dinoytab,x  ; is within 
            cmp   #$ff        ; legal value space
            beq   descendino  ; if end of table reached -> go to descent

            inx               ; step pointer
            lda   dinoytab,x  ; and
            cmp   #$ff        ; check value range once more
            beq   descendino  ; if end of table --> descend
            sta   dino2y      ; write y value

            stx   dino2yread  ; and pointer back
            rts

            ;--------------------------------------
            ; if stick is held down during descent,
            ; hurry fall

descendino  jsr   lowerdino
            lda   stick2_d
            beq   nohurry
hurrydesc   jsr   lowerdino
nohurry     rts

            ;--------------------------------------
            ; descending animation

lowerdino   ldx   dino2yread  ; check if y position pointer
            dex               ; is in
            bmi   dinofloored ; positive value range
            stx   dino2yread  ; write pointer

            lda   #$01
            sta   dinofalling ; report descent in progress

            lda   dinoytab,x  ; get
            sta   dino2y      ; new position

            rts

            ;--------------------------------------
            ; saur has landed

dinofloored lsr   stick2_u    ; resetting the entire
            lsr   stick2_d    ; joystick complex when flooring
            lsr   joy2last    ; removes any delays
            lsr   dinofalling ; report that dino is no longer falling
            lda   #$00        ; reset up+hold timer
            sta   joy2_uptime ;

            lda   #$00
            sta   dinopointr

            lda   #$01        ; report that dino is
            sta   isfloored   ; now on the floor
            rts

            ;-------------------------------------------------------
            ; y-position table for jump animation, 
            ; values generated from drawn curve with BMP2Y tool

dinoytab    byte  $CE,$C4,$BD,$B6,$B0,$AB,$A6,$A2,$9E,$9B,$98,$96,$94,$92,$91,$90
            byte  $8F,$8F,$8E,$8E,$8D,$8D,$8D,$8C,$8C,$8C

            byte  $ff ;terminator

            ;------------------------------------------

isfloored   byte  1
iscrouching byte  0

dinopointr  byte  0
dinoframe   byte  $c0
dinobframe  byte  $cd

dinoframes  byte  $c0,$c2,$c3,$c6,$c8,$ca,$c5,$c4
dinobfrmes  byte  $cd,$cd,$cd,$c7,$c9,$cb,$cd,$cd
dincrouchk  byte  $ce,$cf

dinomaxfrm  = #$03      ;highest frame number for "standing"
dinodnmfxr  = #$06      ;highest frame number for "ducked"

animdelay   byte  5     ;speed of animation
andeltime   = #$5       ;speed of animation both values should be the same



            ;------------------------------------------
            ; animate dinosaur

andelwait   stx   animdelay
            rts

anim_dinos  ldx   animdelay
            dex
            bne   andelwait
            ldx   andeltime
            stx   animdelay

            lda   isfloored         ; is the dino on the floor?
            beq   nopl1_anim        ; it ain't - no animation!

            lda   stick2_d          ; stick is pulled down?
            bne   stick2dnaim       ; yes - ducked animation

            lda   #$00
            sta   iscrouching       ; report that we are NOT crouching

            ;---------------------------------
            ; upright animation

            ldx   dinopointr
            inx   
            cpx   dinomaxfrm
            bne   no_dfprst
            ldx   #$01
no_dfprst   stx   dinopointr
            rts


nopl1_anim  lda   #$00
            sta   dinopointr
            rts

            ;---------------------------------
            ; ducked animation
 
stick2dnaim lda   #$01
            sta   iscrouching       ; report that we are crouching

            ldx   dinopointr
            inx   
            cpx   dinodnmfxr
            bne   no_dfprst2
            ldx   #$03
no_dfprst2  stx   dinopointr
            rts


;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================


            ;-------------------------------------------------------------
            ; simple polling rastersync routine (from codebase64)

*=$0fb0     ;<<<<<<<<-----------------------------------------------------
            ; align to some page so branches do not cross a page boundary and fuck up the timing

rastersync  cpx   $d012
            bne   *-3
            jsr   cycles
            bit   $ea
            nop
            cpx   $d012
            beq   skip1
            nop
            nop
skip1       jsr   cycles
            bit   $ea
            nop
            cpx   $d012
            beq   skip2
            bit   $ea
skip2       jsr   cycles
            nop
            nop
            nop
            cpx   $d012
            bne   onecycle
onecycle    rts

cycles      ldy   #$06
            dey
            bne   *-1
            inx
            nop
            nop
            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

            ;---------------------------------------------------------------
            ; music - To Die For II  by LFT! *awesome* tune! Thanks!!

      
*=$1000 ; *=&0f82 ;*= - $7e
music
incbin      "tdf2.sid",126 ;126 = skip $7e #126 byte of file

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

*=$2000     ;<<<<<<<<-----------------------------------------------------

            ;---------------------------------------------------------------

            ; --- press space to replay gfx ---
p_spc0      byte  $1B,$1C,$1D,$1E
p_spc1      byte  $40,$41,$42,$43
p_spc2      byte  $44,$45,$46,$47
p_spc3      byte  $3C,$3E,$48,$49

            ; --- no internet conn gfx ---
noconn      text "        no internet connection.",0
noclen      byte 0

            ; --- scoreboard gfx ---
                  ;x         1         2         3        x
                  ;0123456789012345678901234567890123456789
scorbrd
            text  " c= ********    hi 000000   *1up 000000",0

            ;---------------------------------------------------------------
            ; copy text to screen

plothud     ldx   #$00
lptconn     lda   noconn,x
            beq   nctxend
            and   #%10111111
            sta   screenbase+81,x ; 40*10
            inx   
            bne   lptconn
nctxend     stx   noclen

            ldx   #$03
p_spc_lp    lda   p_spc0,x
            sta   screenbase+378,x
            lda   p_spc1,x
            sta   screenbase+418,x            
            lda   p_spc2,x
            sta   screenbase+458,x
            lda   p_spc3,x
            sta   screenbase+498,x
            dex
            bpl   p_spc_lp

            ldx   #$00
lptscor     lda   scorbrd,x
            beq   sctxend
            and   #%10111111
            sta   screenbase,x ; 
            inx   
            bne   lptscor

sctxend     jsr   plotscor
            jsr   refreshhi
            rts

2ndrun      byte  1

            ;---------------------------------------------------------------
            ; blinking mechanisms

            ;-------------------------------------------------------
            ; connection text blink

connblink   lda   connblinkdl
            dec   connblink+1
            bne   connbl_out

            lda   connblinkdl
            sta   connblink+1

            lda   cblbit+1
            eor   #$01
            sta   cblbit+1

cblbit      lda   #$01
            beq   cbl_dark

            lda   desertsky
            jmp   conpreloop

cbl_dark    lda   clscol

conpreloop  ldx   noclen
conbloop    sta   colorrom+81,x
            dex
            bpl   conbloop

connbl_out  rts

*=$2100     ;<<<<<<<<-----------------------------------------------------

            ;-------------------------------------------------------
            ; restart game text blink

keyblink    lda   c_collided
            bne   kbusual

            lda   desertsky
            jmp   keypreloop

kbusual     lda   keyblinkdl
            dec   kbusual+1
            bne   keybl_out

            lda   keyblinkdl
            sta   kbusual+1

            lda   keyblbit+1
            eor   #$01
            sta   keyblbit+1

keyblbit    lda   #$01
            beq   keybl_dark

            lda   keyothercol
            jmp   keypreloop

keybl_dark  lda   keycol

keypreloop  ldx   #03
keybloop    sta   colorrom+378,x
            sta   colorrom+418,x            
            sta   colorrom+458,x
            sta   colorrom+498,x
            dex
            bpl   keybloop

keybl_out   rts

            ;-------------------------------------------------------
            ; scoreboard text blink

scorblink   lda   scorblinkdl
            dec   scorblink+1
            bne   scorbl_out

            lda   scorblinkdl
            sta   scorblink+1

            lda   scorblbit+1
            eor   #$01
            sta   scorblbit+1

scorblbit   lda   #$01
            beq   scorbl_dark

            lda   desertsky
            tay
            jmp   scorpreloop

scorbl_dark ldy   scorcol2

scorpreloop ldx   #$02

scorbloop   tya
            sta   colorrom+29,x          

            dex
            bpl   scorbloop

scorbl_out  rts


titleflicker
            ldx   #$01
            lda   tflicol,x
            ldy   #$0d
tfldo       sta   colorrom,y
            dey
            bpl   tfldo
            dec   titleflicker+1
            bne   tflend
            lda   #$13
            sta   titleflicker+1
tflend      rts
tflicol     byte  $0c,$0c,$0c,$0c, $0c,$0c,$0c,$0c, $0c,$0c
            byte  $0f,$0f,$0c,$0c, $0b,$0b,$00,$00, $0b,$0b

            ;-------------------------------------------------------
            ; flap the bird gfx : exchange graphics sets in characters

            ; screen codes matrix
            ;    $E8,$E9
            ;$EA,$EB,$EC,$ED
            ;    $EE,$EF,$F0,$F1,$F2
            ;        $F3,$F4,$F5
            ;        $F6


waitbird    dec flapbird+1
            rts

flapbird    lda birddelay
            bne waitbird
            lda birddelay
            sta flapbird+1

            lda birdbit+1
            eor #$01          ; flip bird frame pointer
            sta birdbit+1
birdbit     lda #$00
            bne initbird2

initbird1   ldx #$77          ;15 used chars * 8 = 120 bytes
copybird1   lda birdbuffer1,x
            sta font2+1856,x  ; start screen code #$E8 = 232 * 8 = 1856
            dex
            bne copybird1
            rts

initbird2   ldx #$77
copybird2   lda birdbuffer2,x
            sta font2+1856,x
            dex
            bne copybird2
            rts


            ;two lines of bird gfx characters were  
            ;prepared manually in photoshop and then
            ;converted using bmp2hires tool by WS
birdbuffer1
incasm      "birdbuffer1.asm" ;hires gfx as asm data
birdbuffer2 
incasm      "birdbuffer2.asm" ;hires gfx as asm data

;fun fun fun until her daddy took the t-bird away
;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================


            ;---------------------------------------------------------------
            ; setup sprites


spriteup    jsr   initsprites
            rts

;-----------------------------------------------------

initsprites ldx   #$00
            ldy   #$00

spr_stuplp  lda   sprx,x     
            sta   $d000,y     
            
            lda   spry,x
            sta   $d001,y
     
            lda   desertcol;sprcol,x
            sta   $d027,x     
            
            lda   initbank,x
            sta   screenbase+1016,x     
            
            iny
            iny
            inx
            lda   sprx,x      
            eor   #$ff        
            bne   spr_stuplp

            lda   spry
            sta   dino2y 

            lda   #$0b
            sta   $d025
            sta   $d026

            lda   #%00000000  ;sprite x bit 8 
            sta   $d010                  

            lda   #%00000111  ;sprites infront gfx     
            sta   $d01b 
            lda   #%00000000  ;sprites multicol
            sta   $d01c       
            lda   #%00000111  ;xstretch
            sta   $d01d       
            lda   #%00000111  ;ystretch
            sta   $d017       
            lda   #%01111111  ;sprite enable
            sta   $d015       
            rts

            ;sprite value definition tables
            ;      0   2   4    6   8   A   C    E    v--terminator
sprx        byte  $24,$54,$54, $00,$00,$00,$00, $00 ,$ff ;sprite x coordinates
            ;      1   3   5    7   9   B   D    F
spry        byte  $ce,$ce,$ce, $53,$53,$90,$90, $00      ;sprite y coordinates

            ;      f8  f9  fa   fb  fc  fd  fe   ff
initbank    byte  $c0,$cd,$cd, $d0,$d1,$d0,$d1, $cd      ;sprite bank pointers

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================

;           isfloored   ; now on the floor
;           iscrouching ; report that we are crouching
;           dino2y      ; dinosaursprite y
;           d000        ; dinosaursprite x
;           softx       ; screen x displacement
;           font2       ; font gfx base
;           $07f0       ; sprite bank pointer 1 (upright/main)
;           $07f1       ; sprite bank pointer 2 (extension for crouching)


            ;screen-adressen
linexylo    byte    $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0,$08,$30
            byte    $58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
linexyhi    byte    $00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$02,$02
            byte    $02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$ff,$ff,$ff,$ff,$ff
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

            ;---------------------------------------------------------------
            ; collision detection

end_ccheck  rts

collision2  

        ifdef cheat
            rts
        endif

        ifdef d_bugging
            inc   $d020
        endif

            lda   c_collided
            bne   end_ccheck

            lda   iscrouching
            beq   cd_direct

            lda   dino2y 
            clc
            adc   #$10
            jmp   cd_ducked

cd_direct   lda   dino2y
cd_ducked   sec
            sbc   #$28
            lsr
            lsr
            lsr
            tax

            lda   linexylo,x
            cmp   #$ff
            beq   coll2end

            sta   $fb

            lda   linexyhi,x
            cmp   #$ff
            beq   coll2end          ; probably uneccessary safety

            clc
            adc   #>screenbase
            sta   $fc 

            lda   softx             ;
            lsr                     ;
            sta   halfsoftx         ; at least some collision adjustment


            lda   iscrouching
            beq   cd_xdirect
  
            lda   $d000
            clc   
            adc   #$10
            jmp   cd_xducked

cd_xdirect  lda   $d000
cd_xducked  clc
            adc   #$08
            sec
            sbc   halfsoftx
            lsr
            lsr
            lsr

            tay
            pha               ; value is rescued
            jsr   garry_k     ; head1
            pla               ; so we can post calc other positions

            clc
            adc   #$25        ;tail
            tay
            pha
            jsr   garry_k                 ;https://www.youtube.com/watch?v=fp7Pq7_tHsY
            pla                                             ;DefCon25: Garry Kasparov - The Brain's Last Stand

            clc
            adc   #$03        ;head2
            tay
            pha
            jsr   garry_k
            pla

            ldx   iscrouching
            bne   coll2end

            clc
            adc   #$4e        ;feet
            tay
            pha
            jsr   garry_k
            pla


coll2end    
      ifdef d_bugging
            dec   $d020
      endif
            rts

            ;----------------------------------

halfsoftx   byte  0
c_collided  byte  0

garry_k     lda   ($fb),y     ;get char number

      ifdef d_bugging
            pha
            lda   #$00
            sta   ($fb),y
            pla
      endif

            cmp   #$20
            beq   c_easyout
            cmp   #$e4
            beq   c_easyout
            cmp   #$de 
            beq   c_easyout
            cmp   #$df
            beq   c_easyout

      ifdef d_bugging
            cmp   #$00
            beq   c_easyout
      endif

            lda   #$01
            sta   c_collided
            sta   sfx_puff

c_easyout   rts



*=$2500     ;<<<<<<<<-----------------------------------------------------
            ;---------------------------------------------------------------
            ; score

hiscore     byte  0,0,1,3,3,7
score       byte  0,0,0,0,0,0

calcscore   inc   score+5
            lda   score+5
            cmp   #$0a
            bne   plotscor
            lda   #$00
            sta   score+5

            inc   score+4
            lda   score+4
            cmp   #$0a
            bne   plotscor
            lda   #$00
            sta   score+4

            inc   score+3
            jsr   announcer
            lda   score+3
            cmp   #$0a
            bne   plotscor
            lda   #$00
            sta   score+3

            jsr   shadepalette
            jsr   speedup
            inc   score+2
            lda   score+2
            cmp   #$0a
            bne   plotscor

            lda   #$00
            sta   score+2
            inc   score+1
            lda   score+1
            cmp   #$0a
            bne   plotscor

            lda   #$00
            sta   score+1
            inc   score
            lda   score
            cmp   #$0a
            bne   plotscor

            lda   #$00
            sta   score

plotscor    ldx   #$05
dpltscr     lda   score,x
            ora   #$30
            sta   screenbase+33,x
            dex
            bpl   dpltscr
noscoring   rts

speedupdl   byte  0
speedup     lda   scrollspeed
            cmp   #$07
            beq   maxspeed
            inc   speedupdl
            lda   speedupdl
            cmp   #$02
            bne   maxspeed
            lda   #$00
            sta   speedupdl
            inc   scrollspeed
maxspeed    rts

announcespd byte  0
announcer   lda   score+3
            cmp   #$09
            bne   maxspeed
            lda   score+2
            cmp   #$00
            beq   maxspeed
            cmp   #$02
            beq   maxspeed
            cmp   #$04
            beq   maxspeed
            cmp   #$06
            beq   maxspeed
            cmp   #$08
            beq   maxspeed
            lda   #$01
            sta   announcespd
            rts



            ;---------------------------------------------------------------
            ; hiscore calculation

copyhi      ldx   #$00
checkhiscor lda   hiscore,x
            cmp   score,x
            bmi   isnewhi           ; if its higher make it a hiscore
            bne   refreshhi         ; and if it is not zero - END! this cost me 4 hours to figure out
            inx
            cpx   #$06
            bne   checkhiscor

            ldy   #$00              ;
            ldx   #$00              ;
            jmp   pre_freshhi       ; reset scorecolor to grey if not hiscoring


isnewhi     ldx   #$05
nuhisc      lda   score,x
            sta   hiscore,x
            dex
            bpl   nuhisc

            ;---------------------------------
            ; blink hiscore while its updating

            inc   hiblcnt
            lda   hiblcnt
            cmp   #$06
            bne   skiphblcr
            lda   #$00
            sta   hiblcnt
skiphblcr   ldx   #$00
            ldy   hiblcnt
pre_freshhi lda   hiblink,y
updhiblnk   sta   colorrom+16,x
            inx
            cpx         #$09
            bne   updhiblnk


            ;----------------------------------
            ; update hiscore numbers on screen

refreshhi   ldx   #$05
plothiscor  lda   hiscore,x
            ora   #$30
            sta   screenbase+19,x

            dex
            bpl   plothiscor

            rts

hiblcnt     byte  $0
hiblink     byte  $0b,$02,$07,$07,$02,$0b


            ;-----------------------------------
            ; change color thru scores

shadecols   ldx   shadecount
            lda   skycolorz,x
            sta   desertsky
            lda   soilcolor,x
            sta   desertsoil
            rts

shadepalette
            inc   shad_base
            lda   shad_base
            cmp   shadval
            bne   shadman
            lda   #$00
            sta   shad_base
            inc   shadecount
            lda   shadecount
            cmp   #$0a
            bne   shadman
            lda   #$00
            sta   shadecount
shadman     rts
shad_base   byte  0
shadecount  byte  0

                 ;  0   1  2   3    4   5   6   7   8  9
skycolorz   byte  $0f,$0f,$03,$03,$0f,$0c,$00,$00,$00,$00
soilcolor   byte  $0f,$0c,$0e,$05,$07,$00,$0f,$01,$00,$06


;---------------------------------------------------------------------------------------------------

incasm      "cloudmeister.asm"

;--------------------------------------------------------------------------------------------------------------------

incasm      "mussfx.asm"

;====================================================================================================================


            ;---------------------------------------------------------------
            ; gfx data


*=$2c00
desertile
incasm      "desertiles.asm"
      
            ;first #5 bytes are 0
            ;#9 lines of #7 bytes
            ;then #12 bytes $ff terminator
            ;all lines are #80 bytes apart

            ;0 smallcact 1
            ;1 smallcact 2
            ;2 bigcact 1
            ;3 bigcact 1-b
            ;4 smallcact 3
            ;5 bigcact 2
            ;6 bigcact 3
            ;7 bird up
            ;8 bird down
            ;9 desert 1
            ;a desert 2
            ;b desert 3

            ;---------------------------------------------------------------

*=$3000
spritegfx
incbin      "dinospr.bin"
incbin      "dinospr_surpr_crouch.bin"
incbin      "cloudsprite.bin"

            ;---------------------------------------------------------------

*=$3500
rextex      
incasm      "basescreen.asm"

            ;---------------------------------------------------------------

*=$3800
font2
incbin      "c-rex-chars2.bin"

            ;---------------------------------------------------------------

      ifdef d_intro

incasm      "titles.asm"

      endif

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;EOF
