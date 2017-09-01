*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

*=$080e     
            sei
            lda   #$00
            jsr   $1000


lorp        lda   $dc00
            and   #%00010000      ;joy2 fire
            beq   t2fire          ;was pressed --> trigger

            lda   fireold         ;is it unpressed
            beq   soundirq        ;for longer time? yes, just let all things roll off

            lsr   fireold         ;no, it is unpressed for the first time
            lsr   dobing
            lsr   dobold
            jmp   soundirq

t2fire      lda   #$01
            sta   dobing
            sta   fireold
 
soundirq    lda   dobing
            sta   $0400

            inc   $d020
            jsr   soundplay
            dec   $d020

            lda   #$55
vvvwait     cmp   $d012
            bne   vvvwait

            jmp   lorp


fireold     byte  0
mus_sfx     byte  1     ;0=music 1=sfx
mus_sfxlast byte  0
cbmold      byte  0
isplay      byte  0
dobing      byte  0
sfx_puff    byte  1
sfx_txt     text        "sound fx "
mus_txt     text        "music    "



            ;--------------------------------------------------------------

do_sfx      jsr   sfx_jump
            jsr   sfx_walk
            jsr   sfx_smash
            lda   announcespd
            beq   end_sfx
            lsr   announcespd
            jsr   sfxannounce

end_sfx     rts

;==========================================================================

rolloff     byte  $0
dobold      byte  $0
jumpitch    byte  $0
pitchtab    byte  $0c,$0c,$0d,$0d,$0e,$0f,$10,$11,$12,$14,$16,$18
            byte  $1b,$1e,$22,$26,$ff;$bd,$c4,$ce,$ff

sfx_jump    lda   dobing      ;init jump?
            beq   sfxexit     ;no jump has been triggered

            lda   dobold      ;jump has been triggered
            bne   sfxexit     ;is it being held?

            lda   #$01        ;no, its fresh
            sta   dobold
            lda   #$00             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d404
            ldx   #$00
            stx   $d400
            stx   $d401
            stx   jumpitch

            jmp   initjump    ;thus init the jump

            ;---------------------------------------------

sfxexit     ldx   jumpitch
            inx
            lda   pitchtab,x
            cmp   #$ff
            bne   weregood

jumpoff     lda   #$00             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d404
            ldx   #$00
            stx   $d400
            stx   $d401

weregood    stx   jumpitch         
            lda   pitchtab,x
            sec
            sbc   #$05
            lsr   


            sta   $d401
            rts

            ;--------------

initjump    lda   #$a0             ;freq 1 fine
            sta   $d400
           ; lda   #$10             ;freq 1 coarse
           ; sta   $d401
            ;-------------------------
            lda   #$92
            sta   $d402             ;pwidth lo
            lda   #$03
            sta   $d403             ;pwidth hi nybble
            ;-------------------------
            lda   #$1f             ;AD
            sta   $d405
            lda   #$e5             ;SR
            sta   $d406
            ;-------------------------
            lda   #$11             ;Voice #1 control register. Bits:
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


            jmp   sfxexit

            ;======================================================================


sfxconfirm  lda   #$42             ;freq 1 fine
            sta   $d40e
            lda   #$70             ;freq 1 coarse
            sta   $d40f
            ;-------------------------
            lda   #$2f             ;AD
            sta   $d413
            lda   #$F6             ;SR
            sta   $d414
            ;-------------------------
            lda   #$11             ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
            sta   $d412            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------

            lda   #$10             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d412

            lda   #$00
            sta   $d417

            lda   #$1f
            sta   $d418

            rts

            ;======================================================================


stepdel     byte  $2

nowalk      rts
sfx_walk    lda   isfloored
            beq   nowalk

            lda   c_collided
            bne   nowalk

            dec   stepdel
            bne   nowalk
            
            lda   #$0a
            sec
            sbc   scrollspeed
            sta   stepdel

            
simplestep  lda   #$a0             ;freq 1 fine
            sta   $d407

            lda   dinopointr
            asl
            clc
            adc   #$20             ;freq 1 coarse
            sta   $d408

            ;-------------------------
            lda   #$33             ;AD
            sta   $d40c
            lda   #$31             ;SR
            sta   $d40d
            ;-------------------------
            lda   #$81             ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
            sta   $d40b            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------
            lda   #$80             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d40b
            rts

            ;===============================================================


sfx_smash   jsr   randomizer
            sta   $d416
            lsr
            lsr
            lsr
            lsr
            sta   $d415

            lda   sfx_puff
            beq   smashexit

            lsr   sfx_puff

            lda   #$a0             ;freq 1 fine
            sta   $d40e
            lda   #$a0             ;freq 1 coarse
            sta   $d40f
            ;-------------------------
            lda   #$1f             ;AD
            sta   $d413
            lda   #$fa             ;SR
            sta   $d414
            ;-------------------------
            lda   #$81             ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
            sta   $d412            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------

            lda   #$80             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d412

            lda   #$f0
            sta   $d417

            lda   #$1f
            sta   $d418

smashexit   rts

            ;======================================================================


rndcalc     byte  0

randomizer  lda   $da30       
            lsr               
            lsr               
            lsr               
            lsr               
            sta   rndcalc    
            lda   $d910       
            and   #$f0        
            ora   rndcalc    
            eor   $dc08       
            eor   $dc09       
            
            tax               
            lsr               
            lsr               
            lsr               
            lsr               
            sta   rndcalc    

            txa
            and   #$0f
            eor   rndcalc
            sta   rndcalc    ; this shall be random.

            rts


           ;======================================================================


sfxannounce lda   #$30             ;freq 1 fine
            sta   $d40e
            lda   scrollspeed
            asl
            asl
            adc   #$20             ;freq 1 coarse
            sta   $d40f
            ;-------------------------
            lda   #$af             ;AD
            sta   $d413
            lda   #$F9             ;SR
            sta   $d414
            ;-------------------------
            lda   #$21             ;Voice #1 control register. Bits:
                                   ;Bit #0: 0 = Voice off, Release cycle
                                   ;  --->  1 = Voice on, Attack-Decay-Sustain cycle.
                                   ;Bit #1: 1 = Synchronization enabled.
                                   ;Bit #2: 1 = Ring modulation enabled.
                                   ;Bit #3: 1 = Disable voice, reset noise generator.

                                   ;Bit #4: 1 = Triangle waveform enabled. ($10)
                                   ;Bit #5: 1 = Saw waveform enabled.      ($20)
                                   ;Bit #6: 1 = Rectangle waveform enabled.($40) --> requres pulswidth
      ;      sta   $d412            ;Bit #7: 1 = Noise enabled.             ($80)
            ;-------------------------

            lda   #$20             ;Bit #0: 0 = Voice off, Release cycle
            sta   $d412

            lda   #$00
            sta   $d417

            lda   #$1f
            sta   $d418

            rts

            ;======================================================================


soundplay   lda   $dc01
            cmp   #$df
            bne   nosndsw

            lda   cbmold
            bne   issndsw

            lda   #$01
            sta   cbmold

            lda   mus_sfx
            eor   #$01
            sta   mus_sfx 

            jmp   issndsw

            ;-------------

nosndsw     lda   #$00     
            sta   cbmold

issndsw     lda   mus_sfx
            cmp   mus_sfxlast
            bne   switchplay

            cmp   #$01
            beq   go_sfx

            jsr   $1003
            rts

go_sfx      jmp   do_sfx

            ;-------------

switchplay  sta   mus_sfxlast
firstmus    lda   #$00
            jsr   $1000

mussfx      lda   mus_sfx
            beq   print_mus

            ldx   #$07
p_mussfx    lda   sfx_txt,x
            and   #%10111111
            sta   $0404,x
            dex   
            bpl   p_mussfx

            lda   #$1f             ;Bits #0-#3: Volume.
                                   ;Bit #4: 1 = Low pass filter enabled.
                                   ;Bit #5: 1 = Band pass filter enabled.
                                   ;Bit #6: 1 = High pass filter enabled.
            sta   $d418            ;Bit #7: 1 = Voice #3 disabled.
            jmp   sfxconfirm


print_mus   ldx   #$07
p_musmus    lda   mus_txt,x
            and   #%10111111
            sta   $0404,x
            dex   
            bpl   p_musmus
            rts

            ;======================================================================








announcespd byte  0
isfloored   byte  0
scrollspeed byte  0
dinopointr  byte  0
c_collided  byte  0

*=$1000 ; *=&0f82 ;*= - $7e
music
incbin      "tdf2.sid",126 ;126 = skip $7e #126 byte of file

