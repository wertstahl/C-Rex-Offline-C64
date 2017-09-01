;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; Most graphics where imported 1:1 unchanged from the original game
;
; Conversion-Idea & Production : Hedning / G*P
; Code, GFX, SFX, additonal Production : Wertstahl / G*P
; Music : LFT
;

; cloud movement system

;---------------------------------------------------


cloudwidth  = #$18

cloud1xspan = #$07      ;how many times wider than the screen is the x-position range of cloud1
cloud1xhi   byte $02
cloud1x     byte $40
cloud1speed = #$0

speedcup    byte $00

cloud2xspan = #$05      ;how many times wider than the screen is the x-position range of cloud2
cloud2xhi   byte $04
cloud2x     byte $80
cloud2speed = #$01


cloudmeister
            jsr   docloud1
            jmp   docloud2

docloud1    lda   cloud1speed
            clc
            adc   scrollspeed
            sta   speedcup

            lda   cloud1x     ;
            sec               ;
            sbc   speedcup    ; decrease x low of cloud left part by speed
            bcs   cl1nounder  ; just pos vals, all fine

            dec   cloud1xhi   ; wrapped below 0?
            ldx   cloud1xhi
            cpx   #$ff
            bne   cl1nounder  ;
            ldx   cloud1xspan ; yes, set way outside of screen for some time
            stx   cloud1xhi   ;

cl1nounder  sta   cloud1x     ;

            lda   cloud1xhi
            cmp   #$00
            beq   shocl1
            cmp   #$01
            beq   shocl1
            cmp   cloud1xspan
            bne   endcl1

            ;cloud1xhi currently equals freshly set maximum. this, second half of cloud1 
            ;could still be visible
            lda   cloud1x
            clc
            adc   cloudwidth
            bcs   shocl1   ;if result is below -18, then both clouds have disappeared

            ;cloud1xhi is neither 0,1 or freshly set maximum, thus, show no cloud at all
endcl1      lda   #$00
            sta   $d006
            sta   $d008
            lda   $d010
            and   #%11100111
            sta   $d010
            rts

            ;---------------------

shocl1      lda   cloud1xhi
            cmp   #$01
            beq   cloud1ishi

            ;---- hi is max and 2nd part of cloud has yet to leave screen
            cmp   cloud1xspan
            beq   c1inversehi

            cmp   #$00
            bne   endcl1

            ;---- cloud hi is 0 ----

            lda   $d010
            and   #%11100111
            sta   $d010

            lda   cloud1x
            sta   $d006
            clc
            adc   cloudwidth
            bcc   nocl1hi

            ;---- 2nd part of cloud is still hi
            sta   $d008
            lda   $d010
            ora   #%00010000
            sta   $d010
            rts

            ;---- cloud seems in hi. is it visible?
cloud1ishi  lda   cloud1x
            sec
            sbc   #$40
            bcs   endcl1

            ;----- entire cloud is in visible hi
cl1hivis    lda   cloud1x
            sta   $d006
            clc
            adc   cloudwidth
            sta   $d008
            lda   $d010
            and   #%11100111
            ora   #%00011000
            sta   $d010
            rts

c1inversehi lda   #$00
            sta   $d006
            lda   cloud1x
            clc
            adc   cloudwidth
            bcc   endcl1

nocl1hi     ;------ no parts of cloud are hi ------
            sta   $d008
            rts

  ;========================================================================

docloud2    lda   cloud2speed
            clc
            adc   scrollspeed
            sta   speedcup
            
            lda   cloud2x     ;
            sec               ;
            sbc   speedcup    ; decrease x low of cloud left part by speed
            bcs   cl2nounder  ; just pos vals, all fine

            dec   cloud2xhi   ; wrapped below 0?
            ldx   cloud2xhi
            cpx   #$ff
            bne   cl2nounder  ;
            ldx   cloud2xspan ; yes, set way outside of screen for some time
            stx   cloud2xhi   ;

cl2nounder  sta   cloud2x     ;

            lda   cloud2xhi
            cmp   #$00
            beq   shocl2
            cmp   #$01
            beq   shocl2
            cmp   cloud2xspan
            bne   endcl2

            ;cloud1xhi currently equals freshly set maximum. this, second half of cloud1 
            ;could still be visible
            lda   cloud2x
            clc
            adc   cloudwidth
            bcs   shocl2   ;if result is below -18, then both clouds have disappeared

            ;cloud1xhi is neither 0,1 or freshly set maximum, thus, show no cloud at all
endcl2      lda   #$00
            sta   $d00a
            sta   $d00c
            lda   $d010
            and   #%10011111
            sta   $d010
            rts

            ;---------------------

shocl2      lda   cloud2xhi
            cmp   #$01
            beq   cloud2ishi

            ;---- hi is max and 2nd part of cloud has yet to leave screen
            cmp   cloud2xspan
            beq   c2inversehi

            cmp   #$00
            bne   endcl2

            ;---- cloud hi is 0 ----

            lda   $d010
            and   #%10011111
            sta   $d010

            lda   cloud2x
            sta   $d00a
            clc
            adc   cloudwidth
            bcc   nocl2hi

            ;---- 2nd part of cloud is still hi
            sta   $d00c
            lda   $d010
            ora   #%01000000
            sta   $d010
            rts

            ;---- cloud seems in hi. is it visible?
cloud2ishi  lda   cloud2x
            sec
            sbc   #$40
            bcs   endcl2

            ;----- entire cloud is in visible hi
cl2hivis    lda   cloud2x
            sta   $d00a
            clc
            adc   cloudwidth
            sta   $d00c
            lda   $d010
            and   #%10011111
            ora   #%01100000
            sta   $d010
            rts

c2inversehi lda   #$00
            sta   $d00a
            lda   cloud2x
            clc
            adc   cloudwidth
            bcc   endcl2

nocl2hi     ;------ no parts of cloud are hi ------
            sta   $d00c
            rts
