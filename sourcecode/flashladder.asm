
; 
; CBMprgStudio 3.10.0+ Syntax 
;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; "Flash" Color Ram Bar for animated G*P Logo screen 


flashladder txa
            sta $d800,y
            sta $d828,y
            lda fshade
            sta $d801,y
            sta $d829,y
            lda clscol
            sta $d802,y
            sta $d803,y
            sta $d82a,y
            sta $d82b,y
            dey
            bne cwf2
            rts
cwf2        txa
            sta $d850,y
            sta $d878,y
            lda fshade
            sta $d851,y
            sta $d879,y
            lda clscol
            sta $d852,y
            sta $d853,y    
            sta $d87a,y
            sta $d87b,y
            dey
            bne cwf3
            rts
cwf3        txa
            sta $d8a0,y
            sta $d8c8,y
            lda fshade
            sta $d8a1,y
            sta $d8c9,y
            lda clscol
            sta $d8a2,y
            sta $d8a3,y
            sta $d8ca,y
            sta $d8cb,y
            dey
            bne cwf4
            rts
cwf4        txa
            sta $d8f0,y
            sta $d918,y
            lda fshade
            sta $d8f1,y
            sta $d919,y
            lda clscol
            sta $d8f2,y
            sta $d8f3,y
            sta $d91a,y
            sta $d91b,y
            dey
            bne cwf5
            rts
cwf5        txa
            sta $d940,y
            sta $d968,y
            lda fshade
            sta $d941,y
            sta $d969,y
            lda clscol
            sta $d942,y
            sta $d943,y
            sta $d96a,y
            sta $d96b,y
            dey
            bne cwf6
            rts
cwf6        txa
            sta $d990,y
            sta $d9b8,y
            lda fshade
            sta $d991,y
            sta $d9b9,y
            lda clscol
            sta $d992,y
            sta $d993,y
            sta $d9ba,y
            sta $d9bb,y
            dey
            bne cwf7
            rts
cwf7        txa
            sta $d9e0,y
            sta $da08,y
            lda fshade
            sta $d9e1,y
            sta $da09,y
            lda clscol
            sta $d9e2,y
            sta $d9e3,y
            sta $da0a,y
            sta $da0b,y
            dey
            bne cwf8
            rts
cwf8        txa
            sta $da30,y
            sta $da58,y
            lda fshade
            sta $da31,y
            sta $da59,y
            lda clscol
            sta $da32,y
            sta $da33,y
            sta $da5a,y
            sta $da5b,y
            dey
            bne cwf9
            rts
cwf9        txa
            sta $da80,y
            sta $daa8,y
            lda fshade
            sta $da81,y
            sta $daa9,y
            lda clscol
            sta $da82,y
            sta $da83,y
            sta $daaa,y
            sta $daab,y
            dey
            bne cwf10
            rts
cwf10       txa
            sta $dad0,y
            lda fshade
            sta $dad1,y
            lda clscol
            sta $dad2,y
            sta $dad3,y
            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;EOF
