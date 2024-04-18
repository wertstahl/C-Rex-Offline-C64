
; 
; CBMprgStudio 3.10.0+ Syntax 
;
; C=Rex Offline - an adaption of 2016's Chrome Offline T-Rex Browser Game
;
; Copy Routine for animated G*P Logo


xposoffset  = 4
scrn_ofs01  = screenbase+xposoffset+40*0
scrn_ofs02  = screenbase+xposoffset+40*1
scrn_ofs03  = screenbase+xposoffset+40*2
scrn_ofs04  = screenbase+xposoffset+40*3
scrn_ofs05  = screenbase+xposoffset+40*4
scrn_ofs06  = screenbase+xposoffset+40*5
scrn_ofs07  = screenbase+xposoffset+40*6
scrn_ofs08  = screenbase+xposoffset+40*7
scrn_ofs09  = screenbase+xposoffset+40*8
scrn_ofs10  = screenbase+xposoffset+40*9
scrn_ofs11  = screenbase+xposoffset+40*10
scrn_ofs12  = screenbase+xposoffset+40*11
scrn_ofs13  = screenbase+xposoffset+40*12
scrn_ofs14  = screenbase+xposoffset+40*13
scrn_ofs15  = screenbase+xposoffset+40*14
scrn_ofs16  = screenbase+xposoffset+40*15
scrn_ofs17  = screenbase+xposoffset+40*16
scrn_ofs18  = screenbase+xposoffset+40*17
scrn_ofs19  = screenbase+xposoffset+40*18
scrn_ofs20  = screenbase+xposoffset+40*19


dosettx0    lda aline01,y
            sta scrn_ofs01,y
            lda aline02,y
            sta scrn_ofs02,y
            lda aline03,y
            sta scrn_ofs03,y
            lda aline04,y
            sta scrn_ofs04,y
            lda aline05,y
            sta scrn_ofs05,y
            lda aline06,y
            sta scrn_ofs06,y
            lda aline07,y
            sta scrn_ofs07,y

            lda aline08,y
            sta scrn_ofs08,y
            lda aline09,y
            sta scrn_ofs09,y
            lda aline10,y
            sta scrn_ofs10,y
            lda aline11,y
            sta scrn_ofs11,y
            lda aline12,y
            sta scrn_ofs12,y
            lda aline13,y
            sta scrn_ofs13,y
            lda aline14,y
            sta scrn_ofs14,y
            lda aline15,y
            sta scrn_ofs15,y
            lda aline16,y
            sta scrn_ofs16,y
            lda aline17,y
            sta scrn_ofs17,y
            lda aline18,y
            sta scrn_ofs18,y
            lda aline19,y
            sta scrn_ofs19,y
;            lda aline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx0

            rts

dosettx1    lda bline01,y
            sta scrn_ofs01,y
            lda bline02,y
            sta scrn_ofs02,y
            lda bline03,y
            sta scrn_ofs03,y
            lda bline04,y
            sta scrn_ofs04,y
            lda bline05,y
            sta scrn_ofs05,y
            lda bline06,y
            sta scrn_ofs06,y
            lda bline07,y
            sta scrn_ofs07,y
            lda bline08,y
            sta scrn_ofs08,y
            lda bline09,y
            sta scrn_ofs09,y
            lda bline10,y
            sta scrn_ofs10,y
            lda bline11,y
            sta scrn_ofs11,y
            lda bline12,y
            sta scrn_ofs12,y
            lda bline13,y
            sta scrn_ofs13,y
            lda bline14,y
            sta scrn_ofs14,y
            lda bline15,y
            sta scrn_ofs15,y
            lda bline16,y
            sta scrn_ofs16,y
            lda bline17,y
            sta scrn_ofs17,y
            lda bline18,y
            sta scrn_ofs18,y
            lda bline19,y
            sta scrn_ofs19,y
;            lda bline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx1
            rts

dosettx2    lda cline01,y
            sta scrn_ofs01,y
            lda cline02,y
            sta scrn_ofs02,y
            lda cline03,y
            sta scrn_ofs03,y
            lda cline04,y
            sta scrn_ofs04,y
            lda cline05,y
            sta scrn_ofs05,y
            lda cline06,y
            sta scrn_ofs06,y
            lda cline07,y
            sta scrn_ofs07,y
            lda cline08,y
            sta scrn_ofs08,y
            lda cline09,y
            sta scrn_ofs09,y
            lda cline10,y
            sta scrn_ofs10,y
            lda cline11,y
            sta scrn_ofs11,y
            lda cline12,y
            sta scrn_ofs12,y
            lda cline13,y
            sta scrn_ofs13,y
            lda cline14,y
            sta scrn_ofs14,y
            lda cline15,y
            sta scrn_ofs15,y
            lda cline16,y
            sta scrn_ofs16,y
            lda cline17,y
            sta scrn_ofs17,y
            lda cline18,y
            sta scrn_ofs18,y
            lda cline19,y
            sta scrn_ofs19,y
;            lda cline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx2
            rts

dosettx3    lda dline01,y
            sta scrn_ofs01,y
            lda dline02,y
            sta scrn_ofs02,y
            lda dline03,y
            sta scrn_ofs03,y
            lda dline04,y
            sta scrn_ofs04,y
            lda dline05,y
            sta scrn_ofs05,y
            lda dline06,y
            sta scrn_ofs06,y
            lda dline07,y
            sta scrn_ofs07,y
            lda dline08,y
            sta scrn_ofs08,y
            lda dline09,y
            sta scrn_ofs09,y
            lda dline10,y
            sta scrn_ofs10,y
            lda dline11,y
            sta scrn_ofs11,y
            lda dline12,y
            sta scrn_ofs12,y
            lda dline13,y
            sta scrn_ofs13,y
            lda dline14,y
            sta scrn_ofs14,y
            lda dline15,y
            sta scrn_ofs15,y
            lda dline16,y
            sta scrn_ofs16,y
            lda dline17,y
            sta scrn_ofs17,y
            lda dline18,y
            sta scrn_ofs18,y
            lda dline19,y
            sta scrn_ofs19,y
;            lda dline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx3
            rts

dosettx4    lda eline01,y
            sta scrn_ofs01,y
            lda eline02,y
            sta scrn_ofs02,y
            lda eline03,y
            sta scrn_ofs03,y
            lda eline04,y
            sta scrn_ofs04,y
            lda eline05,y
            sta scrn_ofs05,y
            lda eline06,y
            sta scrn_ofs06,y
            lda eline07,y
            sta scrn_ofs07,y
            lda eline08,y
            sta scrn_ofs08,y
            lda eline09,y
            sta scrn_ofs09,y
            lda eline10,y
            sta scrn_ofs10,y
            lda eline11,y
            sta scrn_ofs11,y
            lda eline12,y
            sta scrn_ofs12,y
            lda eline13,y
            sta scrn_ofs13,y
            lda eline14,y
            sta scrn_ofs14,y
            lda eline15,y
            sta scrn_ofs15,y
            lda eline16,y
            sta scrn_ofs16,y
            lda eline17,y
            sta scrn_ofs17,y
            lda eline18,y
            sta scrn_ofs18,y
            lda eline19,y
            sta scrn_ofs19,y
;            lda eline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx4
            rts

dosettx5    lda fline01,y
            sta scrn_ofs01,y
            lda fline02,y
            sta scrn_ofs02,y
            lda fline03,y
            sta scrn_ofs03,y
            lda fline04,y
            sta scrn_ofs04,y
            lda fline05,y
            sta scrn_ofs05,y
            lda fline06,y
            sta scrn_ofs06,y
            lda fline07,y
            sta scrn_ofs07,y
            lda fline08,y
            sta scrn_ofs08,y
            lda fline09,y
            sta scrn_ofs09,y
            lda fline10,y
            sta scrn_ofs10,y
            lda fline11,y
            sta scrn_ofs11,y
            lda fline12,y
            sta scrn_ofs12,y
            lda fline13,y
            sta scrn_ofs13,y
            lda fline14,y
            sta scrn_ofs14,y
            lda fline15,y
            sta scrn_ofs15,y
            lda fline16,y
            sta scrn_ofs16,y
            lda fline17,y
            sta scrn_ofs17,y
            lda fline18,y
            sta scrn_ofs18,y
            lda fline19,y
            sta scrn_ofs19,y
;            lda fline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx5
            rts

dosettx6    lda gline01,y
            sta scrn_ofs01,y
            lda gline02,y
            sta scrn_ofs02,y
            lda gline03,y
            sta scrn_ofs03,y
            lda gline04,y
            sta scrn_ofs04,y
            lda gline05,y
            sta scrn_ofs05,y
            lda gline06,y
            sta scrn_ofs06,y
            lda gline07,y
            sta scrn_ofs07,y
            lda gline08,y
            sta scrn_ofs08,y
            lda gline09,y
            sta scrn_ofs09,y
            lda gline10,y
            sta scrn_ofs10,y
            lda gline11,y
            sta scrn_ofs11,y
            lda gline12,y
            sta scrn_ofs12,y
            lda gline13,y
            sta scrn_ofs13,y
            lda gline14,y
            sta scrn_ofs14,y
            lda gline15,y
            sta scrn_ofs15,y
            lda gline16,y
            sta scrn_ofs16,y
            lda gline17,y
            sta scrn_ofs17,y
            lda gline18,y
            sta scrn_ofs18,y
            lda gline19,y
            sta scrn_ofs19,y
;            lda gline20,y
;            sta scrn_ofs20,y
            dey
            bpl dosettx6
            rts

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;EOF
