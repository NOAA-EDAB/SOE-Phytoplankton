; $ID:	SOE_SST.PRO,	2021-08-17-11,	USER-KJWH	$
  PRO SOE_SST, VERSION, BUFFER=BUFFER

;+
; NAME:
;   SOE_SST
;
; PURPOSE:
;   $PURPOSE$
;
; CATEGORY:
;   $CATEGORY$
;
; CALLING SEQUENCE:
;   SOE_SST,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
;
; REQUIRED INPUTS:
;   Parm1.......... Describe the positional input parameters here. 
;
; OPTIONAL INPUTS:
;   Parm2.......... Describe optional inputs here. If none, delete this section.
;
; KEYWORD PARAMETERS:
;   KEY1........... Document keyword parameters like this. Note that the keyword is shown in ALL CAPS!
;
; OUTPUTS:
;   OUTPUT.......... Decribe the output of this program or function
;
; OPTIONAL OUTPUTS:
;   None
;
; COMMON BLOCKS: 
;   None
;
; SIDE EFFECTS:  
;   None
;
; RESTRICTIONS:  
;   None
;
; EXAMPLE:
; 
;
; NOTES:
;   $Citations or any other useful notes$
;   
; COPYRIGHT: 
; Copyright (C) 2021, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on August 17, 2021 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Aug 17, 2021 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_SST'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF NONE(VERSION)   THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'
  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VSTR = SOE_VERSION_INFO(VER)
    EPU_OUTLINE = VSTR.INFO.SUBAREA_OUTLINE
    DATASETS = [VSTR.PROD_INFO.SST.DATASET]

    CASE VER OF
      'V2021': BEGIN & PRODS=['SST'] & TYPES = ['STATS','ANOMS'] & PERIODS='M' & END
      'V2022': BEGIN & PRODS=['SST'] & TYPES = ['STATS','ANOMS'] & PERIODS='M' & END
    ENDCASE
    
    DT = '20210608' & TITLE='2021-06-08'
    F = GET_FILES('MUR',PRODS='SST',DATERANGE=[DT,DT])
    PNGFILE = VSTR.DIRS.DIR_SST + 'D_'+DT+'-MUR-SST-WCR.PNG'
    OPROD = 'SST_10_30'
    IF FILE_MAKE(F,PNGFILE,OVERWRITE=OVERWRITE) EQ 1 THEN BEGIN
      PRODS_2PNG, F, PROD=OPROD, MAPP='NWA', CROP=[150,1200,600,1250], /CURRENT, OBJ=IMG, ADD_BATHY=200, C_LEVELS=[20,24], C_COLOR=249, C_THICK=2, C_ANNOTATION=[' ',' ']
      TD = TEXT(0.22,0.965, TITLE, FONT_SIZE=20, FONT_STYLE='BOLD', FONT_COLOR='BLACK', ALIGNMENT=0.5)
      E1 = ELLIPSE(.22,.27,MAJOR=.025,MINOR=.048,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E2 = ELLIPSE(.28,.31,MAJOR=.025,MINOR=.048,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E3 = ELLIPSE(.40,.40,MAJOR=.050,MINOR=.095,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E4 = ELLIPSE(.48,.46,MAJOR=.028,MINOR=.057,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E5 = ELLIPSE(.59,.46,MAJOR=.072,MINOR=.114,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E6 = ELLIPSE(.64,.62,MAJOR=.025,MINOR=.035,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E7 = ELLIPSE(.70,.64,MAJOR=.025,MINOR=.035,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0)
      E8 = ELLIPSE(.78,.52,MAJOR=.040,MINOR=.084,THICK=6,COLOR='BLACK',TARGET=IMG,FILL_BACKGROUND=0,THETA=16) 
      CBAR, OPROD, OBJ=IMG, FONT_SIZE=14, FONT_STYLE=FONT_STYLE, CB_TYPE=3, CB_POS=[0.02,0.92,0.46,0.96], CB_TITLE=UNITS('SST'), PAL=PAL
      IMG.SAVE, PNGFILE
      IMG.CLOSE
    ENDIF  
          
    
    FOR D=0, N_ELEMENTS(DATASETS)-1 DO BEGIN
      DATASET = DATASETS[D]
      
      FOR R=0, N_ELEMENTS(PRODS)-1 DO BEGIN
        APROD = PRODS[R]
        
        FOR T=0, N_ELEMENTS(TYPES)-1 DO BEGIN
          TYPE = TYPES[T]
          FILES = GET_FILES(DATASET,PRODS=APROD,FILE_TYPE=TYPE,PERIODS=PERIODS,DATERANGE=VSTR.INFO.SOE_YEAR)
          FP = PARSE_IT(FILES,/ALL)
          TXT=MONTH_NAMES(FP.MONTH_START)
          CASE TYPE OF 
            'STATS': BEGIN & PROD=VSTR.PROD_INFO.SST.PROD_SCALE & CB_TITLE=UNITS('TEMP') & SPAL=VSTR.PROD_INFO.SST.PAL & END
            'ANOMS': BEGIN & PROD=VSTR.PROD_INFO.SST.ANOM_SCALE & CB_TITLE=UNITS('TEMP') & SPAL=VSTR.PROD_INFO.SST.ANOM_PAL & END
          ENDCASE
          
          FOR M=0, N_ELEMENTS(VSTR.INFO.MAP_OUT)-1 DO BEGIN
            OMAP = VSTR.INFO.MAP_OUT[M]
            DIR_OUT = VSTR.DIRS.DIR_SST + STRJOIN([DATASET,OMAP,TYPE,APROD],SL) + SL 
            FOR F=0, N_ELEMENTS(FILES)-1 DO PRODS_2PNG,FILES[F],DIR_OUT=DIR_OUT,MAPP=OMAP,PROD=PROD,OUTLINE=EPU_OUTLINE,OUT_COLOR=0,OUT_THICK=4,CB_TITLE=CB_TITLE,ADD_TXT=TXT[F],TXT_POS=[0.98,0.02],TXT_ALIGN=1.0,TXT_SIZE=22,/ADD_CB,CB_RELATIVE=CB_RELATIVE,PAL=SPAL

          
          ENDFOR ; MAPS
        ENDFOR ; TYPES
      ENDFOR ; PRODS    
      
    ENDFOR ; DATASETS
    
  ENDFOR ; VERSION    


END ; ***************** End of SOE_SST *****************
