; $ID:	SOE_PHYTOSIZE_COMPOSITES.PRO,	2022-01-10-13,	USER-KJWH	$
  PRO SOE_PHYTOSIZE_COMPOSITES, VERSION, BUFFER=BUFFER

;+
; NAME:
;   SOE_PHYTOSIZE_COMPOSITES
;
; PURPOSE:
;   $PURPOSE$
;
; PROJECT:
;   READ_EDAB_SOE_PHYTOPLANKTON
;
; CALLING SEQUENCE:
;   SOE_PHYTOSIZE_COMPOSITES,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
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
; Copyright (C) 2022, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on January 10, 2022 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jan 10, 2022 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_PHYTOSIZE_COMPOSITES'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF ~N_ELEMENTS(VERSION) THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'
  IF ~N_ELEMENTS(BUFFER) THEN BUFFER=0

  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VERSTR = SOE_VERSION_INFO(VER)
    IF ~N_ELEMENTS(DATFILE) THEN DATFILE = VERSTR.INFO.DATAFILE
    IF ~N_ELEMENTS(DIR_PLOTS) THEN DIR_PLT = VERSTR.DIRS.DIR_COMP+'PSC'+SL ELSE DIR_PLT = DIR_PLOTS & DIR_TEST, DIR_PLT
    IF ~N_ELEMENTS(DIR_MOVIE) THEN DIR_MOV = VERSTR.DIRS.DIR_MOVIE+'PSC'+SL ELSE DIR_MOV = DIR_MOVIE & DIR_TEST, DIR_MOV
    MAP_OUT = VERSTR.INFO.MAP_OUT
    DR = VERSTR.INFO.DATERANGE
  
    VPRODS = TAG_NAMES(VERSTR.PROD_INFO)
    ALG = VALIDS('ALGS',VERSTR.PROD_INFO.MICRO.PROD)
    GROUP_PRODS = VERSTR.INFO.COMPOSITE_PRODS ; LIST([['MICRO','NANO','PICO']+'-'+ALG],[['MICRO','NANO','PICO']+'_PERCENTAGE-'+ALG])
    COMP_PERIOD = VERSTR.INFO.COMPOSITE_PERIODS
    MOV_PERIOD = VERSTR.INFO.MOVIE_PERIODS
        
    OUTLINE = VERSTR.INFO.SUBAREA_OUTLINE
    OCOLOR=0
  
    FOR C=0, N_ELEMENTS(COMP_PERIOD)-1 DO BEGIN
      CPER = COMP_PERIOD[C]
      
      FOR G=0, N_ELEMENTS(GROUP_PRODS)-1 DO BEGIN
        GPRODS = GROUP_PRODS[G]
        CASE GPRODS[0] OF
          'MICRO': BEGIN & CB_TITLE = UNITS('CHLOR_A') & CB_TICKS=[] & END
          'MICRO_PERCENTAGE': BEGIN & CB_TITLE = 'Fraction of Total Chlorophyll' & CB_TICKS=ROUNDS(FINDGEN(9)*0.1,1) & END
        ENDCASE
        
        PSCFILES = []
        DTSET = []
        FOR R=0, N_ELEMENTS(GPRODS)-1 DO BEGIN
          GPR = VALIDS('PRODS',GPRODS[R])
          OK = WHERE(VPRODS EQ GPR,COUNT)
          IF COUNT NE 1 THEN MESSAGE, 'ERROR: ' + GPR + ' not found in the SOE Version structure'
          DTSET = [DTSET,VERSTR.PROD_INFO.(OK).DATASET]
          PSCFILES = [PSCFILES,GET_FILES(DTSET[R], PRODS=GPR+'-'+ALG, PERIODS=CPER)]
        ENDFOR
        
        GSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ GPRODS[0]))
        PAL = GSTR.PAL
        SCL = GSTR.IMAGE_SCALE
        
        FP = PARSE_IT(PSCFILES) & MP = PERIOD_2STRUCT(FP.PERIOD)
        SETS = WHERE_SETS(FP.PERIOD) 
        PNGS = []
        FOR S=0, N_ELEMENTS(SETS)-1 DO BEGIN
          PER = SETS[S].VALUE
          DP = PERIOD_2STRUCT(PER)
          PSC = PSCFILES[WHERE_SETS_SUBS(SETS[S])]
          PSC = PSC[SORT(PSC)]
          IF N_ELEMENTS(PSC) NE N_ELEMENTS(GPRODS) THEN MESSAGE, 'ERROR: The number of files are not correct.'
          IF ~SAME(DTSET) THEN MESSAGE, 'ERROR: The group products ' + STRJOIN(GPRODS,', ') + ' do not have the same dataset.'
          IF HAS(GPR,'PERCENT') THEN PLABEL = 'PHYTO_SIZE_FRACTION' ELSE PLABEL = 'PHYTO_SIZE_CHLOR_A'
          PNG = DIR_PLT + PER + '-' + DTSET[0] + '-' + PLABEL + '-' + ALG + '.png'
          PNGS = [PNGS,PNG]
          IF FILE_MAKE(PSC,PNG,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE
          W = WINDOW(DIMENSIONS=[768,380],BUFFER=BUFFER)
          CBAR, SCL, OBJ=W, FONT_SIZE=12, CB_TYPE=3, CB_POS=[0.15,0.12,0.85,0.19], CB_TITLE=CB_TITLE, CB_TICKVALUES=CB_TICKS, PAL=PAL
    
          CASE DP.PERIOD_CODE OF
            'ANNUAL': DTXT = 'Annual Climatology'
            'MONTH':  DTXT = MONTH_NAMES(DP.MONTH_START)
            'M':      DTXT = MONTH_NAMES(DP.MONTH_START) + ' - ' + DP.YEAR_START
            'WEEK':   DTXT = MONTH_NAMES(DP.MONTH_START) + ' ' + DP.DAY_START + ' - ' + MONTH_NAMES(DP.MONTH_END) + ' ' + DP.DAY_END            
            'W':      DTXT = (DATE_PARSE(JD_2DATE(JD_ADD(DATE_2JD(DP.DATE_START),3,/DAY)))).STRING_DATE
          ENDCASE
          
          TXT = TEXT(0.5,0.92,DTXT,ALIGNMENT=0.5,FONT_STYLE='BOLD',FONT_SIZE=20)
    
          PRODS_2PNG,PSC[0],SPROD=SCL,MAPP=MAP_OUT,OUTLINE=OUTLINE,OUT_COLOR=OCOLOR,/CURRENT,IMG_POS=[0,  85,256,341],/DEVICE,PAL=PAL
          PRODS_2PNG,PSC[1],SPROD=SCL,MAPP=MAP_OUT,OUTLINE=OUTLINE,OUT_COLOR=OCOLOR,/CURRENT,IMG_POS=[256,85,512,341],/DEVICE,PAL=PAL
          PRODS_2PNG,PSC[2],SPROD=SCL,MAPP=MAP_OUT,OUTLINE=OUTLINE,OUT_COLOR=OCOLOR,/CURRENT,IMG_POS=[512,85,768,341],/DEVICE,PAL=PAL
              
          TM = TEXT(0.01,0.84,'Microplankton',FONT_SIZE=12,FONT_STYLE='BOLD')
          TN = TEXT(0.35,0.84,'Nanoplankton',FONT_SIZE=12,FONT_STYLE='BOLD')
          TP = TEXT(0.68,0.84,'Picoplankton',FONT_SIZE=12,FONT_STYLE='BOLD')
    
          W.SAVE, PNG
          W.CLOSE
          PFILE, PNG
        ENDFOR ; PERIOD SETS  
        FPS = 15
        MPER = CPER + '_' + MIN(MP.YEAR_START) + '_' + MAX(MP.YEAR_END)
        MOV = DIR_MOV + MPER + '-' + DTSET[0] + '-' + PLABEL + '-' + ALG + '.mp4'
        IF N_ELEMENTS(PNGS) GT 1 THEN MAKE_MOVIE, PNGS, MOVIE_FILE=MOV, FRAME_SEC=FPS
      ENDFOR ; GROUP_PRODS
    ENDFOR ; COMP_PERIODS
  ENDFOR ; VERSIONS  


END ; ***************** End of SOE_PHYTOSIZE_COMPOSITES *****************
