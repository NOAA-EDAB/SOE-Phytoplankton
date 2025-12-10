; $ID:	SOE_ANIMATIONS.PRO,	2025-01-27-17,	USER-KJWH	$
  PRO SOE_ANIMATIONS, VERSION_STRUCT, EVENTS=EVENTS, BUFFER=BUFFER

;+
; NAME:
;   SOE_ANIMATIONS
;
; PURPOSE:
;   $PURPOSE$
;
; PROJECT:
;   SOE_PHYTOPLANKTON
;
; CALLING SEQUENCE:
;   SOE_ANIMATIONS,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
;
; REQUIRED INPUTS:
;   VERSION_STRUCT...... The version structure for the SOE
;
; OPTIONAL INPUTS:
;   EVENTS.............. The specific "events" to create an animation for (e.g. 2024_UPWELLING)
;
; KEYWORD PARAMETERS:
;   BUFFER.............. Turn on/off the graphics buffer
;
; OUTPUTS:
;   OUTPUT.............. SOE specific animation
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
; Copyright (C) 2025, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on January 27, 2025 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jan 27, 2025 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_ANIMATIONS'
  COMPILE_OPT IDL3
  SL = PATH_SEP()
  
  IF ~N_ELEMENTS(VERSION_STRUCT) THEN MESSAGE, 'ERROR: Must provide the SOE VERSION structure'
  IF ~N_ELEMENTS(BUFFER) THEN BUFFER=0
  IF ~N_ELEMENTS(EVENTS) THEN EVENTS = 'CURRENT_YEAR_SST'
  
  VERSTR = VERSION_STRUCT
  MPS = VERSTR.INFO.MAP_OUT
  DTR = VERSTR.INFO.DATERANGE
  CURRENT_YEAR = VERSTR.INFO.YEAR
  ANIMATION_DIR = VERSTR.DIRS.DIR_ANIMATIONS
    
  FOR E=0, N_ELEMENTS(EVENTS)-1 DO BEGIN
    EVENT = EVENTS[E]
    EDIR = ANIMATION_DIR + EVENT + SL & PDIR = EDIR + 'PNGS' + SL & DIR_TEST, PDIR
    SST_SCALE = [0,30] & SST_TICKS = [0,5,10,15,20,25,30] & SST_CONTOUR = []
    SST_ASCALE = [-5,5] & SST_ATICKS = [-5,-2.5,0,2.5,5] 
    CHL_SCALE = [0.1,10] & CHL_TICKS = [0.1,0.3,1,3,10]   & CHL_CONTOUR = []
    CHL_ASCALE = [0.2,6] & CHL_ATICKS = [0.2,0.6,1,3,6]
    PPD_SCALE = [0.1,3] & PPD_TICKS = []                  & PPD_CONTOUR = []
    PPD_ASCALE = [0.2,6] & PPD_ATICKS = [0.2,0.6,1,3,6]
    CASE EVENT OF
      '2024_UPWELLING': BEGIN
        DTR = GET_DATERANGE([20240601,20240830])
        PRODS = LIST('SST');,'CHLOR_A',['SST','CHLOR_A']);,'SST-ANOMALY',['SST','SST-ANOMALY'])
        MPS = ['MAB','NJ_COAST'];,'NES','NJ_COAST']
        SST_SCALE = [14,26] & SST_TICKS = [14,16,18,20,22,24,26] & SST_CONTOUR = 20
        SST_ASCALE = [-5,5]
        CHL_SCALE = [0.1,30]
        PERIOD = 'D'
      END  
    ENDCASE
    
    FOR R=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      APRODS = STR_BREAK(PRODS[R],'-') & APROD = APRODS[0] & IF N_ELEMENTS(APRODS) GT 1 THEN IF APRODS[0,1] EQ 'ANOMALY' THEN ATYPE = 'ANOMALY' ELSE ATYPE = []
      IF N_ELEMENTS(APRODS) GT 1 THEN COMBO = 1 ELSE COMBO = 0
      IF N_ELEMENTS(PRODS[R]) GT 2 THEN MESSAGE, 'ERROR: Currently this program is only set up to work with 2 products'
      
      CASE VALIDS('PRODS',APROD) OF
        'SST':     BEGIN & SYTITLE=UNITS('TEMP')    & AYTITLE=UNITS('TEMP',/NO_UNIT)+' Anomaly ' + UNITS('TEMP',/NO_NAME) & SRNG=SST_SCALE & ARNG=SST_ASCALE & STICKS=SST_TICKS & ATICKS=SST_ATICKS & SCONTOUR=SST_CONTOUR & PSTATS='MEAN' & ALG=0 & AMID=0 & ASTATS='AMEAN' & AYTICKS=[] & END
        'CHLOR_A': BEGIN & SYTITLE=UNITS('CHLOR_A') & AYTITLE=UNITS('CHLOR_A',/NO_UNIT) + ' Ratio Anomaly'                & SRNG=CHL_SCALE & ARNG=CHL_ASCALE & STICKS=CHL_TICKS & ATICKS=CHL_ATICKS & SCONTOUR=CHL_CONTOUR & PSTATS='MED'  & ALG=1 & AMID=1 & ASTATS='AMEAN' & AYTICKS=[] & END
        'PPD':     BEGIN & SYTITLE=UNITS('PPD')     & AYTITLE=UNITS('PPD',/NO_UNIT) + ' Ratio Anomaly'                    & SRNG=PPD_SCALE & ARNG=PPD_ASCALE & STICKS=PPD_TICKS & ATICKS=PPD_ATICKS & SCONTOUR=PPD_CONTOUR & PSTATS='MED'  & ALG=1 & AMID=1 & ASTATS='AMEAN' & AYTICKS=[] & END
      ENDCASE
      IF ATYPE EQ [] THEN APROD_SCALE = APROD + '_' + STRJOIN(NUM2STR(SRNG),'_') ELSE APROD_SCALE = APROD + '-' + STRJOIN(NUM2STR(ARNG),'_')

      FOR M=0, N_ELEMENTS(MPS)-1 DO BEGIN
        MP = MPS[M]
        PROJECT_MAKE_ANIMATION, VERSTR, PRODS=APROD,PROD_SCALE=APROD_SCALE,CB_TITLE=ATITLE,CB_TICKVALUES=STICKS,C_LEVELS=SCONTOUR,C_COLOR=1,MAPP=MP,DATERANGE=DTR,/ADD_COLORBAR,/ADD_BATHY,/ADD_LONLAT,/ADD_BORDER,/ADD_DATEBAR, BUFFER=BUFFER
      ENDFOR
      
;      ASTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ APROD,/NULL))
;      AFILES = GET_FILES(ASTR.DATASET, PRODS=APROD, PERIOD=PERIOD, FILE_TYPE=ATYPE)
;      
;      IF KEYWORD_SET(COMBO) THEN BEGIN
;        IF APRODS[1,1] EQ 'ANOMALY' THEN BTYPE = 'ANOMALY' ELSE BTYPE = []
;        BSTR = VERSTR.PROD_INFO.(WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ PRODS[1],/NULL))
;        BFILES = GET_FILES(BSTR.DATASET, PRODS=BSTR.PROD, PERIOD=PERIOD, FILE_TYPE=BTYPE)
;        IF BTYPE EQ [] THEN BPROD_SCALE = APROD + '-' + STRJOIN(NUM2STR(SRNG),'_') ELSE BPROD_SCALE = APROD + '-' + STRJOIN(NUM2STR(ARNG),'_')
;      ENDIF ELSE BFILES = []
;            
;      FOR M=0, N_ELEMENTS(MPS)-1 DO BEGIN
;        MP = MPS[M]
;        CASE MP OF
;          'NES': BEGIN & RESIZE=.85 & END
;          'MAB': BEGIN & RESIZE=.9 & END
;          'MAB_GS': BEGIN & RESIZE=.9 & END
;        ENDCASE
;      ENDFOR
;      
;      IMG = PROJECT_MAKE_IMAGE(VERSTR, FILE=AFILE, BUFFER=BUFFER, MAPP=MP, $
;        /ADD_BATHY, ADD_LONLAT=ADD_LONLAT, PROD_SCALE=PROD_SCALE, CBTITLE=CBTITLE, CB_TICKVALUES=CB_TICKVALUES, CB_POS=CB_POS, SCLR=SCLR, PAL=PAL, LONS=LONS, LATS=LATS,$
;        RESIZE=RESIZE,/ADD_COLORBAR, ADD_PIONEER=ADD_PIONEER,/ADD_BORDER,/ADD_TITLE,TITLE_TEXT=TXT, _EXTRA=_EXTRA)
;      IMG.SAVE, PNGFILE, RESOLUTION=RESOLUTION
;      IMG.CLOSE
      
    ENDFOR
    
    
  ENDFOR


END ; ***************** End of SOE_ANIMATIONS *****************
