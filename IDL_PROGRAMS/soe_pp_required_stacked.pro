; $ID:	SOE_PP_REQUIRED_STACKED.PRO,	2023-01-10-16,	USER-KJWH	$
  PRO SOE_PP_REQUIRED_STACKED, VERSION, DIR_DATA=DIR_DATA, PRODUCTS=PRODUCTS, SHAPEFILE=SHAPEFILE, SAVEFILES=SAVEFILES

;+
; NAME:
;   SOE_PP_REQUIRED_STACKED
;
; PURPOSE:
;   Extract the CHL and PP data from "stacked" files for the PP Required SOE analyses (for Andy Beet)
;
; PROJECT:
;   SOE_PHYTOPLANKTON
;
; CALLING SEQUENCE:
;   SOE_PP_REQUIRED_STACKED,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
;
; REQUIRED INPUTS:
;   VERSION........ The SOE version
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
; Copyright (C) 2023, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on January 10, 2023 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Jan 10, 2023 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_PP_REQUIRED_STACKED'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF ~N_ELEMENTS(VERSION) THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'

  VERSTR = SOE_VERSION_INFO(VER)
  VPRODS = TAG_NAMES(VERSTR.PROD_INFO)
  IF ~KEYWORD_SET(VERSTR.INFO.STACKED) THEN MESSAGE, 'ERROR: Expecting version that uses "stacked" input files

  IF ~N_ELEMENTS(DIR_DATA) THEN DIR_OUT = VERSTR.DIRS.DIR_PP_REQUIRED ELSE DIR_OUT = DIR_DATA
  PROD_DATASETS = []
  IF ANY(PRODUCTS) THEN BEGIN
    PPRODS    = PRODUCTS
    IF N_ELEMENTS(DATASETS) EQ N_ELEMENTS(PRODUCTS) THEN PROD_DATASETS = DATASETS
  ENDIF ELSE PPRODS = VERSTR.INFO.PPREQ_PRODS
  IF ANY(DATERANGE) THEN DR       = DATERANGE ELSE DR = VERSTR.INFO.DATERANGE
  YEARS = YEAR_RANGE(DR,/STRING)

  ; ===> Get the SHAPEFILE information for the subarea extracts
  IF ~N_ELEMENTS(SHAPEFILE)  THEN SHPFILES  = VERSTR.INFO.PPREQ_SHPFILE ELSE SHPFILES = SHAPEFILE
  IF ~N_ELEMENTS(SUBAREAS) THEN BEGIN
    IF VERSTR.INFO.SHAPEFILE EQ VERSTR.INFO.PPREQ_SHPFILE[0] THEN NAMES = VERSTR.INFO.SUBAREA_NAMES $
    ELSE NAMES = ['GOM','GB','MAB']
  ENDIF ELSE NAMES = SUBAREAS

  MP = VERSTR.INFO.MAP_OUT
  PPERIODS = VERSTR.INFO.PPREQ_PERIODS

  FOR SA=0, N_ELEMENTS(SHPFILES)-1 DO BEGIN ; LOOP THROUGH SUBAREA SHAPE FILES
    SHPFILE = SHPFILES[SA]
    DIR_PPSUB = DIR_OUT + SHPFILE + SL

    SHPSTR = []
    PFILES = []
    FOR PP=0, N_ELEMENTS(PPRODS)-1 DO BEGIN ; LOOP THROUGH PRODS
      PROD = PPRODS[PP]
      VPROD = VALIDS('PRODS',PROD)
      OK = WHERE(VPRODS EQ VPROD,COUNT)
      IF COUNT NE 1 THEN MESSAGE, 'ERROR: ' + GPR + ' not found in the SOE Version structure'
      DTSET = VERSTR.PROD_INFO.(OK).DATASET
      DVERSION = VERSTR.PROD_INFO.(OK).VERSION
      CASE VPROD OF
        'PPD': BEGIN & RTAG = 'GMEAN' & RNGE = '0.001_50.0' & SUM_STATS=1 & END
        'CHLOR_A': BEGIN & RTAG = 'GMEAN' & RNGE = '0.001_80.0' & SUM_STATS=0 & END
      ENDCASE
      DIR_MONTH = DIR_PPSUB + 'MONTHLY_EXTRACTS-' + PROD + SL & DIR_TEST, DIR_MONTH

      SAVEFILES = []
      
      ; ===> GET FILES
      FILES = GET_FILES(DTSET, PRODS=PROD, PERIODS='M', DATERANGE=YR, FILE_TYPE='STACKED_STATS', VERSION=DVERSION)
      FA = PARSE_IT(FILES,/ALL)
      MPIN = FA[0].MAP
      IF ~SAME(FA.MAP) THEN MESSAGE, 'ERROR: All files do not have the same MAP.'
      IF SHPSTR EQ [] THEN SHPSTR = READ_SHPFILE(SHPFILE, MAPP=MPIN, ATT_TAG=ATT_TAG, COLOR=COLOR, VERBOSE=VERBOSE, NORMAL=NORMAL, AROUND=AROUND)
      SUBAREA_TAGS = TAG_NAMES(SHPSTR)

      FOR Y=0, N_ELEMENTS(FILES)-1 DO BEGIN
        YFILE = FILES[Y]
        FP = FA[Y]
        
        SAVEFILE = DIR_MONTH + 'M_'+FP.YEAR_START + '-' + REPLACE(FP.NAME,FP.PERIOD,SHPFILE) +'.SAV'
        SAVEFILES = [SAVEFILES,SAVEFILE]
        IF FILE_MAKE(FILES,SAVEFILE,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE
   
 ;       NCINFO = NETCDF_INFO(YFILE)                                         ; Get the netcdf information based on the file name
 ;       NCSTR = STRUCT_COPY(NCINFO,TAGNAMES=['FILE','PROD_NAME','ALG_NAME','TIME_START','TIME_END','ALG_REFERENCE',$
 ;                                            'SENSOR','SOURCE_DATA_VERSION','SOURCE_DATA_URL','SOURCE_DATA_DOI','TITLE','CONTRIBUTOR_NAME'])
        PFILE, YFILE, /R
        STR = STACKED_READ(YFILE,KEYS=KEYS,DB=DB,BINS=BINS,OUTHASH=DHASH,INFO=INFO,METADATA=META)
        
        TEMP = CREATE_STRUCT(INFO,STRUCT_COPY(FP[0],['PROD','ALG']))
        TEMP = CREATE_STRUCT(TEMP,'FILE_METADATA', STRUCT_MERGE(META.GLOBAL,META.FILE_INFORMATION))
        
        FOR N=0, N_ELEMENTS(NAMES)-1 DO BEGIN
          OK = WHERE(SUBAREA_TAGS EQ NAMES[N],/NULL,COUNTSHP)
          IF COUNTSHP EQ 0 THEN CONTINUE
          ATEMP = []
          SUBS  = SHPSTR.(OK).SUBS
        
          FOR D=0, N_ELEMENTS(DB.PERIOD)-1 DO BEGIN
            PER = DB.PERIOD[D]
            DAT = MAPS_L3B_2ARR(DHASH[VPROD+'_'+RTAG,*,*,D],MP=FP.MAP,BINS=BINS)
            ATEMP = CREATE_STRUCT(ATEMP,PER+'_'+NAMES[N],DAT[SUBS])
          ENDFOR ; Periods
          TEMP = CREATE_STRUCT(TEMP,NAMES[N],ATEMP)
        ENDFOR ; Subarea names
        PRINT, 'Writing: ' + SAVEFILE
        SAVE,FILENAME=SAVEFILE,TEMP,/COMPRESS
      ENDFOR ; Files/Years
    ENDFOR ; Products
  ENDFOR ; Shapefiles
 


END ; ***************** End of SOE_PP_REQUIRED_STACKED *****************
