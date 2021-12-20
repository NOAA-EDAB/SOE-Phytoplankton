; $ID:	SOE_NETCDFS.PRO,	2020-09-01-16,	USER-KJWH	$
  PRO SOE_SUBAREA_EXTRACTS, VERSION, PRODUCTS=PRODUCTS, DATASETS=DATASETS, MAP_IN=MAP_IN, SHAPEFILE=SHAPEFILE, SUBAREAS=SUBAREAS, DATERANGE=DATERANGE, $
                            PERIOD=PERIOD, FILETYPE=FILETYPE, OUTSTATS=OUTSTATS, DATFILE=DATFILE, DIR_DATA=DIR_DATA, VERBOSE=VERBOSE

;+
; NAME:
;   SOE_NETCDFS
;
; PURPOSE:
;   Run subarea extracts for the State of the Ecosystem report
;
; CATEGORY:
;   SOE Project
;
; CALLING SEQUENCE:
;   SOE_NETCDFS, VERSION
;
; REQUIRED INPUTS:
;   VERSION.......... The version for this specific report
;
; OPTIONAL INPUTS:
;   PRODS............ The name of the products to extract data from (note, the datasets are currently defaults based on the input product)
;   DATASETS......... The name of the dataset for each product
;   MAP_IN........... The name of the "map" to use when finding the files
;   SHAPEFILE........ The name of the SHPFILE to use for the data extractsion (default = 'NES_EPU_NOESTUARIES
;   SUBAREAS......... The name(s) of the subareas within the shpfile (default is all subareas in the shpfile)
;   PERIOD........... The period of the files to extract
;   FILETYPE......... The "type" of file (STATS, ANOMS)
;   OUTSTATS......... The output statistics from SUBAREAS_EXTRACT
;   DATERANGE........ The daterange of the files
;   DATFILE.......... The name of the merged output data file
;
; KEYWORD PARAMETERS:
;   VERBOSE.......... Sent to SUBAREAS_EXTRACT to print out progress
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
;
;   
; COPYRIGHT: 
; Copyright (C) 2020, Department of Commerce, National Oceanic and Atmospheric Administration, National Marine Fisheries Service,
;   Northeast Fisheries Science Center, Narragansett Laboratory.
;   This software may be used, copied, or redistributed as long as it is not sold and this copyright notice is reproduced on each copy made.
;   This routine is provided AS IS without any express or implied warranties whatsoever.
;
; AUTHOR:
;   This program was written on October 29, 2020 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Oct 29, 2020 - KJWH: Initial code written
;   Dec 23, 3030 - KJWH: Added IF NONE(VERSION)  THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'
;
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_SUBAREA_EXTRACTS'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  
  IF ~N_ELEMENTS(VERSION)   THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'  
  IF ~N_ELEMENTS(FILETYPE)  THEN FILETYPES = ['STATS','ANOMS']  ELSE FILETYPES=FILETYPE
  IF ~N_ELEMENTS(PERIOD)    THEN PERIOD = [] 
    
  PHYSIZE_PRODS = [['MICRO','NANO','PICO']+'_PERCENTAGE','MICRO','NANO','PICO']
  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VERSTR = SOE_VERSION_INFO(VER)
    
    IF ~N_ELEMENTS(DIR_DATA)       THEN DIR_OUT = VERSTR.DIRS.DIR_EXTRACTS    ELSE DIR_OUT = DIR_DATA
    IF ~N_ELEMENTS(PRODUCTS)       THEN PRODS   = VERSTR.INFO.EXTRACT_PRODS   ELSE PRODS   = PRODUCTS  
    IF ~N_ELEMENTS(PERIODS)        THEN DPERS   = VERSTR.INFO.EXTRACT_PERIODS ELSE DPER    = PERIODS
    IF ~N_ELEMENTS(DATERANGE)      THEN DTR     = VERSTR.INFO.DATERANGE       ELSE DTR     = DATERANGE  
    IF ~N_ELEMENTS(SHAPEFILE)      THEN SHPFILE = VERSTR.INFO.SHAPEFILE       ELSE SHPFILE = SHAPEFILE
    IF ~N_ELEMENTS(MAP_IN)         THEN MAPIN   = VERSTR.INFO.MAP_IN          ELSE MAPIN   = MAP_IN
    
    IF ~N_ELEMENTS(SUBAREAS) THEN BEGIN
      IF SHPFILE EQ VERSTR.INFO.SHAPEFILE THEN NAMES = VERSTR.INFO.SUBAREA_NAMES ELSE NAMES = []
    ENDIF ELSE NAMES = SUBAREAS

    TEMP_DR = VERSTR.INFO.TEMP_DATERANGE
    TEMP_PRODS = VERSTR.INFO.TEMP_PRODS
        
    DFILES = []
    FOR A=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      APROD = PRODS[A]
      OK = WHERE(TEMP_PRODS EQ APROD,COUNT) 
      IF COUNT GE 1 THEN TPROD = TEMP_PRODS[OK] ELSE TPROD = []
                        
      CASE APROD OF
        'PHYSIZE': BEGIN & DPRODS=PHYSIZE_PRODS & END
        ELSE:      BEGIN & DPRODS=''            & END
          
      ENDCASE
      
      EFILES = []
      FOR D=0, N_ELEMENTS(DPRODS)-1 DO BEGIN
        DPROD = DPRODS[D]
        IF DPROD EQ '' THEN POK = WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ APROD,/NULL) ELSE POK = WHERE(TAG_NAMES(VERSTR.PROD_INFO) EQ DPROD,/NULL)
        PSTR = VERSTR.PROD_INFO.(POK)
        DPRD = PSTR.PROD
        DSET = PSTR.DATASET
        DVER = PSTR.VERSION & IF DVER NE '' AND ~HAS(DVER,'VERSION') THEN DIRVER='VERSION_'+DVER ELSE DIRVER=''
        DIR_EXTRACT = DIR_OUT + DSET + SL & DIR_TEST, DIR_EXTRACT
        DTR = GET_DATERANGE(DTR)
        FOR F=0, N_ELEMENTS(FILETYPES)-1 DO BEGIN
          ATYPE = FILETYPES[F]
          FILES = GET_FILES(DSET, PRODS=DPRD, PERIODS=DPERS, FILE_TYPE=ATYPE, MAPS=MAPIN, VERSION=DVER, DATERANGE=DTR, COUNT=COUNT)
          IF FILES EQ [] THEN CONTINUE
          SAV = DIR_EXTRACT + ROUTINE_NAME + '-SOE_' +VER + '-' + STRJOIN(DPERS,'_') + '-' + DSET + '-' +DIRVER + '-' + SHPFILE + '-' + MAPIN + '-' + DPRD + '-' + ATYPE + '.SAV'
          SAV = REPLACE(SAV,['--','-.'],['-',''])
          SUBAREAS_EXTRACT, FILES, SHP_NAME=SHPFILE, SUBAREAS=NAMES, VERBOSE=VERBOSE, DIR_OUT=DIR_EXTRACT, STRUCT=STR, SAVEFILE=SAV, OUTPUT_STATS=OUTSTATS, /ADD_DIR
          EFILES = [EFILES,SAV]
        ENDFOR ; FILETYPE
      ENDFOR ; DPRODS  
      
      IF TPROD NE [] THEN BEGIN
        TPERS = ['W','M','A']
        TSET = PSTR.TEMP_DATASET
        TPRD = PSTR.TEMP_PROD
        DVER = ''
        TDTR = TEMP_DR
        DIR_EXTRACT = DIR_OUT + TSET + SL & DIR_TEST, DIR_EXTRACT
        FOR F=0, N_ELEMENTS(FILETYPES)-1 DO BEGIN
          ATYPE = FILETYPES[F]
          FILES = GET_FILES(TSET, PRODS=TPRD, PERIODS=TPERS, FILE_TYPE=ATYPE, MAPS=MAPIN, VERSION=DVER, DATERANGE=TDTR, COUNT=COUNT)
          IF HAS(TPERS,'A') THEN FILES = [FILES,GET_FILES(TSET,PRODS=TPRD,PERIODS='A',MAPS=MAPIN, VERSION=DVER, DATERANGE=[STRMID(TDTR[0],0,4)+'0101',TDTR[1]],COUNT=COUNTA)]
          IF FILES EQ [] THEN CONTINUE
          SAV = DIR_EXTRACT + ROUTINE_NAME + '-SOE_' +VER + '-' + STRJOIN(TPERS,'_') + '-' + TSET + '-' +DVER + '-' + SHPFILE + '-' + MAPIN + '-' + TPRD + '-' + ATYPE + '.SAV'
          SAV = REPLACE(SAV,['--','-.'],['-',''])
          SUBAREAS_EXTRACT, FILES, SHP_NAME=SHPFILE, SUBAREAS=NAMES, VERBOSE=VERBOSE, DIR_OUT=DIR_EXTRACT, STRUCT=STR, SAVEFILE=SAV, OUTPUT_STATS=OUTSTATS, /ADD_DIR
          EFILES = [EFILES,SAV]
        ENDFOR ; FILETYPE
        DATASETS = [DSET,TSET]
      ENDIF ELSE DATASETS = DSET
      
      DATFILE = DIR_OUT + STRJOIN(DTR,'_') + '-' + STRJOIN(DATASETS,'_') + '-' + APROD + '-' + STRJOIN(FILETYPES,'_') + '-' + SHPFILE + '-SOE_' + VER +  '.SAV'
      DFILES = [DFILES,DATFILE]
      IF EFILES NE [] AND FILE_MAKE(EFILES,DATFILE,OVERWRITE=OVERWRITE) EQ 1 THEN BEGIN
        STRUCT = IDL_RESTORE(EFILES[0])
        FOR E=1, N_ELEMENTS(EFILES)-1 DO STRUCT = STRUCT_CONCAT(STRUCT,IDL_RESTORE(EFILES[E]))
        STRUCT = STRUCT_DUPS(STRUCT, TAGNAMES=['PERIOD','SUBAREA','PROD','ALG','MATH','MIN','MAX','MED']) ; Remove any duplicates
        SAVE, STRUCT, FILENAME=DATFILE ; ===> SAVE THE MERGED DATAFILE
        SAVE_2CSV, DATFILE
      ENDIF
    
    ENDFOR ; PRODS 

; ===> Merge the product based DATFILE into a single combined DATAFILE    
    DATAFILE = VERSTR.INFO.DATAFILE
    IF ANY(DIR_DATA) THEN DATAFILE = REPLACE(DATAFILE,VERSTR.DIRS.DIR_EXTRACTS,DIR_OUT)
    IF DFILES NE [] AND FILE_MAKE(DFILES,DATAFILE,OVERWRITE=OVERWRITE) EQ 1 THEN BEGIN
      STRUCT = IDL_RESTORE(DFILES[0])
      FOR T=1, N_ELEMENTS(DFILES)-1 DO STRUCT = STRUCT_CONCAT(STRUCT,IDL_RESTORE(DFILES[T]))
      STRUCT = STRUCT_DUPS(STRUCT, TAGNAMES=['PERIOD','SUBAREA','PROD','ALG','MATH','MIN','MAX','MED'],SUBS=SUBS,DUPS_REMOVED=DUPS_REMOVED) ; Remove any duplicates
      SAVE, STRUCT, FILENAME=DATAFILE ; ===> SAVE THE MERGED DATAFILE
      SAVE_2CSV, DATAFILE
    ENDIF
     
    SOE_EXTRACTS_2LONGFORM, VER, DIR_DATA=DIR_OUT ; Convert the output to the SOE longform format 
  ENDFOR  ; VERSION


END ; End of SOE_NETCDFS
