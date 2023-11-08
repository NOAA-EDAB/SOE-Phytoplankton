; $ID:	SOE_EXTRACTS_2LONGFORM.PRO,	2020-12-23-10,	USER-KJWH	$
  PRO SOE_EXTRACTS_2LONGFORM, VERSION_STRUCT, FILES=FILES, DIR_DATA=DIR_DATA, DIR_OUT=DIR_OUT

;+
; NAME:
;   SOE_EXTRACTS_2LONGFORM
;
; PURPOSE:
;   Convert output from SUBAREA_EXTRACTS to the SOE longform format
;
; CATEGORY:
;   SOE Project
;
; CALLING SEQUENCE:
;   SOE_EXTRACTS_2LONGFORM, VERSION
;
; REQUIRED INPUTS:
;   VERSION......... The version for this specific report
;
; OPTIONAL INPUTS:
;   FILES........... The filename(s) of the subarea extracts (the default is to search for files based on the version)
;   DIR_OUT......... The location of the output file
;   
; KEYWORD PARAMETERS:
;   VERBOSE......... Print out steps
;
; OUTPUTS:
;   OUTPUT.......... Output .csv files that can be submitted for the SOE report
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
;   This program was written on December 23, 2020 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Dec 23, 2020 - KJWH: Initial code written
;   Dec 06, 2022 - KJWH: Added a step to remove any blank rows
;                        Removed the steps to indicate the GSTATS for specific products since 
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_EXTRACTS_2LONGFORM'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF NONE(VERSION_STRUCT)  THEN MESSAGE, 'ERROR: Must provide the SOE VERSION structure'
  VERSTR = VERSION_STRUCT
  
  IF NONE(DIR_DATA) THEN DIR_EXTRACTS = VERSTR.DIRS.DIR_EXTRACTS ELSE DIR_EXTRACTS = DIR_DATA
  IF NONE(DIR_OUT) THEN DIR_OUT = DIR_EXTRACTS + 'SOE_FORMAT' + SL & DIR_TEST, DIR_OUT
  IF NONE(FILES) THEN FILES = FILE_SEARCH(DIR_EXTRACTS + STRMID(VERSTR.INFO.DATERANGE[0],0,4) + '*_' + STRMID(VERSTR.INFO.DATERANGE[1],0,4) +'*.SAV')

  MERGED = []
  FOR F=0, N_ELEMENTS(FILES)-1 DO BEGIN
    FP = FILE_PARSE(FILES[F])
    SOEFILE = DIR_OUT + FP.NAME + '-SOE_FORMAT.csv'                                       ; Output file name
    IF FILE_MAKE(FILES[F],SOEFILE,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE                 ; Check if the file needs to be created
    DAT = IDL_RESTORE(FILES[F])                                                           ; Read the input file
    DAT = DAT[WHERE(DAT.NAME NE '',/NULL)]                                                ; Remove any blank entries
    NCINFO = NETCDF_INFO(DAT.DIR+DAT.NAME+'.SAV')                                         ; Get the netcdf information based on the file name
    
    ; ===> Create a new structure from the input data      
    STR = STRUCT_MERGE(STRUCT_COPY(DAT,TAGNAMES=['NAME','PERIOD','REGION','SUBAREA','MATH','EXTRACT_TAG']),REPLICATE(STRUCT_2MISSINGS(CREATE_STRUCT('VARIABLE','','VALUE',0.0,'NOTES','')),N_ELEMENTS(DAT)))
    BP = WHERE_SETS(DAT.PERIOD_CODE)                                                      ; Group by period code
    FOR A=0, N_ELEMENTS(BP)-1 DO BEGIN                                                    ; Loop on periods
      SUBS = WHERE_SETS_SUBS(BP[A])                                                       ; Get the subscripts for each period
      CASE BP[A].VALUE OF                                                                 ; Create the "PERIOD" name
        'A':      PERIOD = 'ANNUAL'               
        'M':      PERIOD = 'MONTHLY'              
        'W':      PERIOD = 'WEEKLY'               
        'D8':     PERIOD = '8 DAY'                
        'ANNUAL': PERIOD = 'LONGTERM_ANNUAL' 
        'MONTH':  PERIOD = 'CLIMATOLOGICAL_MONTH'  
        'WEEK':   PERIOD = 'CLIMATOLOGICAL_WEEK'   
        'DOY':    PERIOD = 'CLIMATOLOGICAL_DAY_OF_YEAR'    
      ENDCASE
      STR[SUBS].VARIABLE = PERIOD + '_' + DAT[SUBS].PROD                                  ; Make the variable name based on the period and the product
    ENDFOR

;; ===> Add math specific information to the structure           
;      BP = WHERE_SETS(DAT.PROD)                                                             ; Group based on product
;      MATHS = REPLICATE('',N_ELEMENTS(DAT))                                                 ; Create a blank MATH array
;      FOR A=0, N_ELEMENTS(BP)-1 DO BEGIN                                                    ; Loop through products
;        SUBS = WHERE_SETS_SUBS(BP[A])                                                       ; Get the subscripts of each product
;        CASE BP[A].VALUE OF                                                                 ; Determine the math type based on the product
;          'CHLOR_A': MATH = 'GSTATS'
;          'PAR': MATH = 'STATS'
;          'SST': MATH = 'STATS'
;          'PPD': MATH = 'GSTATS'
;          'MICRO': MATH = 'GSTATS'
;          'NANO': MATH = 'GSTATS'
;          'PICO': MATH = 'GSTATS'
;          'MICRO_PERCENTAGE': MATH = 'STATS'
;          'NANO_PERCENTAGE': MATH = 'STATS'
;          'PICO_PERCENTAGE': MATH = 'STATS'
;        ENDCASE                                                                             ; Will need to add products if not found in the CASE statement
;        MATHS[SUBS] = MATH                                                                  ; Fill in the blank array with the stat information
;      ENDFOR
;      OK = WHERE(MATHS EQ 'GSTATS' AND DAT.MATH EQ 'STATS',COUNT)                           ; Find where the math type in the structure is STATS (i.e. not an ANOM) and the prod math type is GSTATS
;      IF COUNT GT 1 THEN STR[OK].MATH = 'GSTATS'                                            ; Change the math in the structure to GSTATS
;     
;      IF HAS(TAG_NAMES(DAT),'GSTATS_N') THEN BEGIN                                          ; Loook for any GSTATS in the structure
;        OK = WHERE(DAT.N GT 0 AND DAT.GSTATS_N GT 0, COUNT)                                 ; Find entries that have both arithmetic and geometric stats
;        IF COUNT GT 0 THEN STR[OK].MATH = 'GSTATS'                                          ; Change MATH to GSTATS if the GEO stats are to be used
;      ENDIF
        BP = WHERE_SETS(STR.MATH+'_'+STR.EXTRACT_TAG)                                         ; Group by the MATH and EXTRACT_STAT
        FOR A=0, N_ELEMENTS(BP)-1 DO BEGIN                                                    ; Loop on MATH types
          SUBS = WHERE_SETS_SUBS(BP[A])                                                       ; Get the subscripts for each math group
          CASE BP[A].VALUE OF                                                                 ; Get info for each math type
            'STATS':              BEGIN & MTH = 'MEDIAN'             & TAG = 'MED'          & NOTE = 'Spatial median of the temporal mean' & END
            'STACKED_STATS_MEAN': BEGIN & MTH = 'MEDIAN'             & TAG = 'MED'          & NOTE = 'Spatial median of the temporal mean' & END
            'GSTATS':        BEGIN & MTH = 'MEDIAN'             & TAG = 'GSTATS_MED'   & NOTE = 'Spatial median of the temporal geometric mean' & END
            'ANOM_RATIO': BEGIN & MTH = 'RATIO_ANOMALY'      & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly ratio' & END
            'DIF':           BEGIN & MTH = 'DIFFERENCE_ANOMALY' & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly difference' & END
            'ANOM_DIF':   BEGIN & MTH = 'DIFFERENCE_ANOMALY' & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly difference' & END
          ENDCASE
          STR[SUBS].VARIABLE = STR[SUBS].VARIABLE + '_' + MTH                                 ; Add the math information to the VARIABLE
          STR[SUBS].NOTES = NOTE                                                              ; Add a note about the math information
          TP = WHERE(TAG_NAMES(DAT) EQ TAG,/NULL)                                             ; Find the tag for the specific tag information
          STR[SUBS].VALUE = DAT[SUBS].(TP)                                                    ; Add the data values to the structure
        ENDFOR
      
    ; ===> Create the final output structure
    MERGED = STRUCT_MERGE($
             STRUCT_COPY(STR,TAGNAMES=['NAME','VARIABLE','PERIOD','MATH','REGION','SUBAREA']),$
             STRUCT_COPY(NCINFO,TAGNAMES=['PROD','UNITS']),$
             STRUCT_COPY(STR,TAGNAMES=['VALUE','NOTES']),$
             STRUCT_COPY(NCINFO,TAGNAMES=['PROD_NAME','ALG_NAME','TIME_START','TIME_END','DURATION','FILE']),$
             STRUCT_COPY(NCINFO,TAGNAMES=['ALG_REFERENCE','SENSOR','SOURCE_DATA_VERSION','SOURCE_DATA_URL','SOURCE_DATA_DOI','TITLE','CONTRIBUTOR_NAME']))
    OK = WHERE(STRUPCASE(MERGED.NAME+'.SAV') NE STRUPCASE(MERGED.FILE),COUNT)
    IF COUNT GT 0 THEN MESSAGE, 'ERROR merging the data and netcdf info structures.'
    MERGED = STRUCT_COPY(MERGED, TAGNAMES='NAME',/REMOVE)
    MERGED.PROD = MERGED.PROD + '-' + NCINFO.ALG
    MERGED.CONTRIBUTOR_NAME = 'Kimberly Hyde | kimberly.hyde@noaa.gov | NEFSC'
    MERGED.ALG_REFERENCE = REPLACE(MERGED.ALG_REFERENCE,', ','_')
    IF HAS(MERGED.ALG_REFERENCE,',') THEN MESSAGE, 'ERROR: Commas found in the structure'
    MERGED = STRUCT_RENAME(MERGED,['PROD_NAME',        'ALG_NAME', 'FILE',           'ALG_REFERENCE'],$
                                  ['PRODUCT LONG NAME','ALGORITHM','INPUT FILE NAME','ALGORITHM REFERENCE'])

    MERGED = STRUCT_SORT(MERGED,TAGNAMES=['MATH','VARIABLE','REGION'])
    PFILE, SOEFILE
    STRUCT_2CSV, SOEFILE, MERGED


  ENDFOR ; FILES
END ; ***************** End of SOE_EXTRACTS_2LONGFORM *****************
