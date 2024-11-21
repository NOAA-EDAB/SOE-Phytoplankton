; $ID:	SOE_EXTRACTS_2LONGFORM.PRO,	2020-12-23-10,	USER-KJWH	$
  PRO SOE_EXTRACTS_2LONGFORM, VERSION_STRUCT, FILES=FILES, DIR_DATA=DIR_DATA, DIR_OUT=DIR_OUT, PPREQUIRED=PPREQUIRED

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
;   Jan 23, 2024 - KJWH: Updated the PPREQUIRED steps to include the mean CHL data
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_EXTRACTS_2LONGFORM'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF ~N_ELEMENTS(VERSION_STRUCT)  THEN MESSAGE, 'ERROR: Must provide the SOE VERSION structure'
  VERSTR = VERSION_STRUCT
  
  IF ~N_ELEMENTS(DIR_DATA) THEN DIR_EXTRACTS = VERSTR.DIRS.DIR_DATA_EXTRACTS ELSE DIR_EXTRACTS = DIR_DATA
  IF ~N_ELEMENTS(DIR_OUT) THEN DIR_OUT = DIR_EXTRACTS + 'SOE_FORMAT' + SL & DIR_TEST, DIR_OUT
  IF ~N_ELEMENTS(FILES) THEN FILES = FILE_SEARCH(DIR_EXTRACTS + STRMID(VERSTR.INFO.DATERANGE[0],0,4) + '*_' + STRMID(VERSTR.INFO.DATERANGE[1],0,4) +'*.SAV')

  IF KEYWORD_SET(PPREQUIRED) THEN FILES = FILE_SEARCH(VERSTR.DIRS.DIR_PPREQ_EXTRACTS + TAG_NAMES(VERSTR.SHAPEFILES) + SL + 'FINAL_MERGED_SUMS' + SL + 'MERGED_ANNUAL*.SAV')

  FPALL = PARSE_IT(FILES,/ALL)

  MERGED = []
  ALLFILE = DIR_OUT + STRJOIN(FPALL.PROD,'_') + '-SOE_FORMAT.csv' 
  ALLDATA = []
  FOR F=0, N_ELEMENTS(FILES)-1 DO BEGIN
    FP = FPALL[F]
    SOEFILE = DIR_OUT + FP.NAME + '-SOE_FORMAT.csv'                                       ; Output file name
    IF FILE_MAKE(FILES[F],[SOEFILE,ALLFILE],OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE                 ; Check if the file needs to be created
    DAT = IDL_RESTORE(FILES[F])                                                           ; Read the input file

    IF KEYWORD_SET(PPREQUIRED) THEN BEGIN
      ;SOEFILE = REPLACE(SOEFILE,'CHLOR_A-CCI-','')
      IF FILE_MAKE(FILES[F],SOEFILE,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE                 ; Check if the file needs to be created
      PRDS = ['PPD','CHLOR_A']
            
      FOR R=0, N_ELEMENTS(PRDS)-1 DO BEGIN
        PDAT = DAT[WHERE(DAT.PROD EQ PRDS[R],/NULL)]
        DATTAGS = TAG_NAMES(PDAT)
        STR = STRUCT_MERGE(STRUCT_COPY(PDAT,TAGNAMES=['NAME','PERIOD','REGION','SUBAREA','TOTAL_PIXEL_AREA_KM2','N_MONTHS']),REPLICATE(STRUCT_2MISSINGS(CREATE_STRUCT('VARIABLE','','VALUE',0.0,'UNITS','','NOTES','')),N_ELEMENTS(PDAT)))

        CASE PRDS[R] OF 
          'CHLOR_A': PVARS = ['ANNUAL_MEAN'] 
          'PPD':     PVARS = ['ANNUAL_MEAN','ANNUAL_MTON']
        ENDCASE
        PUNIT = (PRODS_READ(PRDS[R])).UNITS
        FOR V=0, N_ELEMENTS(PVARS)-1 DO BEGIN
          PSTR = STR
          PVAR = PVARS[V]
          POS = WHERE(DATTAGS EQ PVAR)
          
          CASE PVAR OF
            'TOTAL_PIXEL_AREA_KM2': BEGIN                         & UNIT='km^2'                  & INPUT_PROD='Pixel area' & ALG_NAME='' & NOTE='The total area of the EPU' & END
            'N_MONTHS':             BEGIN                         & UNIT=''                      & INPUT_PROD='' & ALG_NAME='' & NOTE='The number of months used in the annual sum' & END
            'ANNUAL_MEAN' :         BEGIN & PVAR=PRDS[R]+'_'+PVAR & UNIT=PUNIT                   & NOTE='The mean of the monthly spatial means' & END
            'ANNUAL_MTON':          BEGIN & PVAR=PRDS[R]+'_'+PVAR & UNIT='Metric ton Carbon/EPU' & NOTE='Annual sum of primary production within the EPU' & END 
          ENDCASE
          PSTR.VARIABLE = PVAR
          PSTR.UNITS = UNIT
          PSTR.VALUE = PDAT.(POS)
          PSTR.NOTES = NOTE
          
          OSTR = STRUCT_MERGE($
            STRUCT_COPY(PSTR,TAGNAMES=['PERIOD','REGION','SUBAREA']),$
         ;   STRUCT_COPY(DAT, TAGNAMES=['PROD']),$
            STRUCT_COPY(PSTR,TAGNAMES=['VARIABLE','VALUE','UNITS','NOTES']),$
            STRUCT_COPY(PDAT, TAGNAMES=['PROD_NAME','ALG_NAME','TIME_START','TIME_END','DURATION']),$
            STRUCT_COPY(PDAT, TAGNAMES=['ALG_REFERENCE','SENSOR','SOURCE_DATA_VERSION','SOURCE_DATA_URL','SOURCE_DATA_DOI','CONTRIBUTOR_NAME']))
          OSTR = STRUCT_RENAME(OSTR,'PROD_NAME','INPUT_PRODUCT')
          
          B = WHERE_SETS(OSTR.PERIOD+'_'+OSTR.SUBAREA+'_'+OSTR.VARIABLE+'_'+NUM2STR(OSTR.VALUE))
          OSTR = OSTR[B.FIRST]
          
          CASE PVAR OF
            'TOTAL_PIXEL_AREA_KM2': BEGIN & OSTR.INPUT_PRODUCT='Pixel area' & OSTR.ALG_NAME='' & END
            'N_MONTHS':             BEGIN & OSTR.INPUT_PRODUCT=''           & OSTR.ALG_NAME='' & END
            ELSE:
          ENDCASE
          
          IF MERGED EQ [] THEN MERGED = OSTR ELSE MERGED = STRUCT_CONCAT(MERGED,OSTR)
          
        ENDFOR ; VARS
      ENDFOR ; PRDS  

    ENDIF ELSE BEGIN
  
      DAT = DAT[WHERE(DAT.NAME NE '',/NULL)]                                                ; Remove any blank entries
      IF STRUCT_HAS(DAT,'DIR') THEN NCINFO = NETCDF_INFO(DAT.DIR+DAT.NAME+'.SAV') $
                               ELSE NCINFO = NETCDF_INFO(DAT.NAME+'.SAV')                   ; Get the netcdf information based on the file name

  
      ; ===> Create a new structure from the input data
      STR = STRUCT_MERGE(STRUCT_COPY(DAT,TAGNAMES=['NAME','PERIOD','REGION','SUBAREA','MATH','EXTRACT_TAG']),REPLICATE(STRUCT_2MISSINGS(CREATE_STRUCT('VARIABLE','','VALUE',0.0,'NOTES','')),N_ELEMENTS(DAT)))
      BP = WHERE_SETS(DAT.PERIOD_CODE)                                                      ; Group by period code
      FOR A=0, N_ELEMENTS(BP)-1 DO BEGIN                                                    ; Loop on periods
        SUBS = WHERE_SETS_SUBS(BP[A])                                                       ; Get the subscripts for each period
        CASE BP[A].VALUE OF                                                                 ; Create the "PERIOD" name
          'A':      PERIOD = 'ANNUAL'
          'M':      PERIOD = 'MONTHLY'
          'MM':     PERIOD = 'MONTHLY'
          'W':      PERIOD = 'WEEKLY'
          'D8':     PERIOD = '8 DAY'
          'ANNUAL': PERIOD = 'LONGTERM_ANNUAL'
          'MONTH':  PERIOD = 'CLIMATOLOGICAL_MONTH'
          'WEEK':   PERIOD = 'CLIMATOLOGICAL_WEEK'
          'DOY':    PERIOD = 'CLIMATOLOGICAL_DAY_OF_YEAR'
        ENDCASE
        STR[SUBS].VARIABLE = PERIOD + '_' + DAT[SUBS].PROD                                  ; Make the variable name based on the period and the product
      ENDFOR
  
      BP = WHERE_SETS(STR.MATH+'_'+STR.EXTRACT_TAG)                                         ; Group by the MATH and EXTRACT_STAT
      FOR A=0, N_ELEMENTS(BP)-1 DO BEGIN                                                    ; Loop on MATH types
        SUBS = WHERE_SETS_SUBS(BP[A])                                                       ; Get the subscripts for each math group
        CASE BP[A].VALUE OF                                                                 ; Get info for each math type
          'STATS':              BEGIN & MTH = 'MEDIAN'             & TAG = 'MED'          & NOTE = 'Spatial median of the temporal mean' & END
          'STACKED_STATS_MEAN': BEGIN & MTH = 'MEDIAN'             & TAG = 'MED'          & NOTE = 'Spatial median of the temporal mean' & END
          'GSTATS':             BEGIN & MTH = 'MEDIAN'             & TAG = 'GSTATS_MED'   & NOTE = 'Spatial median of the temporal geometric mean' & END
          'ANOM_RATIO':         BEGIN & MTH = 'RATIO_ANOMALY'      & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly ratio' & END
          'DIF':                BEGIN & MTH = 'DIFFERENCE_ANOMALY' & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly difference' & END
          'ANOM_DIF':           BEGIN & MTH = 'DIFFERENCE_ANOMALY' & TAG = 'AMEAN'        & NOTE = 'Spatial mean of the temporal anomaly difference' & END
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
    ENDELSE
    
    MERGED.CONTRIBUTOR_NAME = 'Kimberly Hyde | kimberly.hyde@noaa.gov | NEFSC'
    MERGED.ALG_REFERENCE = REPLACE(MERGED.ALG_REFERENCE,', ','_')
    IF HAS(MERGED.ALG_REFERENCE,',') THEN MESSAGE, 'ERROR: Commas found in the structure'
    MERGED = STRUCT_RENAME(MERGED,['PROD_NAME',        'ALG_NAME', 'FILE',           'ALG_REFERENCE'],$
                                  ['PRODUCT LONG NAME','ALGORITHM','INPUT FILE NAME','ALGORITHM REFERENCE'])

    MERGED = STRUCT_SORT(MERGED,TAGNAMES=['MATH','VARIABLE','REGION'])
    PFILE, SOEFILE
    STRUCT_2CSV, SOEFILE, MERGED

    IF ALLDATA EQ [] THEN ALLDATA = MERGED ELSE ALLDATA = [ALLDATA,MERGED]
  ENDFOR ; FILES
  PFILE, ALLFILE
  IF ALLDATA NE [] THEN STRUCT_2CSV, ALLFILE, ALLDATA
END ; ***************** End of SOE_EXTRACTS_2LONGFORM *****************
