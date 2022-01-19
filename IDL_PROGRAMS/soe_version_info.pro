; $ID:	SOE_VERSION_INFO.PRO,	2020-12-31-10,	USER-KJWH	$
  FUNCTION SOE_VERSION_INFO, VERSION

;+
; NAME:
;   SOE_VERSION_INFO
;
; PURPOSE:
;   Get the version specific information for the various SOE functions
;
; CATEGORY:
;   SOE Project
;
; CALLING SEQUENCE:
;   Result = SOE_VERSION_INFO()
;
; REQUIRED INPUTS:
;   None.......... If the VERSION is not provided, the function will return a nested structure with information from all available verions
;
; OPTIONAL INPUTS:
;   VERSION....... The name of the SOE version
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   OUTPUT.......... A structure containing version specific information 
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
;   This program was written on December 31, 2020 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Dec 31, 2020 - KJWH: Initial code written
;   Nov 30, 2021 - KJWH: Added V2022 info
;                        Added the EXTRACT_PERIODS to the INFO structure
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_VERSION_INFO'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF NONE(VERSION) THEN VERSION = ['V2022']                       ; Each year, add the next version

  VSTR = []                                                       ; Create a null variable for the version structure  
; ===> Loop throug the version  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]

; ===> Make the project directories    
    DIR_VER = !S.READ_EDAB_SOE_PHYTOPLANKTON + VER + SL
    DNAME = 'DIR_'  + ['EXTRACTS','PP_REQUIRED','COMPARE','MOVIE','NETCDF','COMP','PLOTS','EPU_MAPS','SST']                        ; The tag name for the directory in the structure
    DIRS  = DIR_VER + ['DATA_EXTRACTS','PPREQ_EXTRACTS','COMPARE_DATA','MOVIES','NETCDF','COMPOSITES','PLOTS','EPU_MAPS','SST'] + SL  ; The actual directory name
    DIR_TEST, DIRS                                                                                  ; Make the output directories if they don't already exist
    DSTR = CREATE_STRUCT('DIR_VERSION',DIR_VER)                                                     ; Create the directory structure
    FOR D=0, N_ELEMENTS(DIRS)-1 DO DSTR=CREATE_STRUCT(DSTR,DNAME[D],DIRS[D])                        ; Add each directory to the structure

; ===> Get the VERSION specific product information    
    CASE VER OF 
      'V2022': BEGIN                                                                                ; V2022 specific information
        SOE_YR = '2021'                                                                             ; The last year of the SOE data
        DATERANGE = ['1998',SOE_YR]                                                                 ; The first and last year of the SOE time series
        TEMP_DATERANGE = ['20210501','20211231']                                                    ; The date range for the "temporary" data for the end of the time series
        MAP_IN   = 'L3B2'                                                                           ; The map for the input data
        TEMP_MAP = 'L3B4'
        MAP_OUT  = 'NES'                                                                            ; The map to be used for any plots
        SHPFILE  = 'NES_EPU_NOESTUARIES'                                                            ; The shapefile for any data extractions or image outlines
        SUBAREAS = ['GOM','GB','MAB']                                                               ; The subareas for data extraction
        EXTRACT_PRODS = ['CHLOR_A','PPD','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
        EXTRACT_PERIODS = ['W','WEEK','M','MONTH','A']
        PPREQ_PRODS = ['PPD-VGPM2','CHLOR_A-CCI']
        PPREQ_PERIODS = ['A','M']
        PPREQ_SHPFILE = ['NES_EPU_STATISTICAL_AREAS','NES_EPU_STATISTICAL_AREAS_NOEST']
        PRODS = ['CHLOR_A','PPD','SST','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
        TEMP_PRODS = ['CHLOR_A','PPD','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
        STACKED_PRODS = LIST(['CHLOR_A','PPD'],['CHLOR_A','MICRO','PPD'],['CHLOR_A','MICRO_PERCENTAGE','PPD'],['MICRO','NANO','PICO'],['MICRO','NANO','PICO']+'_PERCENTAGE')
        COMPOSITE_PRODS = LIST([['MICRO','NANO','PICO']],[['MICRO','NANO','PICO']+'_PERCENTAGE'])
        COMPOSITE_PERIODS = ['ANNUAL','MONTH','WEEK','W','M'] 
        MOVIE_PERIODS = ['WEEK','MONTH']
        CHL_DATASET = 'OCCCI' & CHL_TEMP = 'GLOBCOLOUR' & CHL_ALG = 'CCI' & CTEMP_ALG = 'GSM'
        PP_DATASET  = 'OCCCI' & PP_TEMP  = 'GLOBCOLOUR' & PP_ALG  = 'VGPM2'
        PSZ_DATASET = 'OCCCI' & PSZ_TEMP = 'GLOBCOLOUR' & PSZ_ALG = 'TURNER'
        OCCCI_VERSION = '5.0'
        SST_DATASET = 'MUR' & SST_TEMP = ''
        DATFILE = DSTR.DIR_EXTRACTS + VER + '-' + SHPFILE + '-COMPILED_DATA_FILE.SAV'
        GRID_PERIOD = 'W'
      END
      
      'V2021': BEGIN                                                                                ; V2021 specific information
        SOE_YR = '2020'                                                                             ; The last year of the SOE data
        DATERANGE = ['1998','2020']                                                                 ; The first and last year of the SOE time series
        TEMP_DATERANGE = ['20200701','20201231']                                                    ; The date range for the "temporary" data for the end of the time series
        MAP_OUT='NES'                                                                               ; The map to be used for any plots
        SHPFILE='NES_EPU_NOESTUARIES'                                                               ; The shapefile for any data extractions or image outlines
        SUBAREAS=['GOM','GB','MAB']
        EXTRACT_PRODS = ['CHLOR_A','PPD','PHYSIZE']
        PPREQ_PRODS = ['PPD-VGPM2','CHLOR_A-CCI']
        PPREQ_PERIODS = ['A','M']
        PPREQ_SHPFILE = ['NES_EPU_STATISTICAL_AREAS','NES_EPU_STATISTICAL_AREAS_NOEST']
        PRODS = ['CHLOR_A','PPD','SST','MICRO','NANO','PICO',['MICRO','NANO','PICO']+'_PERCENTAGE']
        TEMP_PRODS = ['CHLOR_A','PPD']
        CHL_DATASET = 'OCCCI' & CHL_TEMP = 'MODISA'
        PP_DATASET = 'OCCCI'  & PP_TEMP  = 'MODISA'
        PSZ_DATASET = 'OCCCI' & PSZ_TEMP = ''
        OCCCI_VERSION = '5.0'
        SST_DATASET = 'MUR' & SST_TEMP = ''
        CHL_ALG = 'CCI' & CTEMP_ALG = 'OCI'
        PP_ALG = 'VGPM2'
        PSZ_ALG = 'BREWINSST_NES'
        DATFILE = DSTR.DIR_EXTRACTS + VER + '-' + SHPFILE + '-COMPILED_DATA_FILE.SAV'
        GRID_PERIOD = 'W'
      END  
     
    ENDCASE
    
    SHPS = READ_SHPFILE(SHPFILE, MAPP=MAP_OUT)
    OUTLINE = []
    FOR F=0, N_ELEMENTS(SUBAREAS)-1 DO BEGIN
      POS = WHERE(TAG_NAMES(SHPS) EQ STRUPCASE(SUBAREAS[F]),/NULL)
      OUTLINE = [OUTLINE,SHPS.(POS).OUTLINE]
    ENDFOR
    
    SUBTITLES = REPLICATE('',N_ELEMENTS(SUBAREAS))
    FOR N=0, N_ELEMENTS(SUBAREAS)-1 DO BEGIN
      CASE SUBAREAS[N] OF
        'GOM': SUBTITLES[N] = 'Gulf of Maine'
        'GB':  SUBTITLES[N] = 'Georges Bank'
        'MAB': SUBTITLES[N] = 'Mid-Atlantic Bight'
        'SS':  SUBTITLES[N] = 'Scotian Shelf'
      ENDCASE
    ENDFOR  
    
    ISTR = CREATE_STRUCT('SOE_YEAR',SOE_YR,'DATERANGE',DATERANGE,'MAP_IN',MAP_IN,'MAP_OUT',MAP_OUT, 'DATAFILE',DATFILE,'SHAPEFILE',SHPFILE, $
                         'TEMP_DATERANGE',TEMP_DATERANGE,'TEMP_PRODS',TEMP_PRODS,'TEMP_MAP',TEMP_MAP,'STACKED_PRODS',STACKED_PRODS,'SUBAREA_NAMES',SUBAREAS,'SUBAREA_TITLES',SUBTITLES,'SUBAREA_OUTLINE',OUTLINE,$
                         'COMPOSITE_PRODS',COMPOSITE_PRODS,'COMPOSITE_PERIODS',COMPOSITE_PERIODS,'MOVIE_PERIODS',MOVIE_PERIODS,'EXTRACT_PRODS',EXTRACT_PRODS,'EXTRACT_PERIODS',EXTRACT_PERIODS,'GRID_PERIOD',GRID_PERIOD,'PPREQ_PRODS',PPREQ_PRODS,'PPREQ_PERIODS',PPREQ_PERIODS,'PPREQ_SHPFILE',PPREQ_SHPFILE)
    
    PSTR = []
    FOR P=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      SPROD = PRODS[P]
      CASE SPROD OF
        'CHLOR_A':          BEGIN & DTSET=CHL_DATASET & TPSET=CHL_TEMP & SPROD=SPROD+'-'+CHL_ALG & TPROD=PRODS[P]+'-'+CTEMP_ALG & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.3_3'   & GPAL='PAL_DEFAULT' & ASCALE='RATIO'    & IMSCALE='CHLOR_A_0.1_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'PPD':              BEGIN & DTSET=PP_DATASET  & TPSET=PP_TEMP  & SPROD=SPROD+'-'+PP_ALG  & TPROD=SPROD                  & PTAG='MED'   & PSCALE='PPD_0.1_10'     & PAL='PAL_NAVY_GOLD'  & GSCALE='PPD_0.1_2'       & GPAL='PAL_DEFAULT' & ASCALE='RATIO'    & IMSCALE='PPD_0.1_10'     & APAL='PAL_BLUEGREEN_ORANGE' & END
        'SST':              BEGIN & DTSET=SST_DATASET & TPSET=SST_TEMP & SPROD=SPROD             & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='SST_0_30'       & PAL='PAL_BLUE_RED'   & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='SST_0_30'       & APAL='PAL_ANOM_BWR'    & END
        'MICRO_PERCENTAGE': BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'NANO_PERCENTAGE':  BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'PICO_PERCENTAGE':  BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='AMEAN' & PSCALE='NUM_0.0_1.0'    & PAL='PAL_DEFAULT'    & GSCALE=PSCALE            & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='NUM_0_0.8' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'MICRO':            BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'NANO':             BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
        'PICO':             BEGIN & DTSET=PSZ_DATASET & TPSET=PSZ_TEMP & SPROD=SPROD+'-'+PSZ_ALG & TPROD=SPROD                  & PTAG='MED'   & PSCALE='CHLOR_A_0.1_30' & PAL='PAL_NAVY_GOLD'  & GSCALE='CHLOR_A_0.03_3'  & GPAL='PAL_DEFAULT' & ASCALE='DIF_-5_5' & IMSCALE='CHLOR_A_0.01_10' & APAL='PAL_BLUEGREEN_ORANGE' & END
      ENDCASE
      STR = CREATE_STRUCT('DATASET',DTSET,'TEMP_DATASET',TPSET,'PROD',SPROD,'TEMP_PROD',TPROD,'PLOT_TAG',PTAG,'PROD_SCALE',PSCALE,'PAL',PAL,'GRID_SCALE',GSCALE,'GRID_PAL',GPAL,'IMAGE_SCALE',IMSCALE,'ANOM_SCALE',ASCALE,'ANOM_PAL',APAL)
      IF DTSET EQ 'OCCCI' THEN STR = CREATE_STRUCT(STR,'VERSION',OCCCI_VERSION) ELSE STR = CREATE_STRUCT(STR,'VERSION','')
      PSTR = CREATE_STRUCT(PSTR,PRODS[P],STR)
    ENDFOR
    STR = CREATE_STRUCT('INFO',ISTR,'DIRS',DSTR,'PROD_INFO',PSTR)
    IF N_ELEMENTS(VERSION) EQ 1 THEN RETURN, STR
    VSTR = CREATE_STRUCT(VSTR,VER,STR)
  ENDFOR
  
  RETURN, VSTR


END ; ***************** End of SOE_VERSION_INFO *****************
