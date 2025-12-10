; $ID:	SOE_COMPARE_VERSIONS.PRO,	2023-11-08-14,	USER-KJWH	$
  PRO SOE_COMPARE_VERSIONS, YEARS=YEARS, PRODS=PRODS

;+
; NAME:
;   SOE_COMPARE_VERSIONS
;
; PURPOSE:
;   $PURPOSE$
;
; PROJECT:
;   SOE_PHYTOPLANKTON
;
; CALLING SEQUENCE:
;   SOE_COMPARE_VERSIONS,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
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
;   OUTPUT.......... Describe the output of this program or function
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
;   This program was written on November 08, 2023 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Nov 08, 2023 - KJWH: Initial code written
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_COMPARE_VERSIONS'
  COMPILE_OPT IDL3
  SL = PATH_SEP()

  DP = DATE_PARSE(DATE_NOW())
  VERYEAR = 'V'+NUM2STR(DP.YEAR+1)
  VERSTR = PROJECT_VERSION_DEFAULT('SOE_PHYTOPLANKTON',VERSION=VERYEAR)
  
  PRDS = ['CHLOR_A','PPD']
  OUTPUT_FILES = ['PP_REQUIRED','SOE_MAIN']
  EPUS = ['GOM','GB','MAB']
  
  
  FOR R=0, N_ELEMENTS(PRDS)-1 DO BEGIN
    APROD = PRDS[R]
    CASE APROD OF 
      'CHLOR_A': YRG = [0.6,1.4]
      'PPD': YRG = [0.4,1.0]
    ENDCASE

    FOR F=0, N_ELEMENTS(OUTPUT_FILES)-1 DO BEGIN      
      CASE OUTPUT_FILES[F] OF
        'PP_REQUIRED': BEGIN
          IF N_ELEMENTS(YEARS) LE 1 THEN YRS = YEAR_RANGE('2021',DP.YEAR+1,/STRING) ELSE YRS = YEARS
          VERDAT = VERSTR.DIRS.DIR_PPREQ_EXTRACTS + 'NES_EPU_STATISTICAL_AREAS_NOEST' + SL + 'FINAL_MERGED_SUMS' + SL + 'MERGED_ANNUAL_SUM-NES_EPU_STATISTICAL_AREAS_NOEST-PPD-VGPM2_CHLOR_A-CCI-STATS-' + VERYEAR + '.SAV'
          TYPES = 'PP_REQUIRED'
          PERIODS = 'A'
          CLRS  = ['BLUE','RED','CYAN','MAGENTA','BLACK']
          THICK = 4
          CASE APROD OF
            'CHLOR_A': PLOT_STAT = 'ANNUAL_MEAN'
            'PPD': PLOT_STAT = 'ANNUAL_MEAN'
          ENDCASE
          
          ; ===> Create a concatenated data file
          FDAT = []
          FILES = []
          FOR Y=0, N_ELEMENTS(YRS)-1 DO BEGIN
            AYR = YRS[Y]
            YDAT = REPLACE(VERDAT,VERYEAR,'V'+AYR)
            IF ~FILE_TEST(YDAT) THEN MESSAGE, 'ERROR: ' + YDAT + ' does not exist.'
            FILES = [FILES,YDAT]
            DAT = IDL_RESTORE(YDAT)
            DAT = DAT[WHERE(DAT.PROD EQ APROD,/NULL)]
            DAT = STRUCT_COPY(DAT,['SENSOR','PROD','ALG','YEAR','SUBAREA_NAME','N_MONTHS','ANNUAL_MEAN','ANNUAL_SUM','ANNUAL_MTON','ANNUAL_TTON'])
            DAT = STRUCT_RENAME(DAT, 'SUBAREA_NAME','SUBAREA')
            DAT = STRUCT_MERGE(REPLICATE(CREATE_STRUCT('SOE_VERSION','V'+AYR,'PERIOD_CODE','A','PERIOD','','FILE_TYPE','PP_REQUIRED'),N_ELEMENTS(DAT)),DAT)
            DAT.PERIOD = DAT.PERIOD_CODE + '_' + DAT.YEAR
            FDAT = [FDAT,DAT]  
          ENDFOR ; YEARS
        END ; PP_REQUIRED
      
        'SOE_MAIN': BEGIN
          VERDAT = VERSTR.INFO.DATAFILE
          IF N_ELEMENTS(YEARS) LE 1 THEN YRS = YEAR_RANGE('2023',DP.YEAR+1,/STRING) ELSE YRS = YEARS
          IF ~N_ELEMENTS(PRODS) THEN PRDS = ['CHLOR_A','PPD','PSC_MICRO','PSC_FMICRO'] ELSE PRDS = PRODS
      
          PERIODS = ['W','M','A']
          TYPES = 'STACKED_' + ['STATS'];,'ANOMS']
          PLOT_STAT = 'GMEAN'
          CLRS  = ['BLUE','RED','CYAN','SPRING_GREEN']
          THICK = 4
          
          ; ===> Create a concatenated data file
          FDAT = []
          FILES = []
          FOR Y=0, N_ELEMENTS(YRS)-1 DO BEGIN
            AYR = YRS[Y]
            YDAT = REPLACE(VERDAT,VERYEAR,'V'+AYR)
            IF ~FILE_TEST(YDAT) THEN MESSAGE, 'ERROR: ' + YDAT + ' does not exist.'
            FILES = [FILES,YDAT]
            DAT = IDL_RESTORE(YDAT)
            DAT = DAT[WHERE(DAT.PROD EQ APROD,/NULL)]
            IF HAS(TAG_NAMES(DAT),'GSTATS') THEN DAT = STRUCT_COPY(DAT,'GSTATS_'+['N','MIN','MAX','MED','AMEAN','GMEAN','STD'],/REMOVE)
            DAT = STRUCT_MERGE(REPLICATE(CREATE_STRUCT('SOE_VERSION','V'+AYR),N_ELEMENTS(DAT)),DAT)
            FDAT = [FDAT,DAT]      
          ENDFOR ; YEARS
        END ; SOE_MAIN  
      ENDCASE  
      
      DIR = REPLACE(VERSTR.DIRS.DIR_COMPARE_DATA,VERYEAR,'V'+MAX(YRS)) + OUTPUT_FILES[F] + SL & DIR_TEST, DIR 
            
      FOR D=0, N_ELEMENTS(PERIODS)-1 DO BEGIN
        FOR T=0, N_ELEMENTS(TYPES)-1 DO BEGIN
          FOR E=0, N_ELEMENTS(EPUS)-1 DO BEGIN
            
            PNGFILE = DIR + PERIODS[D] + '_' + MIN(YRS) + '_' + MAX(YRS) + '-' + EPUS[E] + '-' + APROD + '-' + TYPES[T] + '-COMPARE_VERSION.png'
            IF FILE_MAKE(FILES,PNGFILE,OVERWRITE=OVERWRITE) EQ 0 THEN CONTINUE 
            ADAT = FDAT[WHERE(FDAT.PERIOD_CODE EQ PERIODS[D] AND FDAT.FILE_TYPE EQ TYPES[T] AND FDAT.SUBAREA EQ EPUS[E],/NULL)]
            IF ADAT EQ [] THEN MESSAGE, 'Check data subsetting'
            
            SETS = WHERE_SETS(ADAT.SOE_VERSION)
            
            AX = DATE_AXIS(['1998',DP.YEAR],/YEAR,/YY_YEAR,STEP=2,ROOM=2)
            AX.TICKNAME[0] = '' & AX.TICKNAME[-1] = ''
                                  
            FOR S=0, N_ELEMENTS(SETS)-1 DO BEGIN
              CASE OUTPUT_FILES[F] OF
                'SOE_MAIN': THCK = THICK-S*2
                'PP_REQUIRED': THCK = THICK
              ENDCASE
              ASET = ADAT[WHERE_SETS_SUBS(SETS[S])]
              RDATA = GET_TAG(ASET,PLOT_STAT)
              MDATE = DATE_2JD(['19980101',DP.YEAR+'1231'])
              IF S EQ 0 THEN P0 = PLOT(MDATE,[MIN(RDATA),MAX(RDATA)],DIMENSIONS=[1200,600],/NODATA,POSITION=POSITION,XRANGE=AX.JD,YRANGE=YRG,XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=0,XSTYLE=1,YMAJOR=YYMJR,YTICKV=YTKS,YSTYLE=1,YTITLE=UNITS(APROD),TITLE=EPUS[E])
              POS=P0.POSITION
              PL = PLOT(PERIOD_2JD(ASET.PERIOD),RDATA,COLOR=CLRS[S],THICK=THck,/OVERPLOT)
              TXT = SYMBOL(POS[0]+0.06,POS[3]-0.06-(S*0.05),SYM_TEXT=SETS[S].VALUE,SYM_COLOR=CLRS[S],SYM_THICK=THCK,TARGET=P0,/NORMAL,FONT_SIZE=18)     
            ENDFOR ; SETS
            
            P0.SAVE, PNGFILE
            P0.CLOSE
            
          ENDFOR ; EPU
        ENDFOR ; TYPES
      ENDFOR ; PERIODS
    ENDFOR ; OUTPUT FILES   
  ENDFOR ; PRODS  

END ; ***************** End of SOE_COMPARE_VERSIONS *****************
