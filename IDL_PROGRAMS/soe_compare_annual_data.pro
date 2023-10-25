; $ID:	SOE_COMPARE_ANNUAL_DATA.PRO,	2023-09-19-09,	USER-KJWH	$
  FUNCTION SOE_COMPARE_ANNUAL_DATA, VERSION, DATFILE=DATFILE, DIR_DATA=DIR_DATA, DIR_PLOTS=DIR_PLOTS, COMPARE_SENSOR=COMPARE_SENSOR, OVERWRITE=OVERWRITE

;+
; NAME:
;   SOE_COMPARE_ANNUAL_DATA
;
; PURPOSE:
;   $PURPOSE$
;
; CATEGORY:
;   $CATEGORY$
;
; CALLING SEQUENCE:
;   Result = SOE_COMPARE_ANNUAL_DATA($Parameter1$, $Parameter2$, $Keyword=Keyword$, ...)
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
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_COMPARE_ANNUAL_DATA'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF NONE(VERSION)   THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VERSTR = SOE_VERSION_INFO(VER)
    
    CASE VER OF
      'V2021': BEGIN & DATASETS='OCCCI' & PRODS=['CHLOR_A-CCI','PPD-VGPM2'] & OMAP='NES' & DR = ['1998','2020'] & END
    ENDCASE
    
    FOR D=0, N_ELEMENTS(DATASETS)-1 DO BEGIN
      DATASET = DATASETS[D]
    
      IF NONE(DIR_DATA) THEN DIR_DATA = VERSTR.DIRS.DIR_EXTRACTS
      IF NONE(DIR_PLOTS) THEN DIR_PLOTS = VERSTR.DIRS.DIR_COMPOSITES + 'ANNUAL_COMPARE' + SL + DATASET + SL & DIR_TEST, DIR_PLOTS
      IF NONE(DATFILE) THEN DATFILE = VERSTR.INFO.DATAFILE
      IF NONE(COMPARE_SENSOR) THEN COMPARE_SENSOR = 'SAVJ'
      MATHS = ['STATS','RATIO']
      PERIODS = ['A']
      TXT_TAGS = [];['PERIOD','MATH']
      BUFFER = 0
  
      FILES = GET_FILES(DATASET, PRODS=PRODS, PERIODS=PERIODS)
      FA = PARSE_IT(FILES,/ALL)
  
  
      STRUCT = IDL_RESTORE(DATFILE)
      OK_RATIO = WHERE_STRING(STRUCT.MATH,'RATIO',COUNT)
      IF COUNT GT 0 THEN BEGIN
        STRUCT[OK_RATIO].MATH = 'RATIO'
        STRUCT[OK_RATIO].PROD = STRUCT[OK_RATIO].PROD + '_RATIO'
      ENDIF
        
      MSTR = STRUCT[WHERE(STRUCT.PERIOD_CODE EQ 'M',/NULL)]
      ASTR = STRUCT[WHERE(STRUCT.PERIOD_CODE EQ 'A',/NULL)]
      FOR N=0, N_ELEMENTS(YEARS)-1 DO BEGIN
        YR = YEARS(N)
        IF YR EQ '1997' OR YR GT VERSTR.INFO.SOE_YEAR THEN CONTINUE
  
        FOR P=0, N_ELEMENTS(PRODS)-1 DO BEGIN
          APROD = PRODS(P)
          AALG = VALIDS('ALGS',APROD)
    
          FOR S=0, N_ELEMENTS(SENSORS)-1 DO BEGIN
            SENSOR = SENSORS(S)
    
            FOR M=0, N_ELEMENTS(MATHS)-1 DO BEGIN
            AMATH = MATHS(M)
  
            CASE VALIDS('PRODS',PRODS(P)) OF
              'CHLOR_A': BEGIN
                IF AMATH EQ 'RATIO' THEN BEGIN
                  SPROD  = 'CHLOR_A_RATIO'
                  TITLE = 'CHL Ratio Anomaly'
                  SSCL  = 'RATIO'
                  SPAL  = 'PAL_ANOM_GREY'
                  APAL  = 'PAL_ANOM_GREY'
                  MRG   = [0.5,2.0]
                  YRG   = [0.65,1.5]
                  MTKS  = [0.5,0.75,1,1.5,2.0]
                  YTKS  = [0.65,0.8,1,1.2,1.5]
                  MYMJR = 5
                  YYMJR = 5
                  LOGS  = 1
                  TAG   = 'AMEAN'
                  PTAG  = 'ANOMALY'
                  ASENSOR = SENSOR + '_' + COMPARE_SENSOR
                ENDIF ELSE BEGIN
                  SPROD = 'CHLOR_A'
                  SSCL  = 'CHLOR_A_0.1_30'
                  SPAL  = 'PAL_DEFAULT'
                  APAL  = 'PAL_ANOM_GREY'
                  TITLE = '$Chlorophyll \ita\rm$' + ' ' + UNITS('CHLOR_A',/NO_NAME)
                  MRG   = [0.0,2.0]
                  YRG   = [0.4,1.2]
                  MTKS  = []
                  YTKS  = []
                  MYMJR = 5
                  YYMJR = 5
                  LOGS  = 0
                  TAG   = 'GSTATS_MED' ; Subareas extraction tag for plot
                  PTAG  = 'GMEAN' ; PRODS_2PNG tag
                  ASENSOR = SENSOR
                ENDELSE
              END
              'PPD': BEGIN
                IF AMATH EQ 'RATIO' THEN BEGIN
                  SSCL  = 'RATIO'
                  SPROD = 'PPD_RATIO'
                  TITLE = 'PP Ratio Anomaly'
                  SPAL  = 'PAL_ANOM_GREY'
                  APAL  = 'PAL_ANOM_GREY'
                  MRG   = [0.5,2.0]
                  YRG   = [0.8,1.25]
                  MTKS  = [0.5,0.75,1.0,1.5,2.0]
                  YTKS  = [0.8,0.9,1.0,1.111,1.25]
                  MYMJR = 5
                  YYMJR = 5
                  LOGS  = 1
                  TAG   = 'AMEAN'
                  PTAG  = 'ANOMALY'
                  ASENSOR = SENSOR + '_' + COMPARE_SENSOR
                ENDIF ELSE BEGIN
                  SPROD = 'PPD'
                  SSCL  = 'PPD_0.1_10'
                  TITLE = 'Primary Production '    + UNITS('PPD',/NO_NAME)
                  SPAL  = 'PAL_DEFAULT'
                  APAL  = 'PAL_ANOM_GREY'
                  MRG   = [0.0,1.6]
                  YRG   = [0.4,1.0]
                  MTKS  = []
                  YTKS  = []
                  MYMJR = 4
                  YYMJR = 4
                  LOGS  = 0
                  TAG   = 'GSTATS_MED' ; Subareas extraction tag for plot
                  PTAG  = 'GMEAN' ; PRODS_2PNG tag
                  ASENSOR = SENSOR
                ENDELSE
              END
            ENDCASE
  
  
  
            PERIOD = 'A_'+YR
            SS = AFILES[WHERE(FA.PERIOD EQ PERIOD AND FA.PROD EQ SPROD AND FA.ALG EQ VALIDS('ALGS',APROD) AND FA.MATH EQ AMATH AND FA.SENSOR EQ ASENSOR,/NULL,COUNT_SS)]
            OS = AFILES[WHERE(FA.PERIOD EQ PERIOD AND FA.PROD EQ SPROD AND FA.ALG EQ VALIDS('ALGS',APROD) AND FA.MATH EQ AMATH AND FA.SENSOR EQ COMPARE_SENSOR,/NULL,COUNT_OS)]
  
            IF COUNT_SS EQ 0 THEN CONTINUE
            PNGFILE = DIR_PNG + 'A_' + YR + '-' + SENSOR + '_vs_' + COMPARE_SENSOR + '-' + SPROD + '-' + AMATH + '_COMPOSITE.PNG'
            IF FILE_MAKE([SS,OS,DATFILE],PNGFILE) EQ 0 THEN CONTINUE
  
            NROW = 3
            NCOL = 3
            IMGPIX = 300.
            HEIGHT = IMGPIX*NROW
            WIDTH  = IMGPIX*NCOL
  
            EDGE = 0.02
            WSP = 0.05
            HSP = 0.05
            TEDGE = 0.04
            BEDGE = 0.01
            CB   = 0.005
            CBS  = 0.015
  
            TWSP = WSP*(NCOL-1) + EDGE*2
            THSP = TEDGE + BEDGE + HSP*(NROW-1) + CBS*NROW + CB*NROW
            WDIF = (1-TWSP)/NCOL
            LF = [] & RT = [] & TP = [] & BT = []
            FOR L=0, NCOL-1 DO LF = [LF,EDGE+WSP*L+WDIF*L]
            RT = LF + WDIF
            HDIF = WIDTH*WDIF/HEIGHT
            FOR T=0, NROW-1 DO TP = [TP,1-TEDGE-(HSP*T)-HDIF*T-CBS*T-CB*T]
            BT = TP - HDIF
  
            ADD_CB = 1
            CB_TYPE = 3
            CB_FONT = 12
            CB_RELATIVE = 0
  
            W = WINDOW(DIMENSIONS=[WIDTH,HEIGHT],BUFFER=BUFFER)
            T = TEXT(0.5,0.975,YR,ALIGN=0.5,VERTICAL_ALIGN=0.5,FONT_SIZE=20,FONT_STYLE='BOLD')
            IF SS NE [] THEN PRODS_2PNG,SS,MAPP=OMAP,PROD=SSCL,TAG=PTAG,OUTLINE=EPU_OUTLINE,OUT_COLOR=0,OUT_THICK=4,CB_TITLE=TITLE,TXT_TAGS=SENSOR, TXT_POS=[LF[0]+(WSP/2),TP[0]-(HSP*2)],VERBOSE=VERBOSE,/CURRENT,IMG_POS=[LF[0],BT[0],RT[0],TP[0]],/ADD_CB,CB_POS=[LF[0],BT[0]-CBS,RT[0],BT[0]-CB],CB_TYPE=CB_TYPE,CB_RELATIVE=CB_RELATIVE,PAL=SPAL
            IF OS NE [] THEN PRODS_2PNG,OS,MAPP=OMAP,PROD=SSCL,TAG=PTAG,OUTLINE=EPU_OUTLINE,OUT_COLOR=0,OUT_THICK=4,CB_TITLE=TITLE,TXT_TAGS=COMPARE_SENSOR,TXT_POS=[LF[0]+(WSP/2),TP[0]-(HSP*2)],VERBOSE=VERBOSE,/CURRENT,IMG_POS=[LF[1],BT[0],RT[1],TP[0]],/ADD_CB,CB_POS=[LF[1],BT[0]-CBS,RT[1],BT[0]-CB],CB_TYPE=CB_TYPE,CB_RELATIVE=CB_RELATIVE,PAL=SPAL
  
            SM = STRUCT_READ(SS,TAG=PTAG,MAP_OUT=OMAP)
            OC = STRUCT_READ(OS,TAG=PTAG,MAP_OUT=OMAP)
            IF AMATH EQ 'STATS' THEN ANOM = SM/OC ELSE ANOM = SM-OC
            IF AMATH EQ 'STATS' THEN ASCL = 'RATIO_.8_1.25' ELSE ASCL = 'DIF_-1_1'
            IF AMATH EQ 'STATS' THEN ATITLE = SENSOR+':'+COMPARE_SENSOR+' Ratio' ELSE ATITLE = SENSOR+'-'+COMPARE_SENSOR+' Difference'
            BYT = PRODS_2BYTE(ANOM,MP=OMAP,PROD=ASCL,/ADD_LAND,/ADD_COAST)
            IMG = IMAGE(BYT, RGB_TABLE=READ_PAL(APAL),POSITION=[LF(2),BT[0],RT(2),TP[0]], MARGIN=0, /CURRENT, BUFFER=BUFFER)
            CBAR, ASCL, IMG=IMG, FONT_SIZE=10, CB_TYPE=CB_TYPE, CB_POS=[LF(2),BT[0]-CBS,RT(2),BT[0]-CB], CB_TITLE=ATITLE, PAL=APAL, RELATIVE=CB_RELATIVE
            TXT = TEXT(LF(2)+0.01,TP[0]-0.03,SENSOR+':'+COMPARE_SENSOR,/NORMAL,FONT_SIZE=10,FONT_STYLE='BOLD',TARGET=IMG)
  
  
            CLRS  = ['BLUE','CYAN','RED','SPRING_GREEN']
            EPUS  = ['GOM','GB','MAB']
            THICK = 3
            AX = DATE_AXIS([210001,210012],/MONTH,/FYEAR,STEP=1,ROOM=1)
            AX.TICKNAME[0] = '' & AX.TICKNAME[-1] = ''
  
            SENS = [ASENSOR,COMPARE_SENSOR]
            FOR R=0, N_ELEMENTS(EPUS)-1 DO BEGIN
              FOR E=0, N_ELEMENTS(SENS) -1 DO BEGIN
                POSITION=[LF(R)+.015,BT[1]+.02,RT(R),TP[1]+.02]
                STR = MSTR[WHERE(MSTR.PROD EQ SPROD AND MSTR.ALG EQ AALG AND MSTR.SUBAREA EQ EPUS(R) AND DATE_2YEAR(PERIOD_2DATE(MSTR.PERIOD)) EQ YR AND MSTR.SENSOR EQ SENS(E) AND MSTR.MATH EQ AMATH,/NULL,COUNTM)] & IF COUNTM GT 12 THEN STOP
                MDATE = DATE_2JD('2100'+DATE_2MONTH(PERIOD_2DATE(STR.PERIOD)))
                RDATA = GET_TAG(STR,TAG)
                LDATA = LOWESS(DATE_2MONTH(PERIOD_2DATE(STR.PERIOD)),RDATA,WIDTH=7)
                ;IF HAS(PRODS(P),'RATIO') THEN YTICKS = MTKS(P) ELSE YTICKS = []
                P0 = PLOT(MDATE,RDATA,YLOG=LOGS,/NODATA,/CURRENT,POSITION=POSITION,OVERPLOT=S,XRANGE=AX.JD,YRANGE=MRG,XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=0,XSTYLE=1,YMAJOR=MYMJR,YTICKV=MTKS)
                XRANGE = P0.XRANGE
                IF AMATH EQ 'RATIO' THEN PL = PLOT(XRANGE,[1,1],/OVERPLOT,COLOR='BLACK',THICK=3,TRANSPARENCY=90)
                P1 = PLOT(MDATE,RDATA,YLOG=LOGS,COLOR=CLRS(E),/CURRENT,POSITION=POSITION,/OVERPLOT,THICK=THICK,LINESTYLE=6,SYM_SIZE=0.25,SYMBOL='CIRCLE',SYM_FILLED=1);,XRANGE=AX.JD,YRANGE=[MR1(P),MR2(P)],XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=0,XSTYLE=1)
                P2 = PLOT(MDATE,LDATA,YLOG=LOGS,COLOR=CLRS(E),/CURRENT,POSITION=POSITION,/OVERPLOT,THICK=THICK);XRANGE=AX.JD,YRANGE=[MR1(P),MR2(P)],XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=0,XSTYLE=1)
                IF E EQ 0 THEN T = TEXT(POSITION(2)-0.01,POSITION(3)-0.03,EPUS(R),TARGET=P0,/NORMAL,FONT_SIZE=10,ALIGNMENT=1)
                T = TEXT(POSITION[0]+0.01,POSITION(3)-0.03-(S*0.015),SENS(E),COLOR=CLRS(E),TARGET=P0,/NORMAL,FONT_SIZE=10)
              ENDFOR
            ENDFOR
  
            AX = DATE_AXIS([YEARS[0],YEARS(-1)],/YEAR,/YY_YEAR,STEP=2,ROOM=2)
            AX.TICKNAME[0] = '' & AX.TICKNAME[-1] = ''
            FOR R=0, N_ELEMENTS(EPUS)-1 DO BEGIN
              FOR E=0, N_ELEMENTS(SENS) -1 DO BEGIN
                POSITION=[LF(R)+.015,BT(2)+.07,RT(R),TP(2)+.07]
                IF SENS(E) EQ COMPARE_SENSOR THEN STR = ASTR[WHERE(ASTR.PROD EQ SPROD AND ASTR.ALG EQ AALG AND ASTR.SUBAREA EQ EPUS(R) AND ASTR.SENSOR EQ COMPARE_SENSOR AND ASTR.MATH EQ AMATH,/NULL)] ELSE $
                  STR = ASTR[WHERE(ASTR.PROD EQ SPROD AND ASTR.ALG EQ AALG AND ASTR.SUBAREA EQ EPUS(R) AND ASTR.SENSOR NE COMPARE_SENSOR AND ASTR.MATH EQ AMATH,/NULL)]
  
                STR = ASTR[WHERE(ASTR.PROD EQ SPROD AND ASTR.ALG EQ AALG AND ASTR.SUBAREA EQ EPUS(R) AND ASTR.SENSOR EQ SENS(E) AND ASTR.MATH EQ AMATH,/NULL)]
  
                YST = STR[WHERE(STR.PERIOD EQ PERIOD,/NULL)]
                RDATA = GET_TAG(STR,TAG)
                LDATA = LOWESS(DATE_2YEAR(PERIOD_2DATE(STR.PERIOD)),RDATA,WIDTH=7)
                ; IF HAS(PRODS(P),'RATIO') THEN YTICKS = YTKS(P) ELSE YTICKS = []
                P0 = PLOT(MDATE,RDATA,YLOG=LOGS,/NODATA,/CURRENT,POSITION=POSITION,OVERPLOT=S,XRANGE=AX.JD,YRANGE=YRG,XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=0,XSTYLE=1,YMAJOR=YYMJR,YTICKV=YTKS,YSTYLE=1)
                XRANGE = P0.XRANGE
                PL = PLOT(PERIOD_2JD([YST.PERIOD,YST.PERIOD]),YRG,COLOR='YELLOW',THICK=THICK*1.5,/OVERPLOT,TRANSPARENCY=90)
                IF AMATH EQ 'RATIO' THEN PL = PLOT(XRANGE,[1,1],/OVERPLOT,COLOR='BLACK',THICK=3,TRANSPARENCY=90)
                P1 = PLOT(PERIOD_2JD(STR.PERIOD),RDATA,YLOG=LOGS,COLOR=CLRS(E),/CURRENT,POSITION=POSITION,/OVERPLOT,THICK=THICK,LINESTYLE=6,SYM_SIZE=0.25,SYMBOL='CIRCLE',SYM_FILLED=1);,XRANGE=AX.JD,YRANGE=[YR1(P),YR2(P)],XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=1,XSTYLE=1)
                P2 = PLOT(PERIOD_2JD(STR.PERIOD),LDATA,YLOG=LOGS,COLOR=CLRS(E),/CURRENT,POSITION=POSITION,/OVERPLOT,THICK=THICK);,XRANGE=AX.JD,YRANGE=[YR1(P),YR2(P)],XTICKNAME=AX.TICKNAME,XTICKVALUES=AX.TICKV,XMINOR=1,XSTYLE=1)
                IF E EQ 0 THEN T = TEXT(POSITION(2)-0.01,POSITION(3)-0.03,EPUS(R),TARGET=P0,/NORMAL,FONT_SIZE=10,ALIGNMENT=1)
                T = TEXT(POSITION[0]+0.01,POSITION(3)-0.03-(S*0.015),SENS(E),COLOR=CLRS(E),TARGET=P0,/NORMAL,FONT_SIZE=10)
              ENDFOR
            ENDFOR
  
            W.SAVE, PNGFILE
            W.CLOSE
            PFILE, PNGFILE
            ENDFOR ; MATHS
          ENDFOR ; SENSORS
        ENDFOR ; PRODS
      ENDFOR ; YEARS
    ENDFOR ; DATASETS
  ENDFOR ; VERSIONS  
  


END ; ***************** End of SOE_COMPARE_ANNUAL_DATA *****************
