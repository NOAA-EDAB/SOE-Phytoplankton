; $ID:	SOE_MAKE_MOVIES.PRO,	2023-09-19-09,	USER-KJWH	$
  FUNCTION SOE_MAKE_MOVIES, VERSION, DIR_MOVIE=DIR_MOVIE, OVERWRITE=OVERWRITE

;+
; NAME:
;   SOE_MAKE_MOVIES
;
; PURPOSE:
;   $PURPOSE$
;
; CATEGORY:
;   $CATEGORY$
;
; CALLING SEQUENCE:
;   Result = SOE_MAKE_MOVIES($Parameter1$, $Parameter2$, $Keyword=Keyword$, ...)
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
  ROUTINE_NAME = 'SOE_MAKE_MOVIES'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  IF NONE(VERSION)  THEN MESSAGE, 'ERROR: Must provide the SOE VERSION'  
  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    IF NONE(DIR_MOVIE) THEN DIR_MOVIE = !S.SOE + VER + SL + 'MOVIES' + SL & DIR_TEST, DIR_MOVIE
    CASE VER OF
      'V2021': BEGIN & PRODS=['SST'] & OMAP='NES' & DR = ['1998_2020'] & SOE_YR='2020' & END
    ENDCASE
    
    
    FOR P=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      APROD = PRODS
      PERIOD = 'D'
      CASE APROD OF
        'SST': BEGIN
          CASE VER OF 
            'V2021': BEGIN & DATASET = 'MUR' & DR=SOE_YR & END
          ENDCASE  
          SPROD = 'SST_0_30'
          PAL = 'PAL_BLUE_RED'
        END
        'CHLOR_A': BEGIN
          CASE VER OF
            'V2021': BEGIN & DATASET = 'OCCCI' & APROD = 'CHLOR_A-CCI' & DR=['2019','2020'] & END
          ENDCASE
          SPROD = 'CHLOR_A_0.1_30'
          PAL = 'PAL_DEFAULT'
        END    
      ENDCASE
      
      IF ~KEYWORD_SET(COMBO) THEN BEGIN
        DIR_PNGS = DIR_MOVIE + DATASET + SL + APROD + SL & DIR_TEST, DIR_PNGS
        F = GET_FILES(DATASET,PRODS=APROD,PERIOD=PERIOD,DATERANGE=DR)
        PRODS_2PNG, F, PROD=SPROD, MAPP=OMAP, /ADD_CB, DIR_OUT=DIR_MOVIE, BUFFER=1, /ADD_DB, CB_TYPE=3, PAL=PAL
        PNGFILES = FLS(DIR_PNGS+'*.PNG',DATERANGE=DR)
        FPS = 15
        MOVIE_FILE = DIR_MOVIE + STRJOIN(DR,'_') + APROD + '-' + DATATSET + '-' + OMAP + '-FPS_'+ROUNDS(FPS)+'.webm'
        IF FILE_MAKE(PNGFILES,MOVIE_FILE,OVERWRITE=OVERWRITE) THEN MOVIE, PNGFILES, MOVIE_FILE=MOVIE_FILE, FPS=FPS
      ENDIF ELSE BEGIN
        STOP ; Need to set up steps to make a combo movie (e.g. the phytoplankton size classes)
        
      ENDELSE
    ENDFOR ; PRODS
  ENDFOR ; VERSION    



END ; ***************** End of SOE_MAKE_MOVIES *****************
