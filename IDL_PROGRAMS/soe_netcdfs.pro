; $ID:	SOE_NETCDFS.PRO,	2023-09-19-09,	USER-KJWH	$
  PRO SOE_NETCDFS, VERSION, DIR_CDF=DIR_CDF, MAKE_PNG=MAKE_PNG, PNGMAP=PNGMAP

;+
; NAME:
;   SOE_NETCDFS
;
; PURPOSE:
;   Make netcdfs for the State of the Ecosystem
;
; CATEGORY:
;   $CATEGORY$
;
; CALLING SEQUENCE:
;   SOE_NETCDFS,$Parameter1$, $Parameter2$, $Keyword=Keyword$, ....
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
;   This program was written on September 01, 2020 by Kimberly J. W. Hyde, Northeast Fisheries Science Center | NOAA Fisheries | U.S. Department of Commerce, 28 Tarzwell Dr, Narragansett, RI 02882
;    
; MODIFICATION HISTORY:
;   Sep 01, 2020 - KJWH: Initial code written
;   Nov 29, 2021 - KJWH: Added V2022 information
;                        Added VSTR = SOE_VERSION_INFO(VER) to get the version details instead of manually updating them
;                        Removed the APROD CASE block - now using info from VSTR
;-
; ****************************************************************************************************
  ROUTINE_NAME = 'SOE_NETCDFS'
  COMPILE_OPT IDL2
  SL = PATH_SEP()
  
  FOR V=0, N_ELEMENTS(VERSION)-1 DO BEGIN
    VER = VERSION[V]
    VSTR = SOE_VERSION_INFO(VER)

    CASE VER OF
      'V2021': BEGIN & PRODS=['CHLOR_A-CCI','PAR','PPD-VGPM2'] & PERIODS=['M','M3'] & OMAP='NESGRID4' & PMAP='NES' & END
      'V2022': BEGIN & PRODS=['CHLOR_A-CCI','PAR','PPD-VGPM2'] & PERIODS=['M','M3'] & IMAP='L3B2' & OMAP='NESGRID2' & PMAP='NES' & END
    ENDCASE
    
    FOR A=0, N_ELEMENTS(PRODS)-1 DO BEGIN
      APROD = PRODS[A]
      APOS = WHERE(TAG_NAMES(VSTR.PROD_INFO) EQ VALIDS('PRODS',APROD),COUNT)
      IF COUNT EQ 0 THEN MESSAGE, 'ERROR: ' + APROD + ' not found in the "VERSION" structure (VSTR)'
      ASTR = VSTR.PROD_INFO.(APOS)
      DATASETS = ASTR.DATASET
      
      FOR D=0, N_ELEMENTS(DATASETS)-1 DO BEGIN
        DSET = DATASETS[D]
        DIR_CDF = VSTR.DIRS.DIR_NETCDF + DSET + SL + APROD + SL & DIR_TEST, DIR_CDF
        DIR_PNG = REPLACE(DIR_CDF, 'NETCDF','PNG') & DIR_TEST, DIR_PNG
        
        FILES = GET_FILES(DSET, PRODS=APROD, MAPS=IMAP, PERIODS=PERIODS, VERSION=ASTR.VERSION)
        IF FILES EQ [] THEN CONTINUE
        IF KEYWORD_SET(MAKE_PNG) THEN BEGIN
          DIR_TEST, DIR_PNG
          PRODS_2PNG, FILES, PROD=ASTR.PROD_SCALE, PAL=ASTR.PAL, MAPP=PMAP, /ADD_CB, ADD_TEXT='PERIOD'
        ENDIF
        
        WRITE_NETCDF, FILES, DIR_OUT=DIR_CDF, MAP_OUT=OMAP

        
      ENDFOR ; DSET
      
    ENDFOR ; PRODS
  ENDFOR  ; VERSION


END ; End of SOE_NETCDFS
