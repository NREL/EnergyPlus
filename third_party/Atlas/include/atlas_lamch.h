#ifndef ATLAS_LAMCH_H
   #define ATLAS_LAMCH_H
#if defined(SREAL) || defined(SCPLX)
   #include "atlas_slamch.h"
   #ifndef ATLAS_LAMCH_TYPE_ALIAS
      #define ATLAS_LAMCH_TYPE_ALIAS
      #define ATL_laROUND          ATL_slaROUND
      #define ATL_laSAFMIN         ATL_slaSAFMIN
      #define ATL_laOVERTHRESH     ATL_slaOVERTHRESH
      #define ATL_laUNDERTHRESH    ATL_slaUNDERTHRESH
      #define ATL_laMAXEXP         ATL_slaMAXEXP
      #define ATL_laMINEXP         ATL_slaMINEXP
      #define ATL_laMANTDIG        ATL_slaMANTDIG
      #define ATL_laPRECISION      ATL_slaPRECISION
      #define ATL_laBASE           ATL_slaBASE
      #define ATL_laEPSILON        ATL_slaEPSILON
      #define ATL_labadUNDERTHRESH ATL_slaUNDERTHRESH
      #define ATL_labadOVERTHRESH ATL_slaOVERTHRESH
   #endif
#else
   #include "atlas_dlamch.h"
   #ifndef ATLAS_LAMCH_TYPE_ALIAS
      #define ATLAS_LAMCH_TYPE_ALIAS
      #define ATL_laROUND          ATL_dlaROUND
      #define ATL_laSAFMIN         ATL_dlaSAFMIN
      #define ATL_laOVERTHRESH     ATL_dlaOVERTHRESH
      #define ATL_laUNDERTHRESH    ATL_dlaUNDERTHRESH
      #define ATL_laMAXEXP         ATL_dlaMAXEXP
      #define ATL_laMINEXP         ATL_dlaMINEXP
      #define ATL_laMANTDIG        ATL_dlaMANTDIG
      #define ATL_laPRECISION      ATL_dlaPRECISION
      #define ATL_laBASE           ATL_dlaBASE
      #define ATL_laEPSILON        ATL_dlaEPSILON
      #define ATL_labadUNDERTHRESH ATL_dlaUNDERTHRESH
      #define ATL_labadOVERTHRESH ATL_dlaOVERTHRESH
   #endif
#endif

#endif
