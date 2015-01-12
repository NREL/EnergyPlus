#ifndef ATLAS_MV_H
   #define ATLAS_MV_H

#include "atlas_misc.h"
#if defined(SREAL)
   #include "atlas_smvn.h"
   #include "atlas_smvt.h"
#elif defined(DREAL)
   #include "atlas_dmvn.h"
   #include "atlas_dmvt.h"
#elif defined(SCPLX)
   #include "atlas_cmvn.h"
   #include "atlas_cmvt.h"
#elif defined(DCPLX)
   #include "atlas_zmvn.h"
   #include "atlas_zmvt.h"
#endif
#define ATL_GetPartSYMV ATL_GetPartMVN

#endif
