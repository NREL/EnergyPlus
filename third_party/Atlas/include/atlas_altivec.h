#ifndef ATLAS_ALTIVEC_H
   #define ATLAS_ALTIVEC_H

#ifdef ATL_AltiVec
/*
 * Always use gcc rather than apple's altivec, since apple is no longer
 * supporting PowerPCs
 */
   #ifndef ATL_AVgcc
      #define ATL_AVgcc
   #endif
   #ifdef ATL_AVgcc
      #include <altivec.h>

      #define VECTOR_INIT(v0_,v1_,v2_,v3_) (vector float) {v0_,v1_,v2_,v3_}
      #define VECTOR_INITI(v0_,v1_,v2_,v3_) (vector int) {v0_,v1_,v2_,v3_}
   #else
      #define VECTOR_INIT(v0_,v1_,v2_,v3_) (vector float)(v0_,v1_,v2_,v3_)
      #define VECTOR_INITI(v0_,v1_,v2_,v3_) (vector int)(v0_,v1_,v2_,v3_)
      #define VECTOR_INITL(v0_,v1_,v2_,v3_) (vector long)(v0_,v1_,v2_,v3_)
   #endif
   #define ATL_GetCtrl(stride, count, size) \
      (int)((stride) | ((count)<<16) | ((size)<<24))
   #define ATL_pfavR(ptr, cwrd, stream) \
      vec_dst((vector float *)(ptr), (cwrd), (stream))
   #define ATL_pfavW(ptr, cwrd, stream) \
      vec_dstst((vector float *)(ptr), (cwrd), (stream))
#else
   #define ATL_GetCtrl(stride, count, size)
   #define ATL_pfavR(ptr, cwrd, stream)
   #define ATL_pfavW(ptr, cwrd, stream)
#endif

#endif
