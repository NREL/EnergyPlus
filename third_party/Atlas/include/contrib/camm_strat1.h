#include "camm_util.h"

#undef p1_4_swap_1
#define p1_4_swap_1(a_) \
      pls(a_,ax,1) \
      pls(a_,cx,0) \
      pus(0,a_,ax) \
      pus(1,a_,cx) 
#undef p1_2_swap_1
#define p1_2_swap_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      px(0) \
      pld(a_,cx,0) \
      pud(0,a_,ax) \
      pud(1,a_,cx) 
#undef p1_swap_1
#define p1_swap_1(a_) \
      plq(a_,ax,1) \
      pl(a_,cx,0) \
      puq(0,a_,ax) \
      pu(1,a_,cx) 
#undef p2_swap_1
#define p2_swap_1(a_) \
      plq(SS(a_,RS4),ax,3) \
      pl(SS(a_,RS4),cx,2) \
      puq(0,a_,ax) \
      pu(1,a_,cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,0) \
      puq(2,SS(a_,RS4),ax) \
      pu(3,SS(a_,RS4),cx) 
#undef lpswap_1
#define lpswap_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,0) 
#undef dpswap_1
#define dpswap_1(a_) \
      plq(SS(a_,RS4),ax,3) \
      pl(SS(a_,RS4),cx,2) \
      puq(0,a_,ax) \
      pu(1,a_,cx) \
      puq(2,SS(a_,RS4),ax) \
      pu(3,SS(a_,RS4),cx) 
#undef plswap_1
#define plswap_1 8


#undef p1_4_scal_3
#define p1_4_scal_3(a_) \
      pls(a_,ax,0) \
      pmsr(6,0) \
      pus(0,a_,ax)
#undef p1_2_scal_3
#define p1_2_scal_3(a_) \
      pld(a_,ax,0) \
      pm(6,0) \
      pud(0,a_,ax)
#undef p1_scal_3
#define p1_scal_3(a_) \
      plq(a_,ax,0) \
      pm(6,0) \
      puq(0,a_,ax)
#undef p2_scal_3
#define p2_scal_3(a_) \
      plq(a_,ax,0) \
      plq(SS(a_,RS4),ax,1) \
      pm(6,0) \
      pm(6,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_scal_3
#define p4_scal_3(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,2) \
      puq(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(6,3) \
      puq(1,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pm(6,0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(6,1) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpscal_3
#define lpscal_3(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pm(6,1) 
#undef dpscal_3
#define dpscal_3(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,2) \
      puq(0,a_,ax) \
      pm(6,3) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plscal_3
#define plscal_3 16

#undef p1_4_scal_3c
#define p1_4_scal_3c(a_) 
#undef p1_2_scal_3c
#define p1_2_scal_3c(a_) \
      pld(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      pud(0,a_,ax)
#undef p1_scal_3c
#define p1_scal_3c(a_) \
      plq(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      puq(0,a_,ax)
#undef p2_scal_3c
#define p2_scal_3c(a_) \
      plq(a_,ax,0) \
      plq(SS(a_,RS4),ax,1) \
      pc(0,2) \
      pm(6,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      puq(0,a_,ax) \
      pc(1,3) \
      pm(6,1) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,1) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_scal_3c
#define p4_scal_3c(a_) \
      pm(7,5) \
      pa(5,1) \
      puq(0,a_,ax) \
      ps(CSHUF,4,4) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pc(3,5) \
      pm(6,3) \
      pm(7,4) \
      pa(4,2) \
      puq(1,SS(a_,RS4),ax) \
      ps(CSHUF,5,5) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pc(0,4) \
      pm(6,0) \
      pm(7,5) \
      pa(5,3) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pc(1,5) \
      pm(6,1) \
      pm(7,4) \
      pa(4,0) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      ps(CSHUF,5,5) \
      plq(SS(a_,MM(7,RS4)),ax,3) \
      pc(2,4) \
      pm(6,2) 
#undef lpscal_3c
#define lpscal_3c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pc(0,4) \
      pm(6,0) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pc(1,5) \
      pm(6,1) \
      pm(7,4) \
      pa(4,0) \
      ps(CSHUF,5,5) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pc(2,4) \
      pm(6,2) 
#undef dpscal_3c
#define dpscal_3c(a_) \
      pm(7,5) \
      pa(5,1) \
      ps(CSHUF,4,4) \
      puq(0,a_,ax) \
      pm(7,4) \
      pa(4,2) \
      pc(3,5) \
      pm(6,3) \
      puq(1,SS(a_,RS4),ax) \
      ps(CSHUF,5,5) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pm(7,5) \
      pa(5,3) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plscal_3c
#define plscal_3c 16

#undef p1_4_scal_4
#define p1_4_scal_4(a_) \
      pls(SS(a_,MM(0,RS4)),ax,0) \
      pmsr(6,0) \
      pus(0,a_,ax) 
#undef p1_2_scal_4
#define p1_2_scal_4(a_) \
      pld(SS(a_,MM(0,RS4)),ax,0) \
      pm(6,0) \
      pud(0,a_,ax) 
#undef p1_scal_4
#define p1_scal_4(a_) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pm(6,0) \
      puq(0,a_,ax) 
#undef p2_scal_4
#define p2_scal_4(a_) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,0) \
      pm(6,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_scal_4
#define p4_scal_4(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,0) \
      pm(6,1) \
      pm(6,2) \
      pm(6,3) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) 
#undef p8_scal_4
#define p8_scal_4(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      plq(SS(a_,MM(4,RS4)),ax,4) \
      plq(SS(a_,MM(5,RS4)),ax,5) \
      plq(SS(a_,MM(6,RS4)),ax,7) \
      pm(6,0) \
      pm(6,1) \
      pm(6,2) \
      puq(0,a_,ax) \
      pm(6,3) \
      pm(6,4) \
      pm(6,5) \
      plq(SS(a_,MM(7,RS4)),ax,0) \
      pm(6,7) \
      pm(6,0) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      puq(4,SS(a_,MM(4,RS4)),ax) \
      puq(5,SS(a_,MM(5,RS4)),ax) \
      puq(7,SS(a_,MM(6,RS4)),ax) \
      puq(0,SS(a_,MM(7,RS4)),ax) 
#undef lpscal_4
#define lpscal_4(a_) 
#undef dpscal_4
#define dpscal_4(a_) p4_scal_4(a_)
#undef plscal_4
#define plscal_4 16

#undef p1_4_scal_4c
#define p1_4_scal_4c(a_) 
#undef p1_2_scal_4c
#define p1_2_scal_4c(a_) \
      pld(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      pud(0,a_,ax)
#undef p1_scal_4c
#define p1_scal_4c(a_) \
      plq(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      puq(0,a_,ax)
#undef p2_scal_4c
#define p2_scal_4c(a_) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pc(0,4) \
      pc(1,5) \
      pm(6,0) \
      pm(6,1) \
      ps(CSHUF,4,4) \
      ps(CSHUF,5,5) \
      pm(7,4) \
      pa(4,0) \
      pm(7,5) \
      pa(5,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_scal_4c
#define p4_scal_4c(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pc(0,4) \
      pc(1,5) \
      pm(6,0) \
      pm(6,1) \
      ps(CSHUF,4,4) \
      ps(CSHUF,5,5) \
      pm(7,4) \
      pa(4,0) \
      pc(2,4) \
      pm(7,5) \
      pa(5,1) \
      pc(3,5) \
      pm(6,2) \
      pm(6,3) \
      ps(CSHUF,4,4) \
      ps(CSHUF,5,5) \
      pm(7,4) \
      pa(4,2) \
      pm(7,5) \
      pa(5,3) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) 
#undef lpscal_4c
#define lpscal_4c(a_) 
#undef dpscal_4c
#define dpscal_4c(a_) p4_scal_4c(a_)
#undef plscal_4c
#define plscal_4c 16

#undef p1_4_scal_1
#define p1_4_scal_1(a_) \
      pls(a_,ax,1) \
      pmsr(0,1) \
      pus(1,a_,ax) 
#undef p1_2_scal_1
#define p1_2_scal_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      pm(0,1) \
      pud(1,a_,ax) 
#undef p1_scal_1
#define p1_scal_1(a_) \
      plq(a_,ax,1) \
      pm(0,1) \
      puq(1,a_,ax) 
#undef p2_scal_1
#define p2_scal_1(a_) \
      plq(a_,ax,1) \
      plq(SS(a_,RS4),ax,2) \
      pm(0,1) \
      pm(0,2) \
      puq(1,a_,ax) \
      puq(2,SS(a_,RS4),ax)
#undef p4_scal_1
#define p4_scal_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pm(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(0,1) \
      puq(3,SS(a_,MM(1,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pm(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pm(0,7) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef lpscal_1
#define lpscal_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,MM(1,RS4)),ax,3) \
      pm(0,7) 
#undef dpscal_1
#define dpscal_1(a_) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pm(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(0,1) \
      puq(3,SS(a_,MM(1,RS4)),ax) \
      pm(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef plscal_1
#define plscal_1 RS4


#undef p1_4_set_1
#define p1_4_set_1(a_) \
      pls(a_,ax,1) \
      pcs(0,1) \
      pus(1,a_,ax) 
#undef p1_2_set_1
#define p1_2_set_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      pc(0,1) \
      pud(1,a_,ax) 
#undef p1_set_1
#define p1_set_1(a_) \
      plq(a_,ax,1) \
      pc(0,1) \
      puq(1,a_,ax) 
#undef p2_set_1
#define p2_set_1(a_) \
      plq(a_,ax,1) \
      plq(SS(a_,RS4),ax,2) \
      pc(0,1) \
      pc(0,2) \
      puq(1,a_,ax) \
      puq(2,SS(a_,RS4),ax)
#undef p4_set_1
#define p4_set_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pc(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pc(0,1) \
      puq(3,SS(a_,MM(1,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pc(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pc(0,7) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef lpset_1
#define lpset_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,MM(1,RS4)),ax,3) \
      pc(0,7) 
#undef dpset_1
#define dpset_1(a_) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pc(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pc(0,1) \
      puq(3,SS(a_,MM(1,RS4)),ax) \
      pc(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef plset_1
#define plset_1 RS4


#undef p1_4_set_2
#define p1_4_set_2(a_) \
      pus(0,a_,ax) 
#undef p1_2_set_2
#define p1_2_set_2(a_) \
      pud(0,a_,ax) 
#undef p1_set_2
#define p1_set_2(a_) \
      puq(0,a_,ax) 
#undef p2_set_2
#define p2_set_2(a_) \
      puq(0,a_,ax) \
      puq(0,SS(a_,RS4),ax)
#undef p4_set_2
#define p4_set_2(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      puq(0,a_,ax) \
      puq(0,SS(a_,MM(1,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      puq(0,SS(a_,MM(2,RS4)),ax) \
      puq(0,SS(a_,MM(3,RS4)),ax) 
#undef lpset_2
#define lpset_2(a_) 
#undef dpset_2
#define dpset_2(a_) \
      puq(0,a_,ax) \
      puq(0,SS(a_,MM(1,RS4)),ax) \
      puq(0,SS(a_,MM(2,RS4)),ax) \
      puq(0,SS(a_,MM(3,RS4)),ax) 
#undef plset_2
#define plset_2 RS4


#undef p1_4_set_3
#define p1_4_set_3(a_) \
      pus(0,a_,ax) 
#undef p1_2_set_3
#define p1_2_set_3(a_) \
      pud(0,a_,ax) 
#undef p1_set_3
#define p1_set_3(a_) \
      puq(0,SS(a_,MM(0,RS4)),ax) 
#undef p2_set_3
#define p2_set_3(a_) \
      puq(0,SS(a_,MM(0,RS4)),ax) \
      puq(0,SS(a_,MM(1,RS4)),ax) 
#undef p4_set_3
#define p4_set_3(a_) \
      puq(0,SS(a_,MM(0,RS4)),ax) \
      puq(0,SS(a_,MM(1,RS4)),ax) \
      puq(0,SS(a_,MM(2,RS4)),ax) \
      puq(0,SS(a_,MM(3,RS4)),ax) 
#undef p8_set_3
#define p8_set_3(a_) \
      puq(0,SS(a_,MM(0,RS4)),ax) \
      puq(0,SS(a_,MM(1,RS4)),ax) \
      puq(0,SS(a_,MM(2,RS4)),ax) \
      puq(0,SS(a_,MM(3,RS4)),ax) \
      puq(0,SS(a_,MM(4,RS4)),ax) \
      puq(0,SS(a_,MM(5,RS4)),ax) \
      puq(0,SS(a_,MM(6,RS4)),ax) \
      puq(0,SS(a_,MM(7,RS4)),ax) 
#undef lpset_3
#define lpset_3(a_) 
#undef dpset_3
#define dpset_3(a_) p8_set_3(a_)
#undef plset_3
#define plset_3 32


#undef p1_4_0x1_nrm2_1
#define p1_4_0x1_nrm2_1(a_) \
      pls(a_,ax,1) \
      pmsr(1,1) \
      pasr(1,0) 
#undef p1_2_0x1_nrm2_1
#define p1_2_0x1_nrm2_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      pm(1,1) \
      pa(1,0) 
#undef p1_0x1_nrm2_1
#define p1_0x1_nrm2_1(a_) \
      plq(a_,ax,1) \
      pm(1,1) \
      pa(1,0) 
#undef p2_0x1_nrm2_1
#define p2_0x1_nrm2_1(a_) \
      plq(a_,ax,1) \
      plq(SS(a_,RS4),ax,2) \
      pm(1,1) \
      pm(2,2) \
      pa(1,0) \
      pm(2,0) 
#undef p4_0x1_nrm2_1
#define p4_0x1_nrm2_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pm(3,3) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(1,1) \
      pa(3,0) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pm(2,2) \
      pa(1,0) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pm(7,7) \
      pa(2,0) 
#undef lp0x1_nrm2_1
#define lp0x1_nrm2_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,MM(1,RS4)),ax,3) \
      pm(7,7) 
#undef dp0x1_nrm2_1
#define dp0x1_nrm2_1(a_) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pm(3,3) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(1,1) \
      pa(3,0) \
      pm(2,2) \
      pa(1,0) \
      pa(2,0) 
#undef pl0x1_nrm2_1
#define pl0x1_nrm2_1 RS4


#undef p1_4_nrm2_2
#define p1_4_nrm2_2(a_) \
      pls(a_,ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pcs(5,6) dbg(6) \
      pcs(5,7) dbg(7) \
      paxs(1,5) dbg(5) \
      prps(5,2) dbg(2) \
      px(3) \
      pcms(0,2,3) dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pasr(3,7) dbg(7) \
      pcs(7,5) dbg(5) \
      pdsr(5,6) dbg(6) \
      pdsr(5,1) dbg(1) \
      pmsr(6,6) dbg(6) \
      pmsr(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pasr(1,0) dbg(0)
#undef p1_2_nrm2_2
#define p1_2_nrm2_2(a_) \
      px(1) pld(a_,ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef p1_nrm2_2
#define p1_nrm2_2(a_) \
      plq(a_,ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#define p2_nrm2_2(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef lpnrm2_2
#define lpnrm2_2(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef dpnrm2_2
#define dpnrm2_2(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      pan(4,1) dbg(1) \
      pc(5,6) dbg(6) \
      pc(5,7) dbg(7) \
      pax(1,5) dbg(5) \
      prp(5,2) dbg(2) \
      px(3) \
      pcm(0,2,3)dbg(3) \
      pan(3,7) dbg(7) \
      pann(5,3) dbg(3) \
      pa(3,7) dbg(7) \
      pc(7,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0) 
#undef plnrm2_2
#define plnrm2_2 8


#undef p1_4_nrm2_3
#define p1_4_nrm2_3(a_) \
      pls(a_,ax,1) dbg(1) \
      pcs(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      paxs(1,5) dbg(5) \
      pdsr(5,6) dbg(6) \
      pdsr(5,1) dbg(1) \
      pmsr(6,6) dbg(6) \
      pmsr(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pasr(1,0) dbg(0)
#undef p1_2_nrm2_3
#define p1_2_nrm2_3(a_) \
      px(1) pld(a_,ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef p1_nrm2_3
#define p1_nrm2_3(a_) \
      plq(a_,ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#define p2_nrm2_3(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef lpnrm2_3
#define lpnrm2_3(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0)
#undef dpnrm2_3
#define dpnrm2_3(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      pc(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      pax(1,5) dbg(5) \
      pd(5,6) dbg(6) \
      pd(5,1) dbg(1) \
      pm(6,6) dbg(6) \
      pm(1,1) dbg(1) \
      pm(6,0) dbg(0) \
      pa(1,0) dbg(0) 
#undef plnrm2_3
#define plnrm2_3 8

#define block_nrm2_4(a_,b_) \
      Mjoin(pc,a_)(5,6) dbg(6) \
      pan(4,1) dbg(1) \
      Mjoin(pax,a_)(1,5) dbg(5) \
      Mjoin(pc,a_)(2,7) dbg(7) \
      Mjoin(pd,b_)(5,7) dbg(7) \
      Mjoin(pm,b_)(7,6) dbg(6) \
      Mjoin(pm,b_)(7,1) dbg(1) \
      Mjoin(pm,b_)(6,6) dbg(6) \
      Mjoin(pm,b_)(6,0) dbg(0) \
      Mjoin(pm,b_)(1,1) dbg(1) \
      Mjoin(pa,b_)(1,0) dbg(0)


/*  #undef p1_4_nrm2_4 */
/*  #define p1_4_nrm2_4(a_) \ */
/*        pls(a_,ax,1) dbg(1) \ */
/*        pcs(5,6) dbg(6) \ */
/*        pan(4,1) dbg(1) \ */
/*        paxs(1,5) dbg(5) \ */
/*        pcs(2,7) dbg(7) \ */
/*        pdsr(5,7) dbg(7) \ */
/*        pmsr(7,6) dbg(6) \ */
/*        pmsr(7,1) dbg(1) \ */
/*        pmsr(6,6) dbg(6) \ */
/*        pmsr(6,0) dbg(0) \ */
/*        pmsr(1,1) dbg(1) \ */
/*        pasr(1,0) dbg(0) */
#undef p1_4_nrm2_4
#define p1_4_nrm2_4(a_) \
      pls(a_,ax,1) dbg(1) \
      block_nrm2_4(s,sr)
#undef p1_2_nrm2_4
#define p1_2_nrm2_4(a_) \
      px(1) pld(a_,ax,1) dbg(1) \
      block_nrm2_4(,)
#undef p1_nrm2_4
#define p1_nrm2_4(a_) \
      plq(a_,ax,1) dbg(1) \
      block_nrm2_4(,)
#define p2_nrm2_4(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      block_nrm2_4(,) \
      plq(SS(a_,MM(2,RS4)),ax,1) dbg(1) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      block_nrm2_4(,)
#undef lpnrm2_4
#define lpnrm2_4(a_) \
      plq(SS(a_,MM(0,RS4)),ax,1) dbg(1) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      block_nrm2_4(,)
#undef dpnrm2_4
#define dpnrm2_4(a_) \
      plq(SS(a_,RS4),ax,1) dbg(1) \
      block_nrm2_4(,)
#undef plnrm2_4
#define plnrm2_4 8


#undef p1_4_1x1_1
#define p1_4_1x1_1(a_) \
      pls(a_,ax,1) \
      pls(a_,bx,0) \
      pm(0,1) \
      pa(1,6) 
#undef p1_2_1x1_1
#define p1_2_1x1_1(a_) \
      pld(a_,ax,1) \
      pld(a_,bx,0) \
      pm(0,1) \
      pa(1,6) 
#undef p1_1x1_1
#define p1_1x1_1(a_) \
      plq(a_,ax,1) \
      plq(a_,bx,0) \
      pm(0,1) \
      pa(0,6) 
#undef p2_1x1_1
#define p2_1x1_1(a_) \
      plq(a_,ax,1) \
      plq(a_,bx,0) \
      plq(SS(a_,RS4),ax,2) \
      plq(SS(a_,RS4),bx,3) \
      pm(0,1) \
      pm(2,3) \
      pa(1,6) \
      pa(3,6) 
#undef p4_1x1_1
#define p4_1x1_1(a_) \
      f(nta,SS(a_,MM(4,RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pm(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(0,1) \
      puq(3,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM(6,RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pm(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pm(0,7) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef lp1x1_1
#define lp1x1_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,RS4),ax,3) \
      pm(0,7) 
#undef dp1x1_1
#define dp1x1_1(a_) \
      plq(SS(,a_,MM(2,RS4)),ax,1) \
      pm(0,3) \
      puq(7,a_,ax) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(0,1) \
      puq(3,SS(a_,RS4),ax) \
      pm(0,2) \
      puq(1,SS(a_,MM(2,RS4)),ax) \
      puq(2,SS(a_,MM(3,RS4)),ax) 
#undef pl1x1_1
#define pl1x1_1 RS4


#undef p1_4_0x1_asum_1
#define p1_4_0x1_asum_1(a_) \
      pls(a_,ax,1) \
      pan(4,1) \
      pasr(1,0) 
#undef p1_2_0x1_asum_1
#define p1_2_0x1_asum_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      pan(4,1) \
      pa(1,0) 
#undef p1_0x1_asum_1
#define p1_0x1_asum_1(a_) \
      plq(a_,ax,1) \
      pan(4,1) \
      pa(1,0) 
#undef p2_0x1_asum_1
#define p2_0x1_asum_1(a_) \
      plq(a_,ax,1) \
      plq(SS(a_,RS4),ax,2) \
      pan(4,1) \
      pan(4,2) \
      pa(1,0) \
      pa(2,0) 
#undef p4_0x1_asum_1
#define p4_0x1_asum_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pan(4,3) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pan(4,1) \
      pa(3,0) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pan(4,2) \
      pa(1,0) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pan(4,7) \
      pa(2,0) 
#undef lp0x1_asum_1
#define lp0x1_asum_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,MM(1,RS4)),ax,3) \
      pan(4,7) 
#undef dp0x1_asum_1
#define dp0x1_asum_1(a_) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pan(4,3) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pan(4,1) \
      pa(3,0) \
      pan(4,2) \
      pa(1,0) \
      pa(2,0) 
#undef pl0x1_asum_1
#define pl0x1_asum_1 RS4


#undef p1_4_sum_1
#define p1_4_sum_1(a_) \
      pls(a_,ax,1) \
      pasr(1,0) 
#undef p1_2_sum_1
#define p1_2_sum_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      pa(1,0) 
#undef p1_sum_1
#define p1_sum_1(a_) \
      plq(a_,ax,1) \
      pa(1,0) 
#undef p2_sum_1
#define p2_sum_1(a_) \
      plq(a_,ax,1) \
      plq(SS(a_,RS4),ax,2) \
      pa(1,0) \
      pa(2,0) 
#undef p4_sum_1
#define p4_sum_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pa(3,0) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,7) \
      pa(1,0) \
      plq(SS(a_,MM(5,RS4)),ax,3) \
      pa(2,0) 
#undef lpsum_1
#define lpsum_1(a_) \
      plq(a_,ax,7) \
      plq(SS(a_,MM(1,RS4)),ax,3) 
#undef dpsum_1
#define dpsum_1(a_) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      pa(7,0) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pa(3,0) \
      pa(1,0) \
      pa(2,0) 
#undef plsum_1
#define plsum_1 RS4


#undef p1_4_dot_1
#define p1_4_dot_1(a_) \
      pls(a_,ax,1) \
      pls(a_,cx,2) \
      pmsr(2,1) \
      pasr(1,0) 
#undef p1_2_dot_1
#define p1_2_dot_1(a_) \
      px(1) \
      pld(a_,ax,1) \
      px(2) \
      pld(a_,cx,2) \
      pm(2,1) \
      pa(1,0) 
#undef p1_dot_1
#define p1_dot_1(a_) \
      plq(a_,ax,1) \
      pl(a_,cx,2) \
      pm(2,1) \
      pa(1,0) 
#undef p2_dot_1
#define p2_dot_1(a_) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pm(4,3) \
      pa(3,0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(2,1) \
      pa(1,0) 
#undef lpdot_1
#define lpdot_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(a_,ax,3) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(a_,cx,4) 
#undef dpdot_1
#define dpdot_1(a_) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pm(4,3) \
      pa(3,0) \
      pm(2,1) \
      pa(1,0)
#undef pldot_1
#define pldot_1 8

#undef p1_4_dot_1c
#define p1_4_dot_1c(a_)
#undef p1_2_dot_1c
#define p1_2_dot_1c(a_) \
      px(1) \
      pld(a_,ax,1) \
      px(2) \
      pld(a_,cx,2) \
      pc(1,3) \
      ps(HSHUF,1,1) \
      ps(LSHUF,3,3) \
      pm(7,1) \
      pm(2,3) \
      pa(3,0) \
      pm(2,1) \
      pa(1,6)
#undef p1_dot_1c
#define p1_dot_1c(a_) \
      plq(a_,ax,1) \
      pl(a_,cx,2) \
      pc(1,3) \
      ps(HSHUF,1,1) \
      ps(LSHUF,3,3) \
      pm(7,1) \
      pm(2,3) \
      pa(3,0) \
      pm(2,1) \
      pa(1,6)
#undef p2_dot_1c
#define p2_dot_1c(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pc(3,5) \
      ps(HSHUF,3,3) \
      ps(LSHUF,5,5) \
      pm(7,3) \
      pm(4,5) \
      pa(5,0) \
      pm(4,3) \
      pa(3,6) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      plq(SS(a_,MM(2,RS4)),ax,3) \
      pc(1,5) \
      ps(HSHUF,1,1) \
      ps(LSHUF,5,5) \
      pm(7,1) \
      pm(2,5) \
      pa(5,0) \
      pm(2,1) \
      pa(1,6)
#undef lpdot_1c
#define lpdot_1c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(a_,ax,3) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(a_,cx,4) 
#undef dpdot_1c
#define dpdot_1c(a_) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pc(3,5) \
      ps(HSHUF,3,3) \
      ps(LSHUF,5,5) \
      pm(7,3) \
      pm(4,5) \
      pa(5,0) \
      pm(4,3) \
      pa(3,6) \
      pc(1,5) \
      ps(HSHUF,1,1) \
      ps(LSHUF,5,5) \
      pm(7,1) \
      pm(2,5) \
      pa(5,0) \
      pm(2,1) \
      pa(1,6)
#undef pldot_1c
#define pldot_1c 8

#undef p1_4_dot_2c
#define p1_4_dot_2c(a_)
#undef p1_2_dot_2c
#define p1_2_dot_2c(a_) \
      px(1) \
      pld(a_,ax,1) \
      px(2) \
      pld(a_,cx,2) \
      pc(1,3) \
      ps(CSHUF,1,1) \
      pm(2,3) \
      pa(3,0) \
      pm(2,1) \
      pa(1,6)
#undef p1_dot_2c
#define p1_dot_2c(a_) \
      plq(a_,ax,1) \
      pl(a_,cx,2) \
      pc(1,3) \
      ps(CSHUF,1,1) \
      pm(2,3) \
      pa(3,0) \
      pm(2,1) \
      pa(1,6)
#undef p2_dot_2c
#define p2_dot_2c(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pc(3,5) \
      ps(CSHUF,3,3) \
      pm(4,5) \
      pa(5,0) \
      pm(4,3) \
      pa(3,6) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      plq(SS(a_,MM(2,RS4)),ax,3) \
      pc(1,5) \
      ps(CSHUF,1,1) \
      pm(2,5) \
      pa(5,0) \
      pm(2,1) \
      pa(1,6)
#undef lpdot_2c
#define lpdot_2c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(a_,ax,3) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(a_,cx,4) 
#undef dpdot_2c
#define dpdot_2c(a_) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,2) \
      pc(3,5) \
      ps(CSHUF,3,3) \
      pm(4,5) \
      pa(5,0) \
      pm(4,3) \
      pa(3,6) \
      pc(1,5) \
      ps(CSHUF,1,1) \
      pm(2,5) \
      pa(5,0) \
      pm(2,1) \
      pa(1,6)
#undef pldot_2c
#define pldot_2c 8

#undef p1_4_axpby_3
#define p1_4_axpby_3(a_) \
      pls(a_,ax,0) \
      pls(a_,cx,3) \
      pmsr(5,0) \
      pmsr(6,3) \
      pasr(3,0) \
      pus(0,a_,ax)
#undef p1_2_axpby_3
#define p1_2_axpby_3(a_) \
      pld(a_,ax,0) \
      pld(a_,cx,3) \
      pm(5,0) \
      pm(6,3) \
      pa(3,0) \
      pud(0,a_,ax)
#undef p1_axpby_3
#define p1_axpby_3(a_) \
      plq(a_,ax,0) \
      pl(a_,cx,3) \
      pm(5,0) \
      pm(6,3) \
      pa(3,0) \
      punt(0,a_,ax)
#undef p2_axpby_3
#define p2_axpby_3(a_) \
      plq(a_,ax,0) \
      pl(a_,cx,3) \
      plq(SS(a_,RS4),ax,1) \
      pm(5,0) \
      pm(6,3) \
      pa(3,0) \
      pl(SS(a_,RS4),cx,3) \
      punt(0,a_,ax) \
      pm(5,1) \
      pm(6,3) \
      pa(3,1) \
      punt(1,SS(a_,RS4),ax)
#undef p4_axpby_3
#define p4_axpby_3(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(5,2) \
      pl(SS(a_,MM(3,RS4)),cx,7) \
      pm(6,4) \
      pa(4,2) \
      punt(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      pm(5,3) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(6,7) \
      pa(7,3) \
      punt(1,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pm(5,0) \
      pl(SS(a_,MM(5,RS4)),cx,7) \
      pm(6,4) \
      pa(4,0) \
      punt(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,4) \
      pm(5,1) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(6,7) \
      pa(7,1) \
      punt(3,SS(a_,MM(3,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpaxpby_3
#define lpaxpby_3(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,4) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pl(SS(a_,MM(1,RS4)),cx,7) \
      pm(5,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,4) \
      pa(4,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pm(5,1) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(6,7) \
      pa(7,1)
#undef dpaxpby_3
#define dpaxpby_3(a_) \
      pl(SS(a_,MM(3,RS4)),cx,7) \
      pm(5,2) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,4) \
      pa(4,2) \
      pm(5,3) \
      punt(0,a_,ax) \
      pm(6,7) \
      pa(7,3) \
      punt(1,SS(a_,RS4),ax) \
      punt(2,SS(a_,MM(2,RS4)),ax) \
      punt(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpby_3
#define plaxpby_3 16

#undef p1_4_axpby_3c
#define p1_4_axpby_3c(a_) 
#undef p1_2_axpby_3c
#define p1_2_axpby_3c(a_) \
      pld(a_,ax,0) \
      pld(a_,cx,2) \
      pc(0,3) \
      pm(5,0) \
      ps(CSHUF,3,3) \
      pm(4,3) \
      pa(3,0) \
      pc(2,3) \
      pm(6,2) \
      pa(2,0) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,0) \
      pud(0,a_,ax)
#undef p1_axpby_3c
#define p1_axpby_3c(a_) \
      plq(a_,ax,0) \
      pl(a_,cx,2) \
      pc(0,3) \
      pm(5,0) \
      ps(CSHUF,3,3) \
      pm(4,3) \
      pa(3,0) \
      pc(2,3) \
      pm(6,2) \
      pa(2,0) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,0) \
      puq(0,a_,ax)
#undef p2_axpby_3c
#define p2_axpby_3c(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,3) \
      pc(1,2) \
      pm(5,1) \
      ps(CSHUF,2,2) \
      pm(4,2) \
      pa(2,1) \
      pc(3,2) \
      pm(6,3) \
      pa(3,1) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,1) \
      puq(0,a_,ax) \
      plq(SS(a_,MM(2,RS4)),ax,0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pc(0,3) \
      pm(5,0) \
      ps(CSHUF,3,3) \
      pm(4,3) \
      pa(3,0) \
      pc(2,3) \
      pm(6,2) \
      pa(2,0) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,0) \
      puq(1,SS(a_,RS4),ax) 
#undef lpaxpby_3c
#define lpaxpby_3c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,2) \
      pc(0,3) \
      pm(5,0) \
      ps(CSHUF,3,3) \
      pm(4,3) \
      pa(3,0) \
      pc(2,3) \
      pm(6,2) \
      pa(2,0) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,0) 
#undef dpaxpby_3c
#define dpaxpby_3c(a_) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pl(SS(a_,MM(1,RS4)),cx,3) \
      pc(1,2) \
      pm(5,1) \
      ps(CSHUF,2,2) \
      pm(4,2) \
      pa(2,1) \
      pc(3,2) \
      pm(6,3) \
      pa(3,1) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef plaxpby_3c
#define plaxpby_3c 8

#undef p1_4_axpby_2
#define p1_4_axpby_2(a_) \
      pls(a_,cx,5) \
      pls(a_,ax,0) \
      pmsr(6,5) \
      pasr(5,0) \
      pus(0,a_,ax)
#undef p1_2_axpby_2
#define p1_2_axpby_2(a_) \
      pld(a_,cx,5) \
      pld(a_,ax,0) \
      pm(6,5) \
      pa(5,0) \
      pud(0,a_,ax)
#undef p1_axpby_2
#define p1_axpby_2(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pm(6,5) \
      pa(5,0) \
      puq(0,a_,ax)
#undef p2_axpby_2
#define p2_axpby_2(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,5) \
      pa(5,0) \
      plq(SS(a_,RS4),ax,1) \
      puq(0,a_,ax) \
      pm(6,4) \
      pa(4,1) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_axpby_2
#define p4_axpby_2(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      pm(6,4) \
      pa(4,2) \
      puq(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(6,5) \
      pa(5,3) \
      puq(1,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pm(6,4) \
      pa(4,0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,4) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(6,5) \
      pa(5,1) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpaxpby_2
#define lpaxpby_2(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,4) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pl(SS(a_,MM(1,RS4)),cx,5) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,4) \
      pa(4,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(6,5) \
      pa(5,1)
#undef dpaxpby_2
#define dpaxpby_2(a_) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,4) \
      pa(4,2) \
      puq(0,a_,ax) \
      pm(6,5) \
      pa(5,3) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpby_2
#define plaxpby_2 16

#undef p1_4_axpby_2c
#define p1_4_axpby_2c(a_) 
#undef p1_2_axpby_2c
#define p1_2_axpby_2c(a_) \
      pld(a_,cx,5) \
      pld(a_,ax,0) \
      pc(5,1) \
      pm(6,5) \
      pa(5,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      pud(0,a_,ax)
#undef p1_axpby_2c
#define p1_axpby_2c(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pc(5,1) \
      pm(6,5) \
      pa(5,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      puq(0,a_,ax)
#undef p2_axpby_2c
#define p2_axpby_2c(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pl(SS(a_,RS4),cx,4) \
      pc(5,1) \
      pm(6,5) \
      pa(5,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      plq(SS(a_,RS4),ax,1) \
      puq(0,a_,ax) \
      pc(4,3) \
      pm(6,4) \
      pa(4,1) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,1) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_axpby_2c
#define p4_axpby_2c(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      puq(0,a_,ax) \
      pc(4,0) \
      pm(6,4) \
      pa(4,2) \
      ps(CSHUF,0,0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      pm(7,0) \
      pa(0,2) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      puq(1,SS(a_,RS4),ax) \
      pc(5,1) \
      pm(6,5) \
      pa(5,3) \
      ps(CSHUF,1,1) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pm(7,1) \
      pa(1,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,4) \
      pm(7,2) \
      pa(2,0) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      pc(5,3) \
      pm(6,5) \
      pa(5,1) \
      ps(CSHUF,3,3) \
      pl(SS(a_,MM(7,RS4)),cx,5) \
      pm(7,3) \
      pa(3,1) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpaxpby_2c
#define lpaxpby_2c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,4) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pl(SS(a_,MM(1,RS4)),cx,5) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(7,2) \
      pa(2,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pc(5,3) \
      pm(6,5) \
      pa(5,1) \
      ps(CSHUF,3,3) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      pm(7,3) \
      pa(3,1)
#undef dpaxpby_2c
#define dpaxpby_2c(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      puq(0,a_,ax) \
      pc(4,0) \
      pm(6,4) \
      pa(4,2) \
      ps(CSHUF,0,0) \
      puq(1,SS(a_,RS4),ax) \
      pm(7,0) \
      pa(0,2) \
      pc(5,1) \
      pm(6,5) \
      pa(5,3) \
      ps(CSHUF,1,1) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pm(7,1) \
      pa(1,3) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpby_2c
#define plaxpby_2c 16

#undef p1_4_axpby_1
#define p1_4_axpby_1(a_) \
      pls(a_,ax,1) \
      pls(a_,cx,2) \
      pmsr(5,1) \
      pmsr(6,2) \
      pasr(2,1) \
      pus(1,a_,ax)
#undef p1_2_axpby_1
#define p1_2_axpby_1(a_) \
      pld(a_,ax,1) \
      pld(a_,cx,2) \
      pm(5,1) \
      pm(6,2) \
      pa(2,1) \
      pud(1,a_,ax)
#undef p1_axpby_1
#define p1_axpby_1(a_) \
      plq(a_,ax,1) \
      pl(a_,cx,2) \
      pm(5,1) \
      pm(6,2) \
      pa(2,1) \
      puq(1,a_,ax)
#undef p2_axpby_1
#define p2_axpby_1(a_) \
      plq(SS(a_,RS4),ax,3) \
      pl(SS(a_,RS4),cx,4) \
      pm(5,1) \
      pm(6,2) \
      pa(2,1) \
      puq(1,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pm(5,3) \
      pm(6,4) \
      pa(4,3) \
      puq(3,SS(a_,RS4),ax)
#undef lpaxpby_1
#define lpaxpby_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,2) 
#undef dpaxpby_1
#define dpaxpby_1(a_) \
      plq(SS(a_,RS4),ax,3) \
      pl(SS(a_,RS4),cx,4) \
      pm(5,1) \
      pm(6,2) \
      pa(2,1) \
      puq(1,a_,ax) \
      pm(5,3) \
      pm(6,4) \
      pa(4,3) \
      puq(3,SS(a_,RS4),ax)
#undef plaxpby_1
#define plaxpby_1 8

#undef p1_4_axpy_0
#define p1_4_axpy_0(a_) \
      pls(a_,cx,2) \
      pls(a_,ax,1) \
      pmsr(6,2) \
      pasr(2,1) \
      pus(1,a_,ax)
#undef p1_2_axpy_0
#define p1_2_axpy_0(a_) \
      pld(a_,cx,2) \
      pld(a_,ax,1) \
      pm(6,2) \
      pa(2,1) \
      pud(1,a_,ax)
#undef p1_axpy_0
#define p1_axpy_0(a_) \
      pl(a_,cx,2) \
      plq(a_,ax,1) \
      pm(6,2) \
      pa(2,1) \
      puq(1,a_,ax)
#undef p2_axpy_0
#define p2_axpy_0(a_) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,2) \
      pa(2,1) \
      plq(SS(a_,RS4),ax,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      puq(1,a_,ax) \
      pm(6,4) \
      pa(4,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      puq(3,SS(a_,RS4),ax) 
#undef lpaxpy_0
#define lpaxpy_0(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,2) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) 
#undef dpaxpy_0
#define dpaxpy_0(a_) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,2) \
      pa(2,1) \
      plq(SS(a_,RS4),ax,3) \
      puq(1,a_,ax) \
      pm(6,4) \
      pa(4,3) \
      puq(3,SS(a_,RS4),ax)
#undef plaxpy_0
#define plaxpy_0 8

#undef p1_4_axpy_1
#define p1_4_axpy_1(a_) \
      pls(a_,cx,2) \
      pls(a_,ax,1) \
      pmsr(6,2) \
      pasr(2,1) \
      pus(1,a_,ax)
#undef p1_2_axpy_1
#define p1_2_axpy_1(a_) \
      pld(a_,cx,2) \
      pld(a_,ax,1) \
      pm(6,2) \
      pa(2,1) \
      pud(1,a_,ax)
#undef p1_axpy_1
#define p1_axpy_1(a_) \
      pl(a_,cx,2) \
      pm(6,2) \
      pam(a_,ax,2) \
      puq(2,a_,ax)
#undef p2_axpy_1
#define p2_axpy_1(a_) \
      pl(a_,cx,2) \
      pm(6,2) \
      pl(SS(a_,RS4),cx,4) \
      pam(a_,ax,2) \
      pm(6,4) \
      puq(2,a_,ax) \
      pam(SS(a_,RS4),ax,4) \
      puq(4,SS(a_,RS4),ax) 
#undef p4_axpy_1
#define p4_axpy_1(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pm(6,2) \
      pam(SS(a_,MM(2,RS4)),ax,2) \
      puq(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      pl(SS(a_,MM(4,RS4)),cx,0) \
      pm(6,3) \
      pam(SS(a_,MM(3,RS4)),ax,3) \
      puq(1,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(5,RS4)),cx,1) \
      pm(6,0) \
      pam(SS(a_,MM(4,RS4)),ax,0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      pl(SS(a_,MM(6,RS4)),cx,2) \
      pm(6,1) \
      pam(SS(a_,MM(5,RS4)),ax,1) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef lpaxpy_1
#define lpaxpy_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(a_,cx,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      pl(SS(a_,RS4),cx,1) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pm(6,0) \
      pam(a_,ax,0) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      pm(6,1) \
      pam(SS(a_,RS4),ax,1)
#undef dpaxpy_1
#define dpaxpy_1(a_) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pm(6,2) \
      pam(SS(a_,MM(2,RS4)),ax,2) \
      puq(0,a_,ax) \
      pm(6,3) \
      pam(SS(a_,MM(3,RS4)),ax,3) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpy_1
#define plaxpy_1 16

#undef p1_4_axpy_2
#define p1_4_axpy_2(a_) \
      pls(a_,cx,5) \
      pls(a_,ax,0) \
      pmsr(6,5) \
      pasr(5,0) \
      pus(0,a_,ax)
#undef p1_2_axpy_2
#define p1_2_axpy_2(a_) \
      pld(a_,cx,5) \
      pld(a_,ax,0) \
      pm(6,5) \
      pa(5,0) \
      pud(0,a_,ax)
#undef p1_axpy_2
#define p1_axpy_2(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pm(6,5) \
      pa(5,0) \
      puq(0,a_,ax)
#undef p2_axpy_2
#define p2_axpy_2(a_) \
      pl(a_,cx,5) \
      plq(a_,ax,0) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,5) \
      pa(5,0) \
      plq(SS(a_,RS4),ax,1) \
      puq(0,a_,ax) \
      pm(6,4) \
      pa(4,1) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_axpy_2
#define p4_axpy_2(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      pm(6,4) \
      pa(4,2) \
      puq(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(6,5) \
      pa(5,3) \
      puq(1,SS(a_,RS4),ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pm(6,4) \
      pa(4,0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,4) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(6,5) \
      pa(5,1) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpaxpy_2
#define lpaxpy_2(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,4) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pl(SS(a_,MM(1,RS4)),cx,5) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,4) \
      pa(4,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(6,5) \
      pa(5,1)
#undef dpaxpy_2
#define dpaxpy_2(a_) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,4) \
      pa(4,2) \
      puq(0,a_,ax) \
      pm(6,5) \
      pa(5,3) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpy_2
#define plaxpy_2 16

#undef p1_4_axpy_2c
#define p1_4_axpy_2c(a_) 
#undef p1_2_axpy_2c
#define p1_2_axpy_2c(a_) \
      pld(a_,cx,4) \
      pld(a_,ax,0) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      pud(0,a_,ax)
#undef p1_axpy_2c
#define p1_axpy_2c(a_) \
      pl(a_,cx,4) \
      plq(a_,ax,0) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      puq(0,a_,ax)
#undef p2_axpy_2c
#define p2_axpy_2c(a_) \
      pl(a_,cx,4) \
      plq(a_,ax,0) \
      pl(SS(a_,RS4),cx,5) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      plq(SS(a_,RS4),ax,1) \
      puq(0,a_,ax) \
      pc(5,3) \
      pm(6,5) \
      pa(5,1) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,1) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_axpy_2c
#define p4_axpy_2c(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      puq(0,a_,ax) \
      pc(4,0) \
      pm(6,4) \
      pa(4,2) \
      ps(CSHUF,0,0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      pm(7,0) \
      pa(0,2) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      puq(1,SS(a_,RS4),ax) \
      pc(5,1) \
      pm(6,5) \
      pa(5,3) \
      ps(CSHUF,1,1) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pm(7,1) \
      pa(1,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,4) \
      pm(7,2) \
      pa(2,0) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      pc(5,3) \
      pm(6,5) \
      pa(5,1) \
      ps(CSHUF,3,3) \
      pl(SS(a_,MM(7,RS4)),cx,5) \
      pm(7,3) \
      pa(3,1) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) 
#undef lpaxpy_2c
#define lpaxpy_2c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,4) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      pl(SS(a_,MM(1,RS4)),cx,5) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pc(4,2) \
      pm(6,4) \
      pa(4,0) \
      ps(CSHUF,2,2) \
      pl(SS(a_,MM(2,RS4)),cx,4) \
      pm(7,2) \
      pa(2,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pc(5,3) \
      pm(6,5) \
      pa(5,1) \
      ps(CSHUF,3,3) \
      pl(SS(a_,MM(3,RS4)),cx,5) \
      pm(7,3) \
      pa(3,1)
#undef dpaxpy_2c
#define dpaxpy_2c(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      puq(0,a_,ax) \
      pc(4,0) \
      pm(6,4) \
      pa(4,2) \
      ps(CSHUF,0,0) \
      puq(1,SS(a_,RS4),ax) \
      pm(7,0) \
      pa(0,2) \
      pc(5,1) \
      pm(6,5) \
      pa(5,3) \
      ps(CSHUF,1,1) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pm(7,1) \
      pa(1,3) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plaxpy_2c
#define plaxpy_2c 16

#undef p1_4_axpy_1c
#define p1_4_axpy_1c(a_)
#undef p1_2_axpy_1c
#define p1_2_axpy_1c(a_) \
      pld(a_,cx,2) \
      pc(2,0) \
      pld(a_,ax,1) \
      ps(CSHUF,0,0) \
      pm(6,2) \
      pa(2,1) \
      pm(7,0) \
      pa(0,1) \
      pud(1,a_,ax)
#undef p1_axpy_1c
#define p1_axpy_1c(a_) \
      pl(a_,cx,2) \
      pc(2,0) \
      plq(a_,ax,1) \
      ps(CSHUF,0,0) \
      pm(6,2) \
      pa(2,1) \
      pm(7,0) \
      pa(0,1) \
      puq(1,a_,ax)
#undef p2_axpy_1c
#define p2_axpy_1c(a_) \
      plq(SS(a_,RS4),ax,3) \
      ps(CSHUF,0,0) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,2) \
      pa(2,1) \
      pm(7,0) \
      pa(0,1) \
      pc(4,0) \
      puq(1,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,1) \
      ps(CSHUF,0,0) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pm(6,4) \
      pa(4,3) \
      pm(7,0) \
      pa(0,3) \
      pc(2,0) \
      puq(3,SS(a_,RS4),ax)
#undef lpaxpy_1c
#define lpaxpy_1c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,2) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,1) \
      pc(2,0) 
#undef dpaxpy_1c
#define dpaxpy_1c(a_) \
      plq(SS(a_,RS4),ax,3) \
      ps(CSHUF,0,0) \
      pl(SS(a_,RS4),cx,4) \
      pm(6,2) \
      pa(2,1) \
      pm(7,0) \
      pa(0,1) \
      pc(4,0) \
      puq(1,a_,ax) \
      ps(CSHUF,0,0) \
      pm(6,4) \
      pa(4,3) \
      pm(7,0) \
      pa(0,3) \
      puq(3,SS(a_,RS4),ax)
#undef plaxpy_1c
#define plaxpy_1c 8

#undef p1_4_copy_1
#define p1_4_copy_1(a_) \
      pls(a_,cx,2) \
      pus(2,a_,ax)
#undef p1_2_copy_1
#define p1_2_copy_1(a_) \
      pld(a_,cx,2) \
      pud(2,a_,ax)
#undef p1_copy_1
#define p1_copy_1(a_) \
      pl(a_,cx,2) \
      puq(2,a_,ax)
#undef p2_copy_1
#define p2_copy_1(a_) \
      pl(SS(a_,RS4),cx,4) \
      puq(2,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      puq(4,SS(a_,RS4),ax)
#undef lpcopy_1
#define lpcopy_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,2) 
#undef dpcopy_1
#define dpcopy_1(a_) \
      pl(SS(a_,RS4),cx,4) \
      puq(2,a_,ax) \
      puq(4,SS(a_,RS4),ax)
#undef plcopy_1
#define plcopy_1 8

#undef p1_4_copy_2
#define p1_4_copy_2(a_) \
      pls(a_,ax,2) \
      pus(2,a_,cx)
#undef p1_2_copy_2
#define p1_2_copy_2(a_) \
      pld(a_,ax,2) \
      pud(2,a_,cx)
#undef p1_copy_2
#define p1_copy_2(a_) \
      plq(a_,ax,2) \
      pu(2,a_,cx)
#undef p2_copy_2
#define p2_copy_2(a_) \
      plq(SS(a_,RS4),ax,4) \
      pu(2,a_,cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pu(4,SS(a_,RS4),cx)
#undef lpcopy_2
#define lpcopy_2(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,2) 
#undef dpcopy_2
#define dpcopy_2(a_) \
      plq(SS(a_,RS4),ax,4) \
      pu(2,a_,cx) \
      pu(4,SS(a_,RS4),cx)
#undef plcopy_2
#define plcopy_2 8

#undef p1_4_copy_3
#define p1_4_copy_3(a_) \
      pls(a_,cx,2) \
      pus(2,a_,ax)
#undef p1_2_copy_3
#define p1_2_copy_3(a_) \
      pld(a_,cx,2) \
      pud(2,a_,ax)
#undef p1_copy_3
#define p1_copy_3(a_) \
      pl(a_,cx,2) \
      punt(2,a_,ax)
#undef p2_copy_3
#define p2_copy_3(a_) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      punt(0,SS(a_,MM(0,RS4)),ax) \
      punt(1,SS(a_,MM(1,RS4)),ax) 
#undef p4_copy_3
#define p4_copy_3(a_) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      punt(0,SS(a_,MM(0,RS4)),ax) \
      punt(1,SS(a_,MM(1,RS4)),ax) \
      punt(2,SS(a_,MM(2,RS4)),ax) \
      punt(3,SS(a_,MM(3,RS4)),ax) 
#undef p8_copy_3
#define p8_copy_3(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pl(SS(a_,MM(6,RS4)),cx,6) \
      pl(SS(a_,MM(7,RS4)),cx,7) \
      punt(0,SS(a_,MM(0,RS4)),ax) \
      punt(1,SS(a_,MM(1,RS4)),ax) \
      punt(2,SS(a_,MM(2,RS4)),ax) \
      punt(3,SS(a_,MM(3,RS4)),ax) \
      punt(4,SS(a_,MM(4,RS4)),ax) \
      punt(5,SS(a_,MM(5,RS4)),ax) \
      punt(6,SS(a_,MM(6,RS4)),ax) \
      punt(7,SS(a_,MM(7,RS4)),ax) 
#undef lpcopy_3
#define lpcopy_3(a_) 
#undef dpcopy_3
#define dpcopy_3(a_) p8_copy_3(a_)
#undef plcopy_3
#define plcopy_3 32

#undef p1_4_cpsc_3
#define p1_4_cpsc_3(a_) \
      pls(a_,ax,0) \
      pmsr(6,0) \
      pus(0,a_,cx)
#undef p1_2_cpsc_3
#define p1_2_cpsc_3(a_) \
      pld(a_,ax,0) \
      pm(6,0) \
      pud(0,a_,cx)
#undef p1_cpsc_3
#define p1_cpsc_3(a_) \
      plq(a_,ax,0) \
      pm(6,0) \
      pu(0,a_,cx)
#undef p2_cpsc_3
#define p2_cpsc_3(a_) \
      plq(a_,ax,0) \
      plq(SS(a_,RS4),ax,1) \
      pm(6,0) \
      pm(6,1) \
      pu(0,a_,cx) \
      pu(1,SS(a_,RS4),cx) 
#undef p4_cpsc_3
#define p4_cpsc_3(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,2) \
      pu(0,a_,cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(6,3) \
      pu(1,SS(a_,RS4),cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pm(6,0) \
      pu(2,SS(a_,MM(2,RS4)),cx) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(6,1) \
      pu(3,SS(a_,MM(3,RS4)),cx) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) 
#undef lpcpsc_3
#define lpcpsc_3(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pm(6,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pm(6,1) 
#undef dpcpsc_3
#define dpcpsc_3(a_) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(6,2) \
      pu(0,a_,cx) \
      pm(6,3) \
      pu(1,SS(a_,RS4),cx) \
      pu(2,SS(a_,MM(2,RS4)),cx) \
      pu(3,SS(a_,MM(3,RS4)),cx)
#undef plcpsc_3
#define plcpsc_3 16

#undef p1_4_cpsc_3c
#define p1_4_cpsc_3c(a_) 
#undef p1_2_cpsc_3c
#define p1_2_cpsc_3c(a_) \
      pld(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      pud(0,a_,cx)
#undef p1_cpsc_3c
#define p1_cpsc_3c(a_) \
      plq(a_,ax,0) \
      pc(0,1) \
      pm(6,0) \
      ps(CSHUF,1,1) \
      pm(7,1) \
      pa(1,0) \
      pu(0,a_,cx)
#undef p2_cpsc_3c
#define p2_cpsc_3c(a_) \
      plq(a_,ax,0) \
      plq(SS(a_,RS4),ax,1) \
      pc(0,2) \
      pm(6,0) \
      ps(CSHUF,2,2) \
      pm(7,2) \
      pa(2,0) \
      pu(0,a_,cx) \
      pc(1,3) \
      pm(6,1) \
      ps(CSHUF,3,3) \
      pm(7,3) \
      pa(3,1) \
      pu(1,SS(a_,RS4),cx) 
#undef p4_cpsc_3c
#define p4_cpsc_3c(a_) \
      pu(0,a_,cx) \
      pc(2,4) \
      pm(6,2) \
      ps(CSHUF,4,4) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,0) \
      pm(7,4) \
      pa(4,2) \
      pu(1,SS(a_,RS4),cx) \
      pc(3,4) \
      pm(6,3) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(5,RS4)),ax,1) \
      pm(7,4) \
      pa(4,3) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pu(2,SS(a_,MM(2,RS4)),cx) \
      pc(0,4) \
      pm(6,0) \
      ps(CSHUF,4,4) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(6,RS4)),ax,2) \
      pm(7,4) \
      pa(4,0) \
      pu(3,SS(a_,MM(3,RS4)),cx) \
      pc(1,4) \
      pm(6,1) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(7,RS4)),ax,3) \
      pm(7,4) \
      pa(4,1) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) 
#undef lpcpsc_3c
#define lpcpsc_3c(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,0) \
      plq(SS(a_,MM(1,RS4)),ax,1) \
      pc(0,4) \
      pm(6,0) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pm(7,4) \
      pa(4,0) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pc(1,4) \
      pm(6,1) \
      ps(CSHUF,4,4) \
      plq(SS(a_,MM(3,RS4)),ax,3) \
      pm(7,4) \
      pa(4,1)
#undef dpcpsc_3c
#define dpcpsc_3c(a_) \
      pu(0,a_,cx) \
      pc(2,4) \
      pm(6,2) \
      ps(CSHUF,4,4) \
      pu(1,SS(a_,RS4),cx) \
      pm(7,4) \
      pa(4,2) \
      pc(3,4) \
      pm(6,3) \
      ps(CSHUF,4,4) \
      pu(2,SS(a_,MM(2,RS4)),cx) \
      pm(7,4) \
      pa(4,3) \
      pu(3,SS(a_,MM(3,RS4)),cx)
#undef plcpsc_3c
#define plcpsc_3c 16

#undef p1_4_cpsc_4
#define p1_4_cpsc_4(a_) \
      pls(a_,cx,0) \
      pmsr(6,0) \
      pus(0,a_,ax)
#undef p1_2_cpsc_4
#define p1_2_cpsc_4(a_) \
      pld(a_,cx,0) \
      pm(6,0) \
      pud(0,a_,ax)
#undef p1_cpsc_4
#define p1_cpsc_4(a_) \
      pl(a_,cx,0) \
      pm(6,0) \
      puq(0,a_,ax)
#undef p2_cpsc_4
#define p2_cpsc_4(a_) \
      pl(a_,cx,0) \
      pl(SS(a_,RS4),cx,1) \
      pm(6,0) \
      pm(6,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_cpsc_4
#define p4_cpsc_4(a_) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pm(6,2) \
      puq(0,a_,ax) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(4,RS4)),cx,0) \
      pm(6,3) \
      puq(1,SS(a_,RS4),ax) \
      pl(SS(a_,MM(5,RS4)),cx,1) \
      pm(6,0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      pl(SS(a_,MM(6,RS4)),cx,2) \
      pm(6,1) \
      puq(3,SS(a_,MM(3,RS4)),ax) 
#undef lpcpsc_4
#define lpcpsc_4(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pm(6,0) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pm(6,1) 
#undef dpcpsc_4
#define dpcpsc_4(a_) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pm(6,2) \
      puq(0,a_,ax) \
      pm(6,3) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax)
#undef plcpsc_4
#define plcpsc_4 16

#undef p1_4_cpsc_5
#define p1_4_cpsc_5(a_) \
      pls(a_,cx,0) \
      pmsr(6,0) \
      pus(0,a_,ax)
#undef p1_2_cpsc_5
#define p1_2_cpsc_5(a_) \
      pld(a_,cx,0) \
      pm(6,0) \
      pud(0,a_,ax)
#undef p1_cpsc_5
#define p1_cpsc_5(a_) \
      pl(a_,cx,0) \
      pm(6,0) \
      puq(0,a_,ax)
#undef p2_cpsc_5
#define p2_cpsc_5(a_) \
      pl(a_,cx,0) \
      pl(SS(a_,RS4),cx,1) \
      pm(6,0) \
      pm(6,1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_cpsc_5
#define p4_cpsc_5(a_) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pm(6,0) \
      pm(6,1) \
      pm(6,2) \
      pm(6,3) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) 
#undef p8_cpsc_5
#define p8_cpsc_5(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      pl(SS(a_,MM(5,RS4)),cx,5) \
      pl(SS(a_,MM(6,RS4)),cx,7) \
      pm(6,0) \
      pm(6,1) \
      pm(6,2) \
      pm(6,3) \
      puq(0,a_,ax) \
      pl(SS(a_,MM(7,RS4)),cx,0) \
      pm(6,4) \
      pm(6,5) \
      pm(6,7) \
      pm(6,0) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      puq(4,SS(a_,MM(4,RS4)),ax) \
      puq(5,SS(a_,MM(5,RS4)),ax) \
      puq(7,SS(a_,MM(6,RS4)),ax) \
      puq(0,SS(a_,MM(7,RS4)),ax) 
#undef lpcpsc_5
#define lpcpsc_5(a_) 
#undef dpcpsc_5
#define dpcpsc_5(a_) p8_cpsc_5(a_)
#undef plcpsc_5
#define plcpsc_5 32

#undef cpsc_cdp
#define cpsc_cdp(a_) pc(a_,5) pm(6,a_) ps(CSHUF,5,5) pm(7,5) pa(5,a_)
#undef p1_4_cpsc_5c
#define p1_4_cpsc_5c(a_) 
#undef p1_2_cpsc_5c
#define p1_2_cpsc_5c(a_) \
      pld(a_,cx,0) \
      cpsc_cdp(0) \
      pud(0,a_,ax)
#undef p1_cpsc_5c
#define p1_cpsc_5c(a_) \
      pl(a_,cx,0) \
      cpsc_cdp(0) \
      puq(0,a_,ax)
#undef p2_cpsc_5c
#define p2_cpsc_5c(a_) \
      pl(a_,cx,0) \
      pl(SS(a_,RS4),cx,1) \
      cpsc_cdp(0) \
      cpsc_cdp(1) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) 
#undef p4_cpsc_5c
#define p4_cpsc_5c(a_) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      cpsc_cdp(0) \
      cpsc_cdp(1) \
      cpsc_cdp(2) \
      cpsc_cdp(3) \
      puq(0,a_,ax) \
      puq(1,SS(a_,RS4),ax) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      puq(3,SS(a_,MM(3,RS4)),ax) 
#undef p8_cpsc_5c
#define p8_cpsc_5c(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      pl(SS(a_,MM(0,RS4)),cx,0) \
      pl(SS(a_,MM(1,RS4)),cx,1) \
      pl(SS(a_,MM(2,RS4)),cx,2) \
      pl(SS(a_,MM(3,RS4)),cx,3) \
      pl(SS(a_,MM(4,RS4)),cx,4) \
      cpsc_cdp(0) \
      cpsc_cdp(1) \
      puq(0,a_,ax) \
      pl(SS(a_,MM(5,RS4)),cx,0) \
      cpsc_cdp(2) \
      cpsc_cdp(3) \
      puq(1,SS(a_,RS4),ax) \
      pl(SS(a_,MM(6,RS4)),cx,1) \
      cpsc_cdp(4) \
      cpsc_cdp(0) \
      puq(2,SS(a_,MM(2,RS4)),ax) \
      pl(SS(a_,MM(7,RS4)),cx,2) \
      cpsc_cdp(1) \
      cpsc_cdp(2) \
      puq(3,SS(a_,MM(3,RS4)),ax) \
      puq(4,SS(a_,MM(4,RS4)),ax) \
      puq(0,SS(a_,MM(5,RS4)),ax) \
      puq(1,SS(a_,MM(6,RS4)),ax) \
      puq(2,SS(a_,MM(7,RS4)),ax) 
#undef lpcpsc_5c
#define lpcpsc_5c(a_) 
#undef dpcpsc_5c
#define dpcpsc_5c(a_) p8_cpsc_5c(a_)
#undef plcpsc_5c
#define plcpsc_5c 32

#undef p1_4_cpsc_1
#define p1_4_cpsc_1(a_) \
      pls(a_,ax,2) \
      pmsr(3,2) \
      pus(2,a_,cx)
#undef p1_2_cpsc_1
#define p1_2_cpsc_1(a_) \
      pld(a_,ax,2) \
      pm(3,2) \
      pud(2,a_,cx)
#undef p1_cpsc_1
#define p1_cpsc_1(a_) \
      plq(a_,ax,2) \
      pm(3,2) \
      pu(2,a_,cx)
#undef p2_cpsc_1
#define p2_cpsc_1(a_) \
      plq(SS(a_,RS4),ax,4) \
      pm(3,2) \
      pu(2,a_,cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,2) \
      pm(3,4) \
      pu(4,SS(a_,RS4),cx)
#undef lpcpsc_1
#define lpcpsc_1(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,2) 
#undef dpcpsc_1
#define dpcpsc_1(a_) \
      plq(SS(a_,RS4),ax,4) \
      pm(3,2) \
      pu(2,a_,cx) \
      pm(3,4) \
      pu(4,SS(a_,RS4),cx)
#undef plcpsc_1
#define plcpsc_1 8

#undef p1_4_cpsc_2
#define p1_4_cpsc_2(a_) \
      pls(a_,ax,2) \
      pmsr(3,2) \
      pus(2,a_,cx)
#undef p1_2_cpsc_2
#define p1_2_cpsc_2(a_) \
      pld(a_,ax,2) \
      pm(3,2) \
      pud(2,a_,cx)
#undef p1_cpsc_2
#define p1_cpsc_2(a_) \
      plq(a_,ax,2) \
      pm(3,2) \
      pu(2,a_,cx)
#undef p2_cpsc_2
#define p2_cpsc_2(a_) \
      plq(a_,ax,2) \
      plq(SS(a_,RS4),ax,4) \
      pm(3,2) \
      pm(3,4) \
      pu(2,a_,cx) \
      pu(4,SS(a_,RS4),cx)
#undef p4_cpsc_2
#define p4_cpsc_2(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,7) \
      pm(3,6) \
      pu(4,a_,cx) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(3,7) \
      pu(6,SS(a_,RS4),cx) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(4,CL)),RS4)),ax) \
      plq(SS(a_,MM(4,RS4)),ax,4) \
      pm(3,2) \
      pu(7,SS(a_,MM(2,RS4)),cx) \
      plq(SS(a_,MM(5,RS4)),ax,6) \
      pm(3,4) \
      pu(2,SS(a_,MM(3,RS4)),cx) 
#undef lpcpsc_2
#define lpcpsc_2(a_) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(0,CL)),RS4)),ax) \
      plq(SS(a_,MM(0,RS4)),ax,4) \
      plq(SS(a_,MM(1,RS4)),ax,6) \
      pm(3,4) 
#undef dpcpsc_2
#define dpcpsc_2(a_) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),cx) \
      f(nta,SS(a_,MM((SS(2,CL)),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,7) \
      pm(3,6) \
      pu(4,a_,cx) \
      plq(SS(a_,MM(3,RS4)),ax,2) \
      pm(3,7) \
      pu(6,SS(a_,RS4),cx) \
      pm(3,2) \
      pu(7,SS(a_,MM(2,RS4)),cx) \
      pu(2,SS(a_,MM(3,RS4)),cx) 
#undef plcpsc_2
#define plcpsc_2 RS4


#undef p1_4_iamax_1
#define p1_4_iamax_1(a_) \
      px(4) \
      pls(a_,ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      paxs(4,3) \
      pan(5,6) \
      pann(0,5) \
      pasr(5,6) \
      pasr(1,0) \
      ps(57,0,0)
#undef p1_2_iamax_1
#define p1_2_iamax_1(a_) \
      px(4) \
      pld(a_,ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pasr(1,0) \
      ps(57,0,0)\
      pasr(1,0) \
      ps(57,0,0)
#undef p1_iamax_1
#define p1_iamax_1(a_) \
      plq(a_,ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pa(1,0) 
#define p2_iamax_1(a_) \
      plq(SS(a_,RS4),ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pa(1,0) \
      f(nta,SS(a_,MM(SS(2,CL),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pa(1,0) 
#undef lpiamax_1
#define lpiamax_1(a_) \
      f(nta,SS(a_,MM(CL,RS4)),ax) \
      plq(a_,ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pa(1,0) 
#undef dpiamax_1
#define dpiamax_1(a_) \
      plq(SS(a_,RS4),ax,4) \
      pan(2,4) \
      pc(3,5) \
      pcm(6,4,5) \
      pax(4,3) \
      pan(5,6) \
      pann(0,5) \
      pa(5,6) \
      pa(1,0) 
#undef pliamax_1
#define pliamax_1 8

#undef p1_4_iamax_1d
#define p1_4_iamax_1d(a_) 
#undef p1_2_iamax_1d
#define p1_2_iamax_1d(a_) \
      px(4) \
      pld(a_,ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pasr(1,0) \
      dbg(0) \
      ps(1,0,0)
#undef p1_iamax_1d
#define p1_iamax_1d(a_) \
      plq(a_,ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pa(1,0) 
#define p2_iamax_1d(a_) \
      plq(SS(a_,RS4),ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pa(1,0) \
      dbg(0) \
      f(nta,SS(a_,MM(SS(2,CL),RS4)),ax) \
      plq(SS(a_,MM(2,RS4)),ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pa(1,0) 
#undef lpiamax_1d
#define lpiamax_1d(a_) \
      f(nta,SS(a_,MM(CL,RS4)),ax) \
      plq(a_,ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pa(1,0) 
#undef dpiamax_1d
#define dpiamax_1d(a_) \
      plq(SS(a_,RS4),ax,4) \
      dbg(2) \
      pan(2,4) \
      dbg(4) \
      pc(3,5) \
      dbg(5) \
      pcm(6,4,5) \
      dbg(5) \
      pax(4,3) \
      dbg(3) \
      pan(5,6) \
      dbg(6) \
      pann(0,5) \
      dbg(5) \
      pa(5,6) \
      dbg(6) \
      pa(1,0) 
#undef pliamax_1d
#define pliamax_1d 8

