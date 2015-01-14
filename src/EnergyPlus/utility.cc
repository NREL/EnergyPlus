//Geof Sawaya, 2014, LBL & DOE

//nclude <thread>
#include "utility.hh"
#include <iostream>
namespace EppPerformance
{

// extern "C" {
// #include <unistd.h>
// }
//This file is adapted from the AMD example, AMD copyright info left in tact

/***************************************************************************  
 File:    enum.c
 Author:  Tracy W. Carver
 Date:    June 30, 2009
 Desc:    This demonstrates how an application can detect the number of 
          processors (where a "processor" is a part we sell that fits into a 
          processor socket), and the number of cores per processor on AMD 
          processor-based systems.  This has been tested on systems with 
          Family 0Fh and Family 10h processors.
   
          This code uses a combination of an OS-supplied routine and CPUID. 
          It is assumed that the operating system supplies the total number 
          of logical processors on the system. CPUID is executed on each 
          logical processor by affinitizing the current thread to that 
          processor.  Another assumption is that it be able to access all 
          processors on a system, so OS-based restrictions perhaps due to 
          permissions or processor sets may affect the results.
          
          This sample code is AMD-specific.  For detailed explanations,
          refer to: AMD CPUID Specification, publication # 25481.  This code 
          is based on Revision 2.28, April 2008. This program is expected 
          to work under 32-bit and 64-bit versions of Linux, Windows, and 
          Solaris.

          AMD is under no obligation to provide user with any updates, 
          support, or maintenance of the Software or documentation. THE 
          SOFTWARE AND ANY OTHER MATERIALS ARE PROVIDED AS IS, WITH ALL 
          FAULTS, AND WITHOUT WARRANTY OF ANY KIND. FURTHERMORE, NO 
          WARRANTIES, EXPRESS OR IMPLIED, ARE MADE WITH RESPECT TO THE 
          SOFTWARE OR OTHER MATERIALS INCLUDING, BUT NOT LIMITED TO, 
          WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, 
          ANY WARRANTIES THAT MAY ARISE FROM USAGE OF TRADE OR COURSE OF 
          DEALING, AND ANY IMPLIED WARRANTIES OF TITLE OR NON-INFRINGEMENT.

 Copyright 2009 Advanced Micro Devices, Inc. 
****************************************************************************/  

#ifdef _WIN32
#define _WIN32_WINNT 0x0500 // to use certain functions

#ifdef UNICODE
#undef UNICODE
#endif

#ifdef _UNICODE
#undef _UNICODE
#endif

#include <windows.h>
#include <winbase.h>
#include <tchar.h>
// #include <intrin.h>

#define BOOL int
#define bool int
    
typedef BOOL (WINAPI *LPFN_GSI)( LPSYSTEM_INFO );
typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);
BOOL IsWow64( void );

#endif


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
  #ifndef __USE_GNU
     #define __USE_GNU
  #endif
     #include <unistd.h>
     #include <sys/types.h>
#ifdef sun
     #include <sys/processor.h>
     #include <sys/procset.h>
#else
     #include <sched.h>
#endif

#endif


#ifndef _WIN32
#define BOOL int
#define bool int   
#endif 

#ifndef _M_X64

#ifdef _WIN32
// Patterned after their 64-bit intrinsic routine
void mycpuid( int * p, unsigned int param )
{
   __asm {
             mov    edi, p
             mov    eax, param
             cpuid
             mov    [edi+0d],  eax
             mov    [edi+4d],  ebx
             mov    [edi+8d],  ecx
             mov    [edi+12d], edx
         }
}
#else
#ifdef __GNUC__
void mycpuid( int * p, unsigned int param )
{
   __asm__ __volatile__
   (
      "cpuid;"
      : "=a" (p[0]), "=b" (p[1]), "=c" (p[2]), "=d" (p[3])
      : "a" (param)
   );
}
#else /* not __GNUC__ */
void mycpuid( int * p, unsigned int param )
{
#ifndef __x86_64
   __asm__ __volatile__
   (
      "mov    %0, %%edi\n\t"
      "cpuid;\n\t"
      "mov    %%eax, 0(%%edi)\n\t"
      "mov    %%ebx, 4(%%edi)\n\t"
      "mov    %%ecx, 8(%%edi)\n\t"
      "mov    %%edx, 12(%%edi)\n\t"
      :
      :"m" (p),"a" (param)
      :"ebx","ecx","edx","edi"
   );
#else
   __asm__ __volatile__
   (
      "movq    %0, %%rdi\n\t"
      "cpuid;\n\t"
      "mov    %%eax, 0(%%rdi)\n\t"
      "mov    %%ebx, 4(%%rdi)\n\t"
      "mov    %%ecx, 8(%%rdi)\n\t"
      "mov    %%edx, 12(%%rdi)\n\t"
      :
      :"m" (p),"a" (param)
      :"ebx","ecx","edx","rdi"
   );
#endif /* __x86_64 */
}

#endif /* __GNUC__ */
#endif /* _WIN32 */
#endif /* _M_X64 */

#ifdef _M_X64 
#pragma intrinsic(__cpuid)
#else 
#define __cpuid mycpuid
#endif


typedef struct _LOGICALPROCESSORDATA
{
   unsigned int nLargestStandardFunctionNumber;
   unsigned int nLargestExtendedFunctionNumber;
   int nLogicalProcessorCount;
   int nLocalApicId;
   int nCPUcore;
   int nProcessorId;
   int nApicIdCoreIdSize;
   int nNC;
   int nMNC;
   int nCPUCoresperProcessor;
   int nThreadsperCPUCore;
   int nProcId; 
   int nCoreId;
   bool CmpLegacy;
   bool HTT;
}  LOGICALPROCESSORDATA, *PLOGICALPROCESSORDATA;

#define MAX_NUMBER_OF_LOGICAL_PROCESSORS 96
#define MAX_NUMBER_OF_PHYSICAL_PROCESSORS 8
#define MAX_NUMBER_OF_IOAPICS 16
LOGICALPROCESSORDATA LogicalProcessorMap[MAX_NUMBER_OF_LOGICAL_PROCESSORS]; 
int PhysProcIds[MAX_NUMBER_OF_PHYSICAL_PROCESSORS+MAX_NUMBER_OF_IOAPICS];

int more = 0;

#ifdef _WIN32
BOOL IsWow64( void )
{
   BOOL bIsWow64 = FALSE;
   LPFN_ISWOW64PROCESS 
   fnIsWow64Process = (LPFN_ISWOW64PROCESS)GetProcAddress( 
                        GetModuleHandle( TEXT("kernel32") ), 
                        "IsWow64Process" );
   if (NULL != fnIsWow64Process)
   {
      if (!fnIsWow64Process( GetCurrentProcess(), &bIsWow64) )
      {
         // handle error
      }
   }
   return bIsWow64; // For a 64-bit app running under 64-bit Windows, this is FALSE.
}


int QueryNumLogicalProcessors( void )
{
   LPFN_GSI Gsi;
   // return (1);
   // try it this way
   // GetSystemInfo() will return the number of processors in a data field in a SYSTEM_INFO structure. 
   SYSTEM_INFO siSysInfo;
   // Copy the hardware information to the SYSTEM_INFO structure. 
   if ( IsWow64() )
   {
      Gsi = (LPFN_GSI) GetProcAddress( GetModuleHandle( TEXT("kernel32") ),
                                       "GetNativeSystemInfo" );
      // So, they suggest 32-bit apps should call this instead of the other in WOW64
      if (Gsi)
         Gsi( &siSysInfo );             
      else
         GetSystemInfo( &siSysInfo );
   }
   else
      GetSystemInfo( &siSysInfo ); 
   return( siSysInfo.dwNumberOfProcessors );
}

void LockToLogicalProcessor( int n )
// I want to just stick onto one particular core
{
   DWORD_PTR ProcessAffinityMask;
   DWORD_PTR SystemAffinityMask;
   BOOL rc;
   DWORD pm, pmm;
   rc = GetProcessAffinityMask( GetCurrentProcess(), &ProcessAffinityMask, &SystemAffinityMask );
   pm = SystemAffinityMask;
   pmm = 1;
   while (n) 
   {
      pmm = pmm << 1;
      n--;
   }
   ProcessAffinityMask = pmm;
   rc = SetProcessAffinityMask( GetCurrentProcess(), ProcessAffinityMask );
}
#else

int QueryNumLogicalProcessors( void )
{
/* These are the choices.  We'll check number of processors online. 
   _SC_NPROCESSORS_CONF   Number of processors configured
   _SC_NPROCESSORS_MAX    Max number of processors supported by platform
   _SC_NPROCESSORS_ONLN   Number of processors online
*/
   return (int) sysconf( _SC_NPROCESSORS_ONLN );
}

#ifdef sun
void LockToLogicalProcessor( int n )
// I want to just stick onto one particular core
{
   int rc;
   rc = processor_bind( P_PID   /* All LWPs */,
                        P_MYID  /* The current process, LWP, or task. */,
                        n       /*  processor id */,
                        NULL    /* Don't check previous binding. */
                      );
}
#else
void LockToLogicalProcessor( int n )
// I want to just stick onto one particular core
{
   int rc;
   cpu_set_t cpuset;
   memset( &cpuset, 0, sizeof( cpu_set_t ) );
   CPU_SET( n, &cpuset );
   rc = sched_setaffinity( 0, /* this process */
                          sizeof( cpu_set_t ),
                          &cpuset );
}
#endif
#endif

int whichcpu( void )
{
   int CPUInfo[4] = {0,0,0,0};
   __cpuid( CPUInfo, 1 );
   return ((CPUInfo[1] >> 24) & 0xff);
}

void cpuid( int whichlp )
{
   unsigned int i, j, mask, numbits;
   PLOGICALPROCESSORDATA p;
   int CPUInfo[4] = {0,0,0,0};
   p = &LogicalProcessorMap[whichlp]; 
   LockToLogicalProcessor( whichlp );

   __cpuid(CPUInfo, 0);
   p->nLargestStandardFunctionNumber = CPUInfo[0];

   // Get the information associated with each valid Id
   for (i=0; i <= p->nLargestStandardFunctionNumber; ++i)
   {
      __cpuid( CPUInfo, i );
      // Interpret CPU feature information.
      if  (i == 1)
      {
         // Some of the bits of LocalApicId represent the CPU core 
         // within a processor and other bits represent the processor ID. 
         p->nLocalApicId = (CPUInfo[1] >> 24) & 0xff;
         p->HTT = (CPUInfo[3] >> 28) & 0x1; 
         // recalculate later after 0x80000008
         p->nLogicalProcessorCount = (CPUInfo[1] >> 16) & 0x0FF; 
      }
   }

   // Calling __cpuid with 0x80000000 as the InfoType argument
   // gets the number of valid extended IDs.
   __cpuid( CPUInfo, 0x80000000 );
   p->nLargestExtendedFunctionNumber = CPUInfo[0];
 
   // Get the information associated with each extended ID.
   for (i=0x80000000; i<=p->nLargestExtendedFunctionNumber; ++i)
   {
      __cpuid( CPUInfo, i );
      if  (i == 0x80000008)
      {
         p->nApicIdCoreIdSize = (CPUInfo[2] >> 12) & 0xF;
         p->nNC = (CPUInfo[2]) & 0x0FF;
      }
   }
   // MNC
   // A value of zero for ApicIdCoreIdSize indicates that MNC is derived by this
   // legacy formula: MNC = NC + 1
   // A non-zero value of ApicIdCoreIdSize means that MNC is 2^ApicIdCoreIdSize  

   if (p->nApicIdCoreIdSize)
   {
      p->nMNC = 2;
      for (j = p->nApicIdCoreIdSize-1; j>0; j--)
         p->nMNC = p->nMNC * 2;
   }
   else
   {
      p->nMNC = p->nNC + 1;
   }
   // If HTT==0, then LogicalProcessorCount is reserved, and the CPU contains 
   // one CPU core and the CPU core is single-threaded.
   // If HTT==1 and CmpLegacy==1, LogicalProcessorCount represents the number of
   // CPU cores per processor, where each CPU core is single-threaded.  If HTT==1
   // and CmpLegacy==0, then LogicalProcessorCount is the number of threads per
   // processor, which is the number of cores times the number of threads per core.
   // The number of cores is NC+1.
    
   p->nCPUCoresperProcessor = p->nNC + 1;
   p->nThreadsperCPUCore = ( p->HTT==0 ? 1 :
                              ( p->CmpLegacy==1 ? 1 : 
                                p->nLogicalProcessorCount / p->nCPUCoresperProcessor 
                              )
                           ); 

   // Calculate a mask for the core IDs
   mask = 1;
   numbits = 1;
   if (p->nApicIdCoreIdSize)
   {
      numbits = p->nApicIdCoreIdSize;
      for (j = p->nApicIdCoreIdSize; j>1; j--)
         mask = (mask << 1) + 1;
   }
   p->nProcId = p->nLocalApicId & ~mask;
   p->nProcId = p->nProcId >> (numbits);
   p->nCoreId = p->nLocalApicId & mask;
    
   // Display all the information.
   /* if  (more && p->nLargestStandardFunctionNumber >= 1) */
   /* { */
   /*    printf( "Local APIC Id= %d\n", p->nLocalApicId ); */
   /*    printf( "nApicIdCoreIdSize= %d\n", p->nApicIdCoreIdSize ); */
   /*    printf( "HTT = 0x%x\n", p->HTT );  */
   /*    if (p->HTT!=0) */
   /*       printf( "nLogicalProcessorCount = 0x%x\n", p->nLogicalProcessorCount );  */
   /*    printf( "CmpLegacy = 0x%x\n", p->CmpLegacy ); */
   /*    printf( "nNC = 0x%x\n", p->nNC ); */
   /*    printf( "nMNC = 0x%x\n", p->nMNC );  */
   /*    printf( "nProcId = 0x%x\n", p->nProcId ); */
   /*    printf( "nCoreId = 0x%x\n", p->nCoreId );  */
   /*    printf( "nCPUCoresperProcessor = 0x%x\n", p->nCPUCoresperProcessor );  */
   /*    printf( "nThreadsperCPUCore = 0x%x\n\n", p->nThreadsperCPUCore );  */
   /* } */
}




//int main( int argc, char * argv[] )
int getPhysicalProCount()
{
   int nlp, num_processors, i, j, current;
   /* if (argc>1 && !strcmp( argv[1], "-more" )) */
   /*    more = 1; */
   /* else */
   /*    printf( "( %s -more gives you more details. )\n\n", argv[0] ); */
   memset( (void *) &LogicalProcessorMap, 0, sizeof( LogicalProcessorMap ) );
   memset( (void *) &PhysProcIds, 0, sizeof( PhysProcIds ) );
   nlp = QueryNumLogicalProcessors();
   current = whichcpu();
   for ( i = 0; i < nlp; i++ )
      cpuid( i );
   LockToLogicalProcessor( current );
   num_processors = 0;
   for ( i = 0; i < nlp; i++ )
      PhysProcIds[LogicalProcessorMap[i].nProcId]++;
   for ( i = 0; i < (MAX_NUMBER_OF_PHYSICAL_PROCESSORS+MAX_NUMBER_OF_IOAPICS); i++ )
      if (PhysProcIds[i]) 
         num_processors++;      

   /* for ( i = 0; i < (MAX_NUMBER_OF_PHYSICAL_PROCESSORS+MAX_NUMBER_OF_IOAPICS); i++ ) */
   /* { */
   /*    if (PhysProcIds[i]) */
   /*    { */
   /*       printf( "Physical Processor ID %i has %i cores \n", i, PhysProcIds[i] ); */
   /*       printf( "as logical processors " ); */
   /*       for ( j = 0; j < nlp; j++ ) */
   /*          if (LogicalProcessorMap[j].nProcId == i) */
   /*             printf( "%i ", j ); */
   /*       printf( "\n" ); */
   /*    } */
   /* } */
   /* printf( "\n" ); */

   /* printf( "Number of active logical processors: %d\n", nlp ); */
   /* printf( "Number of active physical processors: %d\n", num_processors ); */
   /* printf( "Number of cores per processor: %d\n", LogicalProcessorMap[0].nCPUCoresperProcessor ); */
   /* printf( "Number of threads per processor core: %d\n", LogicalProcessorMap[0].nThreadsperCPUCore ); */
  
   //   return( 0 ) ;
   std::cout << "detected " << num_processors << " physical processors." << std::endl;
   return num_processors;
}
  
  //  const int Perf_Thread_Count = 4; //Utility::getProcElementCount();
  //const int Perf_Thread_Count = Utility::getProcElementCount();
  const int Perf_Thread_Count = getPhysicalProCount();

// #ifdef __WIN32__

// #define cpuid(func,a,b,c,d)\
//   asm {\
// 	 moveax, func\
//     cpuid\
//     mova, eax\
//     movb, ebx\
//     movc, ecx\
//     movd, edx\
//     }

// #else

// #define cpuid(func,ax,bx,cx,dx)\
//   __asm__ __volatile__ ("cpuid":\
//                         "=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func)\
// );
// #endif

  //NOTE: this has been tested on AMD and Intel -- there is some confusion about
  //the information that the 'cpuid' instruction may return with AMD -- if
  //the memory allocation fails because it isn't a power of 2, this is a place to check
long
Utility::getL1CacheLineSize(){
  //  int ax, bx ,cx, dx;
  int regs[4];
  //mycpuid(0x1, regs); //ax, bx, cx, dx);
  mycpuid(regs, 0x1);
  long retval = ((0xFFFF & regs[1]) >> 8) * 8;
  std::cout << "detected l1 data line size: " << retval << std::endl;
  return retval;
}

// int
// Utility::getProcElementCount(){
  // std::ifstream f("/proc/cpuinfo");
  // std::string line;
  // int count = 0;
  // while (getline(f, line)){
  // if (line.find("processor") != std::string::npos) 
  //   ++count;
  // }
  // return count;
  //return std::thread::hardware_concurrency(); On my system at least, this is returning the count including multi-thread cores (i.e. Hyperthreading).  I want only physical cores, as we seem to be compute bound in the areas we're multithreading

  //}
}
