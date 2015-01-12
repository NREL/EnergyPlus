#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <windows.h>
#ifndef CREATE_SUSPENDED
   #define CREATE_SUSPENDED 0x00000004
#endif
#ifndef WAIT_FAILED
   #define WAIT_FAILED (~0)
#endif


static int rank=0;

void *DumbTest(void *vp)
{
   ExitThread((DWORD)(vp));
}
int main(int nargs, char **args)
{
   HANDLE thrH;
   DWORD thrID;
   void *vp, *vpret;

   if (nargs > 1)
      rank = atoi(args[1]);
   vp = (void*) &thrID;
   vpret = NULL;
   thrH = CreateThread(NULL, 0, DumbTest, vp, CREATE_SUSPENDED, &thrID);
   assert(thrH);
   assert(SetThreadAffinityMask(thrH, (((long long)1)<<rank)));
   assert(ResumeThread(thrH) == 1);
   assert(WaitForSingleObject(thrH, INFINITE) != WAIT_FAILED);
   assert(CloseHandle(thrH));

   printf("SUCCESS rank %d\n", rank);
   exit(0);
}
