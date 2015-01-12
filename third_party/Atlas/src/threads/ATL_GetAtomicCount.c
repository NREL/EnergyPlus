int ATL_GetAtomicCount(void *vp)
{
   volatile int *ip = vp;
   return(ip[32]);
}
