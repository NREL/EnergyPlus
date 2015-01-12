#!/bin/sh
#
# USAGE: <topdir> <arch> <mach> <cc> <mcc>
# Makes atlas architectural default file from probe results.
# <topdir> : path to your ATLAS subdir (TOPdir from Make.inc)
# <arch> : ARCH from Make.inc
# <mach> : Machine name (usally <arch> without OS)
# <cc>   : Default C compiler
# <mcc>  : C compiler used for matmul compilation
#
TOPdir=$1
arch=$2
mach=$3
dcc=$4
mcc=$5
#
defdir=$1/CONFIG/ARCHS/
mmdir=$1/tune/blas/gemm/$arch/res
incdir=$1/include/$arch
#
cd $defdir ; rm -rf $mach
cd $defdir ; mkdir $mach
cd $defdir/$mach ; mkdir $dc
cd $TOPdir/CONFIG/ARCHS ; mkdir $arch
cp $TOPdir/tune/sysinfo/$arch/res/?MULADD  $defdir/.
cp $TOPdir/tune/sysinfo/$arch/res/L1CacheSize  $defdir/.
cp $mmdir/?MMRES.sum  $defdir/.
cp $mmdir/?eMMRES.sum  $defdir/.
cp $mmdir/?MMKSSE.sum  $defdir/.
cp $mmdir/?gMMRES.sum  $defdir/.
cp $mmdir/?guMMRES.sum  $defdir/.
cp $mmdir/?gMMRES  $defdir/.
cp $mmdir/?Clean[M,N,K]  $defdir/.
cp $mmdir/?NB  $defdir/.
cp $mmdir/?NCNB  $defdir/.
cp $mmdir/?best[N,T][N,T]_*  $defdir/.
cp $mmdir/?uMMRES $defdir/.
cp $mmdir/?uClean[M,N,K] $defdir/.
cp $mmdir/?uClean[M,N,K]F $defdir/.
cp $incdir/atlas_cacheedge.h  $defdir/.
cp $incdir/atlas_tcacheedge.h  $defdir/.
cp $incdir/atlas_??NKB.h  $defdir/.
cp $incdir/?Xover.h $defdir/.
cp $incdir/atlas_?sysinfo.h $defdir/.
cp $TOPdir/tune/blas/level3/$arch/res/atlas_?trsmXover.h $defdir/.
cp $TOPdir/tune/blas/gemv/$arch/res/?MV[N,T]K.sum $defdir/.
cp $TOPdir/tune/blas/ger/$arch/res/?R1RES $defdir/.
gcc -o xnegfloat negfloat.c
./xnegfloat $defdir/?uClean[M,N,K] 
./xnegfloat $defdir/?uClean[M,N,K]F 
./xnegfloat $defdir/?uMMRES
./xnegfloat $defdir/?MMRES 
./xnegfloat $defdir/?gMMRES 
./xnegfloat $defdir/?best* 
./xnegfloat $defdir/?Clean[M,N,K]
./xnegfloat $defdir/?MULADD
./xnegfloat $defdir/?MVRES
./xnegfloat $defdir/?R1RES
rm -f ./xnegfloat
# vi $defdir/?uClean[M,N,K] $defdir/?uClean[M,N,K]F $defdir/?uMMRES
