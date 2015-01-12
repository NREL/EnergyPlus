#ifndef ATLAS_ASM_H
   #define ATLAS_ASM_H

#ifndef Mjoin
   #define Mjoin(pre, nam) my_join(pre, nam)
   #define my_join(pre, nam) pre ## nam
#endif

#if (defined(ATL_OS_Win64) && !defined(ATL_USE64BITS)) || \
    defined(ATL_OS_Win9x) || defined(ATL_OS_OSX) || defined(ATL_OS_WinNT)
   #define ATL_asmdecor(nam) Mjoin(_,nam)
#elif defined(ATL_OS_AIX) && defined(ATL_GAS_PPC)
   #define ATL_asmdecor(nam) Mjoin(.,nam)
#elif !defined(ATL_OS_OSX) && defined(ATL_GAS_PPC) && defined(ATL_USE64BITS)
   #define ATL_asmdecor(nam) Mjoin(.,nam)
#else
   #define  ATL_asmdecor(nam) nam
#endif

#ifdef ATL_GAS_PARISC
   #ifdef ATL_OS_HPUX
      #define ATL_HPUX_PARISC
   #else
      #define ATL_LINUX_PARISC
   #endif
#endif

#ifdef ATL_GAS_PPC
   #ifdef ATL_OS_OSX
      #define ATL_AS_OSX_PPC
   #elif defined(ATL_OS_AIX)
      #define ATL_AS_AIX_PPC
   #else
      #define ATL_GAS_LINUX_PPC
   #endif
#endif

#if defined(ATL_GAS_LINUX_PPC) || defined(ATL_AS_AIX_PPC)

   #define r0 0
   #define f0 0
   #define v0 0
   #define r1 1
   #define f1 1
   #define v1 1
   #define r2 2
   #define f2 2
   #define v2 2
   #define r3 3
   #define f3 3
   #define v3 3
   #define r4 4
   #define f4 4
   #define v4 4
   #define r5 5
   #define f5 5
   #define v5 5
   #define r6 6
   #define f6 6
   #define v6 6
   #define r7 7
   #define f7 7
   #define v7 7
   #define r8 8
   #define f8 8
   #define v8 8
   #define r9 9
   #define f9 9
   #define v9 9
   #define r10 10
   #define f10 10
   #define v10 10
   #define r11 11
   #define f11 11
   #define v11 11
   #define r12 12
   #define f12 12
   #define v12 12
   #define r13 13
   #define f13 13
   #define v13 13
   #define r14 14
   #define f14 14
   #define v14 14
   #define r15 15
   #define f15 15
   #define v15 15
   #define r16 16
   #define f16 16
   #define v16 16
   #define r17 17
   #define f17 17
   #define v17 17
   #define r18 18
   #define f18 18
   #define v18 18
   #define r19 19
   #define f19 19
   #define v19 19
   #define r20 20
   #define f20 20
   #define v20 20
   #define r21 21
   #define f21 21
   #define v21 21
   #define r22 22
   #define f22 22
   #define v22 22
   #define r23 23
   #define f23 23
   #define v23 23
   #define r24 24
   #define f24 24
   #define v24 24
   #define r25 25
   #define f25 25
   #define v25 25
   #define r26 26
   #define f26 26
   #define v26 26
   #define r27 27
   #define f27 27
   #define v27 27
   #define r28 28
   #define f28 28
   #define v28 28
   #define r29 29
   #define f29 29
   #define v29 29
   #define r30 30
   #define f30 30
   #define v30 30
   #define r31 31
   #define f31 31
   #define v31 31
   #define v32 32
   #define v33 33
   #define v34 34
   #define v35 35
   #define v36 36
   #define v37 37
   #define v38 38
   #define v39 39
   #define v40 40
   #define v41 41
   #define v42 42
   #define v43 43
   #define v44 44
   #define v45 45
   #define v46 46
   #define v47 47
   #define v48 48
   #define v49 49
   #define v50 50
   #define v51 51
   #define v52 52
   #define v53 53
   #define v54 54
   #define v55 55
   #define v56 56
   #define v57 57
   #define v58 58
   #define v59 59
   #define v60 60
   #define v61 61
   #define v62 62
   #define v63 63
   #define cr0 0
   #define cr1 1
   #define cr2 2
   #define cr3 3
   #define cr4 4
   #define cr5 5
   #define cr6 6
   #define cr7 7


#endif

#ifdef ATL_OS_OSX
   #define ALIGN2 .align 1
   #define ALIGN4 .align 2
   #define ALIGN8 .align 3
   #define ALIGN16 .align 4
   #define ALIGN32 .align 5
   #define ALIGN64 .align 6
   #define ALIGN128 .align 7
   #define global globl
   #define local  locl
#else
   #define ALIGN2 .align 2
   #define ALIGN4 .align 4
   #define ALIGN8 .align 8
   #define ALIGN16 .align 16
   #define ALIGN32 .align 32
   #define ALIGN64 .align 64
   #define ALIGN128 .align 128
#endif

#if defined(ATL_SSE1) && !defined(ATL_3DNow)
   #define prefetchw prefetchnta
#endif
/*
 * Solaris doesn't allow division in integer expressions in assembly, but
 * many x86 kernels need to do $MB/mu; we work around this insanity with
 * this kludge
 */
#if defined(ATL_DIV_NUM) && defined(ATL_DIV_DEN)
   #if (ATL_DIV_NUM/ATL_DIV_DEN) == 0
      #define ATL_DivAns 0
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 1
      #define ATL_DivAns 1
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 2
      #define ATL_DivAns 2
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 3
      #define ATL_DivAns 3
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 4
      #define ATL_DivAns 4
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 5
      #define ATL_DivAns 5
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 6
      #define ATL_DivAns 6
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 7
      #define ATL_DivAns 7
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 8
      #define ATL_DivAns 8
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 9
      #define ATL_DivAns 9
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 10
      #define ATL_DivAns 10
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 11
      #define ATL_DivAns 11
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 12
      #define ATL_DivAns 12
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 13
      #define ATL_DivAns 13
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 14
      #define ATL_DivAns 14
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 15
      #define ATL_DivAns 15
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 16
      #define ATL_DivAns 16
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 17
      #define ATL_DivAns 17
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 18
      #define ATL_DivAns 18
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 19
      #define ATL_DivAns 19
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 20
      #define ATL_DivAns 20
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 21
      #define ATL_DivAns 21
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 22
      #define ATL_DivAns 22
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 23
      #define ATL_DivAns 23
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 24
      #define ATL_DivAns 24
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 25
      #define ATL_DivAns 25
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 26
      #define ATL_DivAns 26
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 27
      #define ATL_DivAns 27
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 28
      #define ATL_DivAns 28
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 29
      #define ATL_DivAns 29
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 30
      #define ATL_DivAns 30
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 31
      #define ATL_DivAns 31
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 32
      #define ATL_DivAns 32
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 33
      #define ATL_DivAns 33
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 34
      #define ATL_DivAns 34
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 35
      #define ATL_DivAns 35
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 36
      #define ATL_DivAns 36
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 37
      #define ATL_DivAns 37
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 38
      #define ATL_DivAns 38
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 39
      #define ATL_DivAns 39
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 40
      #define ATL_DivAns 40
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 41
      #define ATL_DivAns 41
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 42
      #define ATL_DivAns 42
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 43
      #define ATL_DivAns 43
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 44
      #define ATL_DivAns 44
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 45
      #define ATL_DivAns 45
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 46
      #define ATL_DivAns 46
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 47
      #define ATL_DivAns 47
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 48
      #define ATL_DivAns 48
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 49
      #define ATL_DivAns 49
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 50
      #define ATL_DivAns 50
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 51
      #define ATL_DivAns 51
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 52
      #define ATL_DivAns 52
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 53
      #define ATL_DivAns 53
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 54
      #define ATL_DivAns 54
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 55
      #define ATL_DivAns 55
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 56
      #define ATL_DivAns 56
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 57
      #define ATL_DivAns 57
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 58
      #define ATL_DivAns 58
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 59
      #define ATL_DivAns 59
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 60
      #define ATL_DivAns 60
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 61
      #define ATL_DivAns 61
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 62
      #define ATL_DivAns 62
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 63
      #define ATL_DivAns 63
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 64
      #define ATL_DivAns 64
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 65
      #define ATL_DivAns 65
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 66
      #define ATL_DivAns 66
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 67
      #define ATL_DivAns 67
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 68
      #define ATL_DivAns 68
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 69
      #define ATL_DivAns 69
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 70
      #define ATL_DivAns 70
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 71
      #define ATL_DivAns 71
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 72
      #define ATL_DivAns 72
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 73
      #define ATL_DivAns 73
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 74
      #define ATL_DivAns 74
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 75
      #define ATL_DivAns 75
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 76
      #define ATL_DivAns 76
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 77
      #define ATL_DivAns 77
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 78
      #define ATL_DivAns 78
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 79
      #define ATL_DivAns 79
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 80
      #define ATL_DivAns 80
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 81
      #define ATL_DivAns 81
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 82
      #define ATL_DivAns 82
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 83
      #define ATL_DivAns 83
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 84
      #define ATL_DivAns 84
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 85
      #define ATL_DivAns 85
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 86
      #define ATL_DivAns 86
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 87
      #define ATL_DivAns 87
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 88
      #define ATL_DivAns 88
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 89
      #define ATL_DivAns 89
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 90
      #define ATL_DivAns 90
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 91
      #define ATL_DivAns 91
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 92
      #define ATL_DivAns 92
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 93
      #define ATL_DivAns 93
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 94
      #define ATL_DivAns 94
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 95
      #define ATL_DivAns 95
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 96
      #define ATL_DivAns 96
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 97
      #define ATL_DivAns 97
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 98
      #define ATL_DivAns 98
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 99
      #define ATL_DivAns 99
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 100
      #define ATL_DivAns 100
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 101
      #define ATL_DivAns 101
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 102
      #define ATL_DivAns 102
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 103
      #define ATL_DivAns 103
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 104
      #define ATL_DivAns 104
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 105
      #define ATL_DivAns 105
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 106
      #define ATL_DivAns 106
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 107
      #define ATL_DivAns 107
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 108
      #define ATL_DivAns 108
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 109
      #define ATL_DivAns 109
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 110
      #define ATL_DivAns 110
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 111
      #define ATL_DivAns 111
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 112
      #define ATL_DivAns 112
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 113
      #define ATL_DivAns 113
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 114
      #define ATL_DivAns 114
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 115
      #define ATL_DivAns 115
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 116
      #define ATL_DivAns 116
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 117
      #define ATL_DivAns 117
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 118
      #define ATL_DivAns 118
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 119
      #define ATL_DivAns 119
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 120
      #define ATL_DivAns 120
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 121
      #define ATL_DivAns 121
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 122
      #define ATL_DivAns 122
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 123
      #define ATL_DivAns 123
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 124
      #define ATL_DivAns 124
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 125
      #define ATL_DivAns 125
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 126
      #define ATL_DivAns 126
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 127
      #define ATL_DivAns 127
   #elif (ATL_DIV_NUM/ATL_DIV_DEN) == 128
      #define ATL_DivAns 128
   #endif
#endif

/*
 * For GNU/Linux, set no-execute bit for all ATLAS assembly
 */
#if defined(ATL_OS_Linux) && defined(__ELF__) && defined(__GNUC__) && \
    defined(ATL_SSE1)
.section .note.GNU-stack,"",%progbits
#endif

#endif
