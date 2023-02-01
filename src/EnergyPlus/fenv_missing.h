#pragma once

#include <cfenv>

// clang-format off
#ifndef HAVE_FEENABLEEXCEPT
#  if defined(__APPLE__) && defined(__MACH__)
#    if defined __i386__ || defined __x86_64__

    // Public domain polyfill for feenableexcept on OS X
    // http://www-personal.umich.edu/~williams/archive/computation/fe-handling-example.c

    inline int feenableexcept(unsigned int excepts)
    {
      static fenv_t fenv;
      unsigned int new_excepts = excepts & FE_ALL_EXCEPT;
      // previous masks
      unsigned int old_excepts;

      if (std::fegetenv(&fenv)) {
        return -1;
      }
      old_excepts = fenv.__control & FE_ALL_EXCEPT;

      // unmask
      fenv.__control &= ~new_excepts;
      fenv.__mxcsr &= ~(new_excepts << 7);

      return std::fesetenv(&fenv) ? -1 : old_excepts;
    }

    inline int fedisableexcept(unsigned int excepts)
    {
      static fenv_t fenv;
      unsigned int new_excepts = excepts & FE_ALL_EXCEPT;
      // all previous masks
      unsigned int old_excepts;

      if (std::fegetenv(&fenv)) {
        return -1;
      }
      old_excepts = fenv.__control & FE_ALL_EXCEPT;

      // mask
      fenv.__control |= new_excepts;
      fenv.__mxcsr |= new_excepts << 7;

      return std::fesetenv(&fenv) ? -1 : old_excepts;
    }

#    elif defined __arm64__

    // On arm**64**, this is different. the fenv_t has two members that are unsigned long long: __fpcr and __fpsr (for status)
    // Meaning there is a bit shift of 8 between the FE_XXX macros and the fpcr
    // Also, you **SET** the bits, not mask them (the opposite of intel)
    //
    // There are also enums that contain the bit shifting, and we can assert this:
    // assert((FE_INEXACT << 8) == __fpcr_trap_inexact);
    // assert((FE_UNDERFLOW << 8) == __fpcr_trap_underflow);
    // assert((FE_OVERFLOW << 8) == __fpcr_trap_overflow);
    // assert((FE_DIVBYZERO << 8) == __fpcr_trap_divbyzero);
    // assert((FE_INVALID << 8) == __fpcr_trap_invalid);
    // assert((FE_FLUSHTOZERO << 8) != __fpcr_flush_to_zero);
    // assert((FE_FLUSHTOZERO << 8) == __fpcr_trap_denormal);
    static constexpr unsigned int FPCR_EXCEPT_SHIFT = 8;

    // These will be the exception flags we use for exception values normalized
    // from both status word and control word.
    // We add EX_ prefix to the names since macOS <math.h> defines OVERFLOW and
    // UNDERFLOW macros.
    static constexpr uint32_t EX_INVALID = 0x1;
    static constexpr uint32_t EX_DIVBYZERO = 0x2;
    static constexpr uint32_t EX_OVERFLOW = 0x4;
    static constexpr uint32_t EX_UNDERFLOW = 0x8;
    static constexpr uint32_t EX_INEXACT = 0x10;
    // __APPLE__ ARM64 has an extra flag that is raised when a denormal is flushed
    // to zero.
    static constexpr uint32_t EX_FLUSHTOZERO = 0x20;

    inline int feenableexcept(unsigned int excepts)
    {
      static fenv_t fenv;
      if (std::fegetenv(&fenv) != 0) {
        return -1;
      }
      const unsigned long long old_fpcr = fenv.__fpcr;
      const unsigned int old_excepts = (old_fpcr >> FPCR_EXCEPT_SHIFT) & unsigned(FE_ALL_EXCEPT);

      // Check the bits passed are valid, and bit shift them
      const unsigned int new_excepts = excepts & unsigned(FE_ALL_EXCEPT);
      const unsigned long long  new_fpcr = new_excepts << FPCR_EXCEPT_SHIFT;

      // Set the new bits
      fenv.__fpcr = fenv.__fpcr | new_fpcr;

      return (std::fesetenv(&fenv) != 0) ? -1 : static_cast<int>(old_excepts);
    }

    inline int fedisableexcept(unsigned int excepts)
    {
      static fenv_t fenv;
      if (std::fegetenv(&fenv) != 0) {
        return -1;
      }

      const unsigned long long old_fpcr = fenv.__fpcr;
      const unsigned int old_excepts = ((old_fpcr >> FPCR_EXCEPT_SHIFT) &  unsigned(FE_ALL_EXCEPT));

      // Check the bits passed are valid, and bit shift them
      const unsigned int new_excepts = excepts &  unsigned(FE_ALL_EXCEPT);
      const unsigned long long  new_fpcr = new_excepts << FPCR_EXCEPT_SHIFT;

      // Clear the bits
      fenv.__fpcr &= ~new_fpcr;

      return std::fesetenv(&fenv) ? -1 : static_cast<int>(old_excepts);
    }

#    endif

#  else
  inline int feenableexcept(unsigned int excepts)
  {
  #pragma STDC FENV_ACCESS ON
    fexcept_t flags;
    /* Save current exception flags. */
    fegetexceptflag(&flags, FE_ALL_EXCEPT);

    feclearexcept(FE_ALL_EXCEPT); /* clear all fp exception conditions */
    return fesetexceptflag(&flags, excepts) != 0 ? -1 : flags; /* set new flags */
  }

  inline int fedisableexcept(unsigned int excepts)
  {
  #pragma STDC FENV_ACCESS ON
    fexcept_t flags;
    /* Save current exception flags. */
    fegetexceptflag(&flags, FE_ALL_EXCEPT);

    feclearexcept(FE_ALL_EXCEPT); /* clear all fp exception conditions */
    return fesetexceptflag(&flags, ~excepts) != 0 ? -1 : flags; /* set new flags */
  }

#  endif
#endif

// clang-format on
