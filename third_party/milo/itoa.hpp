#ifndef itoa_hpp_INCLUDED
#define itoa_hpp_INCLUDED

// SSE2 implementation according to http://0x80.pl/articles/sse-itoa.html
// Modifications: (1) fix incorrect digits (2) accept all ranges (3) write to user provided buffer.

#include <cassert>
#include <emmintrin.h>
#include <stdint.h>

#ifdef _MSC_VER
#include <intrin.h>
#endif

#ifdef _MSC_VER
#define ALIGN_PRE __declspec(align(16))
#define ALIGN_SUF
#else
#define ALIGN_PRE
#define ALIGN_SUF  __attribute__ ((aligned(16)))
#endif

const char gDigitsLut[200] = {
    '0','0','0','1','0','2','0','3','0','4','0','5','0','6','0','7','0','8','0','9',
    '1','0','1','1','1','2','1','3','1','4','1','5','1','6','1','7','1','8','1','9',
    '2','0','2','1','2','2','2','3','2','4','2','5','2','6','2','7','2','8','2','9',
    '3','0','3','1','3','2','3','3','3','4','3','5','3','6','3','7','3','8','3','9',
    '4','0','4','1','4','2','4','3','4','4','4','5','4','6','4','7','4','8','4','9',
    '5','0','5','1','5','2','5','3','5','4','5','5','5','6','5','7','5','8','5','9',
    '6','0','6','1','6','2','6','3','6','4','6','5','6','6','6','7','6','8','6','9',
    '7','0','7','1','7','2','7','3','7','4','7','5','7','6','7','7','7','8','7','9',
    '8','0','8','1','8','2','8','3','8','4','8','5','8','6','8','7','8','8','8','9',
    '9','0','9','1','9','2','9','3','9','4','9','5','9','6','9','7','9','8','9','9'
};

const uint32_t kDiv10000 = 0xd1b71759;
ALIGN_PRE const uint32_t kDiv10000Vector[4] ALIGN_SUF = { kDiv10000, kDiv10000, kDiv10000, kDiv10000 };
ALIGN_PRE const uint32_t k10000Vector[4] ALIGN_SUF = { 10000, 10000, 10000, 10000 };
ALIGN_PRE const uint16_t kDivPowersVector[8] ALIGN_SUF = { 8389, 5243, 13108, 32768, 8389, 5243, 13108, 32768 }; // 10^3, 10^2, 10^1, 10^0
ALIGN_PRE const uint16_t kShiftPowersVector[8] ALIGN_SUF = {
    1 << (16 - (23 + 2 - 16)),
    1 << (16 - (19 + 2 - 16)),
    1 << (16 - 1 - 2),
    1 << (15),
    1 << (16 - (23 + 2 - 16)),
    1 << (16 - (19 + 2 - 16)),
    1 << (16 - 1 - 2),
    1 << (15)
};
ALIGN_PRE const uint16_t k10Vector[8] ALIGN_SUF = { 10, 10, 10, 10, 10, 10, 10, 10 };
ALIGN_PRE const char kAsciiZero[16] ALIGN_SUF = { '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0' };

inline __m128i Convert8DigitsSSE2(uint32_t value) {
    assert(value <= 99999999);

    // abcd, efgh = abcdefgh divmod 10000 
    const __m128i abcdefgh = _mm_cvtsi32_si128(value);
    const __m128i abcd = _mm_srli_epi64(_mm_mul_epu32(abcdefgh, reinterpret_cast<const __m128i*>(kDiv10000Vector)[0]), 45);
    const __m128i efgh = _mm_sub_epi32(abcdefgh, _mm_mul_epu32(abcd, reinterpret_cast<const __m128i*>(k10000Vector)[0]));

    // v1 = [ abcd, efgh, 0, 0, 0, 0, 0, 0 ]
    const __m128i v1 = _mm_unpacklo_epi16(abcd, efgh);

    // v1a = v1 * 4 = [ abcd * 4, efgh * 4, 0, 0, 0, 0, 0, 0 ]
    const __m128i v1a = _mm_slli_epi64(v1, 2);

    // v2 = [ abcd * 4, abcd * 4, abcd * 4, abcd * 4, efgh * 4, efgh * 4, efgh * 4, efgh * 4 ]
    const __m128i v2a = _mm_unpacklo_epi16(v1a, v1a);
    const __m128i v2 = _mm_unpacklo_epi32(v2a, v2a);

    // v4 = v2 div 10^3, 10^2, 10^1, 10^0 = [ a, ab, abc, abcd, e, ef, efg, efgh ]
    const __m128i v3 = _mm_mulhi_epu16(v2, reinterpret_cast<const __m128i*>(kDivPowersVector)[0]);
    const __m128i v4 = _mm_mulhi_epu16(v3, reinterpret_cast<const __m128i*>(kShiftPowersVector)[0]);

    // v5 = v4 * 10 = [ a0, ab0, abc0, abcd0, e0, ef0, efg0, efgh0 ]
    const __m128i v5 = _mm_mullo_epi16(v4, reinterpret_cast<const __m128i*>(k10Vector)[0]);

    // v6 = v5 << 16 = [ 0, a0, ab0, abc0, 0, e0, ef0, efg0 ]
    const __m128i v6 = _mm_slli_epi64(v5, 16);

    // v7 = v4 - v6 = { a, b, c, d, e, f, g, h }
    const __m128i v7 = _mm_sub_epi16(v4, v6);

    return v7;
}

inline __m128i ShiftDigits_SSE2(__m128i a, unsigned digit) {
    assert(digit <= 8);
    switch (digit) {
        case 0: return a;
        case 1: return _mm_srli_si128(a, 1);
        case 2: return _mm_srli_si128(a, 2);
        case 3: return _mm_srli_si128(a, 3);
        case 4: return _mm_srli_si128(a, 4);
        case 5: return _mm_srli_si128(a, 5);
        case 6: return _mm_srli_si128(a, 6);
        case 7: return _mm_srli_si128(a, 7);
        case 8: return _mm_srli_si128(a, 8);
    }
    return a; // should not execute here.
}

inline void u32toa(uint32_t value, char* buffer) {
    if (value < 10000) {
        const uint32_t d1 = (value / 100) << 1;
        const uint32_t d2 = (value % 100) << 1;
        
        if (value >= 1000)
            *buffer++ = gDigitsLut[d1];
        if (value >= 100)
            *buffer++ = gDigitsLut[d1 + 1];
        if (value >= 10)
            *buffer++ = gDigitsLut[d2];
        *buffer++ = gDigitsLut[d2 + 1];
        *buffer++ = '\0';
    }
    else if (value < 100000000) {
        // Experiment shows that this case SSE2 is slower
#if 0
        const __m128i a = Convert8DigitsSSE2(value);
        
        // Convert to bytes, add '0'
        const __m128i va = _mm_add_epi8(_mm_packus_epi16(a, _mm_setzero_si128()), reinterpret_cast<const __m128i*>(kAsciiZero)[0]);

        // Count number of digit
        const unsigned mask = _mm_movemask_epi8(_mm_cmpeq_epi8(va, reinterpret_cast<const __m128i*>(kAsciiZero)[0]));
        unsigned long digit;
#ifdef _MSC_VER
        _BitScanForward(&digit, ~mask | 0x8000);
#else
        digit = __builtin_ctz(~mask | 0x8000);
#endif

        // Shift digits to the beginning
        __m128i result = ShiftDigits_SSE2(va, digit);
        //__m128i result = _mm_srl_epi64(va, _mm_cvtsi32_si128(digit * 8));
        _mm_storel_epi64(reinterpret_cast<__m128i*>(buffer), result);
        buffer[8 - digit] = '\0';
#else
        // value = bbbbcccc
        const uint32_t b = value / 10000;
        const uint32_t c = value % 10000;

        const uint32_t d1 = (b / 100) << 1;
        const uint32_t d2 = (b % 100) << 1;

        const uint32_t d3 = (c / 100) << 1;
        const uint32_t d4 = (c % 100) << 1;

        if (value >= 10000000)
            *buffer++ = gDigitsLut[d1];
        if (value >= 1000000)
            *buffer++ = gDigitsLut[d1 + 1];
        if (value >= 100000)
            *buffer++ = gDigitsLut[d2];
        *buffer++ = gDigitsLut[d2 + 1];

        *buffer++ = gDigitsLut[d3];
        *buffer++ = gDigitsLut[d3 + 1];
        *buffer++ = gDigitsLut[d4];
        *buffer++ = gDigitsLut[d4 + 1];
        *buffer++ = '\0';
#endif
    }
    else {
        // value = aabbbbbbbb in decimal
        
        const uint32_t a = value / 100000000; // 1 to 42
        value %= 100000000;
        
        if (a >= 10) {
            const unsigned i = a << 1;
            *buffer++ = gDigitsLut[i];
            *buffer++ = gDigitsLut[i + 1];
        }
        else
            *buffer++ = '0' + static_cast<char>(a);

        const __m128i b = Convert8DigitsSSE2(value);
        const __m128i ba = _mm_add_epi8(_mm_packus_epi16(_mm_setzero_si128(), b), reinterpret_cast<const __m128i*>(kAsciiZero)[0]);
        const __m128i result = _mm_srli_si128(ba, 8);
        _mm_storel_epi64(reinterpret_cast<__m128i*>(buffer), result);
        buffer[8] = '\0';
    }
}

void i32toa(int32_t value, char* buffer) {
    uint32_t u = static_cast<uint32_t>(value);
    if (value < 0) {
        *buffer++ = '-';
        u = ~u + 1;
    }
    u32toa(u, buffer);
}

inline void u64toa(uint64_t value, char* buffer) {
    if (value < 100000000) {
        uint32_t v = static_cast<uint32_t>(value);
        if (v < 10000) {
            const uint32_t d1 = (v / 100) << 1;
            const uint32_t d2 = (v % 100) << 1;
            
            if (v >= 1000)
                *buffer++ = gDigitsLut[d1];
            if (v >= 100)
                *buffer++ = gDigitsLut[d1 + 1];
            if (v >= 10)
                *buffer++ = gDigitsLut[d2];
            *buffer++ = gDigitsLut[d2 + 1];
            *buffer++ = '\0';
        }
        else {
            // Experiment shows that this case SSE2 is slower
#if 0
            const __m128i a = Convert8DigitsSSE2(v);
        
            // Convert to bytes, add '0'
            const __m128i va = _mm_add_epi8(_mm_packus_epi16(a, _mm_setzero_si128()), reinterpret_cast<const __m128i*>(kAsciiZero)[0]);

            // Count number of digit
            const unsigned mask = _mm_movemask_epi8(_mm_cmpeq_epi8(va, reinterpret_cast<const __m128i*>(kAsciiZero)[0]));
            unsigned long digit;
#ifdef _MSC_VER
            _BitScanForward(&digit, ~mask | 0x8000);
#else
            digit = __builtin_ctz(~mask | 0x8000);
#endif

            // Shift digits to the beginning
            __m128i result = ShiftDigits_SSE2(va, digit);
            _mm_storel_epi64(reinterpret_cast<__m128i*>(buffer), result);
            buffer[8 - digit] = '\0';
#else
            // value = bbbbcccc
            const uint32_t b = v / 10000;
            const uint32_t c = v % 10000;

            const uint32_t d1 = (b / 100) << 1;
            const uint32_t d2 = (b % 100) << 1;

            const uint32_t d3 = (c / 100) << 1;
            const uint32_t d4 = (c % 100) << 1;

            if (value >= 10000000)
                *buffer++ = gDigitsLut[d1];
            if (value >= 1000000)
                *buffer++ = gDigitsLut[d1 + 1];
            if (value >= 100000)
                *buffer++ = gDigitsLut[d2];
            *buffer++ = gDigitsLut[d2 + 1];

            *buffer++ = gDigitsLut[d3];
            *buffer++ = gDigitsLut[d3 + 1];
            *buffer++ = gDigitsLut[d4];
            *buffer++ = gDigitsLut[d4 + 1];
            *buffer++ = '\0';
#endif
        }
    }
    else if (value < 10000000000000000) {
        const uint32_t v0 = static_cast<uint32_t>(value / 100000000);
        const uint32_t v1 = static_cast<uint32_t>(value % 100000000);

        const __m128i a0 = Convert8DigitsSSE2(v0);
        const __m128i a1 = Convert8DigitsSSE2(v1);

        // Convert to bytes, add '0'
        const __m128i va = _mm_add_epi8(_mm_packus_epi16(a0, a1), reinterpret_cast<const __m128i*>(kAsciiZero)[0]);

        // Count number of digit
        const unsigned mask = _mm_movemask_epi8(_mm_cmpeq_epi8(va, reinterpret_cast<const __m128i*>(kAsciiZero)[0]));
#ifdef _MSC_VER
        unsigned long digit;
        _BitScanForward(&digit, ~mask | 0x8000);
#else
        unsigned digit = __builtin_ctz(~mask | 0x8000);
#endif

        // Shift digits to the beginning
        __m128i result = ShiftDigits_SSE2(va, digit);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(buffer), result);
        buffer[16 - digit] = '\0';
    }
    else {
        const uint32_t a = static_cast<uint32_t>(value / 10000000000000000); // 1 to 1844
        value %= 10000000000000000;
        
        if (a < 10)
            *buffer++ = '0' + static_cast<char>(a);
        else if (a < 100) {
            const uint32_t i = a << 1;
            *buffer++ = gDigitsLut[i];
            *buffer++ = gDigitsLut[i + 1];
        }
        else if (a < 1000) {
            *buffer++ = '0' + static_cast<char>(a / 100);
            
            const uint32_t i = (a % 100) << 1;
            *buffer++ = gDigitsLut[i];
            *buffer++ = gDigitsLut[i + 1];
        }
        else {
            const uint32_t i = (a / 100) << 1;
            const uint32_t j = (a % 100) << 1;
            *buffer++ = gDigitsLut[i];
            *buffer++ = gDigitsLut[i + 1];
            *buffer++ = gDigitsLut[j];
            *buffer++ = gDigitsLut[j + 1];
        }
        
        const uint32_t v0 = static_cast<uint32_t>(value / 100000000);
        const uint32_t v1 = static_cast<uint32_t>(value % 100000000);
        
        const __m128i a0 = Convert8DigitsSSE2(v0);
        const __m128i a1 = Convert8DigitsSSE2(v1);

        // Convert to bytes, add '0'
        const __m128i va = _mm_add_epi8(_mm_packus_epi16(a0, a1), reinterpret_cast<const __m128i*>(kAsciiZero)[0]);
        _mm_storeu_si128(reinterpret_cast<__m128i*>(buffer), va);
        buffer[16] = '\0';
    }
}

void i64toa(int64_t value, char* buffer) {
    uint64_t u = static_cast<uint64_t>(value);
    if (value < 0) {
        *buffer++ = '-';
        u = ~u + 1;
    }
    u64toa(u, buffer);
}

#endif // itoa_hpp_INCLUDED
