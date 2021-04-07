#include <vector>

#ifndef SimpleArray1D_INCLUDED
#define SimpleArray1D_INCLUDED

#define SIMPLEARRAY
// #define EPVECTOR


#ifdef EPVECTOR
#include <numeric>
#endif

namespace EnergyPlus {

#if defined(SIMPLEARRAY)

template <typename Contained> class SimpleArray1D : private std::vector<Contained>
{
public:
    using std::vector<Contained>::vector;
    using std::vector<Contained>::size;
    using std::vector<Contained>::operator[];
    using std::vector<Contained>::empty;
    using std::vector<Contained>::begin;
    using std::vector<Contained>::end;
    using std::vector<Contained>::cbegin;
    using std::vector<Contained>::cend;
    using std::vector<Contained>::clear;
    using std::vector<Contained>::emplace_back;
    using std::vector<Contained>::push_back;
    using const_iterator = typename std::vector<Contained>::const_iterator;

    using value_type = Contained;
    using size_type = typename std::vector<Contained>::size_type;

    void deallocate()
    {
        clear();
    }
    [[nodiscard]] bool allocated() const noexcept
    {
        return !empty();
    }

    [[nodiscard]] auto &operator()(const int idx) noexcept
    {
        return (*this)[idx - 1];
    }

    [[nodiscard]] const auto &operator()(const int idx) const noexcept
    {
        return (*this)[idx - 1];
    }

    [[nodiscard]] int isize() const noexcept
    {
        return static_cast<int>(size());
    }

    void allocate(const std::size_t new_size)
    {
        if (size() <= new_size) {
            (*this).resize(new_size);
        }
    }
};

template <typename T> [[nodiscard]] bool allocated(const SimpleArray1D<T> &v)
{
    return v.allocated();
}
template <typename T> [[nodiscard]] auto isize(const SimpleArray1D<T> &v)
{
    return v.isize();
}

#elif defined(EPVECTOR)

template <typename T> struct EPVector : std::vector<T>
{
    using std::vector<T>::vector;

    T &operator()(std::size_t n)
    {
        return this->at(n - 1);
    }

    const T &operator()(std::size_t n) const
    {
        return this->at(n - 1);
    }

    void allocate(int size)
    {
        this->resize(size);
    }

    void redimension(int size)
    {
        this->resize(size);
    }

    void deallocate()
    {
        this->clear();
    }

    // operator= used for initialization of the vector
    void operator=(T v)
    {
        std::fill(this->begin(), this->end(), v);
    }

    // dimension is often used to initalize the vector instead of allocate + operator=
    void dimension(int size, const T v)
    {
        this->resize(size, v);
    }

    // isize needed for current FindItemInList
    int isize() const
    {
        return static_cast<int>(this->size());
    }
};

template <> struct EPVector<bool> : std::vector<std::uint8_t>
{
    using std::vector<std::uint8_t>::vector;

    std::uint8_t &operator()(std::size_t n)
    {
        return this->at(n - 1);
    }

    const std::uint8_t &operator()(std::size_t n) const
    {
        return this->at(n - 1);
    }

    void allocate(int size)
    {
        this->resize(size);
    }

    void redimension(int size)
    {
        this->resize(size);
    }

    void deallocate()
    {
        this->clear();
    }

    // operator= used for initialization of the vector
    void operator=(bool v)
    {
        std::fill(this->begin(), this->end(), v);
    }

    // dimension is often used to initalize the vector instead of allocate + operator=
    void dimension(int size, const bool v)
    {
        this->resize(size, v);
    }

    // isize needed for current FindItemInList
    int isize() const
    {
        return static_cast<int>(this->size());
    }
};

template <typename T> using SimpleArray1D = EPVector<T>;

template <typename T> inline bool allocated(EPVector<T> const &v)
{
    return v.size();
}

template <typename T> [[nodiscard]] auto isize(const EPVector<T> &v)
{
    return v.isize();
}


inline bool all(EPVector<bool> const &values)
{
    if (values.empty()) return true;
    for (auto v : values) {
        if (!v) return false;
    }
    return true;
}

inline bool any(EPVector<bool> const &values)
{
    if (values.empty()) return false;
    for (auto v : values) {
        if (v) return true;
    }
    return false;
}

inline std::size_t count(EPVector<bool> const &values)
{
    std::size_t c(0u);
    for (auto v : values) {
        if (v) ++c;
    }
    return c;
}

template <typename T> inline EPVector<T> pack(EPVector<T> const &v, EPVector<bool> const &mask)
{
    EPVector<T> r;
    r.reserve(mask.size());
    for (std::size_t i = 0; i < mask.size(); ++i) {
        if (mask[i]) {
            r.emplace_back(v[i]);
        }
    }
    return r;
}

template <typename T> inline Array1D<T> pack(Array1<T> const &a, EPVector<bool> const &mask)
{
    Array1D<T> r;
    r.reserve(mask.size());
    for (std::size_t i = 0, e = mask.size(); i < e; ++i) {
        if (mask[i]) {
            r.emplace_back(a[i]);
        }
    }
    return r;
}

template <typename T> inline T magnitude_squared(const EPVector<T> &v)
{
    return std::inner_product(v.begin(), v.end(), v.begin(), T{});
}

template <typename T, typename V> inline T dot(const EPVector<T> &u, const V &v)
{
    return std::inner_product(u.begin(), u.end(), v.begin(), T{});
}

template <typename Element, typename Member> inline Member maxval(EPVector<Element> const &a, Member Element::*pmem)
{
    Member v(a.empty() ? std::numeric_limits<Member>::lowest() : a(1).*pmem);
    for (int i = 2, e = a.isize(); i <= e; ++i) {
        v = std::max(v, a(i).*pmem);
    }
    return v;
}

template <typename T> inline T maxval(EPVector<T> const &a)
{
    auto max = std::max_element(a.begin(), a.end());
    if (max == a.end()) {
        return std::numeric_limits<T>::lowest();
    }
    return *max;
}

#else

template <typename T> using SimpleArray1D = ObjexxFCL::Array1D<T>;

#endif

} // namespace EnergyPlus

#endif
