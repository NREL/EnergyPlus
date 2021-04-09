#include <vector>

#ifndef EPVector_INCLUDED
#define EPVector_INCLUDED

#include <numeric>

namespace EnergyPlus {

template <typename T> struct EPVector : private std::vector<T>
{
    using std::vector<T>::vector;
    using std::vector<T>::size;
    using std::vector<T>::operator[];
    using std::vector<T>::empty;
    using std::vector<T>::begin;
    using std::vector<T>::end;
    using std::vector<T>::cbegin;
    using std::vector<T>::cend;
    using std::vector<T>::clear;
    using std::vector<T>::emplace_back;
    using std::vector<T>::push_back;
    using const_iterator = typename std::vector<T>::const_iterator;

    using value_type = T;
    using size_type = typename std::vector<T>::size_type;

    [[nodiscard]] T &operator()(std::size_t n)
    {
        return this->at(n - 1);
    }

    [[nodiscard]] const T &operator()(std::size_t n) const
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

    void deallocate() noexcept
    {
        this->clear();
    }

    [[nodiscard]] bool allocated() const noexcept
    {
        return !this->empty();
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
    [[nodiscard]] int isize() const noexcept
    {
        return static_cast<int>(this->size());
    }
};

template <> struct EPVector<bool> : private std::vector<std::uint8_t>
{
    using std::vector<std::uint8_t>::vector;
    using std::vector<std::uint8_t>::size;
    using std::vector<std::uint8_t>::operator[];
    using std::vector<std::uint8_t>::empty;
    using std::vector<std::uint8_t>::begin;
    using std::vector<std::uint8_t>::end;
    using std::vector<std::uint8_t>::cbegin;
    using std::vector<std::uint8_t>::cend;
    using std::vector<std::uint8_t>::clear;
    using std::vector<std::uint8_t>::emplace_back;
    using std::vector<std::uint8_t>::push_back;
    using const_iterator = typename std::vector<std::uint8_t>::const_iterator;

    using value_type = std::uint8_t;
    using size_type = typename std::vector<std::uint8_t>::size_type;

    [[nodiscard]] std::uint8_t &operator()(std::size_t n)
    {
        return this->at(n - 1);
    }

    [[nodiscard]] const std::uint8_t &operator()(std::size_t n) const
    {
        return this->at(n - 1);
    }

    [[nodiscard]] bool allocated() const noexcept
    {
        return !this->empty();
    }

    void allocate(int size)
    {
        this->resize(size);
    }

    void redimension(int size)
    {
        this->resize(size);
    }

    void deallocate() noexcept
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
    [[nodiscard]] int isize() const noexcept
    {
        return static_cast<int>(this->size());
    }
};

template <typename T> [[nodiscard]] bool allocated(EPVector<T> const &v) noexcept
{
    return v.allocated();
}

template <typename T> [[nodiscard]] auto isize(const EPVector<T> &v) noexcept
{
    return v.isize();
}

[[nodiscard]] inline bool all(EPVector<bool> const &values) noexcept
{
    if (values.empty()) return true;
    for (auto v : values) {
        if (!v) return false;
    }
    return true;
}

[[nodiscard]] inline bool any(EPVector<bool> const &values) noexcept
{
    if (values.empty()) return false;
    for (auto v : values) {
        if (v) return true;
    }
    return false;
}

[[nodiscard]] inline std::size_t count(EPVector<bool> const &values) noexcept
{
    std::size_t c(0u);
    for (auto v : values) {
        if (v) ++c;
    }
    return c;
}

template <typename T> [[nodiscard]] EPVector<T> pack(EPVector<T> const &v, EPVector<bool> const &mask)
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

template <typename T> [[nodiscard]] Array1D<T> pack(Array1<T> const &a, EPVector<bool> const &mask)
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

template <typename T> [[nodiscard]] T magnitude_squared(const EPVector<T> &v)
{
    return std::inner_product(v.begin(), v.end(), v.begin(), T{});
}

template <typename T, typename V> [[nodiscard]] T dot(const EPVector<T> &u, const V &v)
{
    return std::inner_product(u.begin(), u.end(), v.begin(), T{});
}

template <typename Element, typename Member> [[nodiscard]] Member maxval(EPVector<Element> const &a, Member Element::*pmem)
{
    Member v(a.empty() ? std::numeric_limits<Member>::lowest() : a(1).*pmem);
    for (int i = 2, e = a.isize(); i <= e; ++i) {
        v = std::max(v, a(i).*pmem);
    }
    return v;
}

// Sum of All Members of a Container
template<typename Element, typename Member> inline Member sum(EPVector<Element> const &c, Member Element::*pmem)
{
    Member s(0);
    for (const auto &elem : c) {
        s += elem.*pmem;
    }
    return s;
}

template <typename T> [[nodiscard]] T maxval(EPVector<T> const &a)
{
    auto max = std::max_element(a.begin(), a.end());
    if (max == a.end()) {
        return std::numeric_limits<T>::lowest();
    }
    return *max;
}

} // namespace EnergyPlus

#endif
