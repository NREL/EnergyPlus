#include <vector>

#ifndef SimpleArray1D_INCLUDED
#define SimpleArray1D_INCLUDED

namespace EnergyPlus
{
  template<typename Contained>
  class SimpleArray1D : private std::vector<Contained>
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

      void deallocate() { clear(); }
      [[nodiscard]] bool allocated() const noexcept { return !empty(); }

      [[nodiscard]] auto &operator()(const int idx) noexcept {
          return (*this)[idx - 1];
      }

      [[nodiscard]] const auto &operator()(const int idx) const noexcept
      {
          return (*this)[idx - 1];
      }


      [[nodiscard]] int isize() const noexcept {
          return static_cast<int>(size());
      }

      void allocate(const std::size_t new_size) {
          if (size() <= new_size) {
              (*this).resize(new_size);
          }
      }
  };

  template<typename T>
  [[nodiscard]] bool allocated(const SimpleArray1D<T> &v) {
      return v.allocated();
  }
  template <typename T> [[nodiscard]] auto isize(const SimpleArray1D<T> &v)
  {
      return v.isize();
  }}

#endif
