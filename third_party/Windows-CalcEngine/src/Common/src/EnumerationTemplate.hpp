#pragma once

namespace FenestrationCommon
{
    template<typename T>
    class Enum
    {
    public:
        class Iterator
        {
        public:
            explicit Iterator(int value) : m_value(value)
            {}

            T operator*(void)const
            {
                return static_cast<T>(m_value);
            }

            void operator++(void)
            {
                ++m_value;
            }

            bool operator!=(Iterator rhs)
            {
                return m_value != rhs.m_value;
            }

        private:
            int m_value;
        };
    };
}