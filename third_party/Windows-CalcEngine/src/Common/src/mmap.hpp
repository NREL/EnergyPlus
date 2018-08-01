#ifndef TUPLEASMAPKEY_VARIADICMAP_H
#define TUPLEASMAPKEY_VARIADICMAP_H

#include <map>

namespace FenestrationCommon {

	// Template for multimap.
	template < typename T, typename ...Values >
	class mmap {
	public:
		mmap() {

		}

		T& operator()( const Values ... a ) {
			return m_Types[ std::make_tuple( a... ) ];
		}

		T at( const Values ... a ) const {
			return m_Types.at( std::make_tuple( a... ) );
		}

	private:
		std::map< std::tuple< Values... >, T > m_Types;
	};

}


#endif //TUPLEASMAPKEY_VARIADICMAP_H
