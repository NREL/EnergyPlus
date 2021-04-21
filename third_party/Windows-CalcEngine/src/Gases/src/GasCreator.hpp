#pragma once

#include <map>
#include "GasData.hpp"

namespace Gases {
  
  enum class GasDef {Air, Argon, Krypton, Xenon};

	class Gas {
	public:
		static Gas & intance();

		CGasData get( GasDef gasDef ) const;

	private:
		Gas();

		// Storage for predefined gases
		const std::map<GasDef, const CGasData> m_Gas;
	};

}
