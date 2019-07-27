#ifndef SURFACECOATING_H
#define SURFACECOATING_H

#include <map>

namespace FenestrationCommon {

	enum class SurfaceType { Coated, Uncoated };

	enum class MaterialType {
		Monolithic,
		Coated,
		Film,
		AppliedFilm,
		Laminate,
		Interlayer,
		Electrochromic,
		Thermochromic
	};

	const std::map< MaterialType, SurfaceType > coatingType = {
		{ MaterialType::Monolithic, SurfaceType::Uncoated },
		{ MaterialType::Coated, SurfaceType::Coated },
		{ MaterialType::Film, SurfaceType::Coated },
		{ MaterialType::AppliedFilm, SurfaceType::Coated },
		{ MaterialType::Laminate, SurfaceType::Coated },
		{ MaterialType::Interlayer, SurfaceType::Uncoated },
		{ MaterialType::Electrochromic, SurfaceType::Coated },
		{ MaterialType::Thermochromic, SurfaceType::Coated }
	};

}

#endif
