cmake_minimum_required(VERSION 3.5)

if( ${BUILD_WCE_COMMON} )
	add_subdirectory( Common )
	set( LINK_TO_Common FenestrationCommon )
endif()

if( ${BUILD_WCE_VIEWER} )
	add_subdirectory( Viewer )
	set( LINK_TO_Viewer Viewer )
endif()

if( ${BUILD_WCE_GASES} )
	add_subdirectory( Gases )
	set( LINK_TO_Gases Gases )
endif()

if( ${BUILD_WCE_THERMAL} )
	add_subdirectory( Tarcog )
	set( LINK_TO_Tarcog Tarcog )
	
	add_subdirectory( Chromogenics )
endif()

if( ${BUILD_WCE_OPTICAL} )
	add_subdirectory( SpectralAveraging )
	set( LINK_TO_SpectralAveraging SpectralAveraging )
	
	add_subdirectory( SingleLayerOptics )
	set( LINK_TO_SingleLayerOptics SingleLayerOptics )
	
	add_subdirectory( MultiLayerOptics )
endif()