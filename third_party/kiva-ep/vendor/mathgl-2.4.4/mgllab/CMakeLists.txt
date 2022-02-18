# cmake_minimum_required(VERSION 2.8.9)
#
# project( MGLLAB )
#
# include(FindFLTK)
# if(NOT FLTK_FOUND)
# 	message(SEND_ERROR "Couldn't find FLTK library.")
# endif(NOT FLTK_FOUND)
#
# include_directories( ${MGLLAB_SOURCE_DIR} ${MGLLAB_BINARY_DIR})
#
# find_package(MathGL2 REQUIRED FLTK)
# #find_package(MathGL REQUIRED FLTK PATHS /usr/local/lib/CMake/MathGL/)
#
# find_package(OpenMP)
# if(OPENMP_FOUND)
# 	set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
# 	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
# else(OPENMP_FOUND)
# 	message(SEND_ERROR "Couldn't find OpenMP.")
# endif(OPENMP_FOUND)
#
# include_directories(${FLTK_INCLUDE_DIR})
# set(mgl_lab_src editor.cpp help.cpp mathgl.cpp grid.cpp dialogs.cpp mgllab.cpp option.cpp table.cpp image.cpp)
# set(mgl_lab_hdr mgllab.h image.h)
# add_executable(mgllab ${mgl_lab_src} ${mgl_lab_hdr})
# target_link_libraries(mgllab ${MATHGL2_FLTK_LIBRARY} ${MATHGL2_LIBRARY} ${FLTK_LIBRARIES})
# install(
# 	TARGETS mgllab
# 	RUNTIME DESTINATION bin	)

if(MGL_HAVE_FLTK)
	configure_file(${MathGL2_SOURCE_DIR}/mgllab/mgl.xml.in ${MathGL2_BINARY_DIR}/mgllab/mgl.xml)
	include_directories(${FLTK_INCLUDE_DIR})
	set(mgllab_src editor.cpp help.cpp mathgl.cpp grid.cpp dialogs.cpp mgllab.cpp table.cpp)
	set(mgllab_hdr mgllab.h)
	if(WIN32)
		set(mgllab_src ${mgllab_src} mgllab.rc)
	endif(WIN32)
	add_executable(mgllab ${mgllab_src} ${mgllab_hdr})
	target_link_libraries(mgllab mgl mgl-fltk ${FLTK_LIBRARIES})
	install(
		TARGETS mgllab
		RUNTIME DESTINATION bin	)
	install(FILES udav.png DESTINATION ${CMAKE_INSTALL_PREFIX}/share/pixmaps/)
	find_program(findupddb update-desktop-database)
	if(findupddb)
		install(FILES mgllab.desktop DESTINATION ${CMAKE_INSTALL_PREFIX}/share/applications/)
		install(CODE "execute_process(COMMAND \"${findupddb}\" \"${CMAKE_INSTALL_PREFIX}/share/applications\")")
	endif(findupddb)
	find_program(findupmdb update-mime-database)
	if(findupmdb)
		install(FILES ${CMAKE_BINARY_DIR}/mgllab/mgl.xml DESTINATION ${CMAKE_INSTALL_PREFIX}/share/mime/packages/)
		install(CODE "execute_process(COMMAND \"${findupmdb}\" \"${CMAKE_INSTALL_PREFIX}/share/mime\")")
	endif(findupmdb)

	mgl_po_src(${mgllab_src} ${mgllab_hdr})

endif(MGL_HAVE_FLTK)
