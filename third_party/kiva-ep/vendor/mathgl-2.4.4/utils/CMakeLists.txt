add_executable(make_pas make_pas.cpp)

add_executable(mgltask mgltask.cpp)
install(
	TARGETS mgltask
	EXPORT MathGLTargets
	RUNTIME DESTINATION ${MathGL_INSTALL_BIN_DIR}
)

add_executable(mglconv mglconv.cpp)
if(MSVC)
set(link_type -static)
else(MSVC)
set(link_type)
endif(MSVC)
target_link_libraries(mglconv mgl${link_type} ${getopt_lib-static})
install(
	TARGETS mglconv
	EXPORT MathGLTargets
	RUNTIME DESTINATION ${MathGL_INSTALL_BIN_DIR}
)

add_executable(mgl.cgi mglcgi.cpp)
target_link_libraries(mgl.cgi mgl-static)
install(
	TARGETS mgl.cgi
	EXPORT MathGLTargets
# should be /usr/lib/cgi-bin/
	RUNTIME DESTINATION ${MGL_CGI_PATH}
)

mgl_po_src(mglconv.cpp mglview.cpp mglcgi.cpp)

if(MGL_HAVE_FLTK)
	add_definitions(-DUSE_FLTK)
	add_executable(mglview mglview.cpp)
	target_link_libraries(mglview mgl-fltk ${getopt_lib-static} ${FLTK_LIBRARIES})
	install(
		TARGETS mglview
		EXPORT MathGLTargets
		RUNTIME DESTINATION ${MathGL_INSTALL_BIN_DIR}
	)

else(MGL_HAVE_FLTK)
	if(QT_ENABLED)
		add_executable(mglview mglview.cpp)
		if(enable-qt5)
			include(../scripts/qt5.cmake)
			target_link_libraries(mglview mgl-qt5${link_type} ${getopt_lib-static} ${MGL_QT5_LIBS})
		else(enable-qt5)
			include(../scripts/qt4.cmake)
			target_link_libraries(mglview mgl-qt4${link_type} ${getopt_lib-static} ${MGL_QT4_LIBS})
		endif(enable-qt5)

		install(
			TARGETS mglview
			EXPORT MathGLTargets
			RUNTIME DESTINATION ${MathGL_INSTALL_BIN_DIR}
		)
	endif(QT_ENABLED)
endif(MGL_HAVE_FLTK)
