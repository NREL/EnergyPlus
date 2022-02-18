configure_file(${MathGL2_SOURCE_DIR}/include/config.h.in ${MathGL2_BINARY_DIR}/include/mgl2/config.h)
configure_file(${MathGL2_SOURCE_DIR}/include/build.h.in ${MathGL2_BINARY_DIR}/include/mgl2/build.h)

#[[
install(DIRECTORY mgl2/ DESTINATION ${MGL_INCLUDE_PATH}
                        PATTERN ".svn" EXCLUDE
                        PATTERN "fltk.h" EXCLUDE
                        PATTERN "glut.h" EXCLUDE
                        PATTERN "wx.h" EXCLUDE
                        PATTERN "qt.h" EXCLUDE
                        PATTERN "qmathgl.h" EXCLUDE
                        PATTERN "window.h" EXCLUDE
                        PATTERN "*old.h" EXCLUDE
                        PATTERN "*~" EXCLUDE
                        PATTERN "*.fs"
                        PATTERN "*.pas"
                        PATTERN "*.h")

install(FILES ${MathGL2_BINARY_DIR}/include/mgl2/config.h DESTINATION ${MGL_INCLUDE_PATH})
if(MGL_HAVE_FLTK)
	install(FILES mgl2/fltk.h DESTINATION ${MGL_INCLUDE_PATH})
endif(MGL_HAVE_FLTK)
if(MGL_HAVE_GLUT)
	install(FILES mgl2/glut.h DESTINATION ${MGL_INCLUDE_PATH})
endif(MGL_HAVE_GLUT)
if(MGL_HAVE_WX)
	install(FILES mgl2/wx.h DESTINATION ${MGL_INCLUDE_PATH})
endif(MGL_HAVE_WX)
if(MGL_HAVE_QT)
	install(FILES mgl2/qt.h mgl2/qmathgl.h DESTINATION ${MGL_INCLUDE_PATH})
endif(MGL_HAVE_QT)

#if(MGL_HAVE_FLTK AND MGL_HAVE_WX AND MGL_HAVE_QT)
if(MGL_HAVE_FLTK AND MGL_HAVE_QT AND MGL_HAVE_WX)
	install(FILES mgl2/window.h DESTINATION ${MGL_INCLUDE_PATH})
#endif(MGL_HAVE_FLTK AND MGL_HAVE_WX AND MGL_HAVE_QT)
endif(MGL_HAVE_FLTK AND MGL_HAVE_QT AND MGL_HAVE_WX)
#]]