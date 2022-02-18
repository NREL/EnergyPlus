include(GenerateExportHeader)

if(MSVC)
	add_definitions(-DMGL_LIB_MSVC)
endif(MSVC)

if(MGL_HAVE_FLTK)
	mgl_add_lib(fltk fltk.cpp  image.cpp  image.h ../include/mgl2/fltk.h ../include/mgl2/Fl_MathGL.h)
	target_include_directories(mgl-fltk SYSTEM PUBLIC ${FLTK_INCLUDE_DIR})
	target_include_directories(mgl-fltk-static SYSTEM PUBLIC ${FLTK_INCLUDE_DIR})
	target_link_libraries(mgl-fltk mgl ${FLTK_LIBRARIES})
	target_link_libraries(mgl-fltk-static mgl-static ${FLTK_LIBRARIES})
endif(MGL_HAVE_FLTK)

if(MGL_HAVE_GLUT)
	mgl_add_lib(glut glut.cpp ../include/mgl2/glut.h)
	target_include_directories(mgl-glut SYSTEM PUBLIC ${GLUT_INCLUDE_DIR})
	target_include_directories(mgl-glut-static SYSTEM PUBLIC ${GLUT_INCLUDE_DIR})
	target_link_libraries(mgl-glut mgl ${GLUT_LIBRARIES} ${OPENGL_LIBRARIES})
	target_link_libraries(mgl-glut-static mgl-static ${GLUT_LIBRARIES} ${OPENGL_LIBRARIES})
endif(MGL_HAVE_GLUT)

if(MGL_HAVE_WX)
	mgl_add_lib(wx wx.cpp ../include/mgl2/wx.h)
	include(${wxWidgets_USE_FILE})
	target_link_libraries(mgl-wx mgl ${wxWidgets_LIBRARIES})
	target_link_libraries(mgl-wx-static mgl-static ${wxWidgets_LIBRARIES})
endif(MGL_HAVE_WX)

add_subdirectory( qt4 )
add_subdirectory( qt5 )

mgl_po_src(fltk.cpp ../include/mgl2/fltk.h ../include/mgl2/Fl_MathGL.h glut.cpp ../include/mgl2/glut.h wx.cpp ../include/mgl2/wx.h qt.cpp ../include/mgl2/qt.h  ../include/mgl2/qmathgl.h)
