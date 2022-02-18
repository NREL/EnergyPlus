find_program(findpdflatex pdflatex)
if(NOT findpdflatex)
	message(SEND_ERROR "Couldn't find pdflatex needed for LATEX example building.")
endif(NOT findpdflatex)
find_program(findkpsewhich kpsewhich)
if(NOT findkpsewhich)
	message(SEND_ERROR "Couldn't find kpsewhich needed for LATEX example install.")
endif(NOT findkpsewhich)
find_program(findmktexlsr mktexlsr)
if(NOT findmktexlsr)
	message(SEND_ERROR "Couldn't find mktexlsr needed for LATEX example install.")
endif(NOT findmktexlsr)
execute_process(
	COMMAND ${findkpsewhich} -var-value=TEXMFLOCAL
	OUTPUT_VARIABLE TEXMFLOCALDIR
	RESULT_VARIABLE TEXMFLOCAL_ERR
	OUTPUT_STRIP_TRAILING_WHITESPACE
)
if(NOT TEXMFLOCALDIR)
	message(SEND_ERROR "Couldn't find TEXMFLOCAL directory.")
endif(NOT TEXMFLOCALDIR)

# set(extramgl Axis_projection Vectorial)

add_custom_command(OUTPUT mgltex.sty
	COMMAND ${CMAKE_COMMAND} -E copy ${MathGL2_SOURCE_DIR}/mgltex/mgltex.ins ${MathGL2_BINARY_DIR}/mgltex/
	COMMAND ${CMAKE_COMMAND} -E copy ${MathGL2_SOURCE_DIR}/mgltex/mgltex.dtx ${MathGL2_BINARY_DIR}/mgltex/
	COMMAND ${CMAKE_COMMAND} -E copy ${MathGL2_SOURCE_DIR}/mgltex/Recompilation_decision.pdf ${MathGL2_BINARY_DIR}/mgltex/
	COMMAND ${findpdflatex} mgltex.ins
	COMMAND ${findpdflatex} mgltex.dtx
	COMMAND ${findpdflatex} mgltex.dtx
	COMMAND ${findpdflatex} mgltex.dtx
	DEPENDS mgltex.dtx Recompilation_decision.pdf )

add_custom_command(OUTPUT sample.pdf
	COMMAND ${CMAKE_COMMAND} -E make_directory ${MathGL2_BINARY_DIR}/mgltex/MGL
	COMMAND ${CMAKE_COMMAND} -E make_directory ${MathGL2_BINARY_DIR}/mgltex/MGL/scripts
	COMMAND ${CMAKE_COMMAND} -E make_directory ${MathGL2_BINARY_DIR}/mgltex/MGL/backups
	COMMAND ${CMAKE_COMMAND} -E make_directory ${MathGL2_BINARY_DIR}/mgltex/MGL/graphics
	COMMAND ${CMAKE_COMMAND} -E copy ${MathGL2_SOURCE_DIR}/mgltex/sample.tex ${MathGL2_BINARY_DIR}/mgltex/
# 	COMMAND ${CMAKE_COMMAND} -E copy ${MathGL2_SOURCE_DIR}/mgltex/mgltex.sty ${MathGL2_BINARY_DIR}/mgltex/
	COMMAND PATH=$<TARGET_FILE_DIR:mglconv> ${findpdflatex} --shell-escape -draftmode sample.tex
	COMMAND PATH=$<TARGET_FILE_DIR:mglconv> ${findpdflatex} --shell-escape -draftmode sample.tex
	COMMAND PATH=$<TARGET_FILE_DIR:mglconv> ${findpdflatex} --shell-escape sample.tex
	DEPENDS mgltex.sty sample.tex )
add_custom_target(pdf ALL DEPENDS sample.pdf)

get_directory_property(mglconv_clean ADDITIONAL_MAKE_CLEAN_FILES)
set(mglconv_clean ${mglconv_clean} scripts mgltex.sty sample.tex sample.aux sample.log)
set_directory_properties(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${mglconv_clean}")

install(FILES ${MathGL2_BINARY_DIR}/mgltex/mgltex.sty DESTINATION ${TEXMFLOCALDIR}/tex/latex/mgltex/)
install(FILES sample.tex mgltex.pdf ${MathGL2_BINARY_DIR}/mgltex/sample.pdf DESTINATION ${TEXMFLOCALDIR}/doc/latex/mgltex/)
install(CODE "execute_process(COMMAND ${findmktexlsr} ${TEXMFLOCALDIR})")
