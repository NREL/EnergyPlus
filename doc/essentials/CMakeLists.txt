# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/acknowledgments/|g' | sed 's/$/.tex/g'
set(INCLUDED_TEX ${PROJECT_SOURCE_DIR}/header.tex ${PROJECT_SOURCE_DIR}/essentials/essentials.tex ${PROJECT_SOURCE_DIR}/title.tex)

# to get the list of included images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
set(INCLUDED_IMAGES
    ${PROJECT_SOURCE_DIR}/essentials/media/coordinatesystem.png
    ${PROJECT_SOURCE_DIR}/essentials/media/energyplusfolder.png
    ${PROJECT_SOURCE_DIR}/essentials/media/ep.png
    ${PROJECT_SOURCE_DIR}/essentials/media/eplaunch2.png
    ${PROJECT_SOURCE_DIR}/essentials/media/eplaunch3.png
    ${PROJECT_SOURCE_DIR}/essentials/media/idfeditor.png
    ${PROJECT_SOURCE_DIR}/essentials/media/IDFVersionUpdater.png
    ${PROJECT_SOURCE_DIR}/essentials/media/kiva-2d-elements.png
    ${PROJECT_SOURCE_DIR}/essentials/media/NodeDiagram.png
    ${PROJECT_SOURCE_DIR}/essentials/media/tariff-charges.png
    ${PROJECT_SOURCE_DIR}/essentials/media/window_frame_and_divider.png)

create_doc_target("essentials" "EnergyPlusEssentials")
