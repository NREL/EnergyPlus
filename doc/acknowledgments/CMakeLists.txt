# to get the list of input files, go to the root of the document folder, and:
# grep -r '\\input{' * | cut -d ':' -f 2 | cut -c 8- | rev | cut -c 2- | rev | sed 's|^|  ${PROJECT_SOURCE_DIR}/acknowledgments/|g' | sed 's/$/.tex/g'
set(INCLUDED_TEX
    ${PROJECT_SOURCE_DIR}/header.tex
    ${PROJECT_SOURCE_DIR}/acknowledgments/acknowledgments.tex
    ${PROJECT_SOURCE_DIR}/acknowledgments/src/notices-and-acknowledgments.tex
    ${PROJECT_SOURCE_DIR}/acknowledgments/src/notices-and-acknowledgments/copyright-notice.tex
    ${PROJECT_SOURCE_DIR}/acknowledgments/src/notices-and-acknowledgments/other-software-included-or-referenced.tex
    ${PROJECT_SOURCE_DIR}/acknowledgments/src/notices-and-acknowledgments/other-acknowledgments.tex
    ${PROJECT_SOURCE_DIR}/title.tex)

# to get the list of inluded images, go to the root of the document folder and:
# grep -r '\\includegraphics' * | cut -d ':' -f 2 | rev | cut -d '{' -f 1 | rev | cut -d '}' -f 1 | sed 's/^/  /g' | sort -n -t'.' -k 1.14
set(INCLUDED_IMAGES)

create_doc_target("acknowledgments" "Acknowledgments")
