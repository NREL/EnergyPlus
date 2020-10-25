# -*- coding: utf-8 -*-
#
# Configuration file for the Sphinx documentation builder.
#
# This file does only contain a selection of the most common options. For a
# full list see the documentation:
# http://www.sphinx-doc.org/en/master/config

from json import load
import os
from shutil import copytree, rmtree
from subprocess import check_call, CalledProcessError, DEVNULL

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
from pathlib import Path
import sys
this_file_path = Path(__file__)
api_source_dir = this_file_path.parent.parent.parent.parent / 'src' / 'EnergyPlus' / 'api'
print(f"**Adding api_source_dir: {api_source_dir}")
sys.path.insert(0, str(api_source_dir))

# add a mock version of pyenergyplus
autodoc_mock_imports = ["pyenergyplus"]

# # build the C docs # #

# assuming doxygen is on PATH, but alter this locally if you need to point to a specific binary
DOXYGEN_BINARY = 'doxygen'

# set up some file paths for convenience
this_file_path = Path(__file__)
repo_root = this_file_path.parent.parent.parent.parent
rtd_dir = this_file_path.parent.parent
doxygen_dir = rtd_dir / 'doxygen'
doxygen_html_output_dir = doxygen_dir / '_build' / 'html'
sphinx_dir = rtd_dir / 'sphinx'
target_c_prebuilt_dir = sphinx_dir / '_build_c'

# test a file path to make sure we are in the right spot before trying to run
if doxygen_dir.exists():
    print("* Directory validation completed successfully")
else:
    raise Exception(f"Expected doxygen config dir to exist at \"{doxygen_dir}\" but it does not; aborting!")

# now try to run doxygen:
try:
    check_call([DOXYGEN_BINARY], cwd=doxygen_dir, stdout=DEVNULL, stderr=DEVNULL)
    print("* Doxygen completed successfully")
except CalledProcessError as e:
    raise Exception(f"Doxygen failed! Exception string: {str(e)}") from None
except FileNotFoundError as e:
    raise Exception(
        f"Doxygen binary not found, was it on path?  Looked for it at: {DOXYGEN_BINARY}; error = {str(e)}"
    ) from None

# ok, so doxygen should've run, validate the output directory exists
if doxygen_html_output_dir.exists():
    print("* Doxygen html output directory existence verified")
else:
    raise Exception(
        f"Although Doxygen appeared to run, the output directory is missing at {doxygen_html_output_dir}"
    ) from None

# alright, it exists, time to clean up the previous version, if it exists
if target_c_prebuilt_dir.exists():
    try:
        rmtree(target_c_prebuilt_dir)
        print("* Successfully deleted previous _build_c html directory")
    except Exception as e:
        raise Exception(f"Could not delete existing _build_c html directory") from None
else:
    print("* No _build_c directory to remove, skipping this step")

# ok, now just copy it
try:
    copytree(doxygen_html_output_dir, target_c_prebuilt_dir)
    print("* Successfully copied doxygen output directory to c_prebuilt directory")
except Exception as e:
    raise Exception(
        f"Could not copy doxygen output from '{doxygen_html_output_dir}' to '{target_c_prebuilt_dir}'"
    ) from None

# rename the root c file to index_c.html so it doesn't override the sphinx index.html file
os.rename(target_c_prebuilt_dir / 'index.html', target_c_prebuilt_dir / 'index_c.html')

print("* C Docs Complete!")

print("* Generating epJSON schema")
# # OK, now we need to make sure the epJSON schema is generated so we can process it
# Since this will primarily just be run by readthedocs, I'm just going to re-run the schema generator
try:
    check_call(['python3', 'scripts/dev/generate_epJSON_schema/generate_epJSON_schema.py', '.'], cwd=repo_root)
except CalledProcessError as e:
    raise Exception(f"Schema Generation failed! Exception string: {str(e)}") from None
except FileNotFoundError as e:
    raise Exception(
        f"python3 binary not found, what?  Looked for it at: `python3'; error = {str(e)}"
    ) from None

# now process that schema into some html blob
target_schema_pre_built = sphinx_dir / '_build_schema'
if target_schema_pre_built.exists():
    try:
        rmtree(target_schema_pre_built)
        print("* Successfully deleted previous _build_schema html directory")
    except Exception as e:
        raise Exception(f"Could not delete existing _build_schema html directory") from None
else:
    print("* No _build_schema directory to remove, skipping this step")

generated_schema_file = repo_root / 'idd' / 'Energy+.schema.epJSON.in'  # I know this will have CMake placeholders
if not generated_schema_file.exists():
    raise Exception("Generated schema file did not exist, aborting.")
print("* Generated schema existence confirmed")

os.makedirs(target_schema_pre_built)
output_schema_file = target_schema_pre_built / 'schema.html'
print("* Processing schema into HTML")
with open(output_schema_file, 'w') as h:
    h.write('<html>')
    h.write("""
<head>
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
.accordion {
  background-color: #eee; color: #444; cursor: pointer; padding: 18px; width: 100%;
  border: none; text-align: left; outline: none; font-size: 15px; transition: 0.4s;
}
.active, .accordion:hover { background-color: #ccc; }
.accordion:after { content: '\\002B'; color: #777; font-weight: bold; float: right; margin-left: 5px; }
.active:after { content: "\\2212"; }
.panel { 
  padding: 0 18px; background-color: white; max-height: 0; overflow: hidden; transition: max-height 0.2s ease-out;
}
</style>
</head>    
    """)
    h.write('<body>')
    with generated_schema_file.open() as f:
        o = load(f)
        # schema_version_number = o["epJSON_schema_version"]
        # schema_version_sha = o["epJSON_schema_build"]
        # h.write(f"<h1>Schema {schema_version_number} - {schema_version_sha}</h1>")
        h.write(f"<h1>Schema Description</h1>")
        idf_objects: dict = o["properties"]
        h.write(f"<h2>All Objects ({len(idf_objects)})</h2>")
        for obj_name, data in idf_objects.items():
            inner_html = '<ul>'  # open the inner html list
            if 'memo' in data:
                inner_html += f"<li>{data['memo']}</li>"
            pattern_props = data['patternProperties']
            value_with_unknown_key = next(iter(pattern_props.values()))  # only key could be '.*' or something else
            fields: dict = value_with_unknown_key['properties']
            inner_html += "<li>Fields</li>"
            inner_html += "<ul>"  # open the fields list
            for field_name, field_data in fields.items():
                default_string = ''
                if 'default' in field_data:
                    default_string = f" (Default: {field_data['default']})"
                field_type = field_data.get('type', 'unknown field type')
                this_field_type = field_type
                if field_type == 'array' and 'items' in field_data:
                    this_field_type = 'Array of {'
                    for i, variable_name in enumerate(field_data['items']['properties']):
                        if i == 0:
                            this_field_type += variable_name
                        else:
                            this_field_type += ', ' + variable_name
                    this_field_type += '}'
                inner_html += f"<li>{field_name} [{this_field_type}]{default_string}</li>"
            inner_html += "</ul>"  # close the fields list
            inner_html += '</ul>'  # close the inner_html list
            h.write(
                "<button class=\"accordion\">%s</button><div class=\"panel\"><pre>%s</pre></div>" % (
                    obj_name, inner_html
                )
            )
    h.write("""
    <script>
var acc = document.getElementsByClassName("accordion");
var i;
for (i = 0; i < acc.length; i++) {
  acc[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var panel = this.nextElementSibling;
    if (panel.style.maxHeight) {
      panel.style.maxHeight = null;
    } else {
      panel.style.maxHeight = panel.scrollHeight + "px";
    }
  });
}
</script>
    """)
    h.write('</body></html>')

print("* Schema docs complete!")

# then add the folder to the sphinx extra paths so the objects get included
html_extra_path = ['_build_c', '_build_schema']

# -- Project information -----------------------------------------------------

project = u'EnergyPlus Live Documentation'
copyright = u'2020, National Renewable Energy Laboratory for the United States Department of Energy'
author = u'National Renewable Energy Laboratory for the United States Department of Energy'

# The short X.Y version
version = u''
# The full version, including alpha/beta/rc tags
release = u'0.2'

extra_nav_links = {
    'Blah': 'https://energyplus.net'
}

# -- General configuration ---------------------------------------------------

# If your documentation needs a minimal Sphinx version, state it here.
#
# needs_sphinx = '1.0'

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.coverage',
    'sphinx.ext.mathjax',
    'sphinx.ext.viewcode',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['.templates']

# The suffix(es) of source filenames.
# You can specify multiple suffix as a list of string:
#
# source_suffix = ['.rst', '.md']
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = None

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [u'_build', 'Thumbs.db', '.DS_Store']

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = None


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
html_theme = 'haiku'
html_logo = '../../../release/favicon.png'
html_show_sphinx = False
# html_show_copyright = False

# Theme options are theme-specific and customize the look and feel of a theme
# further.  For a list of options available for each theme, see the
# documentation.
#
# html_theme_options = {}

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']

# Custom sidebar templates, must be a dictionary that maps document names
# to template names.
#
# The default sidebars (for documents that don't match any pattern) are
# defined by theme itself.  Builtin themes are using these templates by
# default: ``['localtoc.html', 'relations.html', 'sourcelink.html',
# 'searchbox.html']``.
#
html_sidebars = {
    '**': [
        'about.html',
        'navigation.html',
        'relations.html',
        'searchbox.html',
    ]
}

# -- Options for HTMLHelp output ---------------------------------------------

# Output file base name for HTML help builder.
htmlhelp_basename = 'EnergyPlusPythonAPIDocumentationdoc'


# -- Options for LaTeX output ------------------------------------------------

latex_elements = {
    # The paper size ('letterpaper' or 'a4paper').
    #
    # 'papersize': 'letterpaper',

    # The font size ('10pt', '11pt' or '12pt').
    #
    # 'pointsize': '10pt',

    # Additional stuff for the LaTeX preamble.
    #
    # 'preamble': '',

    # Latex figure (float) alignment
    #
    # 'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
    (master_doc, 'EnergyPlusPythonAPIDocumentation.tex', u'EnergyPlus Python API Documentation Documentation',
     u'National Renewable Energy Laboratory for the United States Department of Energy', 'manual'),
]


# -- Options for manual page output ------------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'energypluspythonapidocumentation', u'EnergyPlus Python API Documentation Documentation',
     [author], 1)
]


# -- Options for Texinfo output ----------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
    (master_doc, 'EnergyPlusPythonAPIDocumentation', u'EnergyPlus Python API Documentation Documentation',
     author, 'EnergyPlusPythonAPIDocumentation', 'One line description of project.',
     'Miscellaneous'),
]


# -- Options for Epub output -------------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#
# epub_identifier = ''

# A unique identification for the text.
#
# epub_uid = ''

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']


# -- Extension configuration -------------------------------------------------
