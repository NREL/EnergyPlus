# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
# from pathlib import Path
# sys.path.insert(0, str(Path(__file__).parent.parent))
from sphinx.builders.html import StandaloneHTMLBuilder
import subprocess


source_suffix = '.rst'
# Doxygen
subprocess.call('doxygen Doxyfile', shell=True)

# -- Project information -----------------------------------------------------

project = 'EnergyPlus'
author = 'Julien Marrec'
copyright = 'Julien Marrec'
version = '1.0.0' # feature version
release = '1.0.0' # full version string


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.intersphinx',
    'sphinx.ext.autosectionlabel',
    'sphinx.ext.todo',
    'sphinx.ext.coverage',
    'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig',
    'sphinx.ext.viewcode',
    #'sphinx_sitemap',
    'sphinx.ext.inheritance_diagram',
    'breathe',
    'sphinxcontrib.moderncmakedomain',
    'rst2pdf.pdfbuilder',
]
pdf_documents = [('index', u'cmake-modules-eplus', u'EnergyPlus CMake Custom Modules doc', u'Julien Marrec'),]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['@conf_path@/templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

highlight_language = 'c++'

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'
html_theme_options = {
    'canonical_url': '',
    'analytics_id': '',  #  Provided by Google in your dashboard
    'display_version': True,
    'prev_next_buttons_location': 'bottom',
    'style_external_links': False,

    'logo_only': False,

    # Toc options
    'collapse_navigation': True,
    'sticky_navigation': True,
    'navigation_depth': 4,
    'includehidden': True,
    'titles_only': False
}
# html_logo = ''
# github_url = ''
html_baseurl = ''

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['@conf_path@/static']
# html_style = 'css/cmake.css'
#
# html_js_files = [
#     'js/version_switcher.js',
# ]

# -- Breathe configuration -------------------------------------------------

master_doc = 'index'

breathe_projects = {
	'C++ Sphinx Doxygen Breathe': "@conf_out_path@/xml/"
}
breathe_default_project = 'C++ Sphinx Doxygen Breathe'
breathe_default_members = ('members', 'undoc-members')

############################
# SETUP THE RTD LOWER-LEFT #
############################
try:
   html_context
except NameError:
   html_context = dict()
html_context['display_lower_left'] = True

if 'REPO_NAME' in os.environ:
	REPO_NAME = os.environ['REPO_NAME']
else:
	REPO_NAME = ''

# SET CURRENT_LANGUAGE
if 'current_language' in os.environ:
   # get the current_language env var set by buildDocs.sh
   current_language = os.environ['current_language']
else:
   # the user is probably doing `make html`
   # set this build's current language to english
   current_language = 'en'

# tell the theme which language to we're currently building
html_context['current_language'] = current_language

# SET CURRENT_VERSION
from git import Repo
repo = Repo( search_parent_directories=True )

if 'current_version' in os.environ:
   # get the current_version env var set by buildDocs.sh
   current_version = os.environ['current_version']
elif 'CI_COMMIT_REF_NAME' in os.environ:
   # get the current_version env var set by buildDocs.sh
   current_version = os.environ['CI_COMMIT_REF_NAME']
else:
   # the user is probably doing `make html`
   # set this build's current version by looking at the branch
   current_version = repo.active_branch.name

# tell the theme which version we're currently on ('current_version' affects
# the lower-left rtd menu and 'version' affects the logo-area version)
html_context['current_version'] = current_version
html_context['version'] = current_version
