pip install sphinx sphinxcontrib-moderncmakedomain sphinx-sitemap GitPython sphinx-rtd-theme breathe rst2pdf
sphinx-build -M html $(pwd) out
sphinx-build -b pdf  $(pwd) out
