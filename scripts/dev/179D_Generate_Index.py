#!/usr/bin/env python

import sys
import os


onlyfiles = []

for root, dirs, files in os.walk(sys.argv[1]):
  for name in files:
    onlyfiles.append(os.path.relpath(os.path.join(root, name), sys.argv[1]));


onlyfiles.sort()



index = """
    <!doctype html>
    <html>
      <head>
	<!-- Latest compiled and minified CSS -->
	<link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css">

	<!-- Optional theme -->
	<link rel="stylesheet" href="//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css">

	<!-- Latest compiled and minified JavaScript -->
	<script src="//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"></script>

	<script src="//code.jquery.com/jquery-2.1.1.min.js"></script>
	<script src="//code.jquery.com/jquery-migrate-1.2.1.min.js"></script>

        <style>
          table { margin: 1em; }
          table thead { border-bottom: 1px solid #000; }
          table td { padding: 5px; }
          table th { padding: 5px; }
        </style>
      </head>
      <body>

	<table class='table table-hover'>
	  <tr><th>filename</th><th></th></tr>"""

for filename in onlyfiles:
  index += "<tr><td>" + filename + "</td><td><a href='" + filename + "'>download</a></td></tr>\n"

index += """
	</table>
      </body>
    </html>"""

print(index)


with open(os.path.join(sys.argv[1], "index.html"), 'w') as f:
  f.write(index)
