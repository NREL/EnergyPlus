# expecting to find a file called cover.txt in cwd
# need to generate a cover.md file in cwd
# cover.txt looks like:
#   lines=48 hit=2 functions=2 hit=1
# Processing file EnergyPlus/SurfaceGeometry.hh
#   lines=44 hit=9 functions=4 hit=2
# Overall coverage rate:
#   lines......: 7.9% (28765 of 364658 lines)
#   functions......: 19.6% (2224 of 11327 functions)

from pathlib import Path
cover_input = Path.cwd() / 'cover.txt'
lines = cover_input.read_text().strip().split('\n')
line_coverage = lines[-2].strip().split(':')[1].strip()
line_percent = line_coverage.split(' ')[0]
function_coverage = lines[-1].strip().split(':')[1].strip()
cover_output = Path.cwd() / 'cover.md'
content = f"""
<details>
  <summary>Coverage Summary - {line_percent} of lines - Download Coverage Artifact for Full Details</summary>

  - {line_coverage}
  - {function_coverage}
</details>"""
cover_output.write_text(content)
