import os
import sys
sys.path.append(os.path.dirname(os.path.realpath(__file__)))

from functional import Wall


w = Wall(5.3)
w.set_thickness(9.2)
c = w.calculate(7)
print("Calculated wall = " + str(c))
