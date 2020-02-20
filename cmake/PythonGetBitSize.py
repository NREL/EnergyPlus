# this file helps identify the Python bit size (32 or 64)
# this is important when linking E+ against the Python library
import struct
print(8 * struct.calcsize("P"), end="")
