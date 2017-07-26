from __future__ import print_function
import cbor2
import json
import struct
import sys

energy_plus_jdd_path = sys.argv[1]

with open(energy_plus_jdd_path) as f:
    data_json = json.load(f)

data_cbor = cbor2.dumps(data_json)

# unpacked = struct.unpack("=B", data_cbor[0])
# assert len(unpacked) == 1
# print(hex(*unpacked), end="")
# print(',', end="")
for index, c in enumerate(data_cbor):
    unpacked = struct.unpack("=B", c)
    assert len(unpacked) == 1
    h = hex(*unpacked)
    print(h, end="")
    print(',', end="")
    if index % 40 == 0 and index != 0:
        print()
