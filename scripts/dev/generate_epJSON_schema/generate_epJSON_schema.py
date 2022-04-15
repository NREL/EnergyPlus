# EnergyPlus, Copyright (c) 1996-2022, The Board of Trustees of the University
# of Illinois, The Regents of the University of California, through Lawrence
# Berkeley National Laboratory (subject to receipt of any required approvals
# from the U.S. Dept. of Energy), Oak Ridge National Laboratory, managed by UT-
# Battelle, Alliance for Sustainable Energy, LLC, and other contributors. All
# rights reserved.
#
# NOTICE: This Software was developed under funding from the U.S. Department of
# Energy and the U.S. Government consequently retains certain rights. As such,
# the U.S. Government has been granted for itself and others acting on its
# behalf a paid-up, nonexclusive, irrevocable, worldwide license in the
# Software to reproduce, distribute copies to the public, prepare derivative
# works, and perform publicly and display publicly, and to permit others to do
# so.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# (1) Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
# (2) Redistributions in binary form must reproduce the above copyright notice,
#     this list of conditions and the following disclaimer in the documentation
#     and/or other materials provided with the distribution.
#
# (3) Neither the name of the University of California, Lawrence Berkeley
#     National Laboratory, the University of Illinois, U.S. Dept. of Energy nor
#     the names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# (4) Use of EnergyPlus(TM) Name. If Licensee (i) distributes the software in
#     stand-alone form without changes from the version obtained under this
#     License, or (ii) Licensee makes a reference solely to the software
#     portion of its product, Licensee must refer to the software as
#     "EnergyPlus version X" software, where "X" is the version number Licensee
#     obtained under this License and may not use a different name for the
#     software. Except as specifically required in this Section (4), Licensee
#     shall not use in a company name, a product name, in advertising,
#     publicity, or other promotional activities any name, trade name,
#     trademark, logo, or other designation of "EnergyPlus", "E+", "e+" or
#     confusingly similar designation, without the U.S. Department of Energy's
#     prior written consent.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

import idd_parser
import modify_schema
import json
from os import path
from pathlib import Path
from argparse import ArgumentParser
import multiprocessing as mp


def generate_schema(energyplus_idd: Path, energyplus_epJSON: Path):
    data = idd_parser.Data()
    with open(energyplus_idd, 'r') as f:
        data.file = f.read()

    idd_parser.parse_idd(data)
    modify_schema.change_version(data.schema)
    modify_schema.change_schedule_compact(data.schema)
    modify_schema.change_utility_cost(data.schema)
    modify_schema.change_special_cased_enums(data.schema)
    modify_schema.change_special_cased_name_fields(data.schema)
    modify_schema.change_extensions_name(data.schema)
    modify_schema.change_89_release_issues(data.schema)
    modify_schema.add_explicit_extensible_bounds(data.schema)

    energyplus_epJSON.mkdir(parents=True, exist_ok=True)
    with open(energyplus_epJSON.joinpath('Energy+.schema.epJSON'), 'w') as f2:
        f2.write(json.dumps(data.schema, indent=4))


if __name__ == '__main__':
    parser = ArgumentParser(description='Run epJSON Schema generation')
    parser.add_argument(dest='energyplus_idd', metavar='energyplus_idd',
                        help='Path to input Energy+.idd',
                        type=lambda x: Path(x) if path.isfile(x) else parser.error(f'Energy+.idd not found: {x}'))
    parser.add_argument(dest='energyplus_epJSON', metavar='energyplus_epJSON',
                        help='Path to output directory',
                        type=lambda x: Path(x))
    parsed_args = parser.parse_args()

    p = mp.Process(target=generate_schema, args=[parsed_args.energyplus_idd, parsed_args.energyplus_epJSON])
    p.start()
    # Wait for 30 second timeout or until finished
    p.join(30)

    if p.is_alive():
        # too late, kill it with fire
        p.kill()
        p.join()
        raise RuntimeError('Generating epJSON Schema from IDD failed!')
