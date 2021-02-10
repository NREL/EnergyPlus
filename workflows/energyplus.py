# EnergyPlus, Copyright (c) 1996-2021, The Board of Trustees of the University
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

import os
import platform
import subprocess
import shutil

from eplaunch.utilities.version import Version
from eplaunch.workflows.base import BaseEPLaunchWorkflow1, EPLaunchWorkflowResponse1


class ColumnNames(object):
    Errors = 'Errors'
    Warnings = 'Warnings'
    Runtime = 'Runtime [s]'
    Version = 'Version'


class EPlusRunManager(object):

    @staticmethod
    def get_end_summary_from_err(err_file_path):
        contents = open(err_file_path, 'r').readlines()
        last_line = contents[-1]
        if 'EnergyPlus' not in last_line and 'Elapsed Time' not in last_line:
            return False, None, None, None
        last_line_tokens = last_line.split(' ')
        num_warnings = int(last_line_tokens[7])
        num_errors = int(last_line_tokens[9])
        time_position_marker = last_line.index('Time=')
        time_string = last_line[time_position_marker:]
        num_hours = int(time_string[5:7])
        num_minutes = int(time_string[10:12])
        num_seconds = float(time_string[16:21])
        runtime_seconds = num_seconds + num_minutes / 60 + num_hours / 3600
        return True, num_errors, num_warnings, runtime_seconds

    @staticmethod
    def eplus_suffixes():
        suffixes = list()
        # the following are in the same order as the buttons in EP-Launch 2
        suffixes.append("Table.htm")
        suffixes.append(".csv")
        suffixes.append("Meter.csv")

        suffixes.append(".err")
        suffixes.append(".rdd")
        suffixes.append(".mdd")

        suffixes.append(".eio")
        suffixes.append(".svg")
        suffixes.append(".dxf")

        suffixes.append(".mtd")
        suffixes.append("Zsz.csv")
        suffixes.append("Ssz.csv")

        suffixes.append("DElight.in")
        suffixes.append("DElight.out")
        suffixes.append("Map.csv")

        suffixes.append("DElight.eldmp")
        suffixes.append("DElight.dfdmp")
        suffixes.append("Screen.csv")

        suffixes.append(".expidf")
        suffixes.append(".epmidf")
        suffixes.append(".epmdet")

        suffixes.append(".shd")
        suffixes.append(".wrl")
        suffixes.append(".audit")

        suffixes.append(".bnd")
        suffixes.append(".dbg")
        suffixes.append(".sln")

        suffixes.append("_bsmt.out")
        suffixes.append(".bsmt")
        suffixes.append("_bsmt.audit")

        suffixes.append(".eso")
        suffixes.append(".mtr")
        suffixes.append("Proc.csv")

        suffixes.append("_slab.out")
        suffixes.append(".slab")
        suffixes.append("_slab.ger")

        suffixes.append("_bsmt.csv")
        suffixes.append(".edd")
        suffixes.append("Table.xml")
        suffixes.append("_perflog.csv")

        # the following were not included in EP-Launch 2
        suffixes.append(".end")
        suffixes.append(".sci")
        suffixes.append(".rvaudit")
        suffixes.append(".sql")
        suffixes.append(".log")

        # the rest of these are alternative extensions for the same
        suffixes.append("Table.csv")
        suffixes.append("Table.tab")
        suffixes.append("Table.txt")

        suffixes.append(".tab")
        suffixes.append(".txt")

        suffixes.append("Meter.tab")
        suffixes.append("Meter.txt")

        suffixes.append("Zsz.tab")
        suffixes.append("Zsz.txt")
        suffixes.append("Ssz.tab")
        suffixes.append("Ssz.txt")

        suffixes.append("Map.tab")
        suffixes.append("Map.txt")
        return suffixes

    def run_energyplus(self, isIP, run_directory, file_name, args):

        full_file_path = os.path.join(run_directory, file_name)
        file_name_no_ext, extension = os.path.splitext(file_name)
        # the following is the same when .idf is used

        if 'workflow location' in args:
            energyplus_root_folder, _ = os.path.split(args['workflow location'])

            # Run EnergyPlus binary
            if platform.system() == 'Windows':
                energyplus_binary = os.path.join(energyplus_root_folder, 'energyplus.exe')
            else:
                energyplus_binary = os.path.join(energyplus_root_folder, 'energyplus')
            if not os.path.exists(energyplus_binary):
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="EnergyPlus binary not found: {}!".format(energyplus_binary),
                    column_data=[]
                )
        else:
            return EPLaunchWorkflowResponse1(
                success=False,
                message="Workflow location missing: {}!".format(args['worflow location']),
                column_data=[]
            )

        v = Version()
        is_found, current_version, numeric_version = v.check_energyplus_version(full_file_path)
        if is_found:
            if numeric_version >= 80300:  # EnergyPlus 8.3.0 was the first with command line options.

                # start with the binary name, obviously
                command_line_args = [energyplus_binary]

                if extension == '.imf':
                    command_line_args += ['--epmacro']

                if extension != '.epJSON':
                    command_line_args += ['--expandobjects']

                # if not isIP:  # if using SI output units just use readvars CLI option
                #     command_line_args += ['--readvars']

                # add some config parameters
                command_line_args += ['--output-prefix', file_name_no_ext, '--output-suffix', 'C']

                # add in simulation control args
                if 'weather' in args and args['weather']:
                    command_line_args += ['--weather', args['weather']]
                else:
                    command_line_args += ['--design-day']

                # and at the very end, add the file to run
                command_line_args += [full_file_path]

                # run E+ and gather (for now fake) data
                try:
                    for message in self.execute_for_callback(command_line_args, run_directory):
                        self.callback(message)
                except subprocess.CalledProcessError:
                    self.callback("E+ FAILED")
                    return EPLaunchWorkflowResponse1(
                        success=False,
                        message="EnergyPlus failed for file: %s!" % full_file_path,
                        column_data={}
                    )

                # if isIP:
                # set up the ESO and MTR output files for either unit conversion or just ReadVarsESO
                # *.eso back to eplusout.eso
                eso_path = os.path.join(run_directory, file_name_no_ext + '.eso')
                eplusouteso_path = os.path.join(run_directory, 'eplusout.eso')
                if os.path.exists(eso_path):
                    shutil.copy(eso_path, eplusouteso_path)
                # *.mtr back to eplusout.mtr
                mtr_path = os.path.join(run_directory, file_name_no_ext + '.mtr')
                eplusoutmtr_path = os.path.join(run_directory, 'eplusout.mtr')
                if os.path.exists(mtr_path):
                    shutil.copy(mtr_path, eplusoutmtr_path)

                if isIP:
                    # run the ConvertESOMTR program to create IP versions of the timestep based output files
                    if platform.system() == 'Windows':
                        convertESOMTR_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'convertESOMTRpgm',
                                                            'convertESOMTR.exe')
                    else:
                        convertESOMTR_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'convertESOMTRpgm',
                                                            'convertESOMTR')
                    if os.path.exists(convertESOMTR_binary):
                        converttxt_orig_path = os.path.join(energyplus_root_folder, 'PostProcess', 'convertESOMTRpgm',
                                                            'convert.txt')
                        converttxt_run_path = os.path.join(run_directory, 'convert.txt')
                        shutil.copy(converttxt_orig_path, converttxt_run_path)

                        command_line_args = [convertESOMTR_binary]
                        try:
                            for message in self.execute_for_callback(command_line_args, run_directory):
                                self.callback(message)
                        except subprocess.CalledProcessError:
                            self.callback("ConvertESOMTR FAILED")
                            return EPLaunchWorkflowResponse1(
                                success=False,
                                message="ConvertESOMTR failed for file: %s!" % full_file_path,
                                column_data={}
                            )
                        # copy converted IP version of ESO file to users *.eso file
                        ipeso_path = os.path.join(run_directory, 'ip.eso')
                        if os.path.exists(ipeso_path):
                            shutil.copy(ipeso_path, eso_path)
                            os.replace(ipeso_path, eplusouteso_path)
                        # copy converted IP version of MTR file to users *.mtr file
                        ipmtr_path = os.path.join(run_directory, 'ip.mtr')
                        if os.path.exists(ipmtr_path):
                            shutil.copy(ipmtr_path, mtr_path)
                            os.replace(ipmtr_path, eplusoutmtr_path)
                        os.remove(converttxt_run_path)

                # run ReadVarsESO to convert the timestep based output files to CSV files
                if platform.system() == 'Windows':
                    readvarseso_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'ReadVarsESO.exe')
                else:
                    readvarseso_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'ReadVarsESO')
                if os.path.exists(readvarseso_binary):

                    command_line_args = [readvarseso_binary]
                    rvi_path = os.path.join(run_directory, file_name_no_ext + '.rvi')
                    temp_rvi_path = os.path.join(run_directory, 'temp.rvi')
                    eplusout_rvi_path = os.path.join(run_directory, 'eplusout.rvi')
                    if os.path.exists(rvi_path):
                        shutil.copy(rvi_path, eplusout_rvi_path)
                        command_line_args.append('eplusout.rvi')
                    else:
                        f = open(temp_rvi_path, "w+")
                        f.write('eplusout.eso \n')
                        f.write('eplusout.csv \n')
                        f.close()
                        command_line_args.append('temp.rvi')
                    command_line_args.append('unlimited')  # no number of column limit

                    try:
                        for message in self.execute_for_callback(command_line_args, run_directory):
                            self.callback(message)
                    except subprocess.CalledProcessError:
                        self.callback("ReadVarsESO FAILED on ESO file")
                        return EPLaunchWorkflowResponse1(
                            success=False,
                            message="ReadVarsESO failed for ESO file: %s!" % full_file_path,
                            column_data={}
                        )
                    vari_csv_path = os.path.join(run_directory, file_name_no_ext + '.csv')
                    eplusout_csv_path = os.path.join(run_directory, 'eplusout.csv')
                    if os.path.exists(eplusout_csv_path):
                        os.replace(eplusout_csv_path, vari_csv_path)

                    command_line_args = [readvarseso_binary]
                    mvi_path = os.path.join(run_directory, file_name_no_ext + '.mvi')
                    temp_mvi_path = os.path.join(run_directory, 'temp.mvi')
                    eplusout_mvi_path = os.path.join(run_directory, 'eplusout.mvi')
                    if os.path.exists(mvi_path):
                        shutil.copy(mvi_path, eplusout_mvi_path)
                        command_line_args.append('eplusout.mvi')
                    else:
                        f = open(temp_mvi_path, "w+")
                        f.write('eplusout.mtr \n')
                        f.write('eplusmtr.csv \n')
                        f.close()
                        command_line_args.append('temp.mvi')
                    command_line_args.append('unlimited')  # no number of column limit

                    try:
                        for message in self.execute_for_callback(command_line_args, run_directory):
                            self.callback(message)
                    except subprocess.CalledProcessError:
                        self.callback("ReadVarsESO FAILED on MTR file")
                        return EPLaunchWorkflowResponse1(
                            success=False,
                            message="ReadVarsESO failed for MTR file: %s!" % full_file_path,
                            column_data={}
                        )
                    mtr_csv_path = os.path.join(run_directory, file_name_no_ext + 'Meter.csv')
                    eplusmtr_csv_path = os.path.join(run_directory, 'eplusmtr.csv')
                    if os.path.exists(eplusmtr_csv_path):
                        os.replace(eplusmtr_csv_path, mtr_csv_path)

                    readvars_audit_path = os.path.join(run_directory, 'readvars.audit')
                    rv_audit_path = os.path.join(run_directory, file_name_no_ext + '.rvaudit')
                    if os.path.exists(readvars_audit_path):
                        os.replace(readvars_audit_path, rv_audit_path)

                    # clean up things inside this IF block
                    if os.path.exists(temp_rvi_path):
                        os.remove(temp_rvi_path)
                    if os.path.exists(temp_mvi_path):
                        os.remove(temp_mvi_path)
                    if os.path.exists(eplusout_rvi_path):
                        os.remove(eplusout_rvi_path)
                    if os.path.exists(eplusout_mvi_path):
                        os.remove(eplusout_mvi_path)

                # clean up more things
                if os.path.exists(eplusouteso_path):
                    os.remove(eplusouteso_path)
                if os.path.exists(eplusoutmtr_path):
                    os.remove(eplusoutmtr_path)
                audit_out_path = os.path.join(run_directory, 'audit.out')
                if os.path.exists(audit_out_path):
                    os.remove(audit_out_path)
                expanded_idf_path = os.path.join(run_directory, 'expanded.idf')
                if os.path.exists(expanded_idf_path):
                    os.remove(expanded_idf_path)
                out_idf_path = os.path.join(run_directory, 'out.idf')
                if os.path.exists(out_idf_path):
                    os.remove(out_idf_path)

                # run HVAC-Diagram
                if platform.system() == 'Windows':
                    hvac_diagram_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'HVAC-Diagram.exe')
                else:
                    hvac_diagram_binary = os.path.join(energyplus_root_folder, 'PostProcess', 'HVAC-Diagram')
                if os.path.exists(hvac_diagram_binary):
                    bnd_path = os.path.join(run_directory, file_name_no_ext + '.bnd')
                    eplusout_bnd_path = os.path.join(run_directory, 'eplusout.bnd')
                    if os.path.exists(bnd_path):
                        shutil.copy(bnd_path, eplusout_bnd_path)
                        command_line_args = [hvac_diagram_binary]
                    try:
                        for message in self.execute_for_callback(command_line_args, run_directory):
                            self.callback(message)
                    except subprocess.CalledProcessError:
                        self.callback("HVAC-Diagram FAILED on BND file")
                        return EPLaunchWorkflowResponse1(
                            success=False,
                            message="HVAC-Diagram failed for BND file: %s!" % full_file_path,
                            column_data={}
                        )
                    svg_path = os.path.join(run_directory, file_name_no_ext + '.svg')
                    eplusout_svg_path = os.path.join(run_directory, 'eplusout.svg')
                    if os.path.exists(eplusout_svg_path):
                        os.replace(eplusout_svg_path, svg_path)
                    if os.path.exists(eplusout_bnd_path):
                        os.remove(eplusout_bnd_path)

                # check on .end file and finish up
                err_file_name = "{0}.err".format(file_name_no_ext)
                err_file_path = os.path.join(run_directory, err_file_name)
                success, errors, warnings, runtime = EPlusRunManager.get_end_summary_from_err(err_file_path)

                column_data = {
                    ColumnNames.Errors: errors,
                    ColumnNames.Warnings: warnings,
                    ColumnNames.Runtime: runtime,
                    ColumnNames.Version: current_version
                }

                # now leave
                return EPLaunchWorkflowResponse1(
                    success=True,
                    message="Ran EnergyPlus OK for file: %s!" % file_name,
                    column_data=column_data
                )
            else:
                errors = "wrong version"
                column_data = {ColumnNames.Errors: errors, ColumnNames.Warnings: '', ColumnNames.Runtime: 0,
                               ColumnNames.Version: current_version}

                # now leave
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="Incorrect Version found {}: {}!".format(current_version, file_name),
                    column_data=column_data
                )
        else:

            errors = "wrong version"
            column_data = {
                ColumnNames.Errors: errors,
                ColumnNames.Warnings: '',
                ColumnNames.Runtime: 0,
                ColumnNames.Version: current_version
            }

            # now leave
            return EPLaunchWorkflowResponse1(
                success=False,
                message="Incorrect Version found {}: {}!".format(current_version, file_name),
                column_data=column_data
            )


class EnergyPlusWorkflowSI(BaseEPLaunchWorkflow1):

    def name(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH} SI"

    def context(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"

    def description(self):
        return "Run EnergyPlus with SI unit system"

    def uses_weather(self):
        return True

    def get_file_types(self):
        return ["*.idf", "*.imf", "*.epJSON"]

    def get_output_suffixes(self):
        return EPlusRunManager.eplus_suffixes()

    def get_extra_data(self):
        return {"Hey, it's extra": "data"}

    def get_interface_columns(self):
        return [ColumnNames.Errors, ColumnNames.Warnings, ColumnNames.Runtime, ColumnNames.Version]

    def main(self, run_directory, file_name, args):
        response = EPlusRunManager.run_energyplus(self, False, run_directory, file_name, args)
        if type(response) is not EPLaunchWorkflowResponse1:
            response = EPLaunchWorkflowResponse1(
                success=False,
                message='Current workflow run_energyplus function did not respond properly',
                column_data=None
            )
        return response


class EnergyPlusWorkflowIP(BaseEPLaunchWorkflow1):

    def name(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH} IP"

    def context(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"

    def description(self):
        return "Run EnergyPlus with IP unit system"

    def uses_weather(self):
        return True

    def get_file_types(self):
        return ["*.idf", "*.imf", "*.epJSON"]

    def get_output_suffixes(self):
        return EPlusRunManager.eplus_suffixes()

    def get_extra_data(self):
        return {"Hey, it's extra": "data"}

    def get_interface_columns(self):
        return [ColumnNames.Errors, ColumnNames.Warnings, ColumnNames.Runtime, ColumnNames.Version]

    def main(self, run_directory, file_name, args):
        response = EPlusRunManager.run_energyplus(self, True, run_directory, file_name, args)
        if type(response) is not EPLaunchWorkflowResponse1:
            response = EPLaunchWorkflowResponse1(
                success=False,
                message='Current workflow run_energyplus function did not respond properly',
                column_data=None
            )
        return response
