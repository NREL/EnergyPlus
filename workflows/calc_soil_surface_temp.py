import os
import platform
import shutil
from subprocess import Popen, PIPE

from eplaunch.workflows.base import BaseEPLaunchWorkflow1, EPLaunchWorkflowResponse1


class ColumnNames(object):
    AverageSurfTemp = 'Avg Surf Temp [C]'
    AmplitudeSurfTemp = 'Amp Surf Temp [C]'
    PhaseConstant = 'Phase Constant [days]'


class CalcSoilSurfTempWorkflow(BaseEPLaunchWorkflow1):

    def name(self):
        return "CalcSoilSurfTemp-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}"

    def context(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"

    def description(self):
        return "Run CalcSoilSurfTemp Preprocessor"

    def get_file_types(self):
        return ["*.epw"]

    def get_output_suffixes(self):
        return [".out"]

    def get_extra_data(self):
        return {"Hey, it's extra": "data"}

    def get_interface_columns(self):
        return [ColumnNames.AverageSurfTemp, ColumnNames.AmplitudeSurfTemp, ColumnNames.PhaseConstant]

    def main(self, run_directory, file_name, args):
        if 'workflow location' in args:
            energyplus_root_folder, _ = os.path.split(args['workflow location'])
            preprocess_folder = os.path.join(energyplus_root_folder, 'PreProcess')
            calc_soil_surf_temp_folder = os.path.join(preprocess_folder, 'CalcSoilSurfTemp')
            if platform.system() == 'Windows':
                calc_soil_surf_temp_binary = os.path.join(calc_soil_surf_temp_folder, 'CalcSoilSurfTemp.exe')
            else:
                calc_soil_surf_temp_binary = os.path.join(calc_soil_surf_temp_folder, 'CalcSoilSurfTemp')
            if not os.path.exists(calc_soil_surf_temp_binary):
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="CalcSoilSurfTemp binary not found: {}!".format(calc_soil_surf_temp_binary),
                    column_data=[]
                )
        else:
            return EPLaunchWorkflowResponse1(
                success=False,
                message="Workflow location missing: {}!".format(args['worflow location']),
                column_data=[]
            )

        full_file_path = os.path.join(run_directory, file_name)
        full_file_no_ext, _ = os.path.splitext(full_file_path)
        out_file_path = full_file_no_ext + '.out'

        csst_out = os.path.join(run_directory, 'CalcSoilSurfTemp.out')
        if os.path.exists(csst_out):
            self.callback("Removing previous output file")
            os.remove(csst_out)

        if os.path.exists(out_file_path):
            try:
                os.remove(out_file_path)
            except OSError:
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="Could not delete prior CalcSoilSurfTemp results file: %s!" % out_file_path,
                    column_data={}
                )

        # run E+ and gather (for now fake) data
        self.callback("Running CalcSoilSurfTemp!")
        p1 = Popen([calc_soil_surf_temp_binary, file_name], stdout=PIPE, stdin=PIPE, cwd=run_directory)
        p1.communicate(input=b'1\n1\n')
        self.callback("CalcSoilSurfTemp Completed!")

        if not os.path.exists(csst_out):
            return EPLaunchWorkflowResponse1(
                success=False,
                message="CalcSoilSurfTemp failed for file: %s!" % full_file_path,
                column_data={}
            )

        if os.path.exists(out_file_path):
            os.remove(out_file_path)

        # Copy output files to input/output path
        shutil.copy2(csst_out, out_file_path)

        # Clean up directory.
        if os.path.exists(csst_out):
            os.remove(csst_out)

        try:
            self.callback("Processing output file...")
            with open(out_file_path, 'r') as f:
                lines = f.readlines()
                avg_temp = float(lines[1][40:-1])
                amp_temp = float(lines[2][38:-1])
                phase_constant = int(lines[3][42:-1])
                column_data = {
                    ColumnNames.AverageSurfTemp: avg_temp,
                    ColumnNames.AmplitudeSurfTemp: amp_temp,
                    ColumnNames.PhaseConstant: phase_constant
                }
                self.callback("   ...DONE!")
                return EPLaunchWorkflowResponse1(
                    success=True,
                    message="Ran EnergyPlus OK for file: %s!" % file_name,
                    column_data=column_data
                )
        except Exception:  # noqa -- there could be lots of issues here, file existence, file contents, float conversion
            self.callback("   ...FAILED!")
            return EPLaunchWorkflowResponse1(
                success=False,
                message="CalcSoilSurfTemp seemed to run, but post processing failed for file: %s!" % full_file_path,
                column_data={}
            )
