import os
import platform
import subprocess

from eplaunch.workflows.base import BaseEPLaunchWorkflow1, EPLaunchWorkflowResponse1


class AppGPostProcessWorkflow(BaseEPLaunchWorkflow1):

    def name(self):
        return "AppGPostProcess-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}"

    def context(self):
        return "EnergyPlus-${CMAKE_VERSION_MAJOR}.${CMAKE_VERSION_MINOR}.${CMAKE_VERSION_PATCH}-${CMAKE_VERSION_BUILD}"

    def description(self):
        return "Run Appendix G Post Processor"

    def get_file_types(self):
        return ["*.html"]

    def get_output_suffixes(self):
        return ["-GAVG.html", "-GAVGMeter.csv", "-GAVGTable.txt"]

    def get_extra_data(self):
        return {"Hey, it's extra": "data"}

    def get_interface_columns(self):
        return []

    def main(self, run_directory, file_name, args):
        if 'workflow location' in args:
            energyplus_root_folder, _ = os.path.split(args['workflow location'])
            postprocess_folder = os.path.join(energyplus_root_folder, 'PostProcess')
            appgpp_folder = os.path.join(postprocess_folder, 'AppGPostProcess')
            if platform.system() == 'Windows':
                appgpp_binary = os.path.join(appgpp_folder, 'appgpostprocess.exe')
            else:
                appgpp_binary = os.path.join(appgpp_folder, 'appgpostprocess')
            if not os.path.exists(appgpp_binary):
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="AppGPostProcess binary not found: {}!".format(appgpp_binary),
                    column_data=[]
                )
        else:
            return EPLaunchWorkflowResponse1(
                success=False,
                message="Workflow location missing: {}!".format(args['worflow location']),
                column_data=[]
            )

        html_in_file_with_path = os.path.join(run_directory, file_name)
        html_in_file_no_ext, _ = os.path.splitext(html_in_file_with_path)
        if html_in_file_no_ext[-10:] != '-G000Table':
            return EPLaunchWorkflowResponse1(
                success=False,
                message='A file ending in -G000Table.html must be selected',
                column_data=[]
            )
        # else:
        # out_file_root = html_in_file_no_ext[:-10] + '-GAVG'
        # html_out_file = out_file_root + 'Table.html'
        # csv_out_file = out_file_root + '.csv'
        # csvmeter_out_file = out_file_root + 'Meter.csv'

        # copy input data file to working directory
        if os.path.exists(html_in_file_with_path) and os.path.exists(appgpp_binary) and os.path.exists(run_directory):

            # execute utility
            command_line_args = [appgpp_binary, html_in_file_with_path]
            process = subprocess.run(
                command_line_args,
                creationflags=subprocess.CREATE_NEW_CONSOLE,
                cwd=run_directory
            )
            if process.returncode == 0:
                return EPLaunchWorkflowResponse1(
                    success=True,
                    message="Ran AppendixGPostProcess OK for file: {}!".format(html_in_file_with_path),
                    column_data=[]
                )
            else:
                return EPLaunchWorkflowResponse1(
                    success=False,
                    message="AppendixGPostProcess failed for file: {}!".format(html_in_file_with_path),
                    column_data=[]
                )
        else:
            return EPLaunchWorkflowResponse1(
                success=False,
                message="AppendixGPostProcess files not found",
                column_data=[]
            )
