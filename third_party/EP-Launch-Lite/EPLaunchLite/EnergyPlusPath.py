import glob


class EnergyPlusPath(object):
    """
    This class provides static helpers for moving between EnergyPlus install paths and version numbers

    *Note that this class holds no instance data.*
    """

    @staticmethod
    def get_version_number_from_path(path):
        """
        This function takes a Mac EnergyPlus installation path, and returns just the version number portion

        * path: An installation path on Mac, following the form: '/Applications/EnergyPlus-?-?-?/'
        * Returns: Just the version number suffix, in the form: '?-?-?'
        """
        ep_folder = path.split('/')[2]
        if 'EnergyPlus' not in ep_folder:
            return None
        return ep_folder[11:]

    @staticmethod
    def get_path_from_version_number(version):
        return '/Applications/EnergyPlus-%s' % version

    @staticmethod
    def get_latest_eplus_version():
        # get all the installed versions first, sorted
        install_folders = glob.glob('/Applications/EnergyPlus*')

        # then process them into a nice list
        ep_versions = sorted([EnergyPlusPath.get_version_number_from_path(x) for x in install_folders])

        # set current_entry to something meaningful if needed
        new_version = ep_versions[-1]
        return EnergyPlusPath.get_path_from_version_number(new_version)
