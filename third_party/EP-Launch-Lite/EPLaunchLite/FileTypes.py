import gtk

from International import translate as _


class FileTypes(object):
    IDF = 1
    EPW = 2

    @staticmethod
    def get_materials(file_type):
        filters = []
        if file_type == FileTypes.IDF:
            message = _("Select input file")
            idf_filter = gtk.FileFilter()
            idf_filter.set_name(_("IDF files"))
            idf_filter.add_pattern("*.idf")
            filters.append(idf_filter)
            imf_filter = gtk.FileFilter()
            imf_filter.set_name("IMF files")
            imf_filter.add_pattern("*.imf")
            filters.append(imf_filter)
        elif file_type == FileTypes.EPW:
            message = _("Select weather file")
            epw_filter = gtk.FileFilter()
            epw_filter.set_name(_("EPW files"))
            epw_filter.add_pattern("*.epw")
            filters.append(epw_filter)
        else:
            return None, None
        return message, filters
