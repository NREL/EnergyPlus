import os
import subprocess

import gobject
import gtk

from EnergyPlusPath import EnergyPlusPath
from EnergyPlusThread import EnergyPlusThread
from FileTypes import FileTypes
from International import translate as _, Languages, set_language
from Settings import Keys


__program_name__ = "EP-Launch-Lite (v2.0)"


class Window(gtk.Window):
    """
    This class is the main window class for EP-Launch-Lite
    """

    def __init__(self, settings):
        """
        This initializer function creates instance variables, sets up threading, and builds the GUI
        """

        # initialize the parent class
        super(Window, self).__init__()

        # this flag will be used to trigger a restart from the calling manager
        self.doing_restart = False

        # initialize some class-level "constants"
        self.box_spacing = 4

        # initialize instance variables to be set later
        self.input_file_path = None
        self.weather_file_path = None
        self.button_sim = None
        self.button_cancel = None
        self.ep_run_folder = None
        self.running_simulation_thread = None
        self.status_bar = None
        self.status_bar_context_id = None
        self.ep_version_label = None
        self.edit_idf_button = None

        # try to load the settings very early since it includes initialization
        self.settings = settings
        set_language(self.settings[Keys.language])

        # prepare threading
        gobject.threads_init()

        # connect signals for the GUI
        self.connect("destroy", self.quit)

        # build up the GUI itself
        self.build_gui()

        # update the list of E+ versions
        self.ep_run_folder = EnergyPlusPath.get_latest_eplus_version()
        self.ep_version_label.set_text(EnergyPlusThread.get_ep_version(os.path.join(self.ep_run_folder, 'EnergyPlus')))

        # for good measure, check the validity of the idf/epw versions once at load time
        self.check_file_paths(None)

    def quit(self, widget=None):
        try:
            gtk.main_quit()
        except RuntimeError:
            pass  # ignore the called outside of a mainloop in this instance

    def build_gui(self):
        """
        This function manages the window construction, including position, title, and presentation
        """

        # put the window in the center of the (primary? current?) screen
        self.set_position(gtk.WIN_POS_CENTER)

        # make a nice border around the outside of the window
        self.set_border_width(0)

        # set the window title
        self.set_title(__program_name__)

        # add the body
        self.add(self.gui_build_body())

        # this brings the window to the front (unless the opening terminal is in the way)
        self.present()

        # shows all child widgets recursively
        self.show_all()

    def framed(self, thing, color_code="#DB5700"):
        frames_on = False
        if not frames_on:
            return thing
        f = gtk.Frame()
        f.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse(color_code))
        f.add(thing)
        return f

    def gui_build_body(self):
        """
        This function builds out the specific widgets on the GUI

        * Returns: A gtk.VBox suitable for adding directly onto the main gtk.Window
        """

        # create a vbox here first
        vbox = gtk.VBox(False, self.box_spacing)

        # create the menu bar itself to hold the menus
        mb = gtk.MenuBar()

        # create the actual actionable items under the file menu
        menu_item_file_about = gtk.MenuItem(_("About..."))
        menu_item_file_about.connect("activate", self.about_dialog)
        menu_item_file_about.show()
        menu_item_file_exit = gtk.MenuItem(_("Exit"))
        menu_item_file_exit.connect("activate", self.quit)
        menu_item_file_exit.show()

        # create the actual actionable items under the language menu
        menu_item_english = gtk.MenuItem("Language: English")
        menu_item_english.connect("activate", self.switch_language, Languages.English)
        menu_item_english.show()
        if self.settings[Keys.language] == Languages.English:
            menu_item_english.set_sensitive(False)
        menu_item_spanish = gtk.MenuItem("Idioma: Espanol")
        menu_item_spanish.connect("activate", self.switch_language, Languages.Spanish)
        menu_item_spanish.show()
        if self.settings[Keys.language] == Languages.Spanish:
            menu_item_spanish.set_sensitive(False)
        menu_item_french = gtk.MenuItem("Langue: Francais")
        menu_item_french.connect("activate", self.switch_language, Languages.French)
        menu_item_french.show()
        if self.settings[Keys.language] == Languages.French:
            menu_item_french.set_sensitive(False)

        # create the list of items that will eventually be dropped down, and append items in the right order
        filemenu = gtk.Menu()
        filemenu.append(menu_item_file_about)
        filemenu.append(gtk.SeparatorMenuItem())
        filemenu.append(menu_item_file_exit)
        langmenu = gtk.Menu()
        langmenu.append(menu_item_english)
        langmenu.append(menu_item_spanish)
        langmenu.append(menu_item_french)

        # create the root drop-down-able menu items, and assign their submenus to the lists above
        menu_item_file = gtk.MenuItem(_("File"))
        menu_item_file.set_submenu(filemenu)
        menu_item_lang = gtk.MenuItem("Language/Idioma/Langue")
        menu_item_lang.set_submenu(langmenu)

        # attach the root menus to the main menu bar
        mb.append(menu_item_file)
        mb.append(menu_item_lang)

        # and finally attach the main menu bar to the window
        vbox.pack_start(mb, False)

        # create the input file button and textbox section
        hbox1 = gtk.HBox(False, self.box_spacing)
        button1 = gtk.Button(_("Choose Input File.."))
        button1.connect("clicked", self.select_input_file, FileTypes.IDF)
        alignment = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=0.5)
        alignment.add(button1)
        hbox1.pack_start(alignment, True, True, self.box_spacing)
        self.input_file_path = gtk.Entry()
        self.input_file_path.connect("changed", self.check_file_paths)
        self.input_file_path.set_text(self.settings['last_idf'])  # "/tmp/RefBldgHospitalNew2004_Chicago.idf")
        self.input_file_path.set_size_request(width=500, height=-1)
        alignment = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=0.5)
        alignment.add(self.input_file_path)
        hbox1.pack_start(alignment, True, True, self.box_spacing)
        self.edit_idf_button = gtk.Button(_("Edit Input File.."))
        self.edit_idf_button.connect("clicked", self.open_input_file)
        alignment = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=0.5)
        alignment.add(self.edit_idf_button)
        hbox1.pack_start(alignment, True, True, self.box_spacing)
        vbox.pack_start(self.framed(hbox1), True, True, 0)

        # create the weather file button and textbox section
        hbox2 = gtk.HBox(False, self.box_spacing)
        button1 = gtk.Button(_("Choose Weather File.."))
        button1.connect("clicked", self.select_input_file, FileTypes.EPW)
        alignment = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=0.5)
        alignment.add(button1)
        hbox2.pack_start(alignment, True, True, self.box_spacing)
        self.weather_file_path = gtk.Entry()
        self.weather_file_path.connect("changed", self.check_file_paths)
        self.weather_file_path.set_text(
            self.settings['last_epw'])  # '"/Users/elee/EnergyPlus/repos/2eplus/weather/CZ06RV2.epw")
        self.weather_file_path.set_size_request(width=500, height=-1)
        alignment = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=0.5)
        alignment.add(self.weather_file_path)
        hbox2.pack_start(alignment, True, True, self.box_spacing)
        vbox.pack_start(self.framed(hbox2), True, True, 0)

        # separator
        vbox.pack_start(self.framed(gtk.HSeparator()), False)

        # create the simulate/cancel button section
        hbox3 = gtk.HBox(False, self.box_spacing)
        self.button_sim = gtk.Button(_("Simulate"))
        self.button_sim.connect("clicked", self.run_simulation)
        alignment = gtk.Alignment(xalign=0.5, yalign=0.5, xscale=0.5, yscale=0.5)
        alignment.add(self.button_sim)
        hbox3.pack_start(alignment, True, True, self.box_spacing)
        self.button_cancel = gtk.Button(_("Cancel"))
        self.button_cancel.connect("clicked", self.cancel_simulation)
        alignment = gtk.Alignment(xalign=0.5, yalign=0.5, xscale=0.5, yscale=0.5)
        alignment.add(self.button_cancel)
        self.update_run_buttons(running=False)
        hbox3.pack_start(alignment, True, True, self.box_spacing)
        # self.button_language = gtk.Button(_("Switch language"))
        # self.button_language.connect("clicked", self.switch_language)
        # alignment = gtk.Alignment(xalign=0.5, yalign=0.5, xscale=0.5, yscale=0.5)
        # alignment.add(self.button_language)
        # hbox3.pack_start(alignment, True, True, self.box_spacing)
        vbox.pack_start(self.framed(hbox3), True, True, 0)

        # separator
        vbox.pack_start(self.framed(gtk.HSeparator()), False)

        # create the status bar
        hbox = gtk.HBox(False, self.box_spacing)
        self.ep_version_label = gtk.Label()
        self.ep_version_label.set_text(_("E+ Version"))
        aligner = gtk.Alignment(1, 0.5, 1, 1)
        aligner.add(self.ep_version_label)
        hbox.pack_start(self.framed(gtk.VSeparator()), False)
        hbox.pack_start(aligner, False, True, 0)
        self.status_bar = gtk.Statusbar()
        self.status_bar.set_has_resize_grip(False)
        self.status_bar_context_id = self.status_bar.get_context_id("Statusbar example")
        self.status_bar.push(self.status_bar_context_id, _("Ready for launch"))
        aligner = gtk.Alignment(1, 1, 1, 0)
        aligner.add(self.status_bar)
        hbox.pack_start(self.framed(gtk.VSeparator()), False)
        hbox.pack_start(aligner)
        vbox.pack_end(self.framed(hbox), False, True, 0)
        hbox.pack_start(self.framed(gtk.VSeparator()), False)

        # return the vbox
        return vbox

    def open_input_file(self, widget):
        try:
            subprocess.check_call(['open', self.input_file_path.get_text()], shell=False)
        except Exception:
            self.simple_error_dialog(
                _("Could not open input file, set default application by opening the file separately first.")
            )

    def switch_language(self, widget, language):
        self.settings[Keys.language] = language
        dialog = gtk.MessageDialog(
            parent=self,
            flags=0,
            type=gtk.MESSAGE_ERROR,
            buttons=gtk.BUTTONS_YES_NO,
            message_format=__program_name__)
        dialog.set_title(_("Message"))
        dialog.format_secondary_text(
            _("You must restart the app to make the language change take effect.  Would you like to restart now?"))
        resp = dialog.run()
        if resp == gtk.RESPONSE_YES:
            self.doing_restart = True
        dialog.destroy()
        if self.doing_restart:
            self.quit(None)

    def select_input_file(self, widget, flag):
        message, file_filters = FileTypes.get_materials(flag)
        if flag == FileTypes.IDF:
            key = Keys.last_idf_folder
        else:
            key = Keys.last_epw_folder
        dialog = gtk.FileChooserDialog(
            title=message,
            action=gtk.FILE_CHOOSER_ACTION_OPEN,
            buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_OPEN, gtk.RESPONSE_OK)
        )
        dialog.set_select_multiple(False)
        for file_filter in file_filters:
            dialog.add_filter(file_filter)
        if self.settings[key] is not None:
            dialog.set_current_folder(self.settings[key])
        response = dialog.run()
        if response == gtk.RESPONSE_OK:
            self.settings[key] = dialog.get_current_folder()
            if flag == FileTypes.IDF:
                self.input_file_path.set_text(dialog.get_filename())
            elif flag == FileTypes.EPW:
                self.weather_file_path.set_text(dialog.get_filename())
            dialog.destroy()
        else:
            print(_("Cancelled!"))
            dialog.destroy()

    def run_simulation(self, widget):
        self.running_simulation_thread = EnergyPlusThread(
            os.path.join(self.ep_run_folder, 'energyplus'),
            self.input_file_path.get_text(),
            self.weather_file_path.get_text(),
            self.message,
            self.callback_handler_success,
            self.callback_handler_failure,
            self.callback_handler_cancelled
        )
        self.running_simulation_thread.start()
        self.update_run_buttons(running=True)

    def update_run_buttons(self, running=False):
        self.button_sim.set_sensitive(not running)
        self.button_cancel.set_sensitive(running)

    def message(self, message):
        gobject.idle_add(self.message_handler, message)

    def message_handler(self, message):
        self.status_bar.push(self.status_bar_context_id, message)

    def callback_handler_cancelled(self):
        gobject.idle_add(self.cancelled_simulation)

    def cancelled_simulation(self):
        self.update_run_buttons(running=False)

    def callback_handler_failure(self, std_out, run_dir, cmd_line):
        gobject.idle_add(self.failed_simulation, std_out, run_dir, cmd_line)

    def failed_simulation(self, std_out, run_dir, cmd_line):
        self.update_run_buttons(running=False)
        message = gtk.MessageDialog(parent=self,
                                    flags=0,
                                    type=gtk.MESSAGE_ERROR,
                                    buttons=gtk.BUTTONS_YES_NO,
                                    message_format=_("EnergyPlus Failed!"))
        message.set_title(_("EnergyPlus Failed"))
        message.format_secondary_text(
            _("Error file is the best place to start.  Would you like to open the Run Folder?"))
        response = message.run()
        if response == gtk.RESPONSE_YES:
            subprocess.Popen(['open', run_dir], shell=False)
        message.destroy()

    def callback_handler_success(self, std_out, run_dir):
        gobject.idle_add(self.completed_simulation, std_out, run_dir)

    def completed_simulation(self, std_out, run_dir):
        # update the GUI buttons
        self.update_run_buttons(running=False)
        # create the dialog
        result_dialog = gtk.Dialog(_("Simulation Output"),
                                   self,
                                   gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                                   (_("Open Run Directory"), gtk.RESPONSE_YES, _("Close"), gtk.RESPONSE_ACCEPT)
                                   )
        # put a description label
        label = gtk.Label(_("EnergyPlus Simulation Output:"))
        label.show()
        aligner = gtk.Alignment(xalign=1.0, yalign=0.5, xscale=1.0, yscale=1.0)
        aligner.add(label)
        aligner.show()
        result_dialog.vbox.pack_start(aligner, False, True, 0)

        # put the actual simulation results
        label = gtk.Label(std_out)
        scrolled_results = gtk.ScrolledWindow()
        scrolled_results.set_border_width(10)
        scrolled_results.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
        scrolled_results.add_with_viewport(label)
        scrolled_results.show()
        result_dialog.vbox.pack_start(scrolled_results, True, True, 0)
        label.show()
        result_dialog.set_size_request(width=500, height=600)
        resp = result_dialog.run()
        if resp == gtk.RESPONSE_YES:
            try:
                subprocess.Popen(['open', run_dir], shell=False)
            except Exception:
                self.simple_error_dialog(_("Could not open run directory"))
        result_dialog.destroy()

    def cancel_simulation(self, widget):
        self.button_cancel.set_sensitive(False)
        self.running_simulation_thread.stop()

    def check_file_paths(self, widget):
        if self.weather_file_path is None or self.input_file_path is None or self.status_bar is None:
            return  # we are probably doing early initialization of the GUI
        idf = self.input_file_path.get_text()
        epw = self.weather_file_path.get_text()
        self.settings[Keys.last_idf] = idf
        self.settings[Keys.last_epw] = epw
        if os.path.exists(idf) and os.path.exists(epw):
            self.message_handler(_("Ready for launch"))
            self.button_sim.set_sensitive(True)
            self.edit_idf_button.set_sensitive(True)
        else:
            self.message_handler(_("Input and/or Weather file paths are invalid"))
            self.button_sim.set_sensitive(False)
            self.edit_idf_button.set_sensitive(False)

    def simple_error_dialog(self, message_text):
        message = gtk.MessageDialog(parent=self,
                                    flags=0,
                                    type=gtk.MESSAGE_ERROR,
                                    buttons=gtk.BUTTONS_OK,
                                    message_format=_("Error performing prior action:"))
        message.set_title(__program_name__)
        message.format_secondary_text(message_text)
        message.run()
        message.destroy()

    def about_dialog(self, widget):
        message = gtk.MessageDialog(parent=self,
                                    flags=0,
                                    type=gtk.MESSAGE_INFO,
                                    buttons=gtk.BUTTONS_OK,
                                    message_format=__program_name__)
        message.set_title(__program_name__)
        message.format_secondary_text(_("ABOUT_DIALOG"))
        message.run()
        message.destroy()