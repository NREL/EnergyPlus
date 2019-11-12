/* QtIFW Installer script for EnergyPlus

Allows performing silent installs on all platforms.

NOTE: By running a silent install, you agree to the EnergyPlus License Agreement

Usage:

```
# Linux
sudo ./EnergyPlus-9.2.0-0e6e9c08a0-Linux-x86_64.run --verbose --platform minimal --script install_script.qs

# Windows: open cmd.exe as admin
EnergyPlus-9.2.0-0e6e9c08a0-Windows-x86_64.exe --verbose --platform minimal --script install_script.qs

# Mac: `--plaftorm minimal` appears to produce a segmentation fault
open EnergyPlus-9.2.0-921312fa1d-Darwin-x86_64.dmg
sudo /Volumes/EnergyPlus-9.2.0-921312fa1d-Darwin-x86_64/EnergyPlus-9.2.0-921312fa1d-Darwin-x86_64.app/Contents/MacOS/EnergyPlus-9.2.0-921312fa1d-Darwin-x86_64 --verbose --script install_script.qs
```

You can also customize the install directory by passing `TargetDir`
If you don't, it defaults to;
* Linux: `/usr/local/EnergyPlus-9-2-0`
* Mac: `/Applications/EnergyPlus-9-2-0`
* Windows: `C:\EnergyPlusV9-2-0`

```
 sudo ./EnergyPlus-9.2.0-0e6e9c08a0-Linux-x86_64.run --verbose --platform minimal --script install_script.qs TargetDir=/usr/local/Eplus
```

By default, all components will be installed.
We also built-in a way to unselect components via the command line by passing
`<ComponentName>=false` (case matters, has to be `false` exactly)

Unix:
```
 sudo ./EnergyPlus-9.2.0-0e6e9c08a0-Linux-x86_64.run --verbose --platform minimal --script install_script.qs Documentation=false ExampleFiles=false WeatherData=false Datasets=false Symlinks=false
```

Windows: Open cmd.exe as admin

```
EnergyPlus-9.2.0-0e6e9c08a0-Windows-x86_64.exe --verbose --platform minimal --script install_script.qs Documentation=false ExampleFiles=false WeatherData=false Datasets=false CreateStartMenu=false RegisterFileType=false
```


NOTE: You can also pass this same script to the maintenancetool for a silent
complete uninstall

```
# Linux
sudo /usr/local/EnergyPlus-9-2-0/maintenancetool --verbose --plaftorm minimal --script install_script.qs
# Mac: `--platform minimal` appears to produce a segfault right now
sudo /Applications/EnergyPlus-9-2-0/maintenancetool.app/Contents/MacOS/maintenancetool --verbose --platform minimal --script install_script.qs

```

**/


function Controller() {
  installer.autoRejectMessageBoxes();

  installer.setMessageBoxAutomaticAnswer("OverwriteTargetDirectory",
                                         QMessageBox.Yes);

  installer.installationFinished.connect(function() {
    gui.clickButton(buttons.NextButton);
  });
  // Uninstaller
  installer.uninstallationFinished.connect(function() {
    gui.clickButton(buttons.NextButton);
  });
}

function logCurrentPage() {
    var pageName = gui.currentPageWidget().objectName;
    var pagePrettyTitle = gui.currentPageWidget().title;
    console.log("At page: " + pageName + " ('" + pagePrettyTitle + "')");
}

// NOT USED
/*
 *Controller.prototype.WelcomePageCallback = function() {
 *  console.log("---- WELCOME PAGE");
 *  logCurrentPage();
 *  // click delay because the next button is initially disabled for ~1s
 *  gui.clickButton(buttons.NextButton, 3000);
 *};
 *
 *Controller.prototype.CredentialsPageCallback = function() {
 *  console.log("---- CREDENTIAL PAGE");
 *  logCurrentPage();
 *  gui.clickButton(buttons.NextButton);
 *};
 */

Controller.prototype.IntroductionPageCallback = function() {
  console.log("---- INTRODUCTION PAGE");
  logCurrentPage();
  gui.clickButton(buttons.NextButton);
};

Controller.prototype.TargetDirectoryPageCallback = function()
{
  console.log("---- TARGET DIRECTORY PAGE");
  logCurrentPage();

  console.log("User-suplied TargetDir: " + installer.value("TargetDir"));

  // gui.currentPageWidget().TargetDirectoryLineEdit.setText(installer.value("harcoded/path/EnergyPlus");
  console.log("Target dir: " +
              gui.currentPageWidget().TargetDirectoryLineEdit.text);

  gui.clickButton(buttons.NextButton);
};

Controller.prototype.ComponentSelectionPageCallback = function() {
  console.log("---- COMPONENT SELECTION PAGE");
  logCurrentPage();

  var widget = gui.currentPageWidget();

  var components = installer.components();
  console.log("There are " + components.length + " available components.");
  console.log("Components selection for installation:");
  for (var i = 0; i < components.length; i++) {
    var compName = components[i].name;
    var installStatus = "Yes";

    // Get command line args passed
    var thisCompFlag = installer.value(compName);

    if (thisCompFlag === "false") {
      // Protect against trying to deselect something required
      if (["Unspecified", "Licenses"].indexOf(compName) >= 0) {
        console.log("-- Component '" + compName +
                    "' is mandatory and cannot be unselected");
        installStatus = "Yes (FORCED AS REQUIRED)";
      } else if (compName === "CopyAndRegisterSystemDLLs") {
        console.log("-- CopyAndRegisterSystemDLLs is highly recommended on Windows, and it will not overwrite any existing DLLs so it should not have side effects");
        // Allow unselect anyways
        installStatus = "No (use at your own risk)";
        widget.deselectComponent(compName);
      } else {
        installStatus = "No";
        widget.deselectComponent(compName);
      }
    }
    console.log("* " + compName + ": " + installStatus);
  }

  // widget.deselectAll();

  // All Plaftorms: "Documentation", "Datasets", "ExampleFiles",
  // "WeatherData"

  // Unix (Mac/Linux)
  // widget.deselectComponent("Symlinks");

  // Windows: "CreateStartMenu", "RegisterFileType", "CopyAndRegisterSystemDLLs"


  // Linux/Mac: Packages:  Symlinks Datasets Documentation ExampleFiles Licenses Unspecified WeatherData
  // Windows:   Packages:

  gui.clickButton(buttons.NextButton);
};

Controller.prototype.LicenseAgreementPageCallback = function() {
  console.log("---- LICENSE AGREEMENT PAGE");
  logCurrentPage();
  gui.currentPageWidget().AcceptLicenseRadioButton.setChecked(true);
  gui.clickButton(buttons.NextButton);
};

Controller.prototype.StartMenuDirectoryPageCallback = function() {
  console.log("---- START MENU DIRECTORY PAGE");
  logCurrentPage();
  gui.clickButton(buttons.NextButton);
};

Controller.prototype.ReadyForInstallationPageCallback = function()
{
  console.log("---- READY FOR INSTALLATION PAGE");
  logCurrentPage();
  gui.clickButton(buttons.CommitButton);
};

Controller.prototype.PerformInstallationPageCallback = function()
{
  console.log("---- PERFORM INSTALLATION PAGE");
  logCurrentPage();
  gui.clickButton(buttons.CommitButton);
};

Controller.prototype.FinishedPageCallback = function() {
  console.log("---- FINISHED PAGE");
  logCurrentPage();
  /*
   *var checkBoxForm = gui.currentPageWidget().LaunchQtCreatorCheckBoxForm;
   *if (checkBoxForm && checkBoxForm.launchQtCreatorCheckBox) {
   *  checkBoxForm.launchQtCreatorCheckBox.checked = false;
   *}
   */
  gui.clickButton(buttons.FinishButton);
};
