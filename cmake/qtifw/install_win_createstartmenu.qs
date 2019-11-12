// Windows commands to be performed elevated: copy and register DLLs

function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    // On Windows
    if( kernel == "winnt" ) {

      // Regular case: same as just using "@StartMenuDir@/link_name.lnk"
      var target_dir = installer.value("UserStartMenuProgramsPath") + "/" +
                       installer.value("StartMenuDir");
      console.log("StartMenuDir: " + target_dir);

      // For use with install_script.qs, for headless install via local system
      // admin
      if (installer.value("UseAllUsersStartMenu") === "true") {
        target_dir = installer.value("AllUsersStartMenuProgramsPath") + "/" +
                     installer.value("StartMenuDir");
        console.log("UseAllUsersStartMenu was passed!")
      }

      console.log("Target directory for Start Menu Shortcuts: " + target_dir);
      component.addOperation("CreateShortcut", "@TargetDir@/Documentation/index.html", target_dir + "/EnergyPlus Documentation.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.exe", target_dir + "/EP-Compare.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/EPDraw/EPDrawGUI.exe", target_dir + "/EPDrawGUI.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/EP-Launch.exe", target_dir + "/EP-Launch.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles.html", target_dir + "/Example Files Summary.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles-ObjectsLink.html", target_dir + "/ExampleFiles Link to Objects.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFEditor/IDFEditor.exe", target_dir + "/IDFEditor.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.exe", target_dir + "/IDFVersionUpdater.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/readme.html", target_dir + "/Readme Notes.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/WeatherConverter/Weather.exe", target_dir + "/Weather Statistics and Conversions.lnk");

    }
  }
}
