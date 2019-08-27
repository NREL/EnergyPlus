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

      component.addOperation("CreateShortcut", "@TargetDir@/Documentation/index.html", "@StartMenuDir@/EnergyPlus Documentation.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.exe", "@StartMenuDir@/EP-Compare.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/EPDraw/EPDrawGUI.exe", "@StartMenuDir@/EPDrawGUI.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/EP-Launch.exe", "@StartMenuDir@/EP-Launch.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles.html", "@StartMenuDir@/Example Files Summary.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/ExampleFiles/ExampleFiles-ObjectsLink.html", "@StartMenuDir@/ExampleFiles Link to Objects.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFEditor/IDFEditor.exe", "@StartMenuDir@/IDFEditor.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.exe", "@StartMenuDir@/IDFVersionUpdater.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/readme.html", "@StartMenuDir@/Readme Notes.lnk");
      component.addOperation("CreateShortcut", "@TargetDir@/PreProcess/WeatherConverter/Weather.exe", "@StartMenuDir@/Weather Statistics and Conversions.lnk");

    }
  }
}
