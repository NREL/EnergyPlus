// Regular install commands to be performed
function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    if( kernel == "darwin" ) {

      // Chmod +x for apps
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/IDFVersionUpdater");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/EP-Launch-Lite");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/python");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.app/Contents/MacOS/EP-Compare");

    // Not sure necessary so not doing it yet
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@");
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/");
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/");

    }
  }
}








