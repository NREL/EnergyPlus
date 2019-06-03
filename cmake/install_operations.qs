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
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/IDFVersionUpdater");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/EP-Launch-Lite");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/python");
      component.addOperation("Execute", "chmod", "+x", "@TargetDir@/PostProcess/EP-Compare/EP-Compare.app/Contents/MacOS/EP-Compare");

    // Not sure necessary so not doing it yet
    // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@");
    // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/");
    // component.addOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/");

    }
  }
}








