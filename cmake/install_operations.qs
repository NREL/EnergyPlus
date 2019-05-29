function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    if( kernel == "darwin" ) {

      // Symlinks
      var linktarget = "/usr/bin";
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/Basement", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/BasementGHT.idd", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PostProcess/convertESOMTRpgm/convertESOMTR", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/EnergyPlus", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/Energy+.idd", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/Energy+.schema.epJSON", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/EPMacro", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/ExpandObjects", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PostProcess/HVAC-Diagram", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/ParametricPreProcessor/ParametricPreprocessor", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PostProcess/ReadVarsESO", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/runenergyplus", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/runepmacro", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/runreadvars", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/Slab", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/SlabGHT.idd", linktarget);

      // Move man page
      component.addElevatedOperation("Move", "@TargetDir@/energyplus.1", "/usr/local/share/man/man1/");

      // Chmod +x for apps
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/IDFVersionUpdater");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/EP-Launch-Lite");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PreProcess/EP-Launch-Lite.app/Contents/MacOS/python");
      component.addElevatedOperation("Execute", "chmod", "+x", "@TargetDir@/PostProcess/EP-Compare.app/Contents/MacOS/EP-Compare");

    // Not sure necessary so not doing it yet
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@");
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/IDFVersionUpdater.app/Contents/MacOS/");
    // component.addElevatedOperation("Execute", "chmod", "-R", "a+w", "@TargetDir@/PreProcess/IDFVersionUpdater/");

    }
  }
}








