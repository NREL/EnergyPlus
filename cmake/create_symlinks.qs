// This file gathers the operations that do require Admin privileges

function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    var kernel = systemInfo.kernelType;
    if( kernel == "darwin" ) {

      // Symlinks: require admin privileges
      var linktarget = "/usr/local/bin";
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/Basement", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PreProcess/GrndTempCalc/BasementGHT.idd", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/PostProcess/convertESOMTRpgm/convertESOMTR", linktarget);
      component.addElevatedOperation("Execute", "ln", "-sf", "@TargetDir@/energyplus", linktarget);
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

      // Move man page: admin too
      component.addElevatedOperation("Move", "@TargetDir@/energyplus.1", "/usr/local/share/man/man1/energyplus.1");

    }
  }
}









