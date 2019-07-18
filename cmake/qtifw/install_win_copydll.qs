// Windows commands to be performed elevated: copy and register DLLs

function Component()
{
  Component.prototype.createOperations = function()
  {
    // call default implementation
    component.createOperations();

    // ... add custom operations

    // On Windows
    if( kernel == "winnt" ) {

      var systemArray = ["MSCOMCTL.OCX", "ComDlg32.OCX", "Msvcrtd.dll", "Dforrt.dll", "Gswdll32.dll", "Gsw32.exe", "Graph32.ocx", "MSINET.OCX", "Vsflex7L.ocx", "Msflxgrd.ocx"];
      var systemTargetDir = installer.environmentVariable("WINDIR");
      if( systemInfo.currentCpuArchitecture == "x86_64") {
        systemTargetDir += "/SysWOW64/";
      } else {
        systemTargetDir += "/System32/";
      }
      for (i = 0; i < systemArray.length; i++) {
        var sourceFile = "@TargetDir@/temp/" + systemArray[i];
        var targetFile = systemTargetDir + systemArray[i];
        if (!installer.fileExists(targetFile)) {
          component.addElevatedOperation("Copy", sourceFile, targetFile);
        }
      }
      // Delete this temp directory: use execute to avoid uninstall create
      // the opposite (= Mkdir), plus it doesn't delete an empty directory anyways and we use copy (not move) above...
      // component.addElevatedOperation("Rmdir", "@TargetDir@/temp");
      component.addElevatedOperation("Execute", "cmd" , "/C",  "rmdir", "/S", "@TargetDir@\\temp");

    }
  }
}
