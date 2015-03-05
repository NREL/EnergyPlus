# Group – Detailed Ground Heat Transfer

## Detailed Ground Heat Transfer

The following objects may be included in an EnergyPlus input IDF file but are handled by the Slab preprocessor:

- GroundHeatTransfer:Slab:Materials
- GroundHeatTransfer:Slab:MatlProps
- GroundHeatTransfer:Slab:BoundConds
- GroundHeatTransfer:Slab:BldgProps
- GroundHeatTransfer:Slab:Insulation
- GroundHeatTransfer:Slab:EquivalentSlab
- GroundHeatTransfer:Slab:AutoGrid
- GroundHeatTransfer:Slab:ManualGrid
- GroundHeatTransfer:Slab:XFACE
- GroundHeatTransfer:Slab:YFACE
- GroundHeatTransfer:Slab:ZFACE

The following objects may be included in an EnergyPlus input IDF file but are handled by the Basement preprocessor:

- GroundHeatTransfer:Basement:SimParameters
- GroundHeatTransfer:Basement:MatlProps
- GroundHeatTransfer:Basement:Insulation
- GroundHeatTransfer:Basement:SurfaceProps
- GroundHeatTransfer:Basement:BldgData
- GroundHeatTransfer:Basement:Interior
- GroundHeatTransfer:Basement:ComBldg
- GroundHeatTransfer:Basement:EquivSlab
- GroundHeatTransfer:Basement:EquivAutoGrid
- GroundHeatTransfer:Basement:AutoGrid
- GroundHeatTransfer:Basement:ManualGrid
- GroundHeatTransfer:Basement:XFACE
- GroundHeatTransfer:Basement:YFACE
- GroundHeatTransfer:Basement:ZFACE

The documentation both the Slab and Basement objects appear in the AuxiliaryPrograms document under the "Ground Heat Transfer in EnergyPlus" heading.

The only object described in this section is the control object which activates the use of the preprocessor.

## GroundHeatTransfer:Control

The [GroundHeatTransfer:Control](#groundheattransfercontrol) object determines if the Slab and Basement preprocessors are going to be executed. When a Slab or Basement run is performed the results are saved in files with extensions .SLAB or .BSMT so that they do not need to be rerun if no input changes are made to the GroundHeatTransfer:Slab or GroundHeatTransfer:Basement objects.

### Inputs

#### Field: Name

This alpha field is the name of the [GroundHeatTransfer:Control](#groundheattransfercontrol) object. It is used as an identifier.

#### Field: Run Basement Preprocessor

The field can either be Yes or No and defaults to No. It is used to control whether the Basement preprocessor program is executed. If it has been previously executed the results are stored in the .BSMT file and do not necessary need to be run again unless the GroundHeatTransfer:Basement objects have changed.

#### Field: Run Slab Preprocessor 

The field can either be Yes or No and defaults to No. It is used to control whether the Slab preprocessor program is executed. If it has been previously executed the results are stored in the .SLAB file and do not necessary need to be run again unless the GroundHeatTransfer:Slab objects have changed.