Output Changes
==============

This file documents the structural changes on the output of EnergyPlus that could affect interfaces, etc.

### Description

This will eventually become a more structured file, but currently it isn't clear what format is best. As an intermediate solution, and to allow the form to be formed organically, this plain text file is being used. Entries should be clearly delimited. It isn't expected that there will be but maybe a couple each release at most. Entries should also include some reference back to the repo. At least a PR number or whatever.

### Update Headers in EIO and existing tabular reports for Equipment Ratings

EIO updated for the following:

- The existing 'DX Cooling Coil Standard Rating Information' table was previously reporting ratings for both the 2017 and 2023 ratings defined by AHRI.  This is corrected by splitting the report into two tables.
- Existing table: 'DX Cooling Coil Standard Rating Information' -- This table reports Standard Ratings defined in the AHRI standard 210/240-2017 and AHRI standard 340/360-2017.  
- New table: 'DX Cooling Coil AHRI 2023 Standard Rating Information' to report updated standard ratings defined in AHRI standard 210/240-2023 and AHRI standard 340/360-2022

Headers updated for the following existing tables in the HTML summary report:
 
- 'DX Cooling Coil Standard Ratings 2017'
- 'DX Cooling Coil Standard Ratings 2023'

See Pull Request [#10311] for more detail
 
### Next ...
