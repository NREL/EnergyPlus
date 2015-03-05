# Water-to-Air Heat Pump Parameter / Coefficient Generator (Cooling)

This document gives brief instructions on generating the parameters or coefficients for the water-to-air heat pump models in cooling mode. The Excel™ spreadsheets (WaterAir_PE_Cooling.xls) are used. The spreadsheet generates:

- parameters for the parameter estimation based model.
- coefficients for the curve-fit model. 

The following theses have detailed information about the curve-fit model and parameter estimation based model:

Jin, Hui. 2002. Parameter Estimation Based Models of Water Source Heat Pumps. Phd. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable at www.hvac.okstate.edu)

Shenoy,Arun. 2004. Simulation, Modeling and Analysis of Water to Air Heat Pump. M.S. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable at www.hvac.okstate.edu)

Tang,C.C. 2004. Modeling Packaged Heat Pumps in a Quasi-Steady State Energy Simulation Program. M.S. Thesis, Department of Mechanical and Aerospace Engineering, Oklahoma State University. (downloadable at www.hvac.okstate.edu)

## Step 1: Catalog Data Input

Data points are obtained from the heat pump manufacturer data. Minimum data points for the parameter estimation based model are 32 data points according to Jin (2002). The curve-fit model performance is not affected by the number of data points and a minimum of 6 data points is required since the sensible cooling capacity requires 6 coefficients. The data points must have varying inlet conditions (e.g. air flow rates, inlet water temperatures, and etc.) that covers the entire range of the heat pump operating conditions. Correction tables provided by the manufacturer should be used to extend the catalog data range in order to have a good set of parameters/coefficients.

- Using the heat pump performance data, enter the values to Table 1 in worksheet "CATALOG DATA". The values can be in SI or IP units.
- Click on Button 1 based on the units used. 

For IP units:

![](media/generate-input-from-catalog-data-ip-button.png)\ 


For SI units:

![](media/generate-input-from-catalog-data-si-button.png)\ 


- The program will convert the values to the desired units and display them on Table 2 in worksheet "INPUT".  Then the program will discard bad catalog points by calculating the relative humidity of the exiting air at the load side (relative humidity should be less or equal to 1). Table 3 in worksheet "INPUT" shows the input catalog data that will be used by the parameter/coefficient generator program.
- The button shown below is used clearing Table 1 (worksheet "CATALOG DATA"), Table 2, and Table 3 (worksheet "INPUT"). It is advisable to clear the tables before generating parameters/coefficients for a new heat pump model.

![](media/generate-previous-catalog-data.png)\ 


After Table 3 is created, the parameters/coefficients are then calculated as follows:

- Worksheet "ParamEstimator" is used for generating the parameters for the parameter estimation based model using Nelder Mead Simplex. Refer to the steps in the Parameter Estimation Procedure.
- Worksheet "CoeffCalculator" is used for calculate the coefficients for the curve-fit model using the generalized least square method. Refer to the steps in the Curve Fit Model procedure.

## Parameter Estimation Procedure

### Step 2a: Generating First Set of Parameters (PE-Based Model)

- Using contents of Table 3, the program can generate parameters. The user must fill all the cells colored light blue in worksheet "ParamEstimator".
- **Accuracy:** Start with a low accuracy for faster convergence, recommended value of 0.001.
- **Compressor Type:** User is allowed to select from 3 types of compressors: scroll, rotary or reciprocating. Contact the manufacturer to make sure that the compressor selected is correct. Wrong type of compressor selected would lead to the program crashing or inaccurate parameters.
- **Refrigerant:** Contact the manufacturer on the refrigerant used to generate the catalog data and select from the list of refrigerants. Usually the refrigerant used is R22. .
- **Which Initial Guess?:**  The user may choose a set of initial guesses(1-5) that will be used by the optimization routine in generating the parameters. Start with 1, which is the set of initial guesses 1 at column B.
- **Initial Guess:** Initial guess for all the parameters. For Initial Guess 1, enter a value of 1 for all the parameters except for the loss factor (less than 1.0). The loss factor should be less than 1.0 because the efficiency of the compressor should be less than 100%. Adjust the values in Initial Guess 1 if the program happens to crash and try again.
-  Now click on Button 2 shown below to generate the parameters. 

![](media/generate-parameters-button.png)\ 


It will take some time to generate the parameters depending on the number of data points. The parameters generated will be displayed at Parameters 1.

- Look at the error analysis of Error 1, which gives the user a summary of the errors for Qtotal, Qsensible, Qsource and Power. An average error of 5-8% is achievable for all the values.
- The errors for all the individual catalog data points are displayed in worksheet "RESULT".

### Step 2b: Improving the Set of Parameters (PE-Based Model)

- After the initial set of parameters has been generated, the user can increase the accuracy of the parameters by using parameters generated as the initial guess for the second simulation and increasing the accuracy of the program.
- Copy and Paste Parameters 1 to Initial Guess 2.
- Change the initial guess indicator **(Which Initial Guess?)** from 1 to 2.
- Increase the accuracy by twice. For example, set accuracy to 0.000001.
- Now click on Button 2 shown below to generate the second set of parameters. 

![](media/generate-parameters-button.png)\


The simulation time would most likely be less but it depends on the accuracy value as well. The parameters generated will be displayed at Parameter 2.

- Compare Error 2 to Error 1, the error values should be less which means that the parameters are getting better and more accurate.
- Repeat the steps in 2a and 2b until a desired set of error values is achieved or the errors stop decreasing

### Step 3: Generating EnergyPlus Input Parameters

- Click on the Button 3 shown below to convert and arrange the parameters generated to fit EnergyPlus Input File (IDF), which will be listed from cell B52:B61.

![](media/generate-e+-input-parameters-button.png)\


- The button shown below in worksheet "ParamEstimator" is used for clearing Initial Guess (2-5), Parameters(1-5), Error(1-5), EnergyPlus Input parameters and Result(1-5) in worksheet "RESULT".

![](media/clear-result-button.png)\


## End Parameter Estimation Procedure

## Curve Fit Model Procedure

### Step 2: Generating the coefficients (Curve-Fit Model)

- Using the contents of Table 3, the program can generate the coefficients. The user must fill all the cells colored light blue in worksheet "CoeffCalculator".
- **RatedAirVolFlowRate:** Rated volumetric air flow rate (m^3^/s) which corresponds to the highest total cooling capacity listed in the catalog data.
- **RatedWaterVolFlowRate:** Rated volumetric water flow rate (m^3^/s) which corresponds to the highest total cooling capacity listed in the catalog data.
- **RatedTotalCap:** Rated total cooling capacity (W) which is the highest total cooling capacity listed in the catalog data.
- **RatedSensCap:** Rated sensible cooling capacity (W) which corresponds to the highest total cooling capacity listed in the catalog data.
- **RatedPower:** Rated power consumption (W) which corresponds to the highest total cooling capacity listed in the catalog data.
- Now click on Button 2 shown below to calculate the coefficients.

![](media/generate-coefficients-button.png)\


- The coefficients for the corresponding curves are listed at cell B12:D17. Error analysis of model are listed at cell B19:B30.
- The errors for all the individual catalog data points are displayed in worksheet "RESULT".
- The button shown below in worksheet "CoeffCalculator" is used for clearing the coefficients, the error analysis and the outputs in worksheet "RESULT".

![](media/clear-result-button.png)\


## End Curve Fit Model Procedure