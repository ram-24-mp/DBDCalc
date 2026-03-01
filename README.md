# DBDCalc
An R script for performing Dry Bulk Density (DBD) and Porosity calculations for sediment cores.

Calculations are performed automatically with a TRUE/FALSE user input dialogue. DBDCalc may use two methods (determined by user input dialogue) to calculate DBD and Porosity: usage of wet/dry sediment weights for each interval to estimate volume contributions from both sediments and water (thus implicitly allowing total volume to vary from interval to interval), or usage of dry weights and total volume (assuming an idealized volume based on interval thickness and corer surface area). Generally, method 1 should be preferred, but method 2 should be employed when wet weights are unavailable or there are significant contributions by organic debris to core mass.

Method 1 first subtracts dry weight from wet weight to find the water mass (and thus water volume, assuming a water density of 1g/cm^3), and then estimates sediment volume by diving the dry weight by measured particle density (or 2.65g/cm^3, if no measured particle density is availble). Porosity is then calculated by dividing the water volume by the sum of the estimated sediment and water volumes. DBD is calculated by subtracting the porosity from 1 and multiplying the result by particle density.

Method 2 first calculates the idealized volume of each sediment interval, based upon the corer surface area and interval thickness. Then, it divides the dry weight by the ideal sediment interval volumes to calculate DBD. To calculate porosity, it divides the DBD by the particle density and subtracts the quotient from 1. If the user specifies there was significant organic debris, porosity values are not calculated.

If DBDCalc detects missing data, it gives the user the option to linearly interpolate missing DBD and porosity values. Missing uncertainties are calculated via error propagation. DBDCalc allows for variable sediment interval thicknesses.

See Weight_Data_1 and Weight_Data_2 for formating conventions for methods 1 and 2, respectively.

Required packages include readxl, writexl, ggplot2, rstudioapi, and zoo.
