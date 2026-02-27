# DBDCalc
An R script for performing Dry Bulk Density (DBD) and Porosity calculations for sediment cores.

Calculations are performed automatically with a TRUE/FALSE user input dialogue. DBDCalc may use two methods to calculate DBD and Porosity: usage of wet/dry sediment weights for each interval to estimate volume contributions from both sediments and water (thus implicitly allowing total volume to vary from interval to interval), or usage of dry weights and total volume (assuming an idealized volume based on interval thickness and corer surface area). Generally, method 1 should be preferred, but method 2 should be employed when wet weights are unavailable or there are significant contributions by organic debris.
