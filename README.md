# Detect Outliers

MoveApps

Github repository: https://github.com/ctmm-initiative/moveapps_ctmm_outlier

## Description
This app allows to detect relocation that are likely outlier based on speed and distance traveled. 

## Documentation

### Input data
A `telemetry.list` object. 

### Output data
A `telemetry.list` with relocations that fall below a given user-specified threshold removed. 

### Artefacts
none

### Settings

`Select variable`: select a variable to be used for filtering.  

`Range`: select an acceptable range for the selected variable.   

`Store settings`: click to store the current settings of the app for future workflow runs  


### Most common errors
Please file an issue if you encounter an error. 

### Null or error handling
If all relocations for some animals are removed, these animals will be removed from the data set. 
