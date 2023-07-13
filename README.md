# Detect Outliers

MoveApps

Github repository: https://github.com/ctmm-initiative/ctmmMoveApp_outlier

## Description
This app removes relocations that are likely outliers based on a user-provided threshold for either speed or distance traveled. Based on the outlie() function of the ctmm package.

## Documentation

### Input data
A `telemetry.list` object. 

### Output data
A `telemetry.list` with relocations that fall below a given user-specified threshold removed. 

### Artefacts
none

### Settings

`Select variable`: select a variable to be used for filtering. Options are speed or distance moved. Default is speed. 

`Range`: select an acceptable range for the selected variable. Any relocations that are not within the user-set range will be removed from the data set.  

`Store settings`: click to store the current settings of the app for future workflow runs  


### Most common errors
Please file an issue if you encounter an error. 

### Null or error handling
If all relocations for some animals are removed, these animals will be removed from the data set. 
