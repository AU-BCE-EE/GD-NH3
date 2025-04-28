# GD-NH3
Calculations and related work for the 2025 update of the UNECE guidance document on NH3.
Presently all the contents are related to Chapter 7, on manure application.

# Maintainer
Sasha D. Hafner.

Contact information here: <https://www.au.dk/vis/person/sasha.hafner@bce.au.dk>.

# Contents

## functions
R functions used by scripts in the other subdirectories.

## reductions-ALFAM2
ALFAM2 model predictions of emission reductions for the abatement techniques included in the guidance document.

### inputs
ALFAM2 model inputs for a reference scenario and abatement scenarios for both cattle and pig slurry.
Only cattle results are given in the document.
Inputs are probably easiest to view (and edit) using a spreadsheet program.

### logs
R and R package version log, and ALFAM2 log from running ALFAM2_calcs.R.

### output
Results.
Values for table in guidance document are in reductions_table.csv.
Other files have additional output data.

### plots
Plots of predicted emission and emission reductions.
Not for publications.

### scripts
R scripts for making the ALFAM2 predictions etc.
Run the single script main.R to repeat the complete analysis.
It calls the other scripts.

## reductions-lit
Emission reductions for the abatement techniques based on literature data.
