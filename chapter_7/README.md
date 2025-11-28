# GD-NH3

Calculations and related work for the 2025 update of the UNECE guidance document on NH3, Chapter 7 on manure application.

# Maintainer

Sasha D. Hafner.

Contact information here: [https://www.au.dk/vis/person/sasha.hafner@bce.au.dk](https://www.au.dk/vis/person/sasha.hafner@bce.au.dk).

# Contents

## ATMS

ATMS examples and documents.

## functions

R functions used by scripts in the other subdirectories.

## reductions-ALFAM2

ALFAM2 model predictions of emission reductions for the abatement techniques included in the guidance document.

### inputs

ALFAM2 model inputs for a reference scenario and abatement scenarios for both cattle and pig slurry.
Only cattle results are given in the document.
Inputs are probably easiest to view (and edit) using a spreadsheet program or similar.
These values are averages calculated from v2.64 of the ALFAM2 database ([https://github.com/AU-BCE-EE/ALFAM2-data/releases/tag/v2.64](https://github.com/AU-BCE-EE/ALFAM2-data/releases/tag/v2.64)) with equal weighting by country (see analysis/averages/output files).

### logs

R and R package version log, and ALFAM2 log from running ALFAM2\_calcs.R.

### output

Results.
Values for table in guidance document are in reductions\_table.csv.
Other files have additional output data.

### plots

Plots of predicted emission and emission reductions.
Not for publications.

### scripts

R scripts for making the ALFAM2 predictions etc.
Run the single script main.R to repeat the complete analysis.
It calls the other scripts.

## reductions-lit

Emission reductions for abatement techniques based on literature data.
But ALFAM2 model predictions are included for comparison.
Here inputs are set in individual ALFAM2.R scripts, but they follow the values described above for reductions-ALFAM2/inputs.

### analysis-xlsx

Simple spreadsheet analysis for combining 2014 summary and new literature data.
See data description section below for details.

### data

Emission data from literature.
Main files are describe in the list below.

* lit\_data.xlsx - newly compiled literature data used for calculating new emision reductions, which were combined with results from the earlier summary to develop reductions that are included in the new version of the guidance document. The text below describes how these data were collected from the literature.
* data\_2014.xlsx - summary of the literature data used to develop reduction estimates for the 2014 version of the guidance document. Taken directly from the file SpreadRev\_Summ.xlsx that was included as an appendix to the 2014 document.

### output

Results.

### scripts-acid

R scripts for working up acidification data.
The single script main.R will run the complete analysis, calling other scripts.

### scripts-app-tech

R scripts for working up application method data.
The single script main.R will run the complete analysis, calling other scripts.

### scripts-dilution

R scripts for working up dilution data.
The single script main.R will run the complete analysis, calling other scripts.

### scripts-sep

R scripts for working up application method data.
The single script main.R will run the complete analysis, calling other scripts.

# Data

## Two sources

Emission reductions presented in the new guidance document are based on the summary from the 2014 version of the guidance document and new literature data.
The 2014 summary is from the file SpreadRev\_Summ.xlsx, which was included as an appendix to the 2014 document.

## Literature search

This 2014 document contained literature from 2012 and earlier, so for the search for new results, only literature from 2012 and later was included.
The literature search was done in summer 2024 using Scopus.
The following search terms were used:

“Ammonia” or “NH3” and “volatilization” or “emission” and “manure” and “application” and “soil” and “agriculture” from 2010 to 2024

817 documents were found in Scopus.
The documents titles and abstracts were scanned, and it was found that 61 were empirical studies comparing ammonia volatilization losses from different application methods or using different additives.
Of the 61 studies selected, closer reading revealed that 13 of these either were modeled or did not actually have any comparisons between methods / additives.
Therefore, in the end, data from 48 studies were used.

Data from the studies were entered into the file reductions-lit/data/lit\_dat.xlsx.

A few additional studies on separation (study no 57-61) were included for completeness, see more info in the section on acidification below.



## Acidification data

The database was used and filtered for observations with acidification.
As acidification was not a Category 1 technique in the GD in 2014, there is no data on acidification in the literature summary from 2014.
Therefore, additional data from studies older than 2012 was included.
Additional acidification data are in reductions-lit/data/acidification\_data\_extra.xlsx
The reduction obtained by acidification added to he table is a mean reduction from all the studies.

## Application methods

Data from the new literature compilation were used to calculate average reductions of the different application techniques compared to broadcast and band applied slurry.
Two additional older studies (study no 62 and 63) with data from mechanically assisted infiltration were added for completion of this application technique (was not added in the earlier database as it was not a category 1 technology).
Mean reductions for each combination of reference (broadcast or band application) and low-emission application technique (band application, trailing shoe, open slot injection, closed slot injection, band application combined with aeration) were calculated for each study.
A new average was calculated as a combination of the averages from the new literature compilation and the averages found in the 2014 version of the GD, all weighted by number of studies.

All the calculations were done with broadcast as the reference.
The averages reductions with banded slurry as reference were used to assess if the addition of (broadcast -> banded) and (banded -> technique) reductions were in the same range as the (broadcast -> technique) reduction.
That was the case for all techniques.

Average reductions per study from new literature: ../output/app.meth.csv
Average reductions per study from 2014 version of GD: ../data/SpreadRev\_Summ.docx
Calculation of new means: analysis-xlsx/app\_meth\_combined.xlsx
Scripts for calculations: reductions-lit/scripts-app-tech. 'main.R' calls the other scripts.

## Dilution

As the new literature search didn't have any observations with dilution, a new database was collected based literature provided by co-authors.
The reduction obtained by dilution added to he table is a mean reduction when applying the diluted slurry compared to the undiluted slurry.

## Incorporation of liquid slurry

10 new observations were identified in the literature search
All new literature: ../data/lit\_dat.xlsx.
Studies with slurry incorporated: ../output/df.incorp.liq.csv.
Scripts to filter the data to get df.incorp.liq.csv from lit\_dat.xlsx: ../scripts. The script 'main.R' runs the rest.
The reductions in these studies are in line with the GD from 2014, therefore, we did not find that there was evidence to change these.
The numbers from the 2014 GD are used in this revised version without any changes.

## Separation

As the new literature search only had a few observations with liquid fraction, it was chosen to use the database collected in Pedersen et al. (2022).

Pedersen et al. (2022): Pedersen, J., Hafner, S. D., Adamsen, A. P. S. 2022. Effectiveness of mechanical separation for reducing ammonia loss from field-applied slurry: Assessment through iterature review and model calculations. Journal of Environmental Management 323. 116196. https://doi.org/10.1016/j.jenvman.2022.116196
Database from Pedersen et al. (2022): ../data/separation\_dat\_Pedersen2022.xlsx
Scripts for calculations: ../scripts-sep. 'main.R' calls the other scripts
Output: ../output/df.separation.csv

The reduction obtained by separation added to he table is a mean reduction when applying the liquid fraction after separation compared to the unseparated slurry or digestate. Emissions from the solid fraction is not included.

## Solid manure

Only two new studies (5 observations in total) were identified in the literature search
All new literature: ../data/lit\_dat.xlsx.
Studies with solid manure applied by different techniques: ../output/df.solid.csv.
Scripts to filter the data to get df.solid.csv from lit\_dat.xlsx: ../scripts. The script 'main.R' runs the rest.
The reductions in these studies are in line with the GD from 2014, therefore, we did not find that there was evidence to change these.
The numbers from the 2014 GD are used in this revised version without any changes.

