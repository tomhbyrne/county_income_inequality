# County Income Inequality

This repository includes raw data and programs necessary to create dataset with county-level income inequality measures and other covariates for the period from 2007-2018.  Only counties large enough to be inclued in the 1-Year ACS estimates for this time period are included in the dataset.

Below is a quick summary of the contents of this repository:

1. __programs__: This folder includes the programs used to create the dataset. The programs are numbered sequentially, with program 100 creating gini coefficient measures at county level, program 200 creating other county-level covariates, program 300 creating county-level rent measures based on data from HUD and program 400 combining all of the data into a final dataset.  Note that not all of the raw data used in program 100 are uploaded on this repository because some of the ACS microdata files needed to create the gini coefficient instruments are too large to upload to Github.

2. __data__:  This includes subfolders for the __raw data__ used as inputs in programs 100-300, as well as __cleaned data__ produced by programs 100-300, and the __final dataset__ produced by program 400.  As noted above, not all of the raw data are included due to size limits.  