Reference Information
=====================

Provenance for this README
--------------------------

* File name: README.md
* Authors: Mia Akbar
* Other contributors: Dale Moskoff, Spencer C.H. Barrett, Robert I. Colautti
* Date created: 2024-05-30

Dataset Version and Release History
-----------------------------------

* Current Version:
  * Number: 1.0.0
  * Date: 2024-05-30
  * Persistent identifier: n/a
  * Summary of changes: n/a

* Embargo Provenance: n/a
  * Scope of embargo: n/a
  * Embargo period: n/a

Dataset Attribution and Usage
-----------------------------

* Dataset Title: Data for the article "Latitudinal clines in floral display associated with adaptive evolution during a biological invasion"

* Persistent Identifier: n/a

* Dataset Contributors:

  * Creators: Mia Akbar, Dale Moskoff, Spencer C.H. Barrett, Robert I. Colautti

* Date of Issue: 2024-05-30

* Publisher: Queen's University

* License: Use of these data is covered by the following license:
  * Title: CC0 1.0 Universal (CC0 1.0)
  * Specification: https://creativecommons.org/publicdomain/zero/1.0/; the authors respectfully request to be contacted by researchers interested in the re-use of these data so that the possibility of collaboration can be discussed. 

* Suggested Citations:

  * Dataset citation:
    > Akbar, M., D. Moskoff, S.C.H. Barrett, and R.I. Colautti. 2024. Data for the article "Latitudinal clines in floral display associated with adaptive evolution during a biological invasion", Dryad, Dataset

  * Corresponding publication:
    > Akbar, M., D. Moskoff, S.C.H. Barrett, and R.I. Colautti. 2024. Latitudinal clines in floral display associated with adaptive evolution during a biological invasion. American Journal of Botany. Submitted. 

Contact Information
-------------------

  * Name: Robert I. Colautti
  * Affiliations: Department of Biology, Queen's University
  * ORCID ID: https://orcid.org/0000-0003-4213-0711
  * Email: robert.colautti@queensu.ca

* Contributor ORCID IDs:
  * Mia Akbar: https://orcid.org/0009-0007-7426-4382
  * Spencer C.H. Barrett: https://orcid.org/0000-0002-7762-3455

- - -

Additional Dataset Metadata
===========================

Acknowledgements
----------------

* Funding sources: Funding was provided by NSERC Discovery grants to Spencer C.H. Barrett and Robert I. Colautti and an NSERC CGS-M to Mia Akbar.


Dates and Locations
-------------------

* Dates of data collection: Floral display data were collected between July 02, 2008 and September 30, 2008. 

* Geographic locations of data collection: Experimental plants were grown in a common garden at the Koffler Scientific Reserve at Jokers Hill (44.03° N, 79.54° W) from 2005-2008. (see publication for more details).

- - -

Methodological Information
==========================

* Methods of data collection/generation: see publication for details

- - -

Data and File Overview
======================

Summary Metrics
---------------

*All data files can be 

* Data File count: 1
* Total file size: 37 KB
* File formats: .csv

Naming Conventions
------------------

* File naming scheme: The data file within the "Data" folder has one section to its name with each separate word capitalized. 

Data File List
-----------------
* LythrumDisplay2008.csv

Setup
-----

* Unpacking instructions: n/a

* Relationships between files/folders: "2_MomentsandEmergentProperties.Rmd" must be run to get the data files required for all the other .Rmd files. "3_PCoA.Rmd" produces data used in "7_TraitCorrelations.Rmd". 4_BootstrappedMeans.Rmd" produces the data required for "5_LinearModels.Rmd", "6_VisualizingClines" and "7_TraitCorrelations.Rmd".

* Recommended software/tools: RStudio 2024.05.30; R version 4.4.0

- - -

File/Folder Details
===================

Details for: LythrumDisplay2008.csv
---------------------------------------

* Description: Initial size of Alliaria and Acer plants, plant location within the greenhouse, plant treatment and unique competition number indicating which plants were paired in pots. 

Time-series floral display data for 388 plants, plant location in the garden, plant seed family and source population/latitude information, start, end and duration of flowering, final vegetative size, the first axis of a PCA characterizing the trade-off between flowering time-size, inflorescence mass and total number of flowering produced.

* Format(s): .csv

* Size(s): 34 KB

* Dimensions: 388 rows x 25 columns

* Variables:
  * Num: Unique ID number for each individual plant.
  * Row: Common garden block and row in which the plant was located. Numbers correspond to experimental block.
  *Fam: ID code for each seed family.
  * Pop: ID code for each source population.
  * Lat: Latitude of origin for each source population.
  * Start: the absolute start day of flowering for each plant where 1 is July 2nd, 2008. 
  * End: the absolute end day of flowering for each plant. 
  * Duration: The number of days between Start and End for each plant. 
  * Veg: The final vegetative stem height (cm)
  * PC1: The first axis of PCA on flowering time and vegetative size at flowering characterizing a trade-off between these two traits. Low values of PC1 represent early-flowering individuals with smaller vegetative size and high values of PC1 represent late-flowering individuals with larger vegetative size. More details provided in Colautti & Barrett (2010, 2011).
  * HinfW: Total reproductive biomass of harvested inflorescences (g). 
  * TotFlwr: Total floral display size (number of flowers). 
  * d0-d60: The number of flowers counted on each individual at 5 day intervals where `d0` represented the number of flowers produced on their first day of flowering. 

  
* Missing data codes: "NA". 


## Code/Software

Within the directory “AkbarEtAl_LythrumFloralDisplay”, there are numbered R markdown files for each part of the analysis and generation of figures. 

R packages "tidyr" and "dplyr"" were used for data management. Visualizations were created using the packages "ggplot2", "virdis" and "corrplot". Statistical analyses utilized the "lmtest" and "MuMIn" packages. 

*Conceptual Model*
"1_ConceptualModel.Rmd" contains code used to produce the contents of Box 1. We visualize representative individual and aggregated population flowering schedule curves as well as a table of equations used to calculate the central moments of each.

*Characterizing Flowering Schedules*
We characterize flowering schedules by taking the central moments of individuals and aggregated populations in "2_MomentsandEmergentProperties.Rmd". We additionally use a principal coordinates analysis (PCoA) to characterize individual flowering schedules based on a euclidean distance matrix and generate Figure 3 in "3_PCoA". We compute and visualize a comparative analysis of these metrics (Start, Duration, Central Moments, PCoA Axes) in "7_TraitCorrelations.Rmd". 

*Testing for Clines*
We generate bootstrapped means and confidence intervals for the central moments of population average and aggregated flowering schedules in "4_BoostrappedMeans.Rmd" and test for significant changes with latitude in "5_LinearModels.Rmd". We visualize these results in "6_VisualizingClines.Rmd".


- - -
END OF README