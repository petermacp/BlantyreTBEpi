
<!-- README.md is generated from README.Rmd. Please edit that file -->
BlantyreTBEpi
-------------

This package contains data and code to replicate the analysis of the manuscript:

*"Disparities in access to diagnosis and care in Blantyre, Malawi identified through enhanced citywide tuberculosis surveillance and spatial analysis"*

Authors: Peter MacPherson PhD<sup>1,2</sup>, McEwen Khundi MSc<sup>1</sup>, Marriott Nliwasa MBBS<sup>3</sup>, Augustine T Choko MSc<sup>1,4</sup>, Vincent K Phiri BSc<sup>1</sup>, Emily L Webb PhD<sup>4</sup>, James Mpunga<sup>5</sup>, Peter J Dodd PhD<sup>6</sup>, Ted Cohen DPH<sup>7</sup>, Rebecca Harris<sup>1,4</sup>, Elizabeth L Corbett F. Med Sci<sup>1,4</sup>

Affiliations:

1.  TB/HIV Group, Malawi-Liverpool-Wellcome Trust Clinical Research Programme, Blantyre, Malawi
2.  Department of Clinical Sciences, Liverpool School of Tropical Medicine, UK
3.  Helse-Nord TB Programme, College of Medicine, University of Malawi, Malawi
4.  MRC Tropical Epidemiology Group, London School of Hygiene and Tropical Medicine, UK
5.  National TB Programme, Lilongwe, Malawi
6.  School of Health and Related Research, University of Sheffield, UK
7.  Yale School of Public Health, Yale University, USA

<br>

Installation
------------

You can install the package from [GitHub](https://github.com/petermacp/BlantyreTBEpi) with:

``` r
# install.packages("devtools")
devtools::install_github("petermacp/BlantyreTBEpi")
library(BlantyreTBEpi)
```

<br>

Replicable code
---------------

The script `BlantyreTBEpi_analysis.rmd` should be opened, and can be knitted using the `rmarkdown` package, or the `knit` button in RStudio to produce an .html output copy of analysis included within the manuscript.

<br>

Data
----

Incuded within the package `data/` folder are minimially-reproducable datasets that are used in the analysis code.

To load a dataset, run the following command:

``` r
# install.packages("devtools")
devtools::install_github("petermacp/BlantyreTBEpi")
library(BlantyreTBEpi)

data("dat", package="BlantyreTBEpi")
```

The following datasets and variables are included within the `data/` folder of the package:

### `all_cases_sf.rda`

Individual-level dataset including records of all 4009 TB cases registered for treatment between 2015 and 2017 and included in this analysis.

-   `year`: Calendar year in which case registered for TB treatment
-   `sex`: Sex of TB case (male or female)
-   `tbclass`: Type of TB (pulmonary TB, or extrapulmonary TB)
-   `hivstatus`: HIV status of TB case (HIV-positive, HIV-negative, or unknown)
-   `smearstatus`: Sputum smear status of TB case, done by routine health system (positive, negative, or missing/not done)
-   `age`: Age (in years) of TB case on date of registration for treatment
-   `lab_smear`: sputum-smear status of TB case done at research laboratory on enhanced surveillance specimen (positive, scanty positive, negative, or not done/lab issue)
-   `lab_xpert`: GeneXpert MTB/Rif status of TB case done at research laboratory on enhanced surveillance specimen (positive, negative, or not done/lab issue)
-   `lab_culture`: MGIT culture status of TB case done at research laboratory on enhanced surveillance specimen (culture positive, culture negative, or not done/lab issue)
-   `lab_id`: Speciation of MGIT culture done at research laboratory on enhanced surveillance specimen
-   `any_micro_confirm`: Derived variable: microbiologically-confirmed status of TB case (on the basis of enhanced surveillance samples analysed at the research laboratory)
-   `c02hsaid`: Unique identifer of community health worker catchment area of residence
-   `resident`: Derived variable: was TB case a resident of a community health worker catchment area?

<br>

### `clinics.rda`

Public and private health centres that register TB cases in Blantyre, and that participated in enhanced TB surveillance activities.

-   `Name`: TB registration clinic name
-   `geometry`: Coordinate references for clinics in `sfc_POINT` format with `WGS84` pojection

<br>

### `cluster_cases_sf.rda`

A copy of data within the `all_cases_sf` dataset, but only including the 3723 TB cases who were resident within a community health worker catchment area.

<br>

### `cnrs.rda`

Data on population denominators and annual TB case notification rates for each of the 315 community health worker catchment areas.

-   `c02hsaid`: Unique identifier for community health worker catchment area
-   `total`: Total number of community health worker catchment area residents ennumerated during census
-   `male_adults`: Number of male adults resident in each community health worker catchment area during census
-   `female_adults`: Number of female adults resident in each community health worker catchment area during census
-   `children`: Number of children (aged 5-14 years) in each community health worker catchment area during census
-   `toddlers`: Number of toddlers (aged 1-4 years) resident in each community health worker catchment area during census
-   `infants`: Number of infants (aged &lt;=1 year) resident in each community health worker catchment area during census
-   `mean_people_per_hh`: Mean number of people per household
-   `area`: Area of community health worker catchment area in metres square
-   `area_skm`: Area of community health worker catchment area in kilometres square
-   `popdens`: Number of people per square kilometre in each community health worker catchment area
-   `m_f_adult_ratio`: Ratio of adult (15 years or older) males to females in each community health worker catchment area
-   `prop_adults`: Percentage (0-100%) of residents who were aged 15 years or older
-   `year`: Calendar year in which TB cases were registered
-   `n` Number of TB cases registered per year in each community health worker catchment area
-   `n_smearpositive`: Number of sputum-smear positive TB cases registered per year in each community health worker catchment area
-   `n_confirmed`: Number of microbiologically-confirmed TB cases registered per year in each community health worker catchment area
-   `cnr`: All TB case notification rate (per 100,000 residents per year)
-   `micro_cnr`: Microbiologically-confirmed TB case notification rate (per 100,000 residents per year)
-   `geometry`: Coordinate references for boundaries of community health worker catchment areas in `sfc_MULTIPOLYGON` format with `WGS84` pojection

<br>

### `dat.rda`

Data on population denominators and TB case notification rates over entire study period (2015-2017) for each of the 315 community health worker catchment areas.

-   `c02hsaid`: Unique identifier for community health worker catchment area
-   `total`: Total number of community health worker catchment area residents ennumerated during census
-   `male_adults`: Number of male adults resident in each community health worker catchment area during census
-   `female_adults`: Number of female adults resident in each community health worker catchment area during census
-   `children`: Number of children (aged 5-14 years) in each community health worker catchment area during census
-   `toddlers`: Number of toddlers (aged 1-4 years) resident in each community health worker catchment area during census
-   `infants`: Number of infants (aged &lt;=1 year) resident in each community health worker catchment area during census
-   `mean_people_per_hh`: Mean number of people per household
-   `area`: Area of community health worker catchment area in metres square
-   `area_skm`: Area of community health worker catchment area in kilometres square
-   `popdens`: Number of people per square kilometre in each community health worker catchment area
-   `m_f_adult_ratio`: Ratio of adult (15 years or older) males to females in each community health worker catchment area
-   `prop_adults`: Percentage (0-100%) of residents who were aged 15 years or older
-   `year`: Calendar year in which TB cases were registered
-   `n` Number of TB cases registered per year in each community health worker catchment area
-   `n_confirmed`: Number of microbiologically-confirmed TB cases registered per year in each community health worker catchment area
-   `cnr`: All TB case notification rate (per 100,000 residents per year)
-   `micro_cnr`: Microbiologically-confirmed TB case notification rate (per 100,000 residents per year)
-   `geometry`: Coordinate references for boundaries of community health worker catchment areas in `sfc_MULTIPOLYGON` format with `WGS84` pojection
-   `c02hsaid1` Additional ID for community healthworker catchment area, required for producing k nearest neighbour matrices
-   `mwi11povcons200`: Percentage of community health worker catchment area living on less than US$2 per day (data from worldpop project: <http://www.worldpop.org.uk/data/files/index.php?dataset=171&zip_title=Malawi%201km%20Poverty&action=group>))

<br>

### `hsas.rda`

Reference table for geometries of 315 community health worker catchment areas in Blantyre.

-   `c02hsaid`: Unique identifier for community health worker catchment area
-   `geometry`: Coordinate references for boundaries of community health worker catchment areas in `sfc_MULTIPOLYGON` format with `WGS84` pojection
