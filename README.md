# Estimating Domestic Wells for the United States in 2010

Below, you will find a step by step walkthrough of how to reproduce the estimates for private well locations in the United States using methods first proposed in [Weaver et al., 2017](https://www.sciencedirect.com/science/article/pii/S0048969717315280) and expanded upon in Murray et al., 2020 (in review). All of the scripts referenced below can be located in the DomesticWells/scripts/ folder.

## Data to download:

I recommend downloading all data from the [National Historical Geographic Information System](NHGIS.org).  They have added in easy to use join features for Census data which will avoid some headaches throughout the method.

GIS Boundary Shapefiles:
1.	1990 Census Block Groups
2.	2000 Census Block Groups (2010 Edition)
3.	2010 Census Block Groups

Data Tables:
- 1990 Census
  - Source of Water (1990 Census: STF 3 - Sample-Based Data)
  - Housing Units (1990 Census: STF 1 - 100% Data)
- 2000 Census
  - Housing Units (2000 Census: SF 1b - 100% Data [Blocks & Block Groups])
- 2010 Census
  - Housing Units (2010 Census: SF 1a - P & H Tables [Blocks & Larger Areas])
  - Total Population (2010 Census: SF 1a - P & H Tables [Blocks & Larger Areas])


## Software:

You will need a recent distribution of R and R Studio. The packages you will need to install are listed here:
  
  - tidyverse
  
  - sf
  
  - here
  
  - units
  
  - lubridate

  - raster

`<addr>`

'is this code'

<what about this>

## Scripts

Once you have downloaded this repository, and the necesarry files from NHGIS, you are ready to proceed through the scripts used in the anlysis. The scripts must be run in order as many files needed for each script to run are generated in previous scripts.

## Data Preparation (/scripts/01_dataPrep.R)

spatial data is delivered in shapefile format from NHGIS and while it is still the most widely use spatial file format, it is not particularly fast. For this reason, we want to convert the format to something more compact that will load and process faster. For this, we will use the [geopackage format](https://www.gis-blog.com/geopackage-vs-shapefile/).

Using the script '01_dataPrep.R', convert the block group shapefiles to geopackages and join the tabular data from the Census (downloaded from NHGIS).

## Convert to Densities and Rasterize (/scripts/02_convert_to_densities.R)
Data from the *1990 & 2000 Census* are in vector format, but must be rasterized before conversion to 2010 boundaries can occur. Conversion to 2010 boundaries is essential in order to do calculations based on Census data accross time. 

Now that the data is prepared, the next step is to calculate the rate of well use for each block group in the United States for 1990. Then we convert all of the data we plan to use from 1990 and 2000 into densities and rasterize them. Later on we will overlay 2010 boundaries on top of these rasters to convert census boundaries from 1990 & 2000 to 2010. Use the script titled '02_convert_to_densities.R' to calculate this.

![](/plots/nonzero_Well_Rate_1990.jpeg)

The rate of well usage per block group in 1990. Block groups with a rate of zero (n = 128,725) not shown in this plot

To rasterize 

## Convert rasterized data to 2010 boundaries (/scripts/03_convert_to_2010_boundaries.R)


## Import constructed domestic well data (/scripts/04_Constructed_Wells_Spatial_Join.R)


## Calculate Estimates (/scripts/05_calculate_Estimates.R)


## Refine results to account for spatially induced errors (/scripts/06_refine_results.R)
