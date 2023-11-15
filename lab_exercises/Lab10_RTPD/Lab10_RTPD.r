# BIOS 5211/9211
# Olav Skarpaas, Eva Lieungh, Peter Horvath and Adam Naas,
# NHM, University of Oslo, Nov 2021, with updates by Adam Naas, Nov 2023

# Lab10. RTPD
#############

# The students are split into groups that will address effects of
# ONE of these data properties (see notes on data etc. for each
# theme below):
# 1. Extent of study area
# 2. Grain of study area
# 3. Density of presence points

# Each person in the group works with a different species, and addresses
# the following questions:
# How does extent/resolution/density [ONE of these] affect
# a. Observed response frequencies
# b. Environmental variable covariation
# c. Model structure (selected environmental variables) 
# d. Modelled response curves (for selected environmental variables)
# e. Model predictions (spatial predictions, AUC)

# Compare around the table: how do results for different species compare?
# How/why do they differ?

# Make a presentation (about 10 min in total) of individual results
# (2 mins per person), as well as summary across the group (2 mins).

# Species list (one per person):
# ------------------------------
# Hymenoscyphus fraxineus (askeskuddbeger) - Hymenoscyphus fraxineus - Fungi
# Harlequin ladybird (harlekinmarih??ne) - Harmonia axyridis - Animalia
# Lodgepole pine (vrifuru) - Pinus contorta - Plantae
# Canada goose (kanadag??s) - Branta canadensis ??? Animalia



# Load libraries
library(rgbif)
library(raster)
library(sp)
library(MIAmaxent)

# Source script with functions
source('lab_exercises/Lab10_RTPD/Dataprep_functions.r')



# Theme 1: EXTENT
#################

# You will work with three extents:
# 1. All of Norway
# 2. Southern or northern Norway (depending on distribution of species)
# 3. South-eastern or south-western Norway (depending on species, but extent may not be appropriate for all species)

# Here is the task in words:

# First, download a data set of occurrences for your species from GBIF.
# Then, for each extent, do the following:
# - Load predictor rasters (2km resolution) for the appropriate extent (modify paths and raster stack names as needed)
# - Generate a training data set based on the occurrences and the predictor rasters
# - Fit an appropriate distribution model and assess it with respect to a-e above

# Here is some draft code:

# Download GBIF data
key <- name_backbone(name= "WRITE YOUR SPECIES NAME HERE", kingdom= "WRITE THE APPROPORIATE KINGDOM HERE")$speciesKey
sp <- occ_search(taxonKey=key, hasCoordinate=TRUE, country="NO", limit=1000)

# Modify and repeat the following for all three extents:

# Load environmental predictor data as rasters
r.list <- list.files("lab_data/RTPD/TIFF_EXTENT/NORWAY", pattern="tif$", full.names=TRUE)
predictors.Norway <- stack(r.list)

# Generate training data for your species for the particular extent
training.data.Norway <- generate.training.data(sp,predictors.Norway,generate.absences=TRUE,absence.value=NA,n.abs=1000,factors=c("artype","norge123"))   # To see how the function 'generate.training.data' works, open the file 'Dataprep.functions.r'

# Inspect the data, select an appropriate distribution modelling method,
# fit a model, evaluate it and collect the information you need to answer the questions a-e above.
# Reuse code from other labs!

# Make sure to store the results/plots for each extent before you move on
# to the next, so that you can compare results across extents (i.e. answer the questions a-e above).

# Join with the others in your group to make a presentation of the results,
# as described above.



# Theme 2: GRAIN
################

# You will work with four different grains for the Oslo region:
# 1. 100 m
# 2. 1 km
# 3. 2 km
# 4. 10 km

# Here is the task in words:

# First, download a data set of occurrences for your species from GBIF.
# Then, for each grain, do the following:
# - Load predictor rasters for the appropriate grain (modify paths and raster stack names as needed)
# - Generate a training data set based on the occurrences and the predictor rasters
# - Fit an appropriate distribution model and assess it with respect to a-e above

# Here is some draft code:

# Download GBIF data
key <- name_backbone(name= "WRITE YOUR SPECIES NAME HERE", kingdom= "WRITE THE APPROPORIATE KINGDOM HERE")$speciesKey
sp <- occ_search(taxonKey=key, hasCoordinate=TRUE, country="NO", limit=1000)

# Modify and repeat the following for all three grains:

# Load environmental predictor data as rasters
r.list <- list.files("lab_data/RTPD/TIFF_GRAIN/100m", pattern="tif$", full.names=TRUE)
predictors.100m <- stack(r.list)

# Generate training data for your species for the particular grain
training.data.100m <- generate.training.data(sp,predictors.100m,generate.absences=TRUE,absence.value=NA,n.abs=1000,factors=c("artype","norge123"))   # To see how the function 'generate.training.data' works, open the file 'Dataprep.functions.r'

# Inspect the data, select an appropriate distribution modelling method,
# fit a model, evaluate it and collect the information you need to answer the questions a-e above.
# Reuse code from other labs!

# Make sure to store the results/plots for each grain before you move on
# to the next, so that you can compare results across grains (i.e. answer the questions a-e above).

# Join with the others in your group to make a presentation of the results,
# as described above.



# Theme 3: DENSITY
##################

# You will work with three different densities of occurrence data for each of the species:
# 1. original density (all occurrence data)
# 2. 50% density (50% of the occurrence data)
# 3. 10% density (10% of the occurrence data)

# Here is the task in words:

# First, load predictor rasters for Norway with 2km resolution (modify paths and raster stack names if needed)
# Second, download a data set of occurrences for your species from GBIF.
# Then, for each density of occurrences, do the following:
# - Resample the occurrence data set to the appropriate density
# - Generate a training data set based on the occurrences and the predictor rasters
# - Fit an appropriate distribution model and assess it with respect to a-e above

# Here is some draft code:

# Load environmental predictor data as rasters (Norway, 2km resolution)
r.list <- list.files("lab_data/RTPD/TIFF_EXTENT/NORWAY", pattern="tif$", full.names=TRUE)
predictors <- stack(r.list)

# Download GBIF data
key <- name_backbone(name= "WRITE YOUR SPECIES NAME HERE", kingdom= "WRITE THE APPROPORIATE KINGDOM HERE")$speciesKey
sp <- occ_search(taxonKey=key, hasCoordinate=TRUE, country="NO", limit=1000)

# Modify and repeat the following for all three densities:

# Thin the occurrence data to the appropriate density by sampling without replacement (.100pst indicates 100%)
# (modify 'thin' as needed)
n <- nrow(sp$data)
thin <- 1
sp.100pst <- sp 
sp.100pst$data <- sp$data[sample.int(round(n*thin),replace=FALSE),]

# Generate training data for your species for the particular density (.100pst indicates 100%)
training.data.100pst <- generate.training.data(sp.100pst,predictors,generate.absences=TRUE,absence.value=NA,n.abs=1000,factors=c("artype","norge123"))   # To see how the function 'generate.training.data' works, open the file 'Dataprep.functions.r'

# Inspect the data, select an appropriate distribution modelling method,
# fit a model, evaluate it and collect the information you need to answer the questions a-e above.
# Reuse code from other labs!

# Make sure to store the results/plots for each density before you move on
# to the next, so that you can compare results across densities (i.e. answer the questions a-e above).

# Join with the others in your group to make a presentation of the results,
# as described above.