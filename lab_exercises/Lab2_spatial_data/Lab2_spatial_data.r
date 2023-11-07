#' BIOs 5211/9211
#' Olav Skarpaas, Erik Kusch (rework and addition to environmental data and GBIF data), Peter Horvath (environmental data) and Dag Endresen (GBIF data)
#' NHM, University of Oslo, Nov 2021

# Lab2. Data download, visualization and basic spatial analysis
############################################################# ++
#' In this lab we will go through some tools for working 
#' with spatial data in R to prepare for distribution modelling.

# Data for Norway ----
#' The environmental data set for Norway in this lab is a subset of
#' the data compiled and described in Horvath et al. 2019. Distribution 
#' modelling of vegetation types based on area frame survey
#' data. Applied Vegetation Science, 22(4): 547-560 (see syllabus).

#' We will first import and work with the spatial environmental data 
#' layers, and then combine this with species occurrences to prepare for
#' distribution modelling.

## Spatial Data ----------------------------------------------------------------
## Libraries: spatial data tools ----
#' we show you a different way of handling package installation of loading in one step here:
install.load.package <- function(x) { # custom function that loads the desired package and automatically installs any non-installed packages
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c( # vector of package names
  "sp", # vector tools
  "raster", # raster tools
  "rgdal",  # spatial data tools (geospatial data abstraction library)
  "fields", # color ramps (e.g. tim.colors)
  "rnaturalearth" # for shapefile retrieval
)
sapply(package_vec, install.load.package) # applying install/load function to each name in package vector

### Environmental Data Loading ----
#' if you have NOT downloaded the spatial data for this exercise yet, then do so, by following this link: https://uio-my.sharepoint.com/:f:/g/personal/peterhor_uio_no/EjbrdH5bzjVLmWyz7JVVxhkB9vluk4RznDJbxmY54hKVsw?e=0luLHx
#' save the files and copy file path
#' Set file path for data - modify path to files on your computer
path <- "lab_data/RASTER" 

# Load environmental predictor maps for Norway
# Coordinate Reference System of the raster files is EPSG:32633 - WGS 84 / UTM zone 33N
# Topographic 
DEM <- raster(file.path(path, "dem100.tif"))                    # Digital elevation model (Kartverket)
ASP <- raster(file.path(path, "Aspect.tif"))                    # Aspect (Derived from DEM --- Direction? unit -> radians (?? rad = 180?))
TWI <- raster(file.path(path, "Topographic_Wetness_Index.tif")) # Topographic Wetness Index (Derived from DEM --- unitless - index)
SOL <- raster(file.path(path, "Total_insolation.tif"))          # Potential Incoming Solar Radiation (Derived from DEM --- SAGA, )
# Geological
GEO <- raster(file.path(path, "geo_norge123.tif"))              # Bedrock nutrient status (three classes, SOURCE)
# Climatic
TMP <- raster(file.path(path, "bioclim_1.tif"))                 # Annual mean temperature (Bioclim)
TMP2 <- raster(file.path(path, "bioclim_10.tif"))               # Mean Temperature of Warmest Quarter (Bioclim)
PRC <- raster(file.path(path, "bioclim_12.tif"))                # Annual Precipitation (Bioclim)
GSL <- raster(file.path(path, "Growing_season_length.tif"))     # Growing season length (MET - SeNorge 2, CDO algorithm - derived from temperature)
SW5 <- raster(file.path(path, "swe_5.tif"))                     # Snow water equivalent in May (MET - SeNorge 1.1.1)
SW1 <- raster(file.path(path, "swe_1.tif"))                     # Snow water equivalent in January (MET - SeNorge 1.1.1)
# Land cover
LC  <- raster(file.path(path, "ar50_artype.tif"))             # Land cover classification, based on AR50 (Nibio)
# 10	Developed area. Residential area, town center, city, transport, industrial area and alike.
# 20	Agricultural area. Cropland, cultivated soil and cultivated pastures
# 30	Forest. Afforested area
# 50	Barren land. Land areas with natural vegetation cover that is not forest.
# 60	Bog and Fen. Land areas with characteristics of marshes
# 70	Glacier. Ice and snow that does not melt during the summer.
# 81	Freshwater. Rivers and lakes
# 82	Ocean.
# 99	Not mapped.

### Plotting Raster Data ----
# Plot maps to inspect data
# These are high-resolution maps, so each takes time to render,
par(mfrow=c(4,3)) # split plotting area into 4 rows and 3 columns
plot(DEM,main="Elevation (m)")
plot(ASP,main="Aspect (RAD)")
plot(TWI,main="Topographic Wetness Index")
plot(SOL,main="Solar Radiation")
plot(GEO,main="Bedrock (nutrient class)", breaks = c(0,1,2,3), col = terrain.colors(4))
plot(TMP,main="Temperature (C, annual mean)")
plot(TMP2,main="Temperature (C, warmest quarter)")
plot(PRC,main="Precipitation (mm, annual)")
plot(SW5,main="Snow water equivalent (mm, May)")
plot(SW1,main="Snow water equivalent (mm, January)")
plot(GSL,main="Growing season length (days)")
plot(LC,main="Land Cover AR50")
par(mfrow=c(1,1)) # set plotting area back to normal

### Inspecting Raster Data ----
# Inspect distributions of values
hist(DEM, maxpixels=1000) # continuous
hist(GEO, maxpixels=1000) # categorical

### Calculating with Raster Data ----
# Calculate terrain parameters from DEM on your own - instead of loading pre-calculated
# this will take extra time (50 seconds on my PC)
start_time <- Sys.time() 
TERR <- terrain(DEM,c("slope","aspect","TPI", "TRI"),  unit='degrees')
end_time <- Sys.time()
end_time - start_time
plot(TERR)

# add raster layers DEM and aspect to retrieve an aggregate score of elevation and aspect
ELEASP <- DEM+ASP # you can carry out mathematical operations with raster layers like they were individual numbers

# show only Glaciers across Norway
plot(LC == 70) # you can make logical statements with rasters like they were individual items

# Make background raster for Norway
norway <- predictors[["dem100"]]/predictors[["dem100"]]           # raster with values=1 in mainland Norway, based on elevation raster
plot(norway)

### Handling Raster Data ----
# You can build a raster stack of predictors loaded individually as follows:
predictors <- stack(DEM,TERR,TWI,SOL,GEO,LC,TMP)

# We use an alternative method for loading several files at once,
# which keeps file names as variable names:
r.list <- list.files(path, pattern="tif$", full.names=TRUE) # add all raster files in a given folder to a list
predictors <- stack(r.list)                                 # read and stack raster layers
names(predictors)
# If required, the rasters can be renamed: names(r.stack) <- c("land_cover",...)

# Convert raster layers with factor variables to factors
predictors[["ar50_artype"]] <- as.factor(predictors[["ar50_artype"]])
predictors[["geo_norge123"]] <- as.factor(predictors[["geo_norge123"]])

# Plot individual raster in the stack of predictor data
plot(predictors[[2]])
plot(predictors[["bioclim_1"]])
plot(predictors$Aspect)
# or plot all 
plot(predictors)
# subset layers from rasterstack
LC <- subset(predictors, 1)
LC <- predictors[[1]]
LC <- predictors$ar50_artype

### Playing with Extents ----
# find and save current extent of a layer
extent.LC <- extent(LC)
# modify extent to southern Norway
ymax(extent.LC) <- 7100000
xmax(extent.LC) <- 400000

# Limit to Oslo and surrounding area extent 
# This extent is great for fast testing of scripts
extent.OSL <- extent(220000, 280000, 6620000, 6680000)

# Check plotting
plot(predictors[[1]])
plot(extent.LC, add=TRUE, col="red")
plot(extent.OSL, add=TRUE, col="blue")

# Crop out from raster based on extent
predictors.OSLO <- crop(predictors, extent.OSL)

# Check for correlation between variables (testing only on small extent for practical reasons)
p_cor <- layerStats(predictors.OSLO, 'pearson', na.rm=T)
pairs(p_cor)
# or for example checking correlation like this (only 4 test layers)
pairs(predictors.OSLO[[2:5]])
# second and third are strongly correlated
# you can choose to drop one of them
predictors.OSLO <- dropLayer(predictors.OSLO,3)
# predictors.OSLO <- predictors.OSLO[[-3]] # alternatively you can do this, too

### Raster Resolution ----
# Adjust resolution of rasters
res(predictors.OSLO)
predictors.OSLO.2km <- aggregate(predictors.OSLO, fact=20) # from original resolution 100m down to 2km
res(predictors.OSLO.2km)

### Raster Saving ----
# Saving your raster back to disc
path <- "lab_data/" 
writeRaster(predictors.OSLO.2km, filename=paste0(path, names(predictors.OSLO.2km), "_2km"), bylayer=TRUE, format="GTiff", overwrite=TRUE ) # GTiff files are good, but NETCDF files are the standard in most spatial applications, those are created with format = "CDF", you'll see this in the advanced section at the bottom


### Coordinate Reference Systems ----
#' usually, at larger scales, spatial products are reported in degrees longitude and latitude rather than metres from a point of origin like the data you have loaded right now
#' Different ways of referencing how points on Earth relate to each other in space are referred to as Coordinate Reference Systems (CRS)
#' Read out the CRS of the predictor data for OSLO at high resolution:
crs(predictors.OSLO)
#' now change the CRS to a longitude/latitude representation:
predictors.OSLO.lonlat <- projectRaster(predictors.OSLO, crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#' overview of CRS and their codes can be found here: https://epsg.io/?q=
predictors.OSLO.lonlat
plot(predictors.OSLO.lonlat)
#' Notice the difference?

## GBIF Data -------------------------------------------------------------------
## Libraries: spatial data tools ----
#' we show you a different way of handling package installation of loading in one step here:
install.load.package <- function(x) { # custom function that loads the desired package and automatically installs any non-installed packages
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c( # vector of package names
  "rgbif",      # global biodiversity data (GBIF) tools
  "maps",       # Provides functions that let us plot the maps
  "mapdata",    # Contains the hi-resolution points that mark out the countries.require(maps)
  "mapproj",
  "maptools" # mapping tools for spatial objects
)
sapply(package_vec, install.load.package) # applying install/load function to each name in package vector


# The approach below gives quick access to data sets of limited size, suitable
# for our purpose in this course.
# However, for scientific publications you should use asynchronous downloading,
# as demonstrated here 
# Erik Kusch: https://www.erikkusch.com/courses/gbif/ (some of this material is included in the advanced steps at the bottom)
# by Anders Finstad in a workshop at the Nordic Oikos conference: https://gbif-europe.github.io/nordic_oikos_2018_r/s3_gbif_demo/3.x_async_download_gbif.Rmd
# This allows downloading larger data sets, and citation of a download with a single doi.

### Data Discovery ----
# first, we figure out how GBIF indexes the species we are interested in:
key <- name_backbone(name="Picea sitchensis", kingdom="Plantae")$speciesKey
key

### Data Download ----
# next, we query retrieval of 1000 observation of our target species within Norway
sp <- occ_search(taxonKey=key, # which species we want
                 hasCoordinate=TRUE, # whether data should be geo-referenced
                 country="NO", # what country the data should come from, these are ALPHA-2 ISO country codes: https://www.nationsonline.org/oneworld/country_code_list.htm
                 limit=1000 # how many observations to retrieve at most
                 ) 
#' NOTICE: do NOT use this function for your actual research!!

# data sources GBIF has used for your download:
gbif_citation(sp)                # Overview of data sources with references, including doi. See https://www.gbif.org/tool/81747/rgbif#citations

### Data Handling ----
# Convert lat-long coordinates to coordinate system of environmental raster data
occ_points <- data.frame(x=sp$data$decimalLongitude,y=sp$data$decimalLatitude) # extract coordinates
occ_points <- SpatialPoints(occ_points,proj4string=CRS("+proj=longlat +datum=WGS84")) # make spatial points, CRS is WGS84 because that is how GBIF records the data
occ_UTM33 <- spTransform(occ_points,CRS("+proj=utm +zone=33 ellps=GRS80 +units=m")) # transform to UTM CRS
sp$data$x <- occ_UTM33$x
sp$data$y <- occ_UTM33$y

# Point data like 'sp' can be used directly as input to MIAmaxent (Labs 4-5),
# along with environmental rasters.
# For glm (Lab 3), we need absence data in addition to presences, and we
# need to combine presences and absences with environmental data in a
# data set suitable for the glm function. This can be done as follows.

# Rasterize occurrences
occ_ras <- rasterize(occ_UTM33,norway,fun='count',background=0)   # raster with counts of occurrences in each cell of norway
plot(occ_ras)
occ_ras <- occ_ras*norway                                         # filtering occurrence raster with mainland raster
occ_ras[occ_ras>0] <- 1                                           # reducing counts>0 to 1 (presence)
plot(occ_ras)
zoom(occ_ras)                                                     # zoom by clicking twice to western Norway to see the rasterized presence pixels
plot(occ_ras, xlim = c(-74000,10000), ylim = c(6610000, 6680000)) # zoom to western Norway to see the rasterized presence pixels
summary(occ_ras)
table(values(occ_ras))

## Similar plots as above, with different color scales
# plot(occ_ras,col=colorRampPalette(c("blue","red"))(2))
# plot(occ_ras,col=colorRampPalette(c("blue","red"))(2), xlim = c(-74000,10000), ylim = c(6610000, 6680000) )

# Take the occurrence cells as presences
presences <- which(values(occ_ras)==1)

# Then generate absence data by sampling from the cells without occurrence observations.
# NB! This rests on risky assumptions, but we need the absences
# for logistic regression with glm (Lab 3). We make this absence data set for
# educational purposes, but ideally one would want a data set with true, verified presences
# and absences for logistic regression (and also for validation of 'presence-only' methods,
# such as maxent).
absences <- which(values(occ_ras)==0)
absences_sample <- sample(absences,size=length(presences)) # sample of same number of absence cells as presence cells

### Extracting Environmental Data to match GBIF Data ----
# Combine presences, absences and environmental data
selected <- c(presences,absences_sample)
xy <- coordinates(occ_ras)[selected, ]
training_data <- data.frame(xy,presence=values(occ_ras)[selected],
                  extract(predictors,xy))
head(training_data)
tail(training_data)
plot(training_data[,c("x","y")],col=c("blue","red")[training_data$presence+1])

# Convert discrete environmental predictors to factor variables
training_data$ar50_artype <- factor(training_data$ar50_artype)
training_data$geo_norge123 <- factor(training_data$geo_norge123)

# Save data (you'll overwrite any existing files with these names if you run these lines)
save(training_data,file="lab_data/Norway_sitka_training_data")
save(predictors,file="lab_data/Norway_predictor_raster_stack")


# ADVANCED EXERCISES -----------------------------------------------------------
# Go through the steps make sure you understand what the functions are doing. Look up with '?'
# Then change the species and/or predictors to something of your own choice (i.e. manipulate the code below),
# you can make a new data set and repeat the exercies in Lab1.

#' create a folder (also referred to as a directory) to hold the data we obtain in the advanced section here
Dir.Lab2Adv <- file.path("lab_data", "Lab2ADV")
if(!dir.exists(Dir.Lab2Adv)){dir.create(Dir.Lab2Adv)}

## Climate Data Retrieval from the Internet ----
#' Usually, you won't have access to spatial products ready to go in a folder on your hard drive like we have for the above exercise part
#' Instead, you need to obtain your spatial data from the internet
#' To demonstrate more capability of spatial operations in R, let's obtain a shapefile (polygon) that delimits Norway's area
Shape_shp <- ne_countries(country = "Norway", scale = "medium")
plot(Shape_shp)

### Gridded Observations ----
# The code below is how training data for the DM primer (Lab1) were prepared.
# Environmental predictor variable data: download WorldClim data, extract temperature
env <- raster::getData("worldclim",var="bio",res=10)  # Biologically relevant climate variables: http://www.worldclim.org/bioclim
temp <- env[[1]]; names(temp) <- "temp"       # Selecting annual mean temperature and naming it 'temp'
temp
plot(temp)

#' Now to limit this data to the area of Norway:
temp <- crop(temp, extent(Shape_shp)) # rectangular cutting of data
plot(temp)
temp <- mask(temp, Shape_shp) # actual limiting by polygon outline
plot(temp)

### Climate Reanalyses with KrigR ----
#' as discussed in the lecture content, climate reanalyses come with a host of advantages over the gridded observations products like the above WorldClim
#' To access gobal state-of-the-art climate reanalysis data, you can use the KrigR package
#' KrigR is not on CRAN yet so needs to be installed like this:
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
devtools::install_github("https://github.com/ErikKusch/KrigR") # at this github is where you find the code base for KrigR
library(KrigR)
#' For an introduction to KrigR and details on how to use it to its full capacity (biolcimatic variable download and calculation, statistical interpolation, etc.) see this material: https://www.erikkusch.com/courses/krigr/
#' Most importantly for now, you will need API credentials (a user name and a user key) to interact with the servers that host climate reanalysis data
#' I have already set up a temporary set of CDS credentials so you don't need to worry about this step for now. Our credentials are:
API_User <- 265148
API_Key <- "67a96d06-86b5-458d-94cb-fca1cc8d05c8" 
#' NOTICE: these will be deleted on 08/11/23 (the day after the lab exercise is run)
#' To make your own set of credentials register yourself here: https://cds.climate.copernicus.eu/user/register?destination=%2F%23!%2Fhome or follow this guide https://cds.climate.copernicus.eu/api-how-to

#' Now you can download climate data you need, we will download two sets:
#' 1. Mean air temperature from 1970 to 2000 (same time range and variable as obtained from WorldClim)
#' 2. Mean soil moisture in a depth of 0-7cm from 1970 to 2000 (same time range but a variable you cannot get from WorldClim)
#' Such downloads are handled as follows:
AT_KrigR <- download_ERA(
  Variable = "2m_temperature", # air temperature notation in era5-land data set
  DataSet = "era5-land", # data at 9-9km resolution: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
  DateStart = "1970-01-01", # where our time range starts
  DateStop = "2000-12-31", # where our time range ends
  TResolution = "year", # what temporal resolution forms the base for aggregation
  TStep = 31, # how many of the base temporal resolution form one step
  Extent = Shape_shp, # which spatial area to obtain data for, can be empty (global extent), extent (rectangular box), shapefile (polygon masking), or point locations
  Dir = Dir.Lab2Adv, # where to store the data we download
  FileName = "AT_KrigR", # what to call the downloaded NETCDF file
  API_User = API_User, # API credentials
  API_Key = API_Key, # API credentials
  SingularDL = TRUE # whether to attempt download of all data in one go (there is a limit of 1000 layers of data)
)

# download of soil moisture data in depth layer 0-7cm:
QS_KrigR <- download_ERA(
  Variable = "volumetric_soil_water_layer_1",
  DataSet = "era5-land",
  DateStart = "1970-01-01",
  DateStop = "2000-12-31",
  TResolution = "year",
  TStep = 31,
  Extent = Shape_shp,
  Dir = Dir.Lab2Adv,
  FileName = "QS_KrigR",
  API_User = API_User,
  API_Key = API_Key,
  SingularDL = TRUE
)

# plot the ERA5-Land data:
plot(stack(AT_KrigR, QS_KrigR))

### Comparing Data Products ----
#' investigate the difference between WorldClim and ERA5-Land air temperature data:
temp-AT_KrigR # this doesn't run due to a resolution mismatch
#' which resolution is higher?
res(temp)
res(AT_KrigR)
#' In this case, ERA5-Land has the higher resolution
#' We resolve resolution mismatches either with proper statistical interpolation or by resampling our raster objects (DO NOT do this to achieve higher spatial resolutions):
AT_resamp <- raster::resample(x = AT_KrigR, y = temp)
#' Now we can compare them:
plot(
  (AT_resamp-273.15) - # stored as Kelvin
    temp/10 # stored as degree Celsius * 10
     )
#' As you can see, there exist CONSIDERABLE differences in the data sets, ERA5-Land is more accurate.

## Asynchronous GBIF Downloading ----
#' Downloading GBIF data asynchronously allows for much larger data to be obtained and proper citation of the download itself to be implemented in your research reports/manuscripts
#' You can find an introduction to how to best access GBIF from within R here: https://www.erikkusch.com/courses/gbif/
#' DO THIS for your research!

#' Again, we require API credentials. I have created those for this course and will delete them on 08/11/23 (the day after the lab exercise is run):
options(gbif_user = "uiosdmcourse")
options(gbif_pwd = "UiOSDM23!")
options(gbif_email = "erik.kusch@nhm.uio.no")

#' Data Discovery, feel free to select a different species
sp_name <- "Calluna vulgaris"
sp_backbone <- name_backbone(name = sp_name)
sp_key <- sp_backbone$usageKey

#' Download query, uses a different function that we used above for the simple demo
res <- occ_download(
  pred("taxonKey", sp_key), # which species to obtain data for
  pred_in("basisOfRecord", c("HUMAN_OBSERVATION")), # we just query observations made by humans
  pred("country", "NO"), # we want data only within Norway
  pred("hasCoordinate", TRUE), # data should have coordinates
  pred_gte("year", 1970), # time range start
  pred_lte("year", 2000) # time range end
)

#' Downloading and Loading
## ping GBIF server and wait for data to be ready:
res_meta <- occ_download_wait(res, status_ping = 5, curlopts = list(), quiet = FALSE)
## download data when ready:
res_get <- occ_download_get(res, path = Dir.Lab2Adv)
## load downloaded data into R:
res_data <- occ_download_import(res_get)

#' Data Handling
# Limiting data according to quality control, let's say we want to remove all data whose geolocation is 10 or more metres uncertain:
precise_data <- res_data[which(res_data$coordinateUncertaintyInMeters < 10), ]
# Subsetting data for desired variables and data markers
data_subset <- precise_data[
  ,
  c("scientificName", "decimalLongitude", "decimalLatitude", "basisOfRecord", "year", "month", "day", "eventDate", "countryCode", "municipality", "taxonKey", "species", "catalogNumber", "hasGeospatialIssues", "hasCoordinate", "datasetKey")
]
head(data_subset)

# making spatialpoints
coordinates(data_subset) <- ~ decimalLongitude + decimalLatitude # another way of making spatialpoints
plot(AT_KrigR)
plot(data_subset, add = TRUE, col = "red")

# extracting data
extraction <- extract(stack(AT_KrigR, QS_KrigR), data_subset, df = TRUE)
colnames(extraction)[-1] <- c("AT", "QS")
par(mfrow = c(1,2))
hist(extraction$AT, main = "Air Temperature", n = 20, col = "red")
hist(extraction$QS, main = "Soil Moisture", n = 20, col = "blue")
par(mfrow = c(1,1))

#' Download Citation
paste("GBIF Occurrence Download", occ_download_meta(res)$doi, "accessed via GBIF.org on", Sys.Date())
  

# When you're done, here's a link for inspiration and further independent study:
# GIS with R (shift+click) https://pakillo.github.io/GISwithR/