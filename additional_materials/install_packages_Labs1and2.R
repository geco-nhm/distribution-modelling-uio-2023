# Installation and loading of pacakges in Labs 1 and 2
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
  "rgbif",      # global biodiversity data (GBIF) tools
  "maps",       # Provides functions that let us plot the maps
  "mapdata",    # Contains the hi-resolution points that mark out the countries.require(maps)
  "mapproj",
  "maptools" # mapping tools for spatial objects
)
sapply(package_vec, install.load.package) # applying install/load function to each name in package vector
