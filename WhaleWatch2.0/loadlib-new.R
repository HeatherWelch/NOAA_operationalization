
#1. pkgTest
# Function pkgTest - An R function to test if a package is installed. If not, the package and all dependencies will be installed from the default CRAN mirror.
## Code taken from Stack Overflow - http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE)) stop("Package FOUND")
  {
#
# D NOT ATTEMPT INSTALL
#    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


pkgTest("sp")
pkgTest("lubridate")
# pkgTest("SDMTools")
pkgTest("ncdf4")
pkgTest("here")
pkgTest("raster")
pkgTest("data.table")
pkgTest("lunar")
pkgTest("R.utils")
pkgTest("RColorBrewer")
pkgTest("colorRamps")
pkgTest("maps")
pkgTest("mapdata")
pkgTest("tweedie")
pkgTest("sp")
pkgTest("mgcv")
pkgTest("gbm")
pkgTest("rgdal")
pkgTest("fields")
pkgTest("maptools")
pkgTest("RNetCDF")
pkgTest("sf")
pkgTest("glue")
pkgTest("scales")
pkgTest("tidyverse")
pkgTest("magick")
pkgTest("fasterize")
