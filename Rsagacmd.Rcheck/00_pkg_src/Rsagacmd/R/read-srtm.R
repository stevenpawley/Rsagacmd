#' Get path to the example DEM data
#'
#' Rsagacmd comes bundled with a small tile of example Digital Elevation Model
#' (DEM) data from the NASA Shuttle Radar Topography Mission Global 1 arc second
#' V003. This data is stored in GeoTIFF format in `inst/extdata`.
#' 
#' The dataset contains the land surface elevation of an area located near
#' Jasper, Alberta, Canada, with the coordinate reference system (CRS) EPSG code
#' of 3402 (NAD83(CSRS) / Alberta 10-TM (Forest)).
#' 
#' To access the data, use the convenience function of `read_srtm()` to load
#' the data as a `terra::SpatRaster` object.
#'
#' @export
#' @examples
#' library(Rsagacmd)
#' library(terra)
#' 
#' dem <- read_srtm()
#' plot(dem)
read_srtm <- function() {
  terra::rast(system.file("extdata/srtm.tif", package = "Rsagacmd"))
}