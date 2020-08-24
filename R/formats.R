supported_raster_formats <- c(
  `SAGA` = ".sdat",
  `SAGA Compressed` = ".sg-grd-z",
  `GeoTIFF` = ".tif"
)


supported_vector_formats <- c(
 `ESRI Shapefile` = ".shp",
 `GeoPackage` = ".gpkg",
 `GeoJSON` = "geojson"
)


#' List the available raster formats that can be set as defaults for a `saga` object.
#'
#' @return tibble
#' @export
#'
#' @examples
#' show_raster_formats()
show_raster_formats <- function() {
  tibble::tibble(
    driver = names(supported_raster_formats),
    extension = supported_raster_formats
  )
}


#' List the available vector formats that can be set as defaults for a `saga` object.
#'
#' @return tibble
#' @export
#'
#' @examples
#' show_vector_formats()
show_vector_formats <- function() {
  tibble::tibble(
    driver = names(supported_vector_formats),
    extension = supported_vector_formats
  )
}
