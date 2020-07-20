#' Generic methods to save R in-memory objects to file to SAGA-GIS to access
#' 
#' Designed to be used internally by Rsagacmd for automatically pass data to
#' SAGA-GIS for geoprocessing
#'
#' @param x An R object.
#' @param ... Other parameters such as the temporary directory to use for
#'   storage.
#'
#' @return A character that specifies the file path to where the R object was
#'   saved.
#' @export
save_object <- function(x, ...) {
  UseMethod("save_object", x)
}

#' @export
save_object.default <- function(x, ...) {
  x
}

#' @export
save_object.character <- function(x, ...) {
  paste(x, collapse = ";")
}

#' @export
save_object.sf <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  temp <- tempfile(tmpdir = temp_path, fileext = ".shp")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  sf::st_write(obj = x, dsn = temp, quiet = TRUE)
  
  temp
}

#' @export
save_object.RasterLayer <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  # pass file name to saga if RasterLayer from single band raster
  if (raster::nbands(x) == 1 &
    raster::inMemory(x) == FALSE &
    tools::file_ext(raster::filename(x)) != "grd") {
    x <- raster::filename(x)

    # else save band as a single band temporary file and pass temp file name
  } else if (raster::nbands(x) != 1 |
    raster::inMemory(x) == TRUE |
    tools::file_ext(raster::filename(x)) == "grd") {
    temp <- tempfile(tmpdir = temp_path, fileext = ".tif")
    raster::writeRaster(x, filename = temp)
    x <- temp
  }

  x
}

#' @export
save_object.RasterStack <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  if (raster::nlayers(x) == 1) {
    x <- raster::raster(x)
    x <- save_object(x)
  } else {
    stop(
      paste(
        "Raster object contains multiple layers;",
        "SAGA-GIS requires single layer rasters as inputs"
      ),
      call. = FALSE
    )
  }

  x
}

#' @export
save_object.data.frame <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  temp <- tempfile(tmpdir = temp_path, fileext = ".txt")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  utils::write.table(x = x, file = temp, sep = "\t")
  temp
}

spatial_to_saga <- function(x, temp_path) {
  temp <- tempfile(tmpdir = temp_path, fileext = ".shp")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  rgdal::writeOGR(
    obj = x,
    dsn = temp,
    layer = 1,
    driver = "ESRI Shapefile"
  )
  
  temp
}

#' @export
save_object.SpatialPointsDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}

#' @export
save_object.SpatialLinesDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}

#' @export
save_object.SpatialPolygonsDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}

#' @export
save_object.list <- function(x, ...) {
  lapply(x, save_object)
}
