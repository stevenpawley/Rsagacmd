#' Generic methods to save R in-memory objects to file to SAGA-GIS to access
#' 
#' Designed to be used internally by Rsagacmd for automatically pass data to
#' SAGA-GIS for geoprocessing.
#'
#' @param x An R object.
#' @param ... Other parameters such as the temporary directory to use for
#'   storage.
#'
#' @return A character that specifies the file path to where the R object was
#'   saved.
#' @export
#' 
#' @keywords internal
save_object <- function(x, ...) {
  UseMethod("save_object", x)
}


#' @export
#' @keywords internal
save_object.default <- function(x, ...) {
  x
}


#' @export
#' @keywords internal
save_object.character <- function(x, ...) {
  paste(x, collapse = ";")
}


#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
save_object.SpatRaster <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  # check for multiple layers
  if (terra::nlyr(x) > 1) {
    rlang::abort(
      "SpatRaster object contains multiple layers. SAGA-GIS requires single-layer rasters as inputs"
    )
  }
  
  # check if layer is in-memory
  if (terra::sources(x)$source == "") {
    in_memory <- TRUE
    part_of_multiband <- FALSE
  } else {
    in_memory <- FALSE
  }

  # check if layer is part of a multi-band raster
  if (!in_memory) {
    info <- rgdal::GDALinfo(terra::sources(x)$source)
    n_bands <- nrow(attr(info, "df"))
    part_of_multiband <- n_bands > 1
  }
  
  # single-band raster on disk -> filename -> saga
  if (!part_of_multiband & !in_memory)
    x <- terra::sources(x)$source
  
  # otherwise save to temporary file
  if (part_of_multiband | in_memory) {
    temp <- tempfile(tmpdir = temp_path, fileext = ".tif")
    terra::writeRaster(x, filename = temp)
    x <- temp
  }
  
  x
}


#' @export
#' @keywords internal
save_object.RasterStack <- function(x, ...) {
  
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  if (raster::nlayers(x) == 1) {
    x <- raster::raster(x)
    x <- save_object(x)
  } else {
    rlang::abort("Raster object contains multiple layers. SAGA-GIS requires single layer rasters as inputs")
  }

  x
}


#' @export
#' @keywords internal
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
#' @keywords internal
save_object.SpatialPointsDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}


#' @export
#' @keywords internal
save_object.SpatialLinesDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}


#' @export
#' @keywords internal
save_object.SpatialPolygonsDataFrame <- function(x, ...) {
  args <- list(...)
  temp_path <- args$temp_path
  
  if (is.null(temp_path))
    temp_path <- tempdir()
  
  spatial_to_saga(x, temp_path)
}


#' @export
#' @keywords internal
save_object.list <- function(x, ...) {
  lapply(x, save_object)
}


#' Updates a `parameter` object with file paths to the R data objects.
#'
#' @param param A `parameter` object.
#' @param temp_path A character specifying the tempdir to use for storage (optional).
#' @param backend A character with the raster backend ('raster' or 'terra).
#'
#' @return A `parameter` object with an updated `file` attribute that refers to
#'   the on-disk file for saga_cmd to access.
#' @keywords internal
update_parameter_file <- function(param, temp_path = NULL, backend = NULL) {
  # update the `files` attribute with the file path to the object in `parameter$value` attribute
  if (!is.null(param$value))
    param$files <- save_object(param$value, temp_path = temp_path, backend = backend)
  
  # convert arguments that contain lists into semi-colon separated character strings for use with saga_cmd
  if (length(param$files) > 1) {
    param$files <- paste(param$files, collapse = ";")
    param$files <- gsub(".sdat", ".sgrd", param$files)
  }
  
  param
}


#' Updates a `parameters` object with file paths to the R data objects.
#'
#' @param param A `parameters` object.
#' @param temp_path A character specifying the tempdir to use for storage (optional).
#' @param backend A character with the raster backend ('raster' or 'terra).
#' @param all_outputs A logical indicating whether to use tempfiles for unspecified outputs.
#'
#' @return A `parameters` object with updated `file` attributes that refers to
#'   the on-disk file for saga_cmd to access.
#' @keywords internal
update_parameters_file <- function(params, temp_path = NULL, backend = NULL) {
  
  # update the `file` attribute of each `parameter` object
  params <- lapply(params, update_parameter_file, temp_path = temp_path, backend = backend)
  
  params
}


#' Update a `parameters` object using temporary files for any unspecified output parameters
#'
#' @param params A `parameters` object.
#' @param temp_path A character with the tempdir.
#' @param raster_format A character specifying the raster format.
#' @param vector_format A character specifiying the vector format.
#'
#' @return A `parameters` object.
#'
#' @keywords internal
update_parameters_tempfiles <- function(params, temp_path, raster_format, vector_format) {
  parameter_outputs <- params[sapply(params, function(param) !is.na(param$io))]
  parameter_outputs <- parameter_outputs[sapply(parameter_outputs, function(param) param$io == "Output")]
  parameter_outputs <- names(parameter_outputs)
  
  grid_features <- c("Grid", "Grid list", "Raster")
  shape_features <- c("Shape", "Shapes list")
  table_features <- "Table"
  
  for (n in parameter_outputs) {
    if (params[[n]]$io == "Output" & is.null(params[[n]]$files)) {
      
      # get tempfile with correct file extension
      if (params[[n]]$feature %in% grid_features) {
        params[[n]]$files <- tempfile(tmpdir = temp_path, fileext = raster_format)
        
      } else if (params[[n]]$feature %in% shape_features) {
        params[[n]]$files <- tempfile(tmpdir = temp_path, fileext = vector_format)
        
      } else if (params[[n]]$feature == table_features) {
        params[[n]]$files <- tempfile(tmpdir = temp_path, fileext = ".csv") 
      }
      
      params[[n]]$value <- params[[n]]$files
      
      # add to tempfile list
      tfiles <- params[[n]]$files
      tfiles <- strsplit(tfiles, ";")
      pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, tfiles)
    }
  }
  
  params
}


#' Drops unused/empty parameters from a `parameters` object
#'
#' @param params A `parameters` object
#'
#' @return A `parameters` object with empty `parameter` objects removed
#'
#' @keywords internal
drop_parameters <- function(params) {
  
  params <- params[sapply(params, function(param) 
    !is.null(param$value))]
  
  class(params) <- "parameters"
  
  params
}

