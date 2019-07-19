translate <- function(x, ...) {
  UseMethod("translate", x)
}

translate.default <- function(param, ...) {
  return(param)
}

translate.sf <- function(param, temp_path = tempdir()) {
  temp <- tempfile(tmpdir = temp_path, fileext = ".shp")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  sf::st_write(obj = param, dsn = temp, quiet = TRUE)
  return(temp)
}

translate.RasterLayer <- function(param, temp_path = tempdir()) {
  # pass filename to saga if RasterLayer from singleband raster
  if (raster::nbands(param) == 1 &
    raster::inMemory(param) == FALSE &
    tools::file_ext(raster::filename(param)) != "grd") {
    param <- raster::filename(param)

    # else save band as a singleband temp file and pass temp filename
  } else if (raster::nbands(param) != 1 |
    raster::inMemory(param) == TRUE |
    tools::file_ext(raster::filename(param)) == "grd") {
    temp <- tempfile(tmpdir = temp_path, fileext = ".tif")
    raster::writeRaster(param, filename = temp)
    param <- temp
  }

  return(param)
}

translate.RasterStack <- function(param, temp_path = tempdir()) {
  if (raster::nlayers(param) == 1) {
    param <- raster::raster(param)
    param <- translate(param)
  } else {
    stop(
      paste(
        "Raster object contains multiple layers;",
        "SAGA-GIS requires single layer rasters as inputs"
      ),
      call. = FALSE
    )
  }

  return(param)
}

translate.data.frame <- function(param, temp_path = tempdir()) {
  temp <- tempfile(tmpdir = temp_path, fileext = ".txt")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  utils::write.table(x = param, file = temp, sep = "\t")
  param <- temp
  return(param)
}

spatial_to_saga <- function(param, temp_path) {
  temp <- tempfile(tmpdir = temp_path, fileext = ".shp")
  pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  rgdal::writeOGR(
    obj = param,
    dsn = temp,
    layer = 1,
    driver = "ESRI Shapefile"
  )
  param <- temp
  return(param)
}

translate.SpatialPointsDataFrame <- function(param, temp_path = tempdir()) {
  spatial_to_saga(param, temp_path)
}

translate.SpatialLinesDataFrame <- function(param, temp_path = tempdir()) {
  spatial_to_saga(param, temp_path)
}

translate.SpatialPolygonsDataFrame <- function(param, temp_path = tempdir()) {
  spatial_to_saga(param, temp_path)
}
