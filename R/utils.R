#' Calculate the t_slope value based on DEM resolution for MRVBF
#'
#' Calculates the t_slope value for the Multiresolution Index of Valley Bottom
#' Flatness (Gallant and Dowling, 2003) based on input DEM resolution. MRVBF
#' identified valley bottoms based on classifying slope angle and identifying
#' low areas by ranking elevation in respect to the surrounding topography
#' across a range of DEM resolutions. The MRVBF algorithm was developed using a
#' 25 m DEM, and so if the input DEM has a different resolution then the slope
#' threshold t_slope needs to be adjusted from its default value of 16 in order
#' to maintain the relationship between slope and DEM resolution. This function
#' provides a convenient way to perform that calculation.
#'
#' @param res numeric, DEM resolution
#' @param plot logical, produce plot of relationship
#'
#' @return numeric, t_slope value for MRVBF
#' @export
#'
#' @examples
#' mrvbf_threshold(res = 10, plot = TRUE)
mrvbf_threshold <- function(res, plot = FALSE) {
  # slope decreases by factor of 2 per every 'step' above a 25 m dem resolution
  # a step consists of a 3 factor increase in the dem cell size
  # Gallant and Dowling 2003
  
  dem_res <- c(6075, 2025, 675, 225, 75, 25, 8, 3, 1)
  mrvbf_slope <- c(0.5, 1, 2, 4, 8, 16, 32, 64, 128)
  ds <- data.frame(dem_res, mrvbf_slope)
  
  m <- minpack.lm::nlsLM(
    mrvbf_slope ~ a * I(dem_res^z), 
    data = ds, 
    start = list(
      a = 100, 
      z = 1)
    )

  if (plot == TRUE) {
    # produce nls smooth line
    predx <- seq(min(res, dem_res / 2), max(res * 2, dem_res), 0.1)
    predy <- stats::predict(m, list(dem_res = predx))

    # plot
    graphics::plot(dem_res, mrvbf_slope,
      xlab = "DEM resolution (m)",
      ylab = "MRVBF Initial Slope", 
      xlim = c(min(predx), max(min(dem_res), res) * 1.5), 
      xaxs = "i"
    )
    
    graphics::lines(predx, predy)
    graphics::lines(
      x = c(res, res), 
      y = c(0, stats::predict(m, list(dem_res = res))))
    graphics::lines(
      x = c(0, res), 
      y = c(
        stats::predict(m, list(dem_res = res)), 
        stats::predict(m, list(dem_res = res))
        )
      )
  }

  val <- stats::predict(m, list(dem_res = res))
  as.numeric(val)
}


#' Search for a SAGA-GIS tool
#'
#' @param x saga object
#' @param pattern character, pattern of text to search for within the tool name
#'
#' @return dataframe, tools that match the pattern of the search text and
#' their host library
#' @export
#' @examples
#' \dontrun{
#' # initialize Rsagacmd
#' saga <- saga_gis()
#'
#' # search for a tool
#' search_tools(x = saga, pattern = "terrain")
#' }
search_tools <- function(x, pattern) {

  # get local environment of sagaGIS object (first tool)
  env <- environment(x[[1]][[1]])

  matches <- list()

  for (lib in names(env$senv$libraries)) {
    match_text <- grep(
      pattern, 
      names(env$senv$libraries[[lib]]), 
      ignore.case = TRUE)
    
    if (length(match_text) > 0) {
      matches[[lib]] <- names(env$senv$libraries[[lib]])[match_text]
    }
  }

  matches_df <- data.frame(
    library = rep(names(matches), lapply(matches, length)),
    tool = unlist(matches, use.names = FALSE)
  )
  return(matches_df)
}


#' Split a raster grid into tiles for tile-based processing
#'
#' Split a raster grid into tiles. The tiles are saved as Rsagacmd
#' temporary files, and are loaded as a list of R objects for further
#' processing. This is a function to make the the SAGA-GIS
#' grid_tools / tiling tool more convenient to use.
#'
#' @param x A `saga` object.
#' @param grid A path to a GDAL-supported raster to apply tiling, or a
#'   RasterLayer.
#' @param nx An integer with the number of x-pixels per tile.
#' @param ny An integer with the number of y-pixels per tile.
#' @param overlap An integer with the number of overlapping pixels.
#' @param file_path An optional file file path to store raster tiles.
#'
#' @return A list of RasterLayer objects representing tiled data.
#' @export
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' saga <- saga_gis()
#'
#' # Generate a random DEM
#' dem <- saga$grid_calculus$random_terrain(radius = 15, iterations = 500)
#'
#' # Return tiled version of DEM
#' tiles <- tile_geoprocessor(x = saga, grid = dem, nx = 20, ny = 20)
#' }
tile_geoprocessor <- function(x, grid, nx, ny, overlap = 0, file_path = NULL) {

  # get environment of saga_gis object
  env <- environment(x[[1]][[1]])

  # calculate number of tiles required
  n_widths <- ceiling(1 / (nx / (ncol(grid) + overlap)))
  n_heights <- ceiling(1 / (ny / (nrow(grid) + overlap)))
  n_tiles <- n_widths * n_heights

  # create list of temporary files for tiles
  tile_outputs <- c()

  for (i in seq_len(n_tiles)) {
    if (is.null(file_path)) {
      temp <- tempfile(fileext = ".sgrd")
    } else {
      temp <- tempfile(tmpdir = file_path, fileext = ".sgrd")
    }
    tile_outputs <- c(tile_outputs, temp)
    pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, temp)
  }

  # grid tiling
  x$grid_tools$tiling(
    grid = grid,
    tiles = tile_outputs,
    overlap = 0,
    nx = nx,
    ny = ny
  )
}
