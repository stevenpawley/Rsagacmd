#' Search for a SAGA-GIS tool
#'
#' @param x saga object
#' @param pattern character, pattern of text to search for within the tool name
#'
#' @return a tibble containing the libraries, names and parameters of the tools
#'   that match the pattern of the search text and their host library
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

  # get local environment of saga object
  libraries <- environment(x[[1]][[1]])$senv$libraries

  matches <- tibble::tibble(
    library = character(),
    tool = character(),
    author = character(),
    parameters = list(),
    description = character(),
    .rows = 0
  )

  for (lib in names(libraries)) {
    match_text <- grep(
      pattern,
      names(libraries[[lib]]),
      ignore.case = TRUE
    )

    if (length(match_text) > 0) {
      for (idx in match_text) {
        result <- list(
          library = lib,
          tool = names(libraries[[lib]])[idx],
          author = libraries[[lib]][[idx]]$author,
          saga_cmd = libraries[[lib]][[idx]]$tool_cmd,
          parameters = list(names(libraries[[lib]][[idx]]$params)),
          description = libraries[[lib]][[idx]]$description
        )

        matches <- rbind(matches, tibble::as_tibble(result))
      }
    }
  }

  matches
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
#' @param file_path An optional file file path to store the raster tiles.
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

  if (is.null(file_path)) {
    include_as_tempfiles <- TRUE
    file_path <-
      file.path(tempdir(), paste0("tiles", floor(stats::runif(1, 0, 1e6))))

    if (!dir.exists(file_path))
      dir.create(file_path)

  } else {
    include_as_tempfiles <- FALSE
  }

  x$grid_tools$tiling(
    grid = grid,
    overlap = overlap,
    nx = nx,
    ny = ny,
    tiles_path = file_path,
    tiles_save = TRUE,
    .all_outputs = FALSE,
    .intern = FALSE
  )

  tile_sdats <- list.files(file_path, pattern = "*.sdat$", full.names = TRUE)

  if (include_as_tempfiles)
    pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, tile_sdats)

  senv <- environment(x[[1]][[1]])$senv

  if (senv$backend == "raster")
    tiles <- sapply(tile_sdats, raster::raster)

  if (senv$backend == "terra")
    tiles <- sapply(tile_sdats, terra::rast)

  if (senv$backend == "stars")
    tiles <- sapply(tile_sdats, stars::read_stars)

  tiles
}
