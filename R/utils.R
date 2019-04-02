#' Calculate the T_SLOPE value based on DEM resolution for MRVBF
#'
#' Calculates the T_SLOPE value for the Multiresolution Index of Valley Bottom 
#' Flatness (Gallant and Dowling, 2003) based on input DEM resolution. MRVBF
#' identified valley bottoms based on classifying slope angle and identifying
#' low areas by ranking elevation in respect to the surrounding topography
#' across a range of DEM resolutions. The MRVBF algorithm was developed using a
#' 25 m DEM, and so if the input DEM has a different resolution then the slope
#' threshold T_SLOPE needs to be adjusted from its default value of 16 in order
#' to maintain the relationship between slope and DEM resolution. This function
#' provides a convenient way to perform that calculation.
#'
#' @param res Input DEM resolution
#' @param plot logical, produce plot of relationship
#'
#' @return Numeric. T_SLOPE value for MRVBF
#' @export
#' 
#' @examples
#' \dontrun{
#' MRVBFthreshold(res = 10, plot = TRUE)
#' }
MRVBFthreshold = function(res, plot=FALSE){
  # slope decreases by factor of 2 per every 'step' above a 25 m dem resolution
  # a step consists of a 3 factor increase in the dem cell size
  # Gallant and Dowling 2003
  dem_res = c(6075, 2025, 675, 225, 75, 25, 8, 3, 1)
  mrvbf_slope = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128)
  ds = data.frame(dem_res, mrvbf_slope)
  m = minpack.lm::nlsLM(mrvbf_slope ~ a*I(dem_res^z), data = ds, start = list(a=100, z=1))
  
  if (plot == TRUE){
    # produce nls smooth line
    predx = seq(min(res, dem_res/2), max(res*2, dem_res), 0.1)
    predy = stats::predict(m, list(dem_res = predx))
    
    # plot
    graphics::plot(dem_res, mrvbf_slope, xlab = 'DEM resolution (m)',
         ylab = 'MRVBF Initial Slope', xlim=c(min(predx), max(min(dem_res), res)*1.5), xaxs="i")
    graphics::lines(predx, predy)
    graphics::lines(x=c(res,res), y=c(0, stats::predict(m, list(dem_res=res))))
    graphics::lines(x=c(0,res), y=c(stats::predict(m, list(dem_res=res)), stats::predict(m, list(dem_res=res))))
    }
  
  stats::predict(m, list(dem_res=res))
}


#' Search for a SAGA-GIS tool
#'
#' @param x saga object
#' @param pattern character. Pattern of text to search for within the tool name
#'
#' @return dataframe of tools that match the pattern of the search text and
#' their host library
#' @export
#' @examples 
#' \dontrun{
#' # initialize Rsagacmd
#' saga = sagaGIS()
#' 
#' # search for a tool
#' searchTools(x = saga, pattern = 'Terrain')
#' }
searchTools = function(x, pattern) {
  
  # get local environment of sagaGIS object (first tool)
  env = environment(x[[1]][[1]])
  
  matches = list()
  
  for (lib in names(env$tool_libraries)) {
    match_text = grep(pattern, names(env$tool_libraries[[lib]]), ignore.case = T)
    if (length(match_text) > 0)
      matches[[lib]] = names(env$tool_libraries[[lib]])[match_text]
  }
  
  matches_df = data.frame(
    library = rep(names(matches), lapply(matches, length)),
    tool = unlist(matches, use.names = F))
  return (matches_df)
}


#' Split a raster grid into tiles for tile-based processing
#' 
#' Split a raster grid into tiles. The tiles are saved as Rsagacmd
#' temporary files, and are loaded as a list of R objects for futher
#' processing. This is a function to make the the SAGA-GIS 
#' grid_tools / tiling tool more convenient to use.
#'
#' @param x saga S3 object 
#' @param grid character, or RasterLayer. GDAL-supported raster, as as a path to
#'   the raster or representing a RasterLayer object
#' @param nx numeric. Number of x-pixels per tile
#' @param ny numeric. Number of y-pixels per tile
#' @param overlap numeric. Number of overlapping pixels
#' @param file_path character. Optional file path to store raster tiles
#'
#' @return list. List of RasterLayer objects representing tiled data
#' @export
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' saga = sagaGIS()
#' 
#' # Generate a random DEM
#' dem = saga$grid_calculus$Random_Terrain(RADIUS=15, ITERATIONS=500)
#' 
#' # Return tiled version of DEM
#' tiles = tileGeoprocessor(x = saga, grid = dem, nx = 20, ny = 20)
#' }
tileGeoprocessor = function(x, grid, nx, ny, overlap=0, file_path=NULL) {
  
  # get local environment of sagaGIS object (first tool)
  env = environment(x[[1]][[1]])
  
  # calculate number of tiles required
  n_widths = ceiling(1 / (nx/ (ncol(grid) + overlap)))
  n_heights = ceiling(1 / (ny/ (nrow(grid) + overlap)))
  n_tiles = n_widths * n_heights
  
  # create list of temporary files for tiles
  tile_outputs = c()
  for (i in 1:n_tiles) {
    if (is.null(file_path))
      temp = tempfile(fileext = '.sgrd') else
      temp = tempfile(tmpdir = file_path, fileext = '.sgrd')
    tile_outputs = c(tile_outputs, temp)
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
  }
  
  # grid tiling
  tiles = sagaExecute(
    lib = 'grid_tools', tool = 'Tiling', senv = env$senv, intern = TRUE,
    list(GRID = grid, TILES = tile_outputs, OVERLAP = overlap,
         NX = nx, NY = ny))
}
