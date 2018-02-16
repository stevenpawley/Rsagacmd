devtools::use_package("minpack.lm")

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
    if (res > max(dem_res))
      predx = seq(min(dem_res), max(res*2, dem_res), 1)
    predy = predict(m, list(dem_res = predx))
    
    # plot
    plot(dem_res, mrvbf_slope, xlab = 'DEM resolution (m)',
         ylab = 'MRVBF Initial Slope', xlim=c(min(dem_res), res*1.1), xaxs="i")
    lines(predx, predy)
    lines(x=c(res,res), y=c(0,predict(m, list(dem_res=res))))
    lines(x=c(0,res), y=c(predict(m, list(dem_res=res)),predict(m, list(dem_res=res))))
    }
  
  predict(m, list(dem_res=res))
}


#' Apply a SAGA-GIS tool on grid data using tile-based geoprocessing
#' 
#' Wraps the appplication of grid tiling with a user supplied SAGA-GIS tool
#' and mosaics the result
#'
#' @param grid RasterLayer. Input raster to be used as a base for tiling
#' @param func Function. Function to be called from within a saga.gis object
#' @param saga.gis Class saga.gis. Class of saga.gis from which to call func
#' @param func_args List. Named list of arguments and values to pass to the
#'   selected SAGA-GIS tool. Use the argument placeholder . to indicate which
#'   argument within func refers to the input grid.
#' @param output_arg Character. Name of the output feature from the SAGA-GIS
#'   tool that is to be mosaicked after tiling. E.g., SLOPE. This is required
#'   because many SAGA-GIS tools produce multiple outputs
#' @param nx Numeric. Horiontal size of tiles in terms of number of cells
#' @param ny Numeric. Vertical size of tiles in terms of number of cells
#' @param overlap Numeric. Number of cells to overlap the tiles
#' @param overlap_fun Function. E.g. mean, min or max. Must be a function that
#'   accepts a 'na.rm' argument
#' @param intern Logical. Boolean to load the results as an R object. Default is
#'   TRUE
#' @param cores Numeric. Number of physical processing cores to use while
#'   running the selected SAGA-GIS tool, if supported. Default is to use all
#'   cores.
#'
#' @return Results of the selected SAGA-GIS tool
#' @export
#'
#' @examples
#' \dontrun{
#' # initialize a saga.gis class
#' saga = initSAGA()
#' 
#' # generate a random terrain
#' dem = saga$grid_calculus$Random_Terrain(
#'     TARGET_USER_XMIN = 0,
#'     TARGET_USER_XMAX = 1000,
#'     TARGET_USER_YMIN = 0,
#'     TARGET_USER_YMAX = 1000)
#' 
#' # calculate slope using grid tiling
#' slope = tileGeoprocessor(
#'     grid = dem_clipped,
#'     func = saga$ta_morphometry$Slope_Aspect_Curvature,
#'     saga.gis = saga,
#'     func_args = list(ELEVATION = .),
#'     output_arg = 'SLOPE',
#'     nx = 500, ny = 500, overlap = 50,
#'     overlap_fun = mean
#'     )
#' }
tileGeoprocessor = function(
  grid, func, saga.gis, func_args, output_arg, nx, ny, overlap, overlap_fun,
  intern=TRUE, cores=NULL){
  
  func = as.list(substitute(func))
  lib = strsplit(deparse(func[[2]]), '\\$')[[1]][2]
  tool = deparse(func[[3]])
  
  # calculate number of tiles required
  n_widths = ceiling(1/(nx/(ncol(grid)+overlap)))
  n_heights = ceiling(1/(ny/(nrow(grid)+overlap)))
  n_tiles = n_widths * n_heights
  
  # create list of temporary files for tiles
  tile_outputs = c()
  for (i in 1:n_tiles){
    temp = tempfile(fileext = '.sgrd')
    tile_outputs = c(tile_outputs, temp)
    #pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
  }
  
  # grid tiling
  tiles = saga.gis$grid_tools$Tiling(
    GRID = grid, TILES = tile_outputs, OVERLAP = overlap, NX = nx, NY = ny)
  
  # capture func_args as expression and strip unnamed elements
  func_args = as.list(substitute(func_args))
  func_args = func_args[names(func_args) != '']
  
  # replace (.) in func_args with input tile and run geoprocessing function
  outputs = lapply(tiles, function(input_tile){
    args = func_args
    for (i in seq_along(args)){
      args[[identical(args[[i]], quote(.))]] = input_tile
    }
    return (
      sagaGeo(lib, tool, saga.gis$.env, intern, cores, args)[[output_arg]]
      )
  })
  
  # mosaic outputs
  rasters = lapply(1:length(outputs), function(x) outputs[[x]])
  rasters$fun = overlap_fun
  mosaicked = do.call(raster::mosaic, rasters)

  # crop to data area
  cropped = raster::crop(mosaicked, grid)
  
  return (cropped)
}
