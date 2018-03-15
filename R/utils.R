devtools::use_package("minpack.lm")
devtools::use_package("SpaDES.tools")

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
#' @param func_args List. Named list of arguments and values to pass to the
#'   selected SAGA-GIS tool. Use the argument placeholder . to indicate which
#'   argument within func refers to the input grid.
#' @param output_arg Character. Name of the output feature from the SAGA-GIS
#'   tool that is to be mosaicked after tiling. E.g., SLOPE. This is required
#'   because many SAGA-GIS tools produce multiple outputs
#' @param nx Numeric. Number of horizontal tiles
#' @param ny Numeric. Number of vertical tiles
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
#'     TARGET_USER_YMAX = 1000,
#'     RADIUS = 200)
#' 
#' # calculate slope using grid tiling
#' slope = tileGeoprocessor(
#'     grid = dem,
#'     func = saga$ta_morphometry$Slope_Aspect_Curvature,
#'     func_args = list(ELEVATION = .),
#'     output_arg = 'SLOPE',
#'     nx = 3, ny = 3, overlap = 50,
#'     overlap_fun = mean
#'     )
#' }
tileGeoprocessor = function(
  grid, func, func_args, output_arg, nx, ny, overlap, overlap_fun = mean,
  intern=TRUE, cores=NULL){
  
  # some checks
  if (length(output_arg) > 1)
    stop('Only a single output from a SAGA-GIS tool is currently supported')
  
  # tiling
  tiles = SpaDES.tools::splitRaster(
    r=grid, nx=nx, ny=ny, buffer=overlap, path=tempdir())
  
  # get library and tool name from function attributes
  lib = attr(func, 'lib')
  tool = attr(func, 'tool')
  saga_cmd = attr(func, 'saga_cmd')
  tool_cmd = attr(func, 'tool_cmd')
  tool_params = attr(func, 'tool_params')
  
  # capture func_args as expression and strip unnamed elements
  func_args = as.list(substitute(func_args))
  func_args = func_args[names(func_args) != '']

  # replace (.) in func_args with input tile and run geoprocessing function
  outputs = lapply(tiles, function(input_tile){
    args = func_args
    for (i in seq_along(args))
      args[[identical(args[[i]], quote(.))]] = input_tile
    return (sagaGeo(lib = lib, tool = tool, saga_cmd = saga_cmd,
        tool_cmd = tool_cmd, tool_params = tool_params, intern, cores,
        args)[[output_arg]])
  })
  
  # mosaic outputs
  if (all(lapply(outputs, class) == 'RasterLayer')){
    rasters = lapply(1:length(outputs), function(x) outputs[[x]])
    rasters$fun = overlap_fun
    mosaicked = do.call(raster::mosaic, rasters)
    
    # crop to data area
    cropped = raster::crop(mosaicked, grid)
  } else if (all(lapply(outputs, function(x) is(x, 'sf')))){
    stop('Mosaicking other datatypes not yet implemented')
  }

  return (cropped)
}

#' Removes temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any required outputs for a SAGA-GIS tool are not specified as arguments.
#' Temporary files in R are automatically removed at the end of each session.
#' However, when dealing with raster data, these temporary files potentially can
#' consume large amounts of disk space. These temporary files can be observed
#' during a session by using the sagaShowTmpFiles function, and can be removed
#' using the sagaRemoveTmpFiles function. Note that this function also removes
#' any accompanying files, i.e. the '.prj' and '.shx' files that may be written
#' as part of writing a ESRI Shapefile '.shp' format.
#'
#' @param h Remove temporary files that are older than h (in number of hours)
#'
#' @return Nothing is returned
#' @export
#'
#' @examples
#' \dontrun{
#' # remove all temporary files generated by Rsagacmd
#' sagaRemoveTmpFiles(h=0)
#' }
sagaRemoveTmpFiles = function(h=0){
  for (f in pkg.env$sagaTmpFiles){
    if (file.exists(f) == TRUE){
      tdelay = difftime(Sys.time(), file.mtime(f), units='hours')
      if (tdelay > h){
        message(f)
        assoc_files = list.files(
          path = dirname(f),
          pattern = glob2rx(paste0(tools::file_path_sans_ext(basename(f)), '.*')),
          full.names = T)
        file.remove(assoc_files)
        pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
      }
    } else {
      pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
    }
  }
  return (NULL)
}

#' List temporary files created by Rsagacmd
#' 
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any required outputs for a SAGA-GIS tool are not specified as arguments.
#' Temporary files in R are automatically removed at the end of each session.
#' However, when dealing with raster data, these temporary files potentially can
#' consume large amounts of disk space. These temporary files can be observed
#' during a session by using the sagaShowTmpFiles function, and can be removed
#' using the sagaRemoveTmpFiles function.
#'
#' @return returns the file names of the files in the temp directory that have
#' been generated by Rsagacmd. Note this list of files only includes the primary
#' file extension, i.e. '.shp' for a shapefile without the accessory files
#' (e.g. .prj, .shx etc.).
#' @export
sagaShowTmpFiles = function(){
  message('Rsagacmd temporary files:')
  for (f in pkg.env$sagaTmpFiles){
    message(f)
  }
  return(pkg.env$sagaTmpFiles)
}


# local environment to store vector of tempfiles in package namespace
pkg.env = new.env(parent = emptyenv())
pkg.env$sagaTmpFiles = c()
