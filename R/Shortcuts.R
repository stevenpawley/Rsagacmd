#' Sink Removal
#' 
#' Fill/remove sinks within a digital elevation model
#'
#' @param DEM DEM: Grid (input). Digital Elevation Model that has to be
#'   processed
#' @param SINKROUTE Sink Route: Grid (optional input).
#' @param DEM_PREPROC Preprocessed DEM: Grid (optional output).
#' @param METHOD Method: Choice. Available Choices:
#' \itemize{
#' \item [0] Deepen Drainage Routes
#' \item [1] Fill Sinks
#' \item Default: 1
#' }
#' @param THRESHOLD Threshold: Boolean. Default: 0
#' @param THRSHEIGHT Threshold Height: Floating point. The parameter describes
#'   the maximum depth of a sink to be considered for removal [map units]. This
#'   makes it possible to exclude deeper sinks from filling.
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export

saga.Fill_Sinks = function(DEM,
                           SINKROUTE = NA,
                           DEM_PREPROC = NA,
                           METHOD = NA,
                           THRESHOLD = NA,
                           THRSHEIGHT = NA,
                           senv,
                           intern = TRUE,
                           usage = FALSE) {
  
  # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'ta_preprocessor'
  tool = 'Sink_Removal'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return (saga_results)
}


#' Flow Accumulation (Top-Down)
#' 
#' Top-down processing of cells for calculation of flow accumulation and related
#' parameters. This set of algorithms processes a DEM downwards from the highest
#' to the lowest cell. Flow routing methods provided by this tool:
#' \itemize{
#' \item Deterministic 8 (aka D8, O'Callaghan & Mark 1984)
#' \item Braunschweiger Reliefmodell (Bauer et al. 1985)
#' \item Rho 8 (Fairfield & Leymarie 1991)
#' \item Multiple Flow Direction (Freeman 1991, Quinn et al. 1991)
#' \item Deterministic Infinity (Tarboton 1997)
#' \item Triangular Multiple Flow Direction (Seibert & McGlynn 2007
#' \item Multiple Flow Direction based on Maximum Downslope Gradient (Qin et al. 2011)
#' }
#'
#' @param ELEVATION Elevation: Grid (input)
#' @param SINKROUTE Sink Routes: Grid (optional input)
#' @param WEIGHTS Weights: Grid (optional input)
#' @param VAL_INPUT Input for Mean over Catchment: Grid (optional input)
#' @param ACCU_MATERIAL Material for Accumulation: Grid (optional input)
#' @param ACCU_TARGET Accumulation Target: Grid (input)
#' @param LINEAR_VAL Linear Flow Threshold Grid: Grid (optional input). Optional
#'   grid providing values to be compared with linear flow threshold instead of
#'   flow accumulation
#' @param LINEAR_DIR Channel Direction: Grid (optional input). Use this for
#'   (linear) flow routing, if the value is a valid direction (0-7 = N, NE, E,
#'   SE, S, SW, W, NW)
#' @param FLOW Flow Accumulation: Grid (output)
#' @param VAL_MEAN Mean over Catchment: Grid (output)
#' @param ACCU_TOTAL Accumulated Material: Grid (optional output)
#' @param ACCU_LEFT Accumulated Material (Left Side): Grid (optional output)
#' @param ACCU_RIGHT Accumulated Material (Right Side): Grid (optional output)
#' @param FLOW_LENGTH Flow Path Length: Grid (optional output). Average distance
#'   that a cell's accumulated flow travelled
#' @param WEIGHT_LOSS Loss through Negative Weights: Grid (optional output).
#'   When using weights without support for negative flow: output of the
#'   absolute amount of negative flow that occurred
#' @param STEP Step: Integer. For testing purposes. Only generate flow at cells
#'   with step distance (each step row/column). Minimum: 1; Default: 1
#' @param FLOW_UNIT Flow Accumulation Unit: Choice. Available Choices:
#' \itemize{
#' \item [0] number of cells
#' \item [1] cell area
#' \item Default: 1
#' }
#' @param METHOD Method: Choice. Available Choices:
#' \itemize{
#' \item [0] Deterministic 8
#' \item [1] Rho 8
#' \item [2] Braunschweiger Reliefmodell
#' \item [3] Deterministic Infinity
#' \item [4] Multiple Flow Direction
#' \item [5] Multiple Triangular Flow Directon
#' \item [6] Multiple Maximum Downslope Gradient Based Flow Directon
#' \item Default: 4
#' }
#' @param LINEAR_DO Thresholded Linear Flow: Boolean. Apply linear flow routing
#'   (D8) to all cells, having a flow accumulation greater than the specified
#'   threshold. Default: 1
#' @param LINEAR_MIN Linear Flow Threshold: Integer. Flow accumulation threshold
#'   (cells) for linear flow routing. Minimum: 0. Default: 500
#' @param CONVERGENCE Convergence: Floating point. Convergence factor for
#'   Multiple Flow Direction Algorithm (Freeman 1991). Applies also to the
#'   Multiple Triangular Flow Directon Algorithm. Minimum: 0.000000. Default:
#'   1.100000
#' @param NO_NEGATIVES Prevent Negative Flow Accumulation: Boolean. when using
#'   weights: do not transport negative flow, set it to zero instead; useful
#'   e.g. when accumulating measures of water balance. Default: 1
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export
saga.Flow_Accumulation = function(ELEVATION,
                                  SINKROUTE = NA,
                                  WEIGHTS = NA,
                                  VAL_INPUT = NA,
                                  ACCU_MATERIAL = NA,
                                  ACCU_TARGET,
                                  LINEAR_VAL = NA,
                                  LINEAR_DIR = NA,
                                  FLOW,
                                  VAL_MEAN,
                                  ACCU_TOTAL = NA,
                                  ACCU_LEFT = NA,
                                  ACCU_RIGHT = NA,
                                  FLOW_LENGTH = NA,
                                  WEIGHT_LOSS = NA,
                                  STEP = NA,
                                  FLOW_UNIT = NA,
                                  METHOD = NA,
                                  LINEAR_DO = NA,
                                  LINEAR_MIN = NA,
                                  CONVERGENCE = NA,
                                  NO_NEGATIVES = NA,
                                  senv,
                                  intern = TRUE,
                                  usage = FALSE) {
  
  # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'ta_hydrology'
  tool = 'Flow_Accumulation_Top_Down'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return (saga_results)
}


#' Topographic Wetness Index (TWI)
#' 
#' Calculation of the slope and specific catchment area (SCA) based Topographic
#' Wetness Index (TWI).
#'
#' @param SLOPE Slope: Grid (input)
#' @param AREA Catchment Area: Grid (input)
#' @param TRANS Transmissivity: Grid (optional input)
#' @param TWI Topographic Wetness Index: Grid (output)
#' @param CONV Area Conversion: Choice. Available Choices:
#' \itemize{
#' \item [0] no conversion (areas already given as specific catchment area)
#' \item [1] 1 / cell size (pseudo specific catchment area)
#' Default: 0
#' }
#' @param METHOD Method (TWI): Choice. Available Choices:
#' \itemize{
#' \item [0] Standard
#' \item [1] TOPMODEL
#' }
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export
saga.Topographic_Wetness_Index = function(SLOPE,
                                          AREA,
                                          TRANS = NA,
                                          TWI,
                                          CONV = NA,
                                          METHOD = NA,
                                          senv,
                                          intern = TRUE,
                                          usage = FALSE) {
  # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'ta_hydrology'
  tool = 'Topographic_Wetness_Index_TWI'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  message(args)
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return(saga_results)
}


#' Slope, Aspect, Curvature
#' 
#' Calculates the local morphometric terrain parameters slope, aspect and if
#' supported by the chosen method also the curvature. Besides tangential
#' curvature also its horizontal and vertical components (i.e. plan and profile
#' curvature) can be calculated.
#'
#' @param ELEVATION Elevation: Grid (input)
#' @param SLOPE Slope: Grid (output)
#' @param ASPECT Aspect: Grid (output)
#' @param C_GENE General Curvature: Grid (optional output)
#' @param C_PROF Profile Curvature: Grid (optional output)
#' @param C_PLAN Plan Curvature: Grid (optional output)
#' @param C_TANG Tangential Curvature: Grid (optional output)
#' @param C_LONG Longitudinal Curvature: Grid (optional output)
#' @param C_CROS Cross-Sectional Curvature: Grid (optional output)
#' @param C_MINI Minimal Curvature: Grid (optional output)
#' @param C_MAXI Maximal Curvature: Grid (optional output)
#' @param C_TOTA Total Curvature: Grid (optional output)
#' @param C_ROTO Flow Line Curvature: Grid (optional output)
#' @param METHOD Method: Choice. Available choices:
#' \itemize{
#' \item [0] maximum slope (Travis et al. 1975)
#' \item [1] maximum triangle slope (Tarboton 1997)
#' \item [2] least squares fitted plane (Horn 1981, Costa-Cabral & Burgess 1996)
#' \item [3] 6 parameter 2nd order polynom (Evans 1979)
#' \item [4] 6 parameter 2nd order polynom (Heerdegen & Beran 1982)
#' \item [5] 6 parameter 2nd order polynom (Bauer, Rohdenburg, Bork 1985)
#' \item [6] 9 parameter 2nd order polynom (Zevenbergen & Thorne 1987)
#' \item [7] 10 parameter 3rd order polynom (Haralick 1983)
#' \item Default: 6
#' }
#' @param UNIT_SLOPE Slope units: Choice. Available choices:
#' \itemize{
#' \item [0] radians
#' \item [1] degree
#' \item [2] percent
#' \item Default: 0
#' }
#' @param UNIT_ASPECT Aspect Units: Choice. Available choices:
#' \itemize{
#' \item [0] radians
#' \item [1] degree
#' \item Default: 0
#' }
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export
saga.Slope_Aspect_Curvature = function(ELEVATION,
                                       SLOPE,
                                       ASPECT,
                                       C_GENE = NA,
                                       C_PROF = NA,
                                       C_PLAN = NA,
                                       C_TANG = NA,
                                       C_LONG = NA,
                                       C_CROS = NA,
                                       C_MINI = NA,
                                       C_MAXI = NA,
                                       C_TOTA = NA,
                                       C_ROTO = NA,
                                       METHOD = NA,
                                       UNIT_SLOPE = NA,
                                       UNIT_ASPECT = NA,
                                       senv,
                                       intern = TRUE,
                                       usage = FALSE) {
  # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'ta_morphometry'
  tool = 'Slope_Aspect_Curvature'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return(saga_results)
}


#' Resampling
#' 
#' Resampling of grids.
#'
#' @param INPUT Grids: Grid list (input)
#' @param TARGET_TEMPLATE Target System: Grid (optional input)
#' @param OUTPUT Resampled Grids: Grid list (optional output)
#' @param KEEP_TYPE Preserve Data Type: Boolean. Default: 0
#' @param SCALE_UP Upscaling Method: Choice. Availabe Choices:
#' \itemize{
#' \item [0] Nearest Neighbour
#' \item [1] Bilinear Interpolation
#' \item [2] Bicubic Spline Interpolation
#' \item [3] B-Spline Interpolation
#' \item [4] Mean Value
#' \item [5] Mean Value (cell area weighted)
#' \item [6] Minimum Value
#' \item [7] Maximum Value
#' \item [8] Majority
#' \item Default: 5
#' }
#' @param SCALE_DOWN Downscaling Method: Choice. Available Choices:
#' \itemize{
#' \item [0] Nearest Neighbour
#' \item [1] Bilinear Interpolation
#' \item [2] Bicubic Spline Interpolation
#' \item [3] B-Spline Interpolation
#' \item Default: 3
#' }
#' @param TARGET_DEFINITION Target Grid System: Choice. Available Choices:
#' \itemize{
#' \item [0] user defined
#' \item [1] grid or grid system
#' \item Default: 0
#' }
#' @param TARGET_USER_SIZE Cellsize: Floating point. Minimum: 0.000000; Default: 1.000000
#' @param TARGET_USER_XMIN Left: Floating point. Default: 0.000000
#' @param TARGET_USER_XMAX Right: Floating point. Default: 100.000000
#' @param TARGET_USER_YMIN Bottom: Floating point. Default: 0.000000
#' @param TARGET_USER_YMAX Top: Floating point. Default: 100.000000
#' @param TARGET_USER_FITS Fit: Choice. Available Choices:
#' \itemize{
#' \item [0] nodes
#' \item [1] cells
#' \item Default: 0
#' }
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export
saga.Resampling = function(INPUT,
                           TARGET_TEMPLATE = NA,
                           OUTPUT = NA,
                           KEEP_TYPE = NA,
                           SCALE_UP = NA,
                           SCALE_DOWN = NA,
                           TARGET_DEFINITION = NA,
                           TARGET_USER_SIZE = NA,
                           TARGET_USER_XMIN = NA,
                           TARGET_USER_XMAX = NA,
                           TARGET_USER_YMIN = NA,
                           TARGET_USER_YMAX = NA,
                           TARGET_USER_FITS = NA,
                           senv,
                           intern = TRUE,
                           usage = FALSE) {

  # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'grid_tools'
  tool = 'Resampling'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return(saga_results)
}


#' Mosaicking
#' 
#' Merges multiple grids into one single grid.
#'
#' @param GRIDS Grids: Grid list (input)
#' @param TARGET_TEMPLATE Target System: Grid (optional input)
#' @param TARGET_OUT_GRID Target Grid: Grid (output)
#' @param NAME Name: Text. Default: Mosaic
#' @param TYPE Data Storage Type: Choice. Available Choices:
#' \itemize{
#' \item [0] 1 bit
#' \item [1] 1 byte unsigned integer
#' \item [2] 1 byte signed integer
#' \item [3] 2 byte unsigned integer
#' \item [4] 2 byte signed integer
#' \item [5] 4 byte unsigned integer
#' \item [6] 4 byte signed integer
#' \item [7] 4 byte floating point
#' \item [8] 8 byte floating point
#' \item [9] same as first grid in list
#' \item Default: 9
#' }
#' @param RESAMPLING Resampling: Choice. Available Choices:
#' \itemize{
#' \item [0] Nearest Neighbour
#' \item [1] Bilinear Interpolation
#' \item [2] Bicubic Spline Interpolation
#' \item [3] B-Spline Interpolation
#' \item Default: 3
#' }
#' @param OVERLAP Overlapping Areas: Choice. Available Choices:
#' \itemize{
#' \item [0] first
#' \item [1] last
#' \item [2] minimum
#' \item [3] maximum
#' \item [4] mean
#' \item [5] blend boundary
#' \item [6] feathering
#' }
#' @param BLEND_DIST Blending Distance: Floating point. Minimum: 0.000000; Default: 10.000000
#' @param MATCH Match: Choice. Available Choices:
#' \itemize{
#' \item [0] none
#' \item [1] match histogram of first grid in list
#' \item [2] match histogram of overlapping area
#' \item [3] regression
#' \item Default: 0
#' }
#' @param TARGET_DEFINITION Target Grid System: Choice. Available Choices:
#' \itemize{
#' \item [0] user defined
#' \item [1] grid or grid system
#' \item Default: 0
#' }
#' @param TARGET_USER_SIZE Cellsize: Floating point. Minimum: 0.000000; Default: 1.000000
#' @param TARGET_USER_XMIN Left: Floating point. Default: 0.000000
#' @param TARGET_USER_XMAX Right: Floating point. Default: 100.000000
#' @param TARGET_USER_YMIN Bottom: Floating point. Default: 0.000000
#' @param TARGET_USER_YMAX Top: Floating point. Default: 100.000000
#' @param TARGET_USER_FITS Fit: Choice. Available Choices:
#' \itemize{
#' \item [0] nodes
#' \item [1] cells
#' \item Default: 0
#' }
#' @param senv SAGA-GIS environment returned by initSAGA
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified SAGA-GIS outputs
#' @export
saga.Mosaicking = function(GRIDS,
                           TARGET_TEMPLATE = NA,
                           TARGET_OUT_GRID,
                           NAME = NA,
                           TYPE = NA,
                           RESAMPLING = NA,
                           OVERLAP = NA,
                           BLEND_DIST = NA,
                           MATCH = NA,
                           TARGET_DEFINITION = NA,
                           TARGET_USER_SIZE = NA,
                           TARGET_USER_XMIN = NA,
                           TARGET_USER_XMAX = NA,
                           TARGET_USER_YMIN = NA,
                           TARGET_USER_YMAX = NA,
                           TARGET_USER_FITS = NA,
                           senv,
                           intern = TRUE,
                           usage = FALSE) {

    # get names of function and arguments
  senv = senv$.env
  func_call = sys.call()
  lib = 'grid_tools'
  tool = 'Resampling'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[lib]][[tool]][['options']],
      select = c(validRIdentifier, Name, Type, Description, Constraints)
    ))
    return()
  }
  
  # get argument names and values
  args = as.list(func_call)[2:length(func_call)]
  
  # remove intern and help from saga args list
  if ('intern' %in% names(args))
    args = args[-which(names(args) == 'intern')]
  if ('usage' %in% names(args))
    args = args[-which(names(args) == 'usage')]
  args = args[-which(names(args) == 'senv')]
  
  # evaluate any arg_vals
  for (i in seq_along(args))
    args[[i]] = eval.parent(args[[i]])
  
  # call the saga geoprocessor
  saga_results = sagaGeo(lib, tool, senv, intern, args)
  
  return(saga_results)
}


#' Split a RasterStack or StackBrick into multiple bands
#'
#' @param x RasterStack or RasterBrick
#'
#' @return list of RasterLayers
#' @export
saga.SplitLayers = function(x){
  layers = lapply(seq_along(raster::nlayers(x)), function(i) raster::raster(x[i]))
  return(layers)
}

# For testing
# library(raster)
# saga = initSAGA()
# dem = saga$grid_calculus$Random_Terrain(TARGET_OUT_GRID = tempfile(fileext='.sgrd'))
# plot(dem)
# filled = saga.fill_sinks(DEM = dem, DEM_PREPROC = tempfile(fileext='.sgrd'), senv = saga)
# plot(filled)
# slope = saga.Slope_Aspect_Curvature(ELEVATION = dem, SLOPE = tempfile(fileext = '.sgrd'), ASPECT = tempfile(fileext='.sgrd'), senv=saga)
# plot(slope[[1]])
# flowacc = saga.flow_accumulation(ELEVATION = filled, FLOW = tempfile(fileext = '.sgrd'), senv=saga)
# plot(calc(x = flowacc, fun = log))
# twi = saga.Topographic_Wetness_Index(SLOPE = slope[[1]], AREA = flowacc, TWI = tempfile(fileext='.sgrd'), senv=saga)
# plot(twi)
# 
# lib='ta_morphometry'
# tool='Slope_Aspect_Curvature'
# args = list(ELEVATION = dem, SLOPE = tempfile(fileext = '.sgrd'), ASPECT = tempfile(fileext='.sgrd'))
# senv=saga$.env
# for (i in seq_along(args))
#   args[[i]] = eval.parent(args[[i]])
