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
#' @return Specified outputs
#' @export

saga.fill_sinks = function(DEM,
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
  library = 'ta_preprocessor'
  tool = 'Sink_Removal'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[library]][[tool]][['options']],
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
  saga_results = sagaGeo(library, tool, senv, intern, args)
  
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
#' @return Specified outputs
#' @export
saga.flow_accumulation = function(ELEVATION,
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
  func_call = sys.call()
  lib = 'ta_hydrology'
  tool = 'Flow_Accumulation_Top_Down'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[library]][[tool]][['options']],
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
  saga_results = sagaGeo(library, tool, senv, intern, args)
  
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
#' @param intern Optionally load output as an R object; Default is TRUE
#' @param usage Boolean, display tool help
#'
#' @return Specified outputs
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
  func_call = sys.call()
  lib = 'ta_hydrology'
  tool = 'Topographic_Wetness_Index_TWI'
  
  # optionally display help for selected tool
  if (usage == TRUE) {
    print(subset(
      senv$libraries[[library]][[tool]][['options']],
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
  saga_results = sagaGeo(library, tool, senv, intern, args)
  
  return(saga_results)
}
