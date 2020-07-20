#' Function to execute SAGA-GIS commands through the command line tool
#'
#' Intended to be used internally by each function
#'
#' @param lib A character specifying the name of SAGA-GIS library to execute.
#' @param tool A character specifying the name of SAGA-GIS tool to execute.
#' @param senv A saga environment object.
#' @param .intern A logical specifying whether to load the outputs from the
#'   SAGA-GIS geoprocessing operation as an R object.
#' @param .all_outputs A logical to specify whether to automatically output all
#'   results from the selected SAGA tool and load them results as R objects
#'   (default = TRUE). If .all_outputs = FALSE then the file paths to store the
#'   tool's results will have to be manually specified in the arguments.
#' @param ... Named arguments and values for SAGA tool.
#'
#' @return output of SAGA-GIS tool loaded as an R object.
saga_execute <-
  function(lib,
           tool,
           senv,
           .intern = TRUE,
           .all_outputs = TRUE,
           ...) {
  
  args <- c(...)
  
  # get tool and saga settings
  tools_in_library <- senv$libraries[[lib]]
  selected_tool <- tools_in_library[[tool]]
  tool_options <- selected_tool$options
  tool_cmd <- selected_tool$tool_cmd
  saga_cmd <- senv$saga_cmd
  saga_config <- senv$saga_config
  temp_path <- senv$temp_path
  raster_lib <- senv$raster_lib
  
  # match the syntactically-correct arg_name to the identifier used by saga_cmd
  arg_names <- names(args)
  identifiers_r <- sapply(tool_options, function(x)
    x$identifier)
  arg_names <- identifiers_r[intersect(arg_names, names(identifiers_r))]
  args <- setNames(args, arg_names)
  tool_options <- setNames(tool_options, identifiers_r) # rename to identifiers
  
  # strip other missing arguments and update arg_names
  args <- args[sapply(args, function(x) !is.null(x))]
  arg_names <- names(args)
  
  # save in-memory R objects to files for SAGA-GIS to access
  args <- lapply(args, save_object, temp_path = temp_path)
  args <- lapply(args, paste, collapse = ";")
  args <- lapply(args, function(x) gsub(".sdat", ".sgrd", x))
  
  # merge output args with tool options
  for (n in names(tool_options)) {
    tool_options[[n]]$args <- NA
    
    if (tool_options[[n]]$identifier %in% names(args)) {
      tool_options[[n]]$args <- args[[n]] 
      
    # use temporary files for other outputs if .all_outputs   
    } else if (isTRUE(.all_outputs) & !is.na(tool_options[[n]]$io)) {
      
      if (tool_options[[n]]$io == "Output") {
        if (tool_options[[n]]$feature %in% c("Grid", "Grid list", "Raster")) {
          tool_options[[n]]$args <- tempfile(tmpdir = temp_path, fileext = ".sgrd") 
          
        } else if (tool_options[[n]]$feature %in% c("Shape", "Shapes list")) {
          tool_options[[n]]$args <- tempfile(tmpdir = temp_path, fileext = ".shp") 
          
        } else if (tool_options[[n]]$feature == "Table") {
          tool_options[[n]]$args <- tempfile(tmpdir = temp_path, fileext = ".csv") 
        }
        pkg.env$sagaTmpFiles <- append(pkg.env$sagaTmpFiles, tool_options[[n]]$args)
      }
    }
  }
  
  # check if any outputs will be produced
  tool_outputs <- lapply(tool_options, function(x) 
    if (!is.na(x$io)) if (x$io == "Output") x)
  
  tool_outputs <- tool_outputs[sapply(tool_outputs, function(x) 
    !is.null(x))]
  
  n_outputs <- length(tool_outputs)
  
  if (n_outputs == 0) {
    rlang::abort(
      paste(
        "No outputs have been specified and automatic outputs",
        "to tempfiles are disabled (.all_outputs = FALSE)"
      )
    )
    return(NULL)
  }
  
  # update the arguments and expected outputs for tool
  updated_args <- sapply(tool_options, function(x) 
    if (!is.na(x$args)) x$args)
  
  args <- updated_args[sapply(updated_args, function(x) !is.null(x))]
  arg_names <- names(args)
  
  # add saga_cmd arguments to the command line call
  flags <- "--flags=p"
  
  # create string with argument values within quotes
  quote_type <- ifelse(Sys.info()["sysname"] == "Windows", "cmd", "csh")
  params <- shQuote(string = args, type = quote_type)
  
  # prepare system call
  param_string <- paste("-", arg_names, ":", params, sep = "", collapse = " ")
  
  if (!is.na(saga_config)) {
    config <- paste("-C", shQuote(saga_config), sep = "=")
  } else {
    config <- ""
  }
  
  saga_cmd <- paste(
    shQuote(saga_cmd), config, flags, lib,
    shQuote(tool_cmd, type = quote_type),
    param_string
  )
  
  # execute system call
  msg <- system(saga_cmd, intern = TRUE)
  if (!is.null(attr(msg, "status"))) {
    rlang::abort(msg)
  }
  
  # load SAGA results as list of R objects
  saga_results <- lapply(tool_outputs, read_output, .intern = .intern)
  alias_names <- sapply(tool_outputs, function(x) x$alias)
  saga_results <- rlang::set_names(saga_results, alias_names)
  
  # summarize outputs
  if (length(saga_results) == 1) {
    saga_results <- saga_results[[1]]
  }
  
  saga_results
}
