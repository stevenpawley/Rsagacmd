#' Function to execute SAGA-GIS commands through the command line tool
#'
#' Intended to be used internally
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
#' @param ... Named arguments and values for SAGA tool
#'
#' @return output of SAGA-GIS tool loaded as an R object.
#'   (RasterLayer/sf/data.frame)
saga_execute <-
  function(lib,
           tool,
           senv,
           .intern = TRUE,
           .all_outputs = TRUE,
           ...) {
  
  args <- c(...)
  
  # saga installation settings
  tool_options <- senv$libraries[[lib]][[tool]][["options"]]
  tool_cmd <- senv$libraries[[lib]][[tool]][["tool_cmd"]]
  saga_cmd <- senv$saga_cmd
  saga_config <- senv$saga_config
  temp_path <- senv$temp_path
  
  # match the syntactically-correct arg_name to the identifier used by saga_cmd
  arg_names <- names(args)
  identifiers_r <- sapply(tool_options, function(x) x$identifier)
  arg_names <- identifiers_r[intersect(arg_names, names(identifiers_r))]
  args <- setNames(args, arg_names)
  tool_options <- setNames(tool_options, identifiers_r) # rename to identifiers
  
  # strip other missing arguments and update arg_names
  args <- args[sapply(args, function(x) !is.null(x))]
  arg_names <- names(args)
  
  # save loaded R objects to files for SAGA-GIS to access
  for (i in seq_along(args))
    # if list split list into separate files
    if (any(class(args[[i]]) == "list"))
      for (j in seq_along(args[[i]]))
        args[[i]][[j]] <- translate(args[[i]][[j]], temp_path) else
          args[[i]] <- translate(args[[i]], temp_path)
  
  # collapse argument values that are lists into a semi-colon separated strings
  for (i in seq_along(args))
    if (length(args[[i]]) > 1)
      args[[i]] <- paste(args[[i]], collapse = ";")
  
  # replace sdat fileext used by raster package with sgrd used by SAGA
  args <- sapply(args, function(x) gsub(".sdat", ".sgrd", x))
  
  # merge output args with tool options
  for (n in names(tool_options)) {
    tool_options[[n]]$args <- NA
    
    if (tool_options[[n]]$identifier %in% names(args)) {
      tool_options[[n]]$args <- args[[n]] 
      
      # use tempfiles for other outputs if .all_outputs   
    } else if (.all_outputs == TRUE & !is.na(tool_options[[n]]$io)) {
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
  tool_outputs <- lapply(tool_options, function(x) if (!is.na(x$io)) if (x$io == "Output") x)
  tool_outputs <- tool_outputs[sapply(tool_outputs, function(x) !is.null(x))]
  n_outputs <- length(tool_outputs)
  
  if (n_outputs == 0) {
    rlang::abort("No outputs have been specified and automatic outputs to tempfiles are disabled (.all_outputs = FALSE)")
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
  msg <- system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))) {
    rlang::abort(msg)
  }
  
  # load SAGA results as list of R objects
  saga_results <- list()
  
  for (i in seq_along(tool_outputs)) {
    out_i <- tool_outputs[[i]]$args
    out_i <- gsub(".sgrd", ".sdat", out_i)
    current_id <- tool_outputs[[i]]$alias
    
    if (.intern == TRUE) {
      tryCatch(expr = {
        # shapes
        if ("Shape" %in% tool_outputs[[i]]$feature) {
          saga_results[[paste0(current_id)]] <- sf::st_read(out_i, quiet = TRUE)
          
          # tables
        } else if (tool_outputs[[i]]$feature == "Table") {
          if (tools::file_ext(out_i) == "txt") {
            saga_results[[paste0(current_id)]] <- 
              utils::read.table(out_i, header = T, sep = "\t")
          } else if (tools::file_ext(out_i) == "csv") {
            saga_results[[paste0(current_id)]] <- utils::read.csv(out_i)
          } else if (tools::file_ext(out_i) == "dbf") {
            saga_results[[paste0(current_id)]] <- foreign::read.dbf(out_i)
          }
          
          # grids
        } else if (tool_outputs[[i]]$feature %in% c("Grid", "Raster")) {
          if (tools::file_ext(out_i) == "sg-gds-z") {
            warning("Cannot load SAGA Grid Collection as an R raster object - this is not supported")
          } else {
            saga_results[[paste0(current_id)]] <- raster::raster(out_i)
          }
          
          # grid lists
        } else if (tool_outputs[[i]]$feature  == "Grid list") {
          out_i <- strsplit(out_i, ";")[[1]]
          for (gl in seq_along(out_i))
            saga_results[[paste(current_id, gl, sep = "_")]] <- raster::raster(out_i[gl])
        }
        
      }, error = function(e) {
        message(
          paste0(
            "No geoprocessing output for ", tool_outputs[[i]]$alias,
            ". Results may require other input parameters to be specified"))
      })
    } else {
      # intern == FALSE return list of file paths to outputs
      saga_results[[paste0(current_id)]] <- out_i
    }
  }
  
  # summarize outputs
  if (length(saga_results) == 1) {
    saga_results <- saga_results[[1]]
  }
  
  return(saga_results)
}

