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
#' @param .verbose Option to output all message during the execution of
#'   saga_cmd. Overrides the saga environment setting.
#' @param ... Named arguments and values for SAGA tool.
#'
#' @return output of SAGA-GIS tool loaded as an R object.
#' 
#' @export
saga_execute <-
  function(lib,
           tool,
           senv,
           .intern = NULL,
           .all_outputs = NULL,
           .verbose = NULL,
           ...) {
    
    args <- c(...)
    
    # get tool and saga settings
    tools_in_library <- senv$libraries[[lib]]
    selected_tool <- tools_in_library[[tool]]
    params <- selected_tool$params
    tool_cmd <- selected_tool$tool_cmd
    saga_cmd <- senv$saga_cmd
    saga_config <- senv$saga_config
    temp_path <- senv$temp_path
    backend <- senv$backend
    verbose <- senv$verbose
    intern <- senv$intern
    all_outputs <- senv$all_outputs
    raster_format <- senv$raster_format
    vector_format <- senv$vector_format
    
    # override saga object options with those supplied from tool
    if (!is.null(.verbose))
      verbose <- .verbose
    
    if (!is.null(.intern))
      intern <- .intern
    
    if (!is.null(.all_outputs))
      all_outputs <- .all_outputs
    
    # update parameters object with argument values
    for (arg_name in names(args)) {
      if (arg_name %in% names(params))
        params[[arg_name]]$value <- args[[arg_name]]
    }
    
    # save in-memory R objects to files for saga_cmd to access
    params <- update_parameters_file(params, temp_path)
    
    # optionally use tempfiles for unspecified outputs
    if (all_outputs)
      params <- update_parameters_tempfiles(params, temp_path, raster_format, vector_format)
    
    # remove unused parameter objects
    params <- drop_parameters(params)
    
    if (length(params) == 0) {
      rlang::abort("No outputs have been specified")
    }
    
    # check if any outputs will be produced
    tool_outputs <- params[sapply(params, function(x) !is.na(x$io))]
    tool_outputs <- tool_outputs[sapply(tool_outputs, function(x) x$io == "Output")]
    tool_outputs <- tool_outputs[sapply(tool_outputs, function(x) !is.null(x$files))]
    n_outputs <- length(tool_outputs)
    
    if (n_outputs == 0) {
      rlang::abort("No outputs have been specified")
      return(NULL)
    }
    
    # update the arguments and expected outputs for tool
    cmd_args <- sapply(params, function(param) param[["files"]])
    cmd_args <- setNames(cmd_args, sapply(params, function(param) param[["identifier"]]))
    
    # execute system call
    msg <- run_cmd(saga_cmd, saga_config, lib, tool_cmd, cmd_args, verbose)
    
    if (msg$status == 1) {
      if (verbose)
        message(msg$stdout)
      rlang::abort(msg$stderr)
    }
    
    # load SAGA results as list of R objects
    saga_results <-
      lapply(
        tool_outputs,
        read_output,
        .intern = intern,
        .all_outputs = all_outputs,
        backend = backend
      )
    
    # discard nulls
    saga_results <- saga_results[!sapply(saga_results, is.null)]
    
    # summarize outputs
    if (length(saga_results) == 1) {
      saga_results <- saga_results[[1]]
    }
    
    saga_results
  }
