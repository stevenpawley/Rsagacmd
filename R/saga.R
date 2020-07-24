#' Parses valid SAGA-GIS libraries and tools into a nested list of functions
#'
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing
#' all libraries, tools and options from the help files into a nested list of
#' library, module and options, that are contained within an saga environment
#' object object. Intended to be used internally by \code{\link{saga_gis}}
#'
#' @param saga_bin An optional character vector to specify the path to the
#'   saga_cmd executable. Otherwise the function will perform a search for
#'   saga_cmd.
#' @param opt_lib A character vector of a subset of SAGA-GIS tool libraries to
#'   generate dynamic functions that map to each tool. Used to save time if you
#'   only want to import a single library.
#' @param backend A character vector to specify the library to use for handling
#'   raster data. Currently, either "raster" or "terra" is supported. The
#'   default is "raster".
#'
#' @return A saga environment S3 object containing paths, settings and a nested
#'   list of libraries tools and options.
saga_env <- function(saga_bin = NULL, opt_lib = NULL, backend = "raster") {
  
  if (is.null(saga_bin)) {
    saga_bin <- saga_search()
  } else {
    if (nchar(Sys.which(names = saga_bin)) == 0)
      rlang::abort("The supplied path to the saga_cmd binary is not correct")
  }
  
  saga_vers <- saga_version(saga_bin)
  
  # generate SAGA help files in temporary directory
  help_path <- file.path(tempdir(), basename(tempfile()))
  dir.create(help_path)
  
  # version < 3.0.0 need to use working directory
  if (saga_vers > as.numeric_version("3.0.0")) {
    msg <-
      system(paste0(paste(shQuote(saga_bin), "--create-docs="), help_path), intern = TRUE)
    
  } else {
    olddir <- getwd()
    setwd(help_path)
    msg <- system(paste(shQuote(saga_bin), "--docs"), intern = TRUE)
    setwd(olddir)
  }
  
  if (!is.null(attr(msg, "status"))) {
    rlang::abort()
  }
  
  # parse SAGA help files into nested list of libraries, tools and options
  docs_libraries <- list.dirs(path = help_path)
  docs_libraries <- docs_libraries[2:length(docs_libraries)]
  
  if (!is.null(opt_lib)) {
    docs_libraries <- 
      docs_libraries[which(basename(docs_libraries) %in% opt_lib)]
  }
  
  libraries <- list()
  
  for (libdir in docs_libraries) {
    tool_files <- list.files(path = libdir)
    
    # get module names from file and remove from parameter list
    tool_names_file <- tool_files[which.min(nchar(tool_files))]
    tool_files <- tool_files[tool_files != tool_names_file]
    
    for (tool in tool_files) {
      tryCatch(expr = {
        options <- XML::readHTMLTable(
          doc = paste(libdir, tool, sep = "/"),
          header = TRUE,
          stringsAsFactors = FALSE)
        
        tool_information <- options[[1]]
        tool_options <- options[[length(options)]]
        
        if (!any(grepl("interactive", x = options[[1]][, 2]))) {
          tool_config <- create_tool(tool_information, tool_options)
          libraries[[basename(libdir)]][[tool_config$tool_name]] <-
            tool_config
        }
      }, 
      error = function(e) 
        e)
    }
  }
  
  # remove tools that produce no outputs
  for (lib in names(libraries)) {
    tools <- names(libraries[[lib]])
    
    for (tool in tools) {
      selected_tool <- libraries[[lib]][[tool]]$options
      
      has_output <- sapply(selected_tool, function(x) x$io)
      
      if (!"Output" %in% has_output)
        libraries[[lib]] <- libraries[[lib]][!names(libraries[[lib]]) == tool]
    }
  }
  
  # remove libraries with no tools
  for (lib in names(libraries)) {
    n_tools <- length(libraries[[lib]])
    
    if (n_tools == 0)
      libraries <- libraries[names(libraries) != lib]
  }
  
  # remove invalid libraries for saga_cmd
  invalid_libs <- list(
    "db_odbc",
    "db_pgsql",
    "docs_html",
    "docs_pdf",
    "garden_3d_viewer",
    "garden_games",
    "garden_learn_to_program",
    "garden_webservices",
    "grid_calculus_bsl",
    "pointcloud_viewer",
    "tin_viewer"
  )
  
  libraries <- libraries[!names(libraries) %in% invalid_libs]
  
  return(
    list(
      saga_cmd = saga_bin,
      saga_vers = saga_vers,
      backend = backend,
      libraries = libraries
    )
  )
}


#' Generates a custom saga_cmd configuration file
#'
#' Creates and edits a saga_cmd configuration file in order to change saga_cmd
#' settings related to file caching and number of available processor cores.
#' Intended to be used internally by \code{\link{saga_gis}}
#'
#' @param senv A saga environment object. Contains the SAGA-GIS environment and
#'   settings.
#' @param grid_caching Whether to use file caching. The default is FALSE.
#' @param grid_cache_threshold Any number to use as a threshold (in Mb) before
#'   file caching for loaded raster data is activated.
#' @param grid_cache_dir Optionally specify a path to the used directory for
#'   temporary files. The default uses `base::tempdir`.
#' @param cores An integer specifying the maximum number of processing cores.
#'   Needs to be set to 1 if file caching is activated because file caching in
#'   SAGA-GIS is not thread-safe.
#' @param saga_vers A `numeric_version` that specifies the version of SAGA-GIS.
#'   The generation of a saga_cmd configuration file is only valid for versions
#'   > 4.0.0.
#'
#' @return A character that specifies the path to custom saga_cmd initiation
#'   file.
saga_configure <- function(senv,
                           grid_caching = FALSE,
                           grid_cache_threshold = 100,
                           grid_cache_dir = NULL,
                           cores = NULL,
                           saga_vers) {
  # some checks
  if (missing(senv)) {
    rlang::abort("senv parameter is missing")
  }
  
  if (is.null(grid_cache_dir)) {
    grid_cache_dir <- gsub("\\\\", "/", tempdir())
  }
  
  # create configuration file if any arguments are supplied
  if ((grid_caching == TRUE | !is.null(cores)) &
      saga_vers >= as.numeric_version("4.0.0")) {
    
    saga_config <- tempfile(fileext = ".ini")
    
    msg <- system(
      paste(
        shQuote(senv$saga_cmd),
        paste0("--create-config=", saga_config)), 
      intern = T)
    
    saga_config_settings <- readChar(saga_config, file.info(saga_config)$size)
    
    # configuration for custom number of cores
    if (!missing(cores) & grid_caching == FALSE) {
      
      saga_config_settings <- gsub(
        "OMP_THREADS_MAX=[0-9]*",
        paste0("OMP_THREADS_MAX=", cores),
        saga_config_settings
      )
      
    # configuration for grid caching
    } else if (grid_caching == TRUE) {
      
      if (is.null(cores)) {
        message("Number of processing cores not specified")
        message("SAGA-GIS file caching is not thread-safe. Using cores = 1")
        cores <- 1
      }
      
      if (cores > 1) {
        message("cores > 1. SAGA-GIS file caching is not thread-safe.
                Setting cores = 1")
        cores <- 1
      }
      
      saga_config_settings <- gsub(
        "GRID_CACHE_MODE=[0-3]", "GRID_CACHE_MODE=1", saga_config_settings)
      
      saga_config_settings <- gsub(
        "GRID_CACHE_THRESHLOD=[0-9]*",
        paste0("GRID_CACHE_THRESHLOD=", grid_cache_threshold),
        saga_config_settings)
      
      saga_config_settings <- gsub(
        "GRID_CACHE_TMPDIR=;*",
        paste0("GRID_CACHE_TMPDIR=", shQuote(gsub("\\\\", "/", tempdir()))),
        saga_config_settings)
      
      saga_config_settings <- gsub(
        "OMP_THREADS_MAX=[0-9]*", "OMP_THREADS_MAX=1", saga_config_settings)
    }
    
    # write configuration file
    writeChar(saga_config_settings, saga_config)
    
  } else if ((grid_caching == TRUE | !is.null(cores)) &
             saga_vers < as.numeric_version("4.0.0")) {
    message(
      paste(
        "Cannot enable grid caching or change number cores for SAGA-GIS",
        "versions < 4.0.0. Please use a more recent version of SAGA-GIS"
      )
    )
  }
  
  if (!exists("saga_config")) {
    saga_config <- NA
  }
  
  return(saga_config)
}


#' Initiate a SAGA-GIS geoprocessor object
#'
#' Dynamically generates functions to all valid SAGA-GIS libraries and tools.
#' These functions are stored within a saga S3 object as a named list of
#' functions
#'
#' @param saga_bin The path to saga_cmd executable. If this argument
#'   is not supplied then an automatic search for the saga_cmd executable will
#'   be performed.
#' @param grid_caching A logical whether to use file caching in saga_cmd
#'   geoprocessing operations for rasters that are too large to fit into memory.
#' @param grid_cache_threshold A number to act as a threshold (in Mb) before
#'   file caching is activated for loaded raster data.
#' @param grid_cache_dir The path to directory for temporary files generated by
#'   file caching. If not provided then the result from `base::tempdir()` is 
#'   used.
#' @param cores An integer for the maximum number of processing cores. By
#'   default all cores are utilized. Needs to be set to 1 if file caching is
#'   activated.
#' @param backend A character vector to specify the library to use for handling
#'   raster data. Currently, either "raster" or "terra" is supported. The
#'   default is "raster".
#' @param opt_lib A character vector with the names of a subset of SAGA-GIS
#'   libraries. Used to link only a subset of named SAGA-GIS tool libraries,
#'   rather than creating functions for all available tool libraries.
#' @param temp_path The path to use to store any temporary files that are
#'   generated as data is passed between R and SAGA-GIS. If not specified, then
#'   the system `base::tempdir()` is used.
#' @param verbose Logical to indicate whether to output all messages made during
#'   SAGA-GIS commmands to the R console. Default is FALSE.
#'   
#' @return A S3 `saga` object containing a nested list of functions for SAGA-GIS
#'   libraries and tools.
#' 
#' @export
#' @import raster
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' saga <- saga_gis()
#' 
#' # Alternatively intialize a saga object using file caching to handle large
#' # raster files
#' saga <- saga_gis(grid_caching = TRUE, grid_cache_threshold = 250, cores = 1)
#' 
#' # Example terrain analysis
#' # Generate a random DEM
#' dem <- saga$grid_calculus$random_terrain(radius = 100)
#' 
#' # Use Rsagacmd to calculate the Terrain Ruggedness Index
#' tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = dem)
#' plot(tri)
#' 
#' # Optionally run command and do not load result as an R object
#' saga$ta_morphometry$terrain_ruggedness_index_tri(dem = dem, .intern = FALSE)
#' }
saga_gis <- function(saga_bin = NULL,
                     grid_caching = FALSE,
                     grid_cache_threshold = 100,
                     grid_cache_dir = NULL,
                     cores = NULL,
                     backend = "raster",
                     opt_lib = NULL,
                     temp_path = NULL,
                     verbose = FALSE) {
  
  senv <- saga_env(saga_bin, opt_lib, backend)
  senv$verbose <- verbose
  
  senv[["saga_config"]] <- saga_configure(
    senv,
    grid_caching,
    grid_cache_threshold,
    grid_cache_dir,
    cores,
    senv$saga_vers
  )
  
  if (!is.null(temp_path)) senv[["temp_path"]] <- temp_path else
      senv[["temp_path"]] <- tempdir()
  
  # dynamically create functions
  tool_libraries <- list()
  
  for (lib in names(senv$libraries)) {
    toolnames <- list()
    
    for (tool in names(senv$libraries[[lib]])) {
      tool_options <- senv$libraries[[lib]][[tool]][["options"]]
      tool_cmd <- senv[["libraries"]][[lib]][[tool]][["tool_cmd"]]

      tryCatch(
        expr = {
          # create function body
          body <- create_function(lib = lib, tool = tool)
          
          # create list of arguments and default values
          args <- lapply(tool_options, function(x) NULL)
          args <- c(args, list(.intern = TRUE, .all_outputs = TRUE, .verbose = FALSE))
          
          # coerce arguments to comma-separated character
          args <- mapply(
            function(k, v) paste(k, deparse(v), sep = " = "), 
            names(args), 
            args, 
            USE.NAMES = FALSE
          )
          args <- paste(args, collapse = ", ")
          
          # parse function
          func_code <- paste0("function(", args, ") {", body, "}")
          
          tool_env <- new.env()
          tool_env$senv <- senv
          
          func <- structure(
            eval(expr = parse(text = func_code), envir = tool_env),
            lib = lib,
            tool = tool,
            class = "saga_tool"
          )
          
          # append function to lists
          tool_libraries[[lib]] <- append(tool_libraries[[lib]], func)
          toolnames <- append(toolnames, tool)
          names(tool_libraries[[lib]]) <- toolnames
        },
        error = function(e)
          warning(
            paste0(
              "Problem parsing SAGA-GIS library = ", 
              lib, 
              "; and tool = ", 
              tool),
            call. = FALSE
          )
      )
    }
  }
  
  #  return S3 saga object
  structure(
    tool_libraries,
    class = "saga"
  )
}


#' Generic function to display help and usage information for any SAGA-GIS tool
#'
#' @param x A `saga_tool` object.
#' @param ... Additional arguments to pass to print. Currently not used.
#'
#' @return NULL
#' @method print saga_tool
#' @export
#' @examples
#' \dontrun{
#' # Intialize a saga object
#' saga <- saga_gis()
#' 
#' # Display usage information on a tool
#' print(saga$ta_morphometry$slope_aspect_curvature)
#' 
#' # Or simply:
#' saga$ta_morphometry$slope_aspect_curvature
#' }
print.saga_tool <- function(x, ...) {
  lib <- attr(x, "lib")
  tool <- attr(x, "tool")
  
  # get environment of saga_gis object
  env <- environment(x)
  tool_options <- env$senv$libraries[[lib]][[tool]][["options"]]
  
  cat(paste0("Help for library = ", lib, "; tool = ", tool, ":", "\n"))
  for (i in seq_along(tool_options)) {
    cat(paste0("Name of tool: ", tool_options[[i]]$name), "\n")
    cat(paste0("Argument name: ", tool_options[[i]]$alias, "\n"))
    cat(paste0("Identifier used by SAGA-GIS: ", tool_options[[i]]$identifier, "\n"))
    cat(paste0("Type: ", tool_options[[i]]$type, "\n"))
    cat(paste0("Description: ", tool_options[[i]]$description, "\n"))
    cat(paste0("Constraints: ", tool_options[[i]]$constraints, "\n"))
    cat("\n")
  }
}
