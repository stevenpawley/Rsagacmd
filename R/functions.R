#' Return the installed version of SAGA-GIS.
#'
#' Intended to be used internally by \code{\link{saga_env}}
#'
#' @param saga_cmd character. Path of the saga_cmd binary
#'
#' @return numeric_version. Version of SAGA-GIS found at the cmd path
saga_version <- function(saga_cmd) {
  
  # system call saga_cmd to display version on console
  saga_vers <- system(paste(shQuote(saga_cmd), "--version"), intern = T)[1]
  saga_vers <- regmatches(saga_vers, regexpr("\\d.\\d.\\d", saga_vers))
  saga_vers <- trimws(saga_vers)
  return(as.numeric_version(saga_vers))
}


#' Automatic search for the path to a SAGA-GIS installation
#'
#' Returns the path to the saga_cmd executable On windows, automatic searching will occur first in
#' 'C:/Program Files/SAGA-GIS'; 'C:/Program Files (x86)/SAGA-GIS'; 'C:/SAGA-GIS'; 'C:/OSGeo4W'; and
#' 'C:/OSGeo4W64'. On linux or OS X, saga_cmd is usually included in PATH, if not an automatic
#' search is performed in the '/usr' folder. If multiple versions of SAGA-GIS are installed on the
#' system, the path to the newest version is returned.
#'
#' @return character. Path to installed saga_cmd binary
#' @export
saga_search <- function() {
  
  # check to see if saga_cmd is recognized (i.e. has been added to path)
  saga_cmd <- if (nchar(Sys.which(names = "saga_cmd")) > 0) "saga_cmd" else NULL
  
  if (is.null(saga_cmd)) {
    
    # define search paths
    if (Sys.info()["sysname"] == "Windows") {
      search_paths <- c(
        "C:/Program Files/SAGA-GIS/",
        "C:/Program Files (x86)/SAGA-GIS/",
        "C:/SAGA-GIS/",
        "C:/OSGeo4W/",
        "C:/OSGeo4W64/",
        "C:/Program Files/SAGA-GIS/"
      )
      saga_executable <- "saga_cmd.exe"
      
    } else if (Sys.info()["sysname"] == "Linux") {
      search_paths <- c("/usr/")
      saga_executable <- "saga_cmd$"
      
    } else if (Sys.info()["sysname"] == "Darwin") {
      search_paths <- c(
        "/usr/local/bin/",
        "/Applications/QGIS/Contents/MacOS/bin/",
        "/usr/local/opt/saga-gis/bin/"
      )
      
      saga_executable <- "saga_cmd$"
    }
    
    # search for saga_cmd executable
    saga_cmd <- c()
    for (f in search_paths) {
      saga_cmd <- c(saga_cmd, list.files(
        path = f,
        pattern = saga_executable,
        recursive = TRUE,
        full.names = TRUE
      ))
    }
  }
  
  # error is saga_cmd not found
  if (length(saga_cmd) == 0) {
    stop(paste(
      "SAGA-GIS installation not found. Need to supply a valid path",
      "to the saga_cmd executable"
    ), call. = FALSE)
    
    return(NULL)
    
    # automatically use newest version if multiple installations are found
  } else if (length(saga_cmd) > 1) {
    message("Multiple installations of SAGA-GIS were found at:")
    message(paste(saga_cmd, collapse = "\n"))
    message(paste(
      "Choosing newest version. Manually specify the location when",
      "calling saga_gis() to use an older version"
    ))
    
    saga_vers <- list()
    
    for (saga_inst in saga_cmd)
      saga_vers <- append(saga_vers, saga_version(saga_inst))
    
    saga_cmd <- saga_cmd[which(saga_vers == max(saga_vers))]
  }
  
  return(saga_cmd)
}


#' Parses valid SAGA-GIS libraries and tools into a nested list of functions
#'
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing all libraries, tools
#' and options from the help files into a nested list of library, module and options, that are
#' contained within an saga environment object object. Intended to be used internally by
#' \code{\link{saga_gis}}
#'
#' @param saga_bin character. Optional path to saga_cmd executable
#' @param opt_lib character vector. List of subset of SAGA-GIS tool libraries
#' to generate links to.
#'
#' @return saga environment object object. Contains paths, settings and a nested list of libaries
#'   tools and options
saga_env <- function(saga_bin = NULL, opt_lib = NULL) {
  
  if (is.null(saga_bin)) {
    saga_bin <- saga_search()
  } else {
    if (nchar(Sys.which(names = saga_bin)) == 0)
      stop("The supplied path to the saga_cmd binary is not correct", call. = FALSE)
  }
  
  saga_vers <- saga_version(saga_bin)
  
  # generate SAGA help files in temporary directory
  # note that prior to saga version 3.0.0 a path cannot be specified in which 
  # to create the document pages, i.e. saga will only generate them in 
  # the working directory. In this case we need to manually change the working 
  # directory to tempdir and then restore
  help_path <- file.path(tempdir(), basename(tempfile()))
  dir.create(help_path)
  
  if (saga_vers > as.numeric_version("3.0.0")) {
    msg <- system(
      paste0(paste(shQuote(saga_bin), "--create-docs="), help_path),
      intern = TRUE)
    
  } else {
    olddir <- getwd()
    setwd(help_path)
    msg <- system(paste(shQuote(saga_bin), "--docs"), intern = TRUE)
    setwd(olddir)
  }
  
  if (!is.null(attr(msg, "status"))) {
    cat(msg, sep = "\n")
    stop()
  }
  
  # parse SAGA help files into nested list of libraries, tools and options
  docs_libraries <- list.dirs(path = help_path)
  docs_libraries <- docs_libraries[2:length(docs_libraries)]
  if (!is.null(opt_lib))
    docs_libraries <- docs_libraries[which(basename(docs_libraries) %in% opt_lib)]
  
  libraries <- list()
  
  for (libdir in docs_libraries) {
    tool_files <- list.files(path = libdir)
    
    # get module names from file and remove from parameter list
    tool_names_file <- tool_files[which.min(nchar(tool_files))]
    tool_files <- tool_files[tool_files != tool_names_file]
    
    for (tool in tool_files) {
      # read module options tables
      options <- XML::readHTMLTable(
        doc = paste(libdir, tool, sep = "/"),
        header = TRUE,
        stringsAsFactors = FALSE)
      
      tool_information <- options[[1]]
      tool_options <- options[[length(options)]]
      
      if (!any(grepl("interactive", x = options[[1]][, 2]))) {
        
        # create syntactically valid tool_name
        saga_tool_cmd <- colnames(tool_information)[2]
        tool_name <- saga_tool_cmd %>%
          stringr::str_replace_all("^[0-9]+", "") %>%  # remove digits from start of tool name
          stringr::str_replace_all(" ", "_") %>%       # replace spaces with underscores
          stringr::str_replace_all("\\(", "") %>%      # replace spaces with underscores
          stringr::str_replace_all("\\)", "") %>%      # replace spaces with underscores
          stringr::str_replace_all("\\(", "") %>%      # remove parenthesis
          stringr::str_replace_all("\\)", "") %>%      # remove parenthesis
          stringr::str_replace_all("'", "") %>%        # remove single quotations
          stringr::str_replace_all(",", "_") %>%       # remove commas
          stringr::str_replace_all("/", "_") %>%       # replace forward slash with underscore
          stringr::str_replace_all("-", "_") %>%       # replace minus with underscore
          stringr::str_replace_all(":", "_") %>%       # replace colon with underscore
          stringr::str_replace_all("\\[", "_") %>%     # replace square brackets with underscore
          stringr::str_replace_all("\\]", "_") %>%     # replace square brackets with underscore
          stringr::str_replace_all("&", "_") %>%       # replace ampersand with underscore
          stringr::str_replace_all("_+", "_") %>%      # replace multiple underscores due to above with single _
          stringr::str_replace_all("^_+", "") %>%      # remove underscore from start of tool name
          tolower()
        
        # strip input, output and options lines from table
        tool_options <- 
          tool_options[which(apply(tool_options[, 2:5], 1, function(x) 
            any(is.na(x))) == FALSE), ]
        
        # replace tool arguments with synatically-correct version
        identifiers_saga <- tool_options$Identifier
        identifiers_r <- sapply(identifiers_saga, function(x) 
          if (grepl("^[[:digit:]]", x)) paste0("_", x) else x, USE.NAMES = FALSE)
        identifiers_r <- gsub(" ", "_", identifiers_r)
        
        # convert to nested list
        params <- rep(list(NA), nrow(tool_options)) %>% setNames(identifiers_r)
        
        for (i in seq_len(length(identifiers_r))) {
          identifier_r <- identifiers_r[[i]]
          identifier_saga <- identifiers_saga[[i]]
          
          params[[identifier_r]] <- list(
            type = tool_options[tool_options$Identifier == identifier_saga, "Type"],
            name = tool_options[tool_options$Identifier == identifier_saga, "Name"],
            identifier = identifier_saga,
            description = tool_options[tool_options$Identifier == identifier_saga, "Description"],
            constraints = tool_options[tool_options$Identifier == identifier_saga, "Constraints"],
            io = NA,
            feature = NA,
            default = NA,
            minimum = NA,
            maximum = NA)
          
          # clean constraints
          if (params[[identifier_r]]$constraints == "") 
            params[[identifier_r]]$constraints <- NA
          
          params[[identifier_r]]$constraints <-
            params[[identifier_r]]$constraints %>%
            stringr::str_replace_all("Available Choices:", "")
          
          params[[identifier_r]]$constraints <- 
            params[[identifier_r]]$constraints %>%
            stringr::str_replace_all("^\n", "")
          
          params[[identifier_r]]$constraints <- 
            params[[identifier_r]]$constraints %>%
            stringr::str_replace_all("\n", ";")
          
          params[[identifier_r]]$default <-
            params[[identifier_r]]$constraints %>% 
            stringr::str_extract("(?<=Default: \\s{0,1})[-0-9.]+") 
          params[[identifier_r]]$default <-
            suppressWarnings(as.numeric(params[[identifier_r]]$default))
          
          params[[identifier_r]]$minimum <-
            params[[identifier_r]]$constraints %>% 
            stringr::str_extract("(?<=Minimum: \\s{0,1})[-0-9.]+")
          params[[identifier_r]]$minimum <-
            suppressWarnings(as.numeric(params[[identifier_r]]$minimum))
          
          params[[identifier_r]]$maximum <-
            params[[identifier_r]]$constraints %>% 
            stringr::str_extract("(?<=Maximum: \\s{0,1})[-0-9.]+")
          params[[identifier_r]]$maximum <-
            suppressWarnings(as.numeric(params[[identifier_r]]$maximum))
          
        }
        
        # parse additional parameters
        for (n in names(params)) {
          if (stringr::str_detect(params[[n]]$type, "input")) params[[n]]$io <- "Input"
          if (stringr::str_detect(params[[n]]$type, "output")) params[[n]]$io <- "Output"
          if (stringr::str_detect(params[[n]]$type, "Grid")) params[[n]]$feature <- "Grid"
          if (stringr::str_detect(params[[n]]$type, "Grid list")) params[[n]]$feature <- "Grid list"
          if (stringr::str_detect(params[[n]]$type, "Shapes")) params[[n]]$feature <- "Shape"
          if (stringr::str_detect(params[[n]]$type, "Shapes list")) params[[n]]$feature <- "Shapes list"
          if (stringr::str_detect(params[[n]]$type, "Table")) params[[n]]$feature <- "Table"
          if (stringr::str_detect(params[[n]]$type, "Static table")) params[[n]]$feature <- "Table"
          if (stringr::str_detect(params[[n]]$type, "Table list")) params[[n]]$feature <- "Table list"
          if (stringr::str_detect(params[[n]]$type, "File path")) params[[n]]$feature <- "File path"
          if (stringr::str_detect(params[[n]]$type, "field")) params[[n]]$feature <- "Table field"
          if (stringr::str_detect(params[[n]]$type, "Integer")) params[[n]]$feature <- "Integer"
          if (stringr::str_detect(params[[n]]$type, "Choice")) params[[n]]$feature <- "Choice"
          if (stringr::str_detect(params[[n]]$type, "Floating point")) params[[n]]$feature <- "numeric"
          if (stringr::str_detect(params[[n]]$type, "Boolean")) params[[n]]$feature <- "logical"
          if (stringr::str_detect(params[[n]]$type, "Long text")) params[[n]]$feature <- "character"
          if (stringr::str_detect(params[[n]]$type, "Text")) params[[n]]$feature <- "character"
        }
        
        # exceptions
        if (tool_name == "export_geotiff" | tool_name == "export_raster") {
          params$FILE$io <- "Output"
          params$FILE$feature <- "Grid"
          
        } else if (tool_name == "export_shapes" | tool_name == "export_shapes_to_kml") {
          params$FILE$io <- "Output"
          params$FILE$feature <- "Shapes"
          
        } else if (tool_name == "clip_grid_with_rectangle") {
          params$OUTPUT$feature <- "Grid"
        }
        
        # add parameter options to nested list
        libraries[[basename(libdir)]][[tool_name]] <- 
          list(options = params, tool_cmd = saga_tool_cmd) 
      }

    }
  }
  
  # remove tools that produce no outputs
  for (lib in names(libraries)) {
    tools <- names(libraries[[lib]])
    for (tool in tools) {
      selected_tool <- libraries[[lib]][[tool]]$options
      if (any(selected_tool$io != "Output"))
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
  
  return(list(saga_cmd = saga_bin,
              saga_vers = saga_vers,
              libraries = libraries))
}


#' Generates a custom saga_cmd configuration file
#'
#' Creates and edits a saga_cmd coniguration file in order to change saga_cmd settings related to
#' file caching and number of available processor cores. Intended to be used internally by
#' \code{\link{saga_gis}}.
#'
#' @param senv saga environment object object. SAGA-GIS environment and settings
#' @param grid_caching logical. Optionally use file caching
#' @param grid_cache_threshold numeric. Threshold (in Mb) before file caching for loaded raster data
#'   is activated
#' @param grid_cache_dir character. Path to directory for temporary files
#' @param cores numeric. Maximum number of processing cores. Needs to be set to 1 if file caching is
#'   activated
#' @param saga_vers numeric_version. Version of SAGA-GIS. The generation of a saga_cmd configuration
#'   file is only valid for versions > 4.0.0
#'
#' @return character. Path to custom saga_cmd initiation file
saga_configure <- function(senv,
                           grid_caching = FALSE,
                           grid_cache_threshold = 100,
                           grid_cache_dir = NULL,
                           cores = NULL,
                           saga_vers) {
  # some checks
  if (missing(senv)) {
    stop("senv parameter is missing")
  }
  
  if (is.null(grid_cache_dir)) {
    grid_cache_dir <- gsub("\\\\", "/", tempdir())
  }
  
  # create configuration file if any arguments are supplied
  if ((grid_caching == TRUE | !is.null(cores)) &
      saga_vers >= as.numeric_version("4.0.0")) {
    saga_config <- tempfile(fileext = ".ini")
    msg <- system(paste(
      shQuote(senv$saga_cmd),
      paste0("--create-config=", saga_config)
    ), intern = T)
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
        message("cores > 1. SAGA-GIS file caching is not thread-safe. Setting cores = 1")
        cores <- 1
      }
      
      saga_config_settings <- gsub(
        "GRID_CACHE_MODE=[0-3]", "GRID_CACHE_MODE=1", saga_config_settings
      )
      saga_config_settings <- gsub(
        "GRID_CACHE_THRESHLOD=[0-9]*",
        paste0("GRID_CACHE_THRESHLOD=", grid_cache_threshold),
        saga_config_settings
      )
      saga_config_settings <- gsub(
        "GRID_CACHE_TMPDIR=;*",
        paste0("GRID_CACHE_TMPDIR=", shQuote(gsub("\\\\", "/", tempdir()))),
        saga_config_settings
      )
      saga_config_settings <- gsub(
        "OMP_THREADS_MAX=[0-9]*", "OMP_THREADS_MAX=1", saga_config_settings
      )
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
#' Dynamically generates functions to all valid SAGA-GIS libraries and tools. These functions are
#' stored within a saga S3 object as a named list of functions.
#'
#' @param saga_bin character, optional. Path to saga_cmd executable. If this argument is not
#'   supplied then an automatic search for the saga_cmd executable will be performed.
#' @param grid_caching logical, optional. Use file caching in saga_cmd geoprocessing operations for
#'   rasters that are too large to fit into memory.
#' @param grid_cache_threshold numeric, optional. Threshold (in Mb) before file caching is activated
#'   for loaded raster data.
#' @param grid_cache_dir character, optional. Path to directory for temporary files generated by
#'   file caching.
#' @param cores numeric. Maximum number of processing cores. Needs to be set to 1 if file caching is
#'   activated.
#' @param opt_lib character vector. Names of SAGA-GIS libraries. Used to link only a subset of named
#'   SAGA-GIS tool libraries, rather than creating functions for all available tool libraries.
#' @param temp_path character vector, optional. Path to use to store any temporary files that are
#'   generated as data is passed between R and SAGA-GIS. If not specified, then the system tempdir
#'   is used.
#' @return S3 saga object containing a nested list of functions for SAGA-GIS libraries and tools.
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
#' dem <- saga$grid_calculus$random_terrain(RADIUS = 100)
#' 
#' # Use Rsagacmd to calculate the Terrain Ruggedness Index
#' tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(DEM = dem)
#' plot(tri)
#' 
#' # Optionally run command and do not load result as an R object
#' saga$ta_morphometry$terrain_ruggedness_index_tri(DEM = dem, intern = FALSE)
#' }
saga_gis <- function(saga_bin = NULL,
                     grid_caching = FALSE,
                     grid_cache_threshold = 100,
                     grid_cache_dir = NULL,
                     cores = NULL,
                     opt_lib = NULL,
                     temp_path = NULL) {
  
  senv <- saga_env(saga_bin, opt_lib)
  
  senv[["saga_config"]] <- saga_configure(
    senv,
    grid_caching,
    grid_cache_threshold,
    grid_cache_dir,
    cores,
    senv$saga_vers
  )
  
  if (!is.null(temp_path)) 
    senv[["temp_path"]] <- temp_path else
      senv[["temp_path"]] <- tempdir()
  
  # dynamically create functions
  tool_libraries <- list()
  
  for (lib in names(senv$libraries)) {
    toolnames <- list()
    
    for (tool in names(senv$libraries[[lib]])) {
      tool_options <- senv$libraries[[lib]][[tool]][["options"]]
      tool_cmd <- senv[["libraries"]][[lib]][[tool]][["tool_cmd"]]
      args <- tool_options %>% 
        sapply(function(x) x$identifier, USE.NAMES = FALSE) %>%
        paste0("=NULL", collapse = ", ")
      
      # define function body
      # deparse tool settings into function and create code to call saga_execute
      body <- paste(
        paste0("args = as.list(environment())"),
        paste0("lib = ", deparse(lib)),
        paste0("tool = ", deparse(tool)),
        "
        # remove intern and help from saga args list
        if ('intern' %in% names(args))
            args = args[-which(names(args) == 'intern')]
        
        if ('all_outputs' %in% names(args))
            args = args[-which(names(args) == 'all_outputs')]
        
        # call the saga geoprocessor
        saga_results = saga_execute(lib, tool, senv, intern, all_outputs, args)
        return (saga_results)
        ",
        sep = "\n"
      )
      
      # parse function
      # here we are creating functions by evaluating expressions these functions have some
      # additional attributes used to printing tool information etc.
      
      # The functions are stored as a nested list which is returned as a saga class. These functions
      # exist within the environment of the class object, along with the variables/tools/libraries
      # needed by each function. Thus, when each function is called, it can find the variables that
      # it needs from within its own environment
      tryCatch(
        expr = {
          func_code <- paste0(
            "function(", args, ", intern = TRUE, all_outputs = TRUE", ") {",
            "\n", body, "\n", "}"
          )
          func <- structure(
            eval(expr = parse(text = func_code)),
            lib = lib,
            tool = tool,
            class = "saga_tool"
          )
          
          tool_libraries[[lib]] <- append(tool_libraries[[lib]], func)
          toolnames <- append(toolnames, tool)
        },
        error = function(e)
          warning(
            paste0("Problem parsing SAGA-GIS library = ", lib, "; and tool = ", tool),
            call. = FALSE
          )
      )
    }
    names(tool_libraries[[lib]]) <- toolnames
  }
  
  # clean local environment
  remove(
    args, body, cores, func, func_code, grid_cache_dir,
    grid_cache_threshold, grid_caching, lib, saga_bin, tool, tool_cmd,
    tool_options, toolnames
  )
  
  #  return S3 saga object
  structure(
    tool_libraries,
    class = "saga"
  )
}


#' Generic function to display help and usage information for any SAGA-GIS tool
#'
#' @param x saga_tool object
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
    cat(paste0("Identifier: ", tool_options[[i]]$identifier, "\n"))
    cat(paste0("Type: ", tool_options[[i]]$type, "\n"))
    cat(paste0("Description: ", tool_options[[i]]$description, "\n"))
    cat(paste0("Constraints: ", tool_options[[i]]$constraints, "\n"))
    cat("\n")
  }
}


#' Function to execute SAGA-GIS commands through the command line tool.
#'
#' Intended to be used internally
#'
#' @param lib character. Name of SAGA-GIS library to execute
#' @param tool character. Name of SAGA-GIS tool to execute
#' @param senv saga environment object object
#' @param intern logical. Load outputs as R objects
#' @param all_outputs logical (default = TRUE). Automatically output all results from the selected
#' SAGA tool and load them results as R objects. If all_outputs = FALSE then the file paths to store
#' the tool's results will have to be manually specified in the arguments
#' @param ... Named arguments and values for SAGA tool
#'
#' @return Output of SAGA-GIS tool loaded as an R object
#'   (RasterLayer/sf/dataframe)
saga_execute <- function(lib, tool, senv, intern = TRUE, all_outputs = TRUE, ...) {
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
    
    # use tempfiles for other outputs if all_outputs   
    } else if (all_outputs == TRUE & !is.na(tool_options[[n]]$io)) {
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
    stop("No outputs have been specified and automatic outputs to tempfiles are disabled (all_outputs = FALSE)")
    return(NULL)
  }
  
  # update the arguments and expected outputs for tool
  updated_args <- sapply(tool_options, function(x) if (!is.na(x$args)) x$args)
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
    cat(msg, sep = "\n")
    stop()
  }
  
  # load SAGA results as list of R objects
  saga_results <- list()
  
  for (i in seq_along(tool_outputs)) {
    out_i <- tool_outputs[[i]]$args
    out_i <- gsub(".sgrd", ".sdat", out_i)
    current_id <- tool_outputs[[i]]$identifier
    
    if (intern == TRUE) {
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
            "No geoprocessing output for ", names(tool_outputs)[[i]],
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


#' Removes temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if any outputs for a
#' SAGA-GIS tool are not specified as arguments. Temporary files in R are automatically removed at
#' the end of each session. However, when dealing with raster data, these temporary files
#' potentially can consume large amounts of disk space. These temporary files can be observed during
#' a session by using the saga_show_tmpfiles function, and can be removed using the
#' saga_remove_tmpfiles function. Note that this function also removes any accompanying files, i.e.
#' the '.prj' and '.shx' files that may be written as part of writing a ESRI Shapefile '.shp'
#' format.
#'
#' @param h Remove temporary files that are older than h (in number of hours)
#'
#' @return Nothing is returned
#' @export
#'
#' @examples
#' \dontrun{
#' # Remove all temporary files generated by Rsagacmd
#' saga_remove_tmpfiles(h = 0)
#' }
saga_remove_tmpfiles <- function(h = 0) {
  message(paste0("Removing Rsagacmd temporary files h=", h))
  for (f in pkg.env$sagaTmpFiles) {
    if (file.exists(f) == TRUE) {
      tdelay <- difftime(Sys.time(), file.mtime(f), units = "hours")
      if (tdelay > h) {
        message(f)
        assoc_files <- list.files(
          path = dirname(f),
          pattern = utils::glob2rx(paste0(tools::file_path_sans_ext(basename(f)), ".*")),
          full.names = T
        )
        file.remove(assoc_files)
        pkg.env$sagaTmpFiles <- pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
      }
    } else {
      pkg.env$sagaTmpFiles <- pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
    }
  }
}


#' List temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if any outputs for a
#' SAGA-GIS tool are not specified as arguments. Temporary files in R are automatically removed at
#' the end of each session. However, when dealing with raster data, these temporary files
#' potentially can consume large amounts of disk space. These temporary files can be observed during
#' a session by using the saga_show_tmpfiles function, and can be removed using the
#' saga_remove_tmpfiles function.
#'
#' @return returns the file names of the files in the temp directory that have been generated by
#'   Rsagacmd. Note this list of files only includes the primary file extension, i.e. '.shp' for a
#'   shapefile without the accessory files (e.g. .prj, .shx etc.).
#' @export
#' @examples
#' \dontrun{
#' # Show all temporary files generated by Rsagacmd
#' saga_remove_tmpfiles(h = 0)
#' }
saga_show_tmpfiles <- function() {
  message("Rsagacmd temporary files:")
  for (f in pkg.env$sagaTmpFiles) {
    message(f)
  }
  return(pkg.env$sagaTmpFiles)
}


# local environment to store vector of tempfiles in package namespace
pkg.env <- new.env(parent = emptyenv())
pkg.env$sagaTmpFiles <- c()
