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
#' @param raster_backend A character vector to specify the library to use for
#'   handling raster data. Currently, either "terra" or "stars" is
#'   supported. The default is "terra".
#' @param vector_backend A character to specify the library to use for handling
#'   vector data. Currently, either "sf", "SpatVector" or "SpatVectorProxy" is
#'   supported. The default is "sf".
#'
#' @return A saga environment S3 object containing paths, settings and a nested
#'   list of libraries tools and options.
saga_env <-
  function(saga_bin = NULL,
           opt_lib = NULL,
           raster_backend = "terra",
           vector_backend = "sf") {
    if (!raster_backend %in% c("terra", "stars")) {
      rlang::abort("The `raster_backend` must be one of 'terra' or 'stars'")
    }
    
    if (!vector_backend %in% c("sf", "SpatVector", "SpatVectorProxy")) {
      rlang::abort("The `vector_backend` must be one of 'sf', 'SpatVector' or 'SpatVectorProxy'")
    }
    
    if (is.null(saga_bin)) {
      saga_bin <- search_saga()
    }

    if (nchar(Sys.which(names = saga_bin)) == 0) {
      rlang::abort("The supplied path to the saga_cmd binary is not correct")
    }

    saga_vers <- saga_version(saga_bin)

    # generate saga help files in temporary directory
    help_path <- file.path(tempdir(), basename(tempfile()))
    dir.create(help_path)

    # version < 3.0.0 need to use working directory
    if (saga_vers > as.numeric_version("3.0.0")) {
      cmd <- paste0(paste(shQuote(saga_bin), "--create-docs="), help_path)
      msg <- system(cmd, intern = TRUE)
    } else {
      olddir <- getwd()
      setwd(help_path)
      msg <- system(paste(shQuote(saga_bin), "--docs"), intern = TRUE)
      setwd(olddir)
    }

    if (!is.null(attr(msg, "status"))) {
      rlang::abort()
    }

    # parse saga help files into nested list of libraries, tools and options
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
      
      # get library description
      lib_html <- rvest::read_html(paste(libdir, tool_names_file, sep = "/"))
      
      lib_description_html <- rvest::html_elements(
        lib_html,
        xpath = "/html/body/text()"
      )
      lib_description <- paste(
        rvest::html_text2(lib_description_html),
        collapse = " "
      )
      lib_description <- stringr::str_to_sentence(lib_description)
      
      # remove library description html from the tool html files
      tool_files <- tool_files[tool_files != tool_names_file]

      # create the library tools
      for (tool in tool_files) {
        tryCatch(
          expr = {
            html <- rvest::read_html(paste(libdir, tool, sep = "/"))
            options <- rvest::html_table(html, trim = TRUE)

            description_html <-
              rvest::html_elements(
                html,
                xpath = "/html/body/text()"
              )

            description <- paste(
              rvest::html_text2(description_html),
              collapse = " "
            )

            tool_information <- options[[1]]
            tool_options <- options[[length(options)]]

            if (!any(grepl("interactive", x = tool_information[[2]]))) {
              tool_config <- create_tool(
                tool_information = tool_information,
                tool_options = tool_options,
                description = description,
                html_file = tool
              )
              lib_name <- basename(libdir)
              lib_name <- tolower(lib_name)
              lib_name <- gsub(" ", "_", lib_name)
              libraries[[lib_name]][[tool_config$tool_name]] <- tool_config
            }
          },
          error = function(e) {
            e
          }
        )
      }
      
      tryCatch({
        attr(libraries[[basename(libdir)]], "description") <- lib_description  
      }, error = function(e) {
        e
      })
      
    }

    # remove tools that produce no outputs
    for (lib in names(libraries)) {
      tools <- names(libraries[[lib]])

      for (tool in tools) {
        params <- libraries[[lib]][[tool]]$params
        has_output <- sapply(params, function(x) if ("io" %in% names(x)) x$io)

        if (!"Output" %in% has_output) {
          libraries[[lib]] <- libraries[[lib]][!names(libraries[[lib]]) == tool]
        }
      }
    }

    # remove libraries with no tools
    for (lib in names(libraries)) {
      n_tools <- length(libraries[[lib]])

      if (n_tools == 0) {
        libraries <- libraries[names(libraries) != lib]
      }
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
      "pointcloud_tools",
      "io_pdal",
      "tin_viewer"
    )

    libraries <- libraries[!names(libraries) %in% invalid_libs]

    return(
      list(
        saga_cmd = saga_bin,
        saga_vers = saga_vers,
        raster_backend = raster_backend,
        vector_backend = vector_backend,
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
saga_configure <-
  function(senv,
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
      grid_cache_dir <- tempdir()
      grid_cache_dir <- gsub("//", "/", grid_cache_dir)
      grid_cache_dir <- gsub("\\\\", "/", grid_cache_dir)
    }

    # create configuration file if any arguments are supplied
    if ((grid_caching == TRUE | !is.null(cores)) &
      saga_vers >= as.numeric_version("4.0.0")) {
      saga_config <- tempfile(fileext = ".ini")

      msg <- processx::run(
        command = senv$saga_cmd,
        args = paste0("--create-config=", saga_config)
      )

      if (msg$status == 1) {
        error_msg <- paste(
          "There is a problem with generating the SAGA-GIS documentation,",
          "is the path to the saga_cmd binary set set?"
        )

        rlang::abort(error_msg)
      }

      saga_config_settings <-
        readChar(saga_config, file.info(saga_config)$size)

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
          "GRID_CACHE_MODE=[0-3]",
          "GRID_CACHE_MODE=1",
          saga_config_settings
        )

        saga_config_settings <- gsub(
          "GRID_CACHE_THRESHLOD=[0-9]*",
          paste0("GRID_CACHE_THRESHLOD=", grid_cache_threshold),
          saga_config_settings
        )

        saga_config_settings <- gsub(
          "GRID_CACHE_TMPDIR=;*",
          paste0("GRID_CACHE_TMPDIR=", shQuote(grid_cache_dir)),
          saga_config_settings
        )

        saga_config_settings <- gsub(
          "OMP_THREADS_MAX=[0-9]*",
          "OMP_THREADS_MAX=1",
          saga_config_settings
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
      saga_config <- NULL
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
#' @param raster_backend A character vector to specify the library to use for
#'   handling raster data. Supported options are "terra" or "stars".
#'   The default is "terra".
#' @param vector_backend A character to specify the library to use for handling
#'   vector data. Currently, "sf", "SpatVector" or "SpatVectorProxy" is
#'   supported. The default is "sf", however for large vector datasets, using
#'   the "SpatVectorProxy" backend from the `terra` package has performance
#'   advantages because it allows file-based which can reduce repeated
#'   reading/writing when passing data between R and SAGA-GIS.
#' @param raster_format A character to specify the default format used to save
#'   raster data sets that are produced by SAGA-GIS. Available options are one
#'   of "SAGA", "SAGA Compressed" or "GeoTIFF". The default is "SAGA".
#' @param vector_format A character to specify the default format used for
#'   vector data sets that are produced by SAGA-GIS, and also used to save
#'   in-memory objects to be read by SAGA-GIS. Available options are of of "ESRI
#'   Shapefile", "GeoPackage", or "GeoJSON". The default is "ESRI Shapefile" for
#'   SAGA versions < 7.0 and GeoPackage for more recent versions. Attempting to
#'   use anything other than "ESRI Shapefile" for SAGA-GIS versions < 7.0 will
#'   raise an error.
#' @param all_outputs A logical to indicate whether to automatically use
#'   temporary files to store all output data sets from each SAGA-GIS tool.
#'   Default = TRUE. This argument can be overridden by the `.all_outputs`
#'   parameter on each individual SAGA-GIS tool function that is generated by
#'   `Rsagacmd::saga_gis()`.
#' @param intern A logical to indicate whether to load the SAGA-GIS
#'   geoprocessing results as an R object, default = TRUE. For instance, if a
#'   raster grid is output by SAGA-GIS then this will be loaded as either as
#'   a `SpatRaster` or `stars` object, depending on the `raster_backend`
#'   setting that is used. Vector data sets are always loaded as `sf` objects,
#'   and tabular data sets are loaded as tibbles. The `intern` settings for the
#'   `saga` object can be overridden for individual tools using the `.intern`
#'   argument.
#' @param opt_lib A character vector with the names of a subset of SAGA-GIS
#'   libraries. Used to link only a subset of named SAGA-GIS tool libraries,
#'   rather than creating functions for all available tool libraries.
#' @param temp_path The path to use to store any temporary files that are
#'   generated as data is passed between R and SAGA-GIS. If not specified, then
#'   the system `base::tempdir()` is used.
#' @param verbose Logical to indicate whether to output all messages made during
#'   SAGA-GIS commands to the R console. Default = FALSE. This argument can be
#'   overriden by using the `.verbose` argument on each individual SAGA-GIS tool
#'   function that is generated by `Rsagacmd::saga_gis()`.
#'
#' @return A S3 `saga` object containing a nested list of functions for SAGA-GIS
#'   libraries and tools.
#'
#' @export
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' library(Rsagacmd)
#' library(terra)
#'
#' saga <- saga_gis()
#'
#' # Alternatively initialize a saga object using file caching to handle large
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
#'
#' # Initialize a saga object but do not automatically save all results to
#' # temporary files to load into R. Use this if you are explicitly saving each
#' # output because this will save disk space by not saving results from tools
#' # that output multiple results that you may be want to keep.
#' saga <- saga_gis(all_outputs = FALSE)
#' }
saga_gis <-
  function(saga_bin = NULL,
           grid_caching = FALSE,
           grid_cache_threshold = 100,
           grid_cache_dir = NULL,
           cores = NULL,
           raster_backend = "terra",
           vector_backend = "sf",
           raster_format = "SAGA",
           vector_format = c("ESRI Shapefile", "GeoPackage"),
           all_outputs = TRUE,
           intern = TRUE,
           opt_lib = NULL,
           temp_path = NULL,
           verbose = FALSE) {

    senv <- saga_env(saga_bin, opt_lib, raster_backend, vector_backend)
    senv$verbose <- verbose
    senv$all_outputs <- all_outputs
    senv$intern <- intern

    senv[["saga_config"]] <-
      saga_configure(
        senv = senv,
        grid_caching = grid_caching,
        grid_cache_threshold = grid_cache_threshold,
        grid_cache_dir = grid_cache_dir,
        cores = cores,
        saga_vers = senv$saga_vers
      )

    # check raster formats
    if (!raster_format %in% names(supported_raster_formats)) {
      rlang::abort(paste(
        "`raster_format` must be one of:",
        supported_raster_formats
      ))
    }

    # SAGA versions < 7.5 only allow direct writing to native formats
    if (senv$saga_vers < 7.5 & !raster_format %in% c("SAGA", "SAGA Compressed")) {
      rlang::abort(paste(
        "SAGA versions < 7.5 only allow directly writing of",
        "raster data via the 'SAGA' or 'SAGA Compressed' raster formats"
      ))
    }

    if (senv$saga_vers < 5.0 & raster_format != "SAGA") {
      rlang::abort("SAGA versions < 5.0 only allow the 'SAGA' raster format")
    }

    senv$raster_format <- supported_raster_formats[raster_format]

    # check vector formats
    if (!all(vector_format %in% names(supported_vector_formats))) {
      rlang::abort(paste(
        "`vector_format` must be one of:",
        supported_vector_formats
      ))
    }

    if (all(vector_format == c("ESRI Shapefile", "GeoPackage")) &
      senv$saga_vers < 7.0) {
      vector_format <- "ESRI Shapefile"
    } else {
      vector_format <- "GeoPackage"
    }

    if (senv$saga_vers < 7.0 & vector_format != "ESRI Shapefile") {
      rlang::abort(paste(
        "SAGA versions < 7.0 only allow directly writing of",
        "vector data via the 'ESRI Shapefile' vector format"
      ))
    }

    senv$vector_format <- supported_vector_formats[vector_format]

    # check path to temporary directory if assigned
    if (!is.null(temp_path)) {
      senv[["temp_path"]] <- temp_path
    } else {
      senv[["temp_path"]] <- tempdir()
    }

    # dynamically create functions
    tool_libraries <- list()

    for (lib in names(senv$libraries)) {
      toolnames <- list()

      for (tool in names(senv$libraries[[lib]])) {
        params <- senv$libraries[[lib]][[tool]][["params"]]

        tryCatch(
          expr = {
            # create function body
            body <- create_function(lib = lib, tool = tool)

            # create list of arguments and default values
            args <- lapply(params, function(x) {
              NULL
            })
            args <-
              c(
                args,
                list(
                  .intern = NULL,
                  .all_outputs = NULL,
                  .verbose = NULL
                )
              )

            # coerce arguments to comma-separated character
            args <- mapply(function(k, v) {
              paste(k, deparse(v), sep = " = ")
            },
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
            tool_libraries[[lib]] <-
              append(tool_libraries[[lib]], func)
            toolnames <- append(toolnames, tool)
            names(tool_libraries[[lib]]) <- toolnames
            class(tool_libraries[[lib]]) <- "saga_library"
          },
          error = function(e) {
            warning(
              paste0(
                "Problem parsing SAGA-GIS library = ",
                lib,
                "; and tool = ",
                tool
              ),
              call. = FALSE
            )
          }
        )
      }
    }

    #  return S3 saga object
    structure(tool_libraries,
      class = "saga"
    )
  }
