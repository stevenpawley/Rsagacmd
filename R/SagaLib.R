#' Parses valid SAGA-GIS libraries and tools into a nested list of functions
#'
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing
#' all libraries, tools and options from the help files into a nested list of
#' library, module and options, that are contained within an saga environment
#' object object. Intended to be used internally by \code{\link{saga_gis}}
SagaLib = R6::R6Class(
  classname = "SagaLib",
  public = list(
    saga_cmd = NULL,
    saga_vers = NULL,
    raster_backend = NULL,
    vector_backend = NULL,
    library = NULL,
    verbose = FALSE,
    all_outputs = FALSE,
    intern = TRUE,
    config = NULL,
    temp_path = tempdir(),
    saga_bin = NULL,
    raster_format = NULL,
    vector_format = NULL,
    
    #' @description
    #' Create a new SagaLib R6 class object.
    #' @param saga_bin An optional character vector to specify the path to the
    #' saga_cmd executable. Otherwise the function will perform a search for
    #' saga_cmd.
    #' @param opt_lib A character vector of a subset of SAGA-GIS tool libraries to
    #' generate dynamic functions that map to each tool. Used to save time if you
    #' only want to import a single library.
    #' @param raster_backend A character vector to specify the library to use for
    #' handling raster data. Currently, either "terra" or "stars" is
    #' supported. The default is "terra".
    #' @param vector_backend A character to specify the library to use for handling
    #' vector data. Currently, either "sf", "SpatVector" or "SpatVectorProxy" is
    #' supported. The default is "sf".
    initialize = function(saga_bin = NULL, raster_backend = "terra", 
                          vector_backend = "sf") {
      # check valid backends
      if (!raster_backend %in% c("terra", "stars")) {
        stop("The `raster_backend` must be one of 'terra' or 'stars'")
      }
      if (!vector_backend %in% c("sf", "SpatVector", "SpatVectorProxy")) {
        stop("The `vector_backend` must be one of 'sf', 'SpatVector' or 'SpatVectorProxy'")
      }
      self$raster_backend = raster_backend
      self$vector_backend = vector_backend
      
      # set the path to the saga_cmd binary
      if (is.null(saga_bin)) {
        saga_bin = search_saga()
      }
      if (nchar(Sys.which(names = saga_bin)) == 0) {
        stop("The supplied path to the saga_cmd binary is not correct")
      }
      self$saga_bin = saga_bin
      
      # set the saga gis version
      self$saga_vers = saga_version(saga_bin)
    },
    
    #' @description
    #' Create the binding to the SAGA-GIS command line binary
    create_binding = function(opt_lib = NULL) {
      # generate the documentation
      help_path = file.path(tempdir(), basename(tempfile()))
      dir.create(help_path)
      
      if (self$saga_vers > as.numeric_version("3.0.0")) {
        cmd = paste0(paste(shQuote(self$saga_bin), "--create-docs="), help_path)
        msg = system(cmd, intern = TRUE)
      } else {
        olddir = getwd()
        setwd(help_path)
        msg = system(paste(shQuote(self$saga_bin), "--docs"), intern = TRUE)
        setwd(olddir)
      }
      if (!is.null(attr(msg, "status"))) {
        stop()
      }

      # parse saga help files into nested list of libraries, tools and options
      docs_libraries = list.dirs(path = help_path)
      docs_libraries = docs_libraries[2:length(docs_libraries)]
      
      if (!is.null(opt_lib)) {
        docs_libraries =
          docs_libraries[which(basename(docs_libraries) %in% opt_lib)]
      }
      
      for (libdir in docs_libraries) {
        tool_files = list.files(path = libdir)
        
        # get module names from file and remove from parameter list
        tool_names_file = tool_files[which.min(nchar(tool_files))]
        
        # get library description
        lib_html = rvest::read_html(paste(libdir, tool_names_file, sep = "/"))
        lib_description_html = rvest::html_elements(lib_html, xpath = "/html/body/text()")
        lib_description = paste(rvest::html_text2(lib_description_html), collapse = " ")
        lib_description = stringr::str_to_sentence(lib_description)
        
        # remove library description html from the tool html files
        tool_files = tool_files[tool_files != tool_names_file]
        
        # create the library tools
        for (tool in tool_files) {
          tryCatch(
            expr = {
              # parse the html documents
              html = rvest::read_html(paste(libdir, tool, sep = "/"))
              options = rvest::html_table(html, trim = TRUE)
              description_html = rvest::html_elements(html, xpath = "/html/body/text()")
              description = paste(rvest::html_text2(description_html), collapse = " ")
              
              tool_information = options[[1]]
              tool_options = options[[length(options)]]
              
              if (!any(grepl("interactive", x = tool_information[[2]]))) {
                tool_config = SagaTool$new(
                  tool_information = tool_information,
                  tool_options = tool_options,
                  description = description,
                  html_file = tool
                )
                lib_name = gsub(" ", "_", tolower(basename(libdir)))
                self$library[[lib_name]][[tool_config$tool_name]] = tool_config
              }
            },
            error = function(e) {
              e
            }
          )
        }
        
        tryCatch({
          attr(self$library[[basename(libdir)]], "description") = lib_description
        }, error = function(e) {
          e
        })
        
      }
    },
    
    #' @description
    #' Clean any invalid tools and libraries from the bindings, for example
    #' those that cannot be used in a non-interactive setting
    clean_library = function() {
      # remove tools that produce no outputs
      for (lib in names(self$library)) {
        tools = names(self$library[[lib]])
        for (tool in tools) {
          params = self$library[[lib]][[tool]]$parameters
          has_output = sapply(params$parameters, function(x) if ("io" %in% names(x)) x$io)
          if (!"Output" %in% has_output) {
            self$library[[lib]] = self$library[[lib]][!names(self$library[[lib]]) == tool]
          }
        }
      }
      
      # remove libraries with no tools
      for (lib in names(self$library)) {
        n_tools = length(self$library[[lib]])
        if (n_tools == 0) {
          self$library = self$library[names(self$library) != lib]
        }
      }
      
      # remove invalid libraries for saga_cmd
      invalid_libs = list(
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
      self$library = self$library[!names(self$library) %in% invalid_libs]
    }
  )
)
