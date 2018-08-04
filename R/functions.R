#' Return the installed version of SAGA-GIS.
#'
#' Intended to be used internally by \code{\link{sagaEnv}}
#'
#' @param saga_cmd character. Path of the saga_cmd binary
#'
#' @return numeric_version. Version of SAGA-GIS found at the cmd path
sagaVersion = function(saga_cmd) {
  # system call saga_cmd to display version on console
  saga_version = system(paste(shQuote(saga_cmd), '--version'), intern = T)[1]
  
  saga_version = stringr::str_extract(saga_version, '\\d.\\d.\\d.')
  saga_version = stringr::str_trim(saga_version)
  
  return (as.numeric_version(saga_version))
}


#' Automatic search for the path to a SAGA-GIS installation
#'
#' Returns the path to the saga_cmd executable On windows, automatic searching
#' will occur first in 'C:/Program Files/SAGA-GIS'; 'C:/Program Files
#' (x86)/SAGA-GIS'; 'C:/SAGA-GIS'; 'C:/OSGeo4W'; and 'C:/OSGeo4W64'. On linux or
#' OS X, saga_cmd is usually included in PATH, if not an automatic search is
#' performed in the '/usr' folder. If multiple versions of SAGA-GIS are
#' installed on the system, the path to the newest version is returned.
#'
#' @return character. Path to installed saga_cmd binary
#' @export
sagaSearch = function() {
  
  # check to see if saga_cmd is recognized (i.e. has been added to path)
  saga_cmd = ifelse(nchar(Sys.which(names = 'saga_cmd')) > 0, 'saga_cmd', NA)
  
  # otherwise search for saga_cmd in usual install locations
  if (is.na(saga_cmd)) {
    
    # define search paths
    if (Sys.info()["sysname"] == "Windows") {
      search_paths = c(
        'C:/Program Files/SAGA-GIS/',
        'C:/Program Files (x86)/SAGA-GIS/',
        'C:/SAGA-GIS/',
        'C:/OSGeo4W/',
        'C:/OSGeo4W64/',
        'C:/Program Files/SAGA-GIS/')
      saga_executable = 'saga_cmd.exe'
      
    } else if (Sys.info()["sysname"] == "Linux") {
      search_paths = c('/usr/')
      saga_executable = 'saga_cmd$'
      
    } else if (Sys.info()["sysname"] == "Darwin") {
      search_paths = c('/usr/local/bin/',
                       '/Applications/QGIS/Contents/MacOS/bin/')
      saga_executable = 'saga_cmd$'
    }
    
    # search for saga_cmd executable
    saga_cmd = lapply(search_paths, function(x)
      list.files(
        path = x,
        pattern = saga_executable,
        recursive = TRUE,
        full.names = TRUE))
    
    saga_cmd = saga_cmd[which(length(saga_cmd) > 0)] # remove empty results
    
    if (length(saga_cmd) == 1)
      saga_cmd = unlist(saga_cmd)
  }
  
  # error is saga_cmd not found
  if (length(saga_cmd) == 0) {
    message(paste('SAGA-GIS installation not found. Need to supply a valid path',
                  'to the saga_cmd executable'))
    return(NULL)
    
    # automatically use newest version if multiple installations are found
  } else if (length(saga_cmd) > 1) {
    message('Multiple installations of SAGA-GIS were found at:')
    message(saga_cmd)
    message(paste('Choosing newest version. Manually specify the location when',
                  'calling SAGA() to use an older version'))
    saga_version = list()
    for (saga_inst in saga_cmd)
      saga_version = append(saga_version, sagaVersion(saga_inst))
    saga_cmd = saga_cmd[which(saga_version == max(saga_version))]
  }
  
  return (saga_cmd)
}


#' Parses valid SAGA-GIS libraries and tools into a nested list of functions
#' 
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing
#' all libraries, tools and options from the help files into a nested list of
#' library, module and options, that are contained within an sagaInstallation
#' object. Intended to be used internally by \code{\link{sagaGIS}}
#'
#' @param saga_bin character. Optional path to saga_cmd executable
#' @param opt_lib character vector. List of subset of SAGA-GIS tool libraries
#' to generate links to.
#'
#' @return sagaInstallation object. Contains paths, settings and a nested list
#'   of libaries tools and options
sagaEnv = function(saga_bin = NA, opt_lib = NA) {
  
  if (is.na(saga_bin)) {
    saga_bin = sagaSearch()
  } else {
    # check that supplied saga_bin location is correct
    if (nchar(Sys.which(names = saga_bin)) == 0)
      stop('The supplied path to the saga_cmd binary is not correct',
           call. = FALSE)
  }
  
  # detect saga version
  saga_version = sagaVersion(saga_bin)
  
  # generate SAGA help files in temporary directory
  help_path = file.path(tempdir(), basename(tempfile()))
  dir.create(help_path)
  
  if (saga_version > as.numeric_version('3.0.0')) {
    msg = system(
      paste0(paste(shQuote(saga_bin), '--create-docs='), help_path), intern=T)
  } else {
    setwd(help_path)
    msg = system(paste(shQuote(saga_bin), '--docs'), intern=T)
  }
  
  if (!is.null(attr(msg, "status"))) {
    cat(msg, sep = '\n')
    stop()
  }
  
  # parse SAGA help files into nested list of libraries, tools and options
  docs_libraries = list.dirs(path = help_path)
  docs_libraries = docs_libraries[2:length(docs_libraries)]
  
  if (!is.na(opt_lib))
    docs_libraries = docs_libraries[which(basename(docs_libraries) %in% opt_lib)]
  
  libraries = list()
  
  for (libdir in docs_libraries) {
    tool_files = list.files(path = libdir)
    
    # get module names from file and remove from parameter list
    tool_names_file = tool_files[which.min(nchar(tool_files))]
    tool_files = tool_files[tool_files != tool_names_file]
    
    for (tool in tool_files) {
      # check to see if file is not emptry
      f = file(paste(libdir, tool, sep = '/'), open = "rb")
      
      if (length(readLines(f, warn = F)) > 1) {
        # read module options tables
        options = XML::readHTMLTable(
          doc = paste(libdir, tool, sep = '/'), header = T)
        
        # ignore interactive tools
        if (!any(grepl('interactive', x = options[[1]][,2]))) {
          
          # create valid tool names
          valid_toolname = colnames(options[[1]])[2]
          valid_toolname = gsub("^[0-9]+", '', valid_toolname) # remove digits from start of tool name
          valid_toolname = gsub(" ", "_", valid_toolname) # replace spaces with underscores
          valid_toolname = gsub("\\(", "", valid_toolname) # remove parenthesis
          valid_toolname = gsub("\\)", "", valid_toolname) # remove parenthesis
          valid_toolname = gsub("'", "", valid_toolname) # remove single quotations
          valid_toolname = gsub(",", "_", valid_toolname) # remove commas
          valid_toolname = gsub("/", "_", valid_toolname) # replace forward slash with underscore
          valid_toolname = gsub("-", "_", valid_toolname) # replace minus with underscore
          valid_toolname = gsub(":", "_", valid_toolname) # replace colon with underscore
          valid_toolname = gsub("\\[", "_", valid_toolname) # replace square brackets with underscore
          valid_toolname = gsub("\\]", "_", valid_toolname) # replace square brackets with underscore
          valid_toolname = gsub("&", "_", valid_toolname) # replace ampersand with underscore
          valid_toolname = gsub('_+', '_', valid_toolname) # replace multiple underscores due to above with single _
          valid_toolname = gsub("^_+", '', valid_toolname) # remove underscore from start of tool name
          
          # parse parameter table into a dataframe
          toolName = colnames(options[[1]])[2]
          options = options[[length(options)]] # tool options are last table
          
          # strip input, output, options lines in table
          options = options[which(apply(options[, 2:5], 1, function(x)
            any(is.na(x))) == FALSE), ]
          
          options['IO'] = NA
          options['Feature'] = NA
          
          # inputs/outputs
          options[grep("input", options$Type), 'IO'] = 'Input'
          options[grep("output", options$Type), 'IO'] = 'Output'
          
          # grids
          options[grep("Grid", options$Type), 'Feature'] = 'Grid'
          options[grep("Grid list", options$Type), 'Feature'] = 'Grid list'
          
          # shapes
          options[grep("Shapes", options$Type), 'Feature'] = 'Shape'
          options[grep("Shapes list", options$Type), 'Feature'] = 'Shapes list'
          
          # tables
          options[grep("Table", options$Type), 'Feature'] = 'Table'
          options[grep("Static table", options$Type), 'Feature'] = 'Table'
          options[grep("Table list", options$Type), 'Feature'] = 'Table list'
          
          # paths
          options[grep("File path", options$Type), 'Feature'] = 'File path'
          
          # fields
          options[grep("field", options$Type), 'Feature'] = 'Table field'
          options[grep("Integer", options$Type), 'Feature'] = 'Integer'
          options[grep("Choice", options$Type), 'Feature'] = 'Choice'
          options[grep("Floating point", options$Type), 'Feature'] = 'numeric'
          options[grep("Boolean", options$Type), 'Feature'] = 'logical'
          options[grep("Long text", options$Type), 'Feature'] = 'character'
          options[grep("Text", options$Type), 'Feature'] = 'character'
          
          # exceptions
          if (toolName == 'Export GeoTIFF' | toolName == 'Export Raster') {
            options[grep("File path", options$Type), 'IO'] = 'Output'
            options[grep("File path", options$Type), 'Feature'] = 'Grid'
            
          } else if (toolName == 'Export Shapes' | toolName == 'Export Shapes to KML') {
            options[grep("File path", options$Type), 'IO'] = 'Output'
            options[grep("File path", options$Type), 'Feature'] = 'Shapes'
            
          } else if (toolName == 'Clip Grid with Rectangle') {
            options[grep("Data Object", options$Type), 'Feature'] = 'Grid'
          }
          
          # replace saga tool arguments that start with a numeric
          identifiers = options$Identifier
          numeric_identifiers = which(grepl("[[:digit:]]", substr(identifiers, 1, 1)) == TRUE)
          if (length(numeric_identifiers) > 0)
            levels(identifiers)[levels(identifiers) == identifiers[[numeric_identifiers]]] = sub("^.", "", identifiers[numeric_identifiers])
          identifiers = gsub(" ", "_", identifiers)
          options['identifierR'] = identifiers
          
          # clean-up constraints text
          options$Constraints[options$Constraints == ''] = NA
          options$Constraints = gsub('Available Choices:', '', options$Constraints) # remove text
          options$Constraints = gsub('^\n', '', options$Constraints) # remove line break at start of text
          options$Constraints = gsub('\n', ';', options$Constraints) # replace line seps with ;
          
          # extract default values for tool parameters
          options['Default'] = NA
          default_src = strsplit(options$Constraints, 'Default: ')
          for (i in seq_len(length(default_src)))
            if (all(!is.na(default_src[[i]])))
              options[i, 'Default'] = suppressWarnings(as.numeric(default_src[[i]][2]))
          
          # extract minimum values for tool parameters
          options['Minimum'] = NA
          constraints_split = strsplit(options$Constraints, ';')
          for (i in seq_len(length(constraints_split))){
            constraints_line = constraints_split[i][[1]]
            
            if (any(grepl('Minimum:', constraints_line))) {
              constraints_min = constraints_line[grepl('Minimum:', constraints_line)]
              constraints_min = strsplit(constraints_min, 'Minimum: ')[[1]][2]
              options$Minimum[i] = suppressWarnings(as.numeric(constraints_min))
            }
          }
          
          # extract maximum values for tool parameters
          options['Maximum'] = NA
          constraints_split = strsplit(options$Constraints, ';')
          for (i in seq_len(length(constraints_split))){
            constraints_line = constraints_split[i][[1]]
            
            if (any(grepl('Maximum:', constraints_line))) {
              constraints_max = constraints_line[grepl('Maximum:', constraints_line)]
              constraints_max = strsplit(constraints_max, 'Maximum: ')[[1]][2]
              options$Maximum[i] = suppressWarnings(as.numeric(constraints_max))
            }
          }
          
          # add parameter options to nested list
          libraries[[basename(libdir)]][[valid_toolname]] = list(
            options=options, tool_cmd=toolName)
        } # interactive
      } # length of file
      
      close(f)
    }
  }
  
  # remove tools that produce no outputs
  for (lib in names(libraries)){
    tools = names(libraries[[lib]])
    for (tool in tools){
      selected_tool = libraries[[lib]][[tool]]$options
      
      if(nrow(selected_tool[which(selected_tool$IO == 'Output'),]) == 0)
        libraries[[lib]] = libraries[[lib]][!names(libraries[[lib]]) == tool]
    }
  }
  
  # remove libraries with no tools
  for (lib in names(libraries)){
    n_tools = length(libraries[[lib]])
    if (n_tools == 0)
      libraries = libraries[names(libraries) != lib]
  }
  
  # remove invalid libraries for saga_cmd
  invalid_libs = list(
    'db_odbc',
    'db_pgsql',
    'docs_html',
    'docs_pdf',
    'garden_3d_viewer',
    'garden_games',
    'garden_learn_to_program',
    'garden_webservices',
    'grid_calculus_bsl',
    'pointcloud_viewer',
    'tin_viewer'
  )
  
  libraries = libraries[!names(libraries) %in% invalid_libs]
  
  return(structure(
    list(
      saga_cmd = saga_bin,
      saga_version = saga_version,
      libraries = libraries
    ),
    class = 'sagaInstallation'
  ))
}


#' Generates a custom saga_cmd configuration file
#' 
#' Creates and edits a saga_cmd coniguration file in order to change saga_cmd
#' settings related to file caching and number of available processor cores.
#' Intended to be used internally by \code{\link{sagaGIS}}.
#'
#' @param senv sagaInstallation object. SAGA-GIS environment and settings
#' @param grid_caching logical. Optionally use file caching
#' @param grid_cache_threshlod numeric. Threshold (in Mb) before file caching
#'   for loaded raster data is activated
#' @param grid_cache_dir character. Path to directory for temporary files
#' @param cores numeric. Maximum number of processing cores. Needs to be set to
#'   1 if file caching is activated
#' @param sagaversion numeric_version. Version of SAGA-GIS. The generation of a
#'   saga_cmd configuration file is only valid for versions > 4.0.0
#'
#' @return character. Path to custom saga_cmd initiation file
sagaConfigure = function(senv,
                         grid_caching = FALSE,
                         grid_cache_threshlod = 100,
                         grid_cache_dir = NA,
                         cores = NA,
                         sagaversion) {
  # some checks
  if (missing(senv))
    stop('senv parameter is missing')
  
  if (is.na(grid_cache_dir))
    grid_cache_dir = gsub('\\\\', '/', tempdir())
  
  # create configuration file if any arguments are supplied
  if ((grid_caching == TRUE | !is.na(cores)) &
      sagaversion >= as.numeric_version('4.0.0')){
    
    saga_config = tempfile(fileext = '.ini')
    msg = system(paste(
      shQuote(senv$saga_cmd),
      paste0('--create-config=', saga_config)
    ), intern = T)
    saga_config_settings = readChar(saga_config, file.info(saga_config)$size)
    
    # configuration for custom number of cores
    if (!missing(cores) & grid_caching == FALSE) {
      saga_config_settings = gsub(
        'OMP_THREADS_MAX=[0-9]*',
        paste0('OMP_THREADS_MAX=', cores),
        saga_config_settings)
      
      # configuration for grid caching
    } else if (grid_caching == TRUE) {
      if (cores > 1 | is.na(cores)) {
        message('SAGA-GIS file caching is not thread-safe. Setting cores = 1')
        cores = 1
      }
      
      saga_config_settings = gsub(
        'GRID_CACHE_MODE=[0-3]', 'GRID_CACHE_MODE=1', saga_config_settings)
      saga_config_settings = gsub(
        'GRID_CACHE_THRESHLOD=[0-9]*',
        paste0('GRID_CACHE_THRESHLOD=', grid_cache_threshlod),
        saga_config_settings)
      saga_config_settings = gsub(
        'GRID_CACHE_TMPDIR=;*',
        paste0('GRID_CACHE_TMPDIR=', shQuote(gsub('\\\\', '/', tempdir()))),
        saga_config_settings)
      saga_config_settings = gsub(
        'OMP_THREADS_MAX=[0-9]*', 'OMP_THREADS_MAX=1', saga_config_settings)
    }
    
    # write configuration file
    writeChar(saga_config_settings, saga_config)
    
  } else if ((grid_caching == TRUE | !is.na(cores)) &
             sagaversion < as.numeric_version('4.0.0')){
    message(
      paste('Cannot enable grid caching or change number cores for SAGA-GIS',
            'versions < 4.0.0. Please use a more recent version of SAGA-GIS'))
  }
  
  if(!exists('saga_config'))
    saga_config = NA
  
  return (saga_config)
}


#' Initiate a SAGA-GIS geoprocessor object
#'
#' Dynamically generates functions to all valid SAGA-GIS libraries and tools.
#' These functions are stored within a saga S3 object as a named list of 
#' functions.
#'
#' @param saga_bin character, optional. Path to saga_cmd executable. If this
#'   argument is not supplied then an automatic search for the saga_cmd
#'   executable will be performed.
#' @param grid_caching logical, optional. Use file caching in saga_cmd
#'   geoprocessing operations for rasters that are too large to fit into memory.
#' @param grid_cache_threshlod numeric, optional. Threshold (in Mb) before file
#'   caching is activated for loaded raster data.
#' @param grid_cache_dir character, optional. Path to directory for temporary
#'   files generated by file caching.
#' @param cores numeric. Maximum number of processing cores. Needs to be set to
#'   1 if file caching is activated.
#' @param opt_lib character vector. Names of SAGA-GIS libraries. Used to
#' link only a subset of named SAGA-GIS tool libraries, rather than
#' creating functions for all available tool libraries.
#' @return S3 saga object containing a nested list of functions for SAGA-GIS
#' libraries and tools
#' @export
#' @import raster
#' @examples 
#' \dontrun{
#' # Initialize a saga object
#' saga = sagaGIS()
#' 
#' # Alternatively intialize a saga object using file caching to handle large
#' # raster files
#' saga = sagaGIS(grid_caching = TRUE, grid_cache_threshlod = 250, cores = 1)
#' 
#' # Example terrain analysis
#' # Generate a random DEM
#' dem = saga$grid_calculus$Random_Terrain(RADIUS = 100)
#' 
#' # Use Rsagacmd to calculate the Terrain Ruggedness Index
#' tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem)
#' plot(tri)
#' 
#' # Optionally run command and do not load result as an R object
#' saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem, intern = FALSE)
#' }
sagaGIS = function(saga_bin = NA,
                   grid_caching = FALSE,
                   grid_cache_threshlod = 100,
                   grid_cache_dir = NA,
                   cores = NA,
                   opt_lib = NA) {
  senv = sagaEnv(saga_bin, opt_lib)
  senv[['saga_config']] = sagaConfigure(
    senv,
    grid_caching,
    grid_cache_threshlod,
    grid_cache_dir,
    cores,
    senv$saga_version
  )
  
  # dynamically create functions
  tool_libraries = list()
  for (lib in names(senv$libraries)) {
    toolnames = list()
    
    for (tool in names(senv$libraries[[lib]])) {
      # define tool arguments
      tool_options = senv$libraries[[lib]][[tool]][['options']]
      tool_cmd = senv[['libraries']][[lib]][[tool]][['tool_cmd']]
      args = paste0(tool_options$identifierR, collapse = ',')
      
      # define function body
      # deparse tool settings into function and create code to call sagaExecute
      body = paste(
        paste0('args = as.list(environment())'),
        paste0('lib = ', deparse(lib)),
        paste0('tool = ', deparse(tool)),
        "
        # remove intern and help from saga args list
        if ('intern' %in% names(args))
        args = args[-which(names(args) == 'intern')]
        
        # call the saga geoprocessor
        saga_results = sagaExecute(lib, tool, senv, intern, args)
        return (saga_results)
        ",
        sep = "\n"
      )
      
      # parse function
      # here we are creating functions by evaluating expressions
      # these functions have some additional attributes used to printing tool
      # information etc.
      
      # The functions are stored as a nested list which is returned as a saga
      # class. These functions exist within the environment of the class object, 
      # along with the variables/tools/libraries needed by each
      # function. Thus, when each function is called, it can find the variables
      # that it needs from within its own environment
      tryCatch(
        expr = {
          func_code = paste0('function(', args, ', intern = TRUE', ') {',
                             '\n', body, '\n', '}')
          func = structure(
            eval(expr = parse(text = func_code)),
            lib = lib,
            tool = tool,
            class = 'sagaTool'
          )
          
          tool_libraries[[lib]] = append(tool_libraries[[lib]], func)
          toolnames = append(toolnames, tool)
        },
        error = function(e)
          warning(
            paste0("Problem parsing SAGA-GIS library = ", lib, "; and tool = ", tool),
            call. = FALSE
          )
      )
    }
    names(tool_libraries[[lib]]) = toolnames
  }
  
  # clean local environment
  remove(args, body, cores, func, func_code, grid_cache_dir,
         grid_cache_threshlod, grid_caching, lib, saga_bin, tool, tool_cmd,
         tool_options, toolnames)
  
  #  return S3 saga object
  structure(
    tool_libraries,
    class = 'saga'
  )
}


#' Generic function to display help and usage information for any SAGA-GIS tool
#'
#' @param x sagaTool object
#' @param ... Additional arguments to pass to print. Currently not used.
#'
#' @return NULL
#' @export
#' @examples 
#' \dontrun{
#' # Intialize a saga object
#' saga = sagaGIS()
#' 
#' # Display usage information on a tool
#' print(saga$ta_morphometry$Slope_Aspect_Curvature)
#' 
#' # Or simply:
#' saga$ta_morphometry$Slope_Aspect_Curvature
#' }
print.sagaTool = function(x, ...) {
  
  lib = attr(x, 'lib')
  tool = attr(x, 'tool')
  
  # get local environment of sagaGIS object
  env = environment(x)
  tool_options = env$senv$libraries[[lib]][[tool]][['options']]
  tool_options = tool_options[, c(
    'identifierR', 'Name', 'Type', 'Description', 'Constraints')]
  
  cat(paste0('Help for library = ', lib, '; tool = ', tool, ':', '\n'))
  for (i in 1:nrow(tool_options)) {
    cat(paste0('Name of tool: ', tool_options[i, 'Name']), '\n')
    cat(paste0('Identifier: ', tool_options[i, 'identifierR'], '\n'))
    cat(paste0('Type: ', tool_options[i, 'Type'], '\n'))
    cat(paste0('Description: ', tool_options[i, 'Description'], '\n'))
    cat(paste0('Constraints: ', tool_options[i, 'Constraints'], '\n'))
    cat('\n')
  }
}


#' Function to execute SAGA-GIS commands through the command line tool.
#'
#' Intended to be used internally
#'
#' @param lib character. Name of SAGA-GIS library to execute
#' @param tool character. Name of SAGA-GIS tool to execute
#' @param senv sagaInstallation object
#' @param intern logical. Load outputs as R objects
#' @param ... Named arguments and values for SAGA tool
#'
#' @return Output of SAGA-GIS tool loaded as an R object
#'   (RasterLayer/sf/dataframe)
sagaExecute = function(lib, tool, senv, intern = TRUE, ...) {
  args = c(...)
  
  # sagaInstallation settings
  tool_options = senv$libraries[[lib]][[tool]][['options']]
  tool_cmd = senv$libraries[[lib]][[tool]][['tool_cmd']]
  saga_cmd = senv$saga_cmd
  saga_config = senv$saga_config
  
  # match the identifierR to the identifier used by SAGA-GIS
  arg_names = names(args)
  arg_names = merge(
    x = data.frame(arg_names, stringsAsFactors = FALSE),
    y = tool_options, by.x='arg_names', by.y = 'identifierR',
    sort=FALSE)$Identifier
  args = stats::setNames(args, arg_names)
  
  # strip missing arguments and update arg_names
  args[args == ''] = NA
  args = args[!is.na(args)]
  arg_names = names(args)
  
  # save loaded R objects to files for SAGA-GIS to access
  for (i in seq_along(args)) {
    # if list split list into separate files
    if (any(class(args[[i]]) == "list")){
      for (j in seq_along(args[[i]]))
        args[[i]][[j]] = RtoSAGA(args[[i]][[j]])
    } else {
      args[[i]] = RtoSAGA(args[[i]])
    }
  }
  
  # collapse argument values that are lists into a semi-colon separated strings
  for (i in seq_along(args))
    if (length(args[[i]]) > 1)
      args[[i]] = paste(args[[i]], collapse = ';')
  
  # replace sdat fileext used by raster package with sgrd used by SAGA
  args = gsub('.sdat', '.sgrd', args)
  
  # determine the SAGA output parameters that have been specified as function args
  spec_ind = which(tool_options$IO == "Output" & tool_options$Identifier %in% arg_names)
  n_outputs = length(spec_ind)
  spec_out = tool_options[spec_ind, ]
  
  # process the specified arguments
  if (n_outputs > 0) {
    # create dataframe of containing tool settings merged with the Rsagacmd
    # function specified arguments
    spec_out = merge(x=spec_out, y=cbind.data.frame(arg_names, args),
                     by.x='Identifier', by.y='arg_names', sort=FALSE)
    
    # convert factors to character
    spec_out$args = as.character(spec_out$args)
  }
  
  # determine any outputs that have not been specified as function args
  req_out = tool_options[which(tool_options$IO == "Output"), ]
  unspec_ind = which(!(req_out$Identifier %in% spec_out$Identifier))
  n_temps = length(unspec_ind)
  
  # use tempfiles if any outputs are not specified
  if (n_temps > 0) {
    unspec_out = req_out[unspec_ind,]
    unspec_out['args'] = NA
    
    for (i in 1:nrow(unspec_out)) {
      if (unspec_out[i, 'Feature'] %in% c('Grid', 'Grid list', 'Raster')){
        tfile = tempfile(fileext = '.sgrd')
        unspec_out[i, 'args'] = tfile
      } else if (unspec_out[i, 'Feature'] %in% c('Shape', 'Shapes list')){
        tfile = tempfile(fileext = '.shp')
        unspec_out[i, 'args'] = tfile
      } else if (unspec_out[i, 'Feature'] == 'Table'){
        tfile = tempfile(fileext = '.csv')
        unspec_out[i, 'args'] = tfile
      }
      
      pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, tfile)
    }
    
    # update the arguments and expected outputs for tool
    spec_out = rbind(spec_out, unspec_out)
    arg_names = c(as.character(arg_names), as.character(spec_out$Identifier))
    args = c(args, spec_out$args)
  }
  
  # add saga_cmd arguments to the command line call
  flags = '--flags=p'
  
  # create string with argument values within quotes
  quote_type = ifelse(Sys.info()["sysname"] == "Windows", "cmd", "csh")
  params = shQuote(string = args, type = quote_type)
  
  # prepare system call
  param_string = paste("-", arg_names, ':', params, sep = "", collapse = " ")
  
  if (!is.na(saga_config)) {
    config =  paste('-C', shQuote(saga_config), sep='=')
  } else {
    config = ''
  }
  
  saga_cmd = paste(
    shQuote(saga_cmd), config, flags, lib,
    shQuote(tool_cmd, type = quote_type),
    param_string)
  
  # execute system call
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    cat(msg, sep = '\n')
    stop()
  }
  
  # load SAGA results as list of R objects
  saga_results = list()
  for (i in 1:nrow(spec_out)){
    out_i = spec_out[i, 'args']
    out_i = gsub('.sgrd', '.sdat', out_i)
    current_id = spec_out[i, 'Identifier']
    
    if (intern == TRUE) {
      
      tryCatch(expr = {
        # import OGR supported vector data
        if (spec_out[i, 'Feature'] == 'Shape' |
            spec_out[i, 'Feature'] == 'Shapes list')
          saga_results[[paste0(current_id)]] = sf::st_read(out_i)
        
        # import table data
        if (spec_out[i, 'Feature'] == 'Table'){
          if (tools::file_ext(out_i) == 'txt')
            saga_results[[paste0(current_id)]] = utils::read.table(out_i, header = T, sep = '\t')
          if (tools::file_ext(out_i) == 'csv')
            saga_results[[paste0(current_id)]] = utils::read.csv(out_i)
          if (tools::file_ext(out_i) == 'dbf')
            saga_results[[paste0(current_id)]] = foreign::read.dbf(out_i)
        }
        
        # import grid data
        if (spec_out[i, 'Feature'] %in% c('Grid', 'Raster')){
          if (tools::file_ext(out_i) == 'sg-gds-z'){
            message('Cannot load SAGA Grid Collection as an R raster object - this is not supported')
          } else {
            saga_results[[paste0(current_id)]] = raster::raster(out_i)
          }
        }
        
        # import grid lists data
        if (spec_out[i, 'Feature'] == 'Grid list'){
          out_i = strsplit(out_i, ';')[[1]]
          for (gl in seq_along(out_i))
            saga_results[[paste(current_id, gl, sep='_')]] = raster::raster(out_i[gl])
        }
        
      }, error = function(e){
        warning(
          paste0('No geoprocessing output for ', spec_out[i, 'Identifier'],
                 '. Results may require other input parameters to be specified'),
          call. = FALSE)
      }
      )
      
    } else {
      # if intern=FALSE then only return list of file paths for the sagacmd outputs
      saga_results[[paste0(current_id)]] = out_i
    }
  }
  
  # summarize outputs
  if (length(saga_results) == 1)
    saga_results = saga_results[[1]]
  
  return(saga_results)
}


#' Saves R objects to temporary files for processing by SAGA-GIS.
#'
#' Intended to be used internally. Raster objects are checked to see if the
#' object is linked to a file or exists only in memory. Spatial, sf objects and
#' dataframes are saved to temporary files.
#'
#' @param param A single variable that may be a raster object, sp object, sf
#'   object or dataframe
#'
#' @return character. Character string of filename of saved object
RtoSAGA = function(param) {
  
  # raster package objects
  # ? raster stored as file
  #   ? does the file contain a single band
  #     - TRUE :
  #       ? does the filename refer to an raster tmpfile grd
  #         - TRUE : resave raster as a geotiff in tempdir
  #         - FALSE : get filename of single band
  #     - FALSE:
  #       ? is just a single layer selected
  #         - TRUE : write layer to temporary file
  #         - FALSE : error message that SAGA-GIS needs single bands as inputs
  #
  # ? raster stored in memory
  #   ? does the inMemory object contain a single band?
  #     - TRUE: write RasterLayer to temporary file
  #     - FALSE: error message that SAGA-GIS needs single bands as inputs
  
  # Simple features objects
  if (methods::is(param, 'sf') == TRUE) {
    temp = tempfile(fileext = '.shp')
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
    sf::st_write(obj = param, dsn = temp, quiet = TRUE)
    param = temp
    
    # Raster objects
  } else if (methods::is(param, 'RasterLayer') |
             methods::is(param, 'RasterStack') | methods::is(param, 'RasterBrick')) {
    
    # Rasters stored as files
    if (raster::inMemory(param) == FALSE) {
      if (raster::nbands(param) == 1) {
        if (tools::file_ext(raster::filename(param)) == 'grd') {
          temp = tempfile(fileext = '.tif')
          raster::writeRaster(raster::raster(param), filename = temp)
          param = temp
        } else {
          param = raster::filename(param)
        }
      } else {
        if (raster::nlayers(param) == 1) {
          temp = tempfile(fileext = '.tif')
          raster::writeRaster(param, filename = temp)
          param = temp
        } else {
          stop(
            paste(
              'Raster object contains multiple bands;',
              'SAGA-GIS requires single band rasters as inputs'),
            call. = FALSE
          )
        }
      }
      
      # Rasters stored in memory
    } else if (raster::inMemory(param) == TRUE) {
      if (raster::nlayers(param) == 1) {
        temp = tempfile(fileext = '.tif')
        raster::writeRaster(param, filename = temp)
        param = temp
      } else {
        stop(
          paste(
            'Raster object contains multiple bands;',
            'SAGA-GIS requires single band rasters as inputs'
          ),
          call. = FALSE
        )
      }
    }
    
    # Spatial objects
  } else if (methods::is(param, 'SpatialLinesDataFrame') |
             methods::is(param, 'SpatialPolygonsDataFrame') |
             methods::is(param, 'SpatialPointsDataFrame')) {
    
    temp = tempfile(fileext = '.shp')
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
    rgdal::writeOGR(
      obj = param,
      dsn = temp,
      layer = 1,
      driver = "ESRI Shapefile"
    )
    param = temp
    
    # Tables
  } else if (methods::is(param, "data.frame")) {
    temp = tempfile(fileext = '.txt')
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
    utils::write.table(x = param, file = temp, sep = "\t")
    param = temp
  }
  
  return(param)
}


#' Removes temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any outputs for a SAGA-GIS tool are not specified as arguments.
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
#' # Remove all temporary files generated by Rsagacmd
#' sagaRemoveTmpFiles(h=0)
sagaRemoveTmpFiles = function(h=0) {
  
  message(paste0('Removing Rsagacmd temporary files h=', h))
  for (f in pkg.env$sagaTmpFiles) {
    if (file.exists(f) == TRUE) {
      tdelay = difftime(Sys.time(), file.mtime(f), units='hours')
      if (tdelay > h){
        message(f)
        assoc_files = list.files(
          path = dirname(f),
          pattern = utils::glob2rx(paste0(tools::file_path_sans_ext(basename(f)), '.*')),
          full.names = T)
        file.remove(assoc_files)
        pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
      }
    } else {
      pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
    }
  }
}

#' List temporary files created by Rsagacmd
#' 
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any outputs for a SAGA-GIS tool are not specified as arguments.
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
#' @examples
#' # Show all temporary files generated by Rsagacmd
#' sagaRemoveTmpFiles(h=0)
sagaShowTmpFiles = function() {
  
  message('Rsagacmd temporary files:')
  for (f in pkg.env$sagaTmpFiles){
    message(f)
  }
  return(pkg.env$sagaTmpFiles)
}


# local environment to store vector of tempfiles in package namespace
pkg.env = new.env(parent = emptyenv())
pkg.env$sagaTmpFiles = c()
