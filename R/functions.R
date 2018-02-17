#' Return the installed version of SAGA-GIS.
#'
#' Intended to be used internally.
#'
#' @param cmd Path of the saga_cmd binary
#'
#' @return numeric_version. Version of SAGA-GIS found at the cmd path
getSAGAversion = function(saga_cmd){
  
  # detect saga version
  version = system(
    paste(shQuote(saga_cmd), '--version', sep = ' '), intern = T)[1]
  version = strsplit(x = version, split = ': ')[[1]][2]
  
  if (Sys.info()["sysname"] == "Windows") {
    version = strsplit(x = version, split = ' ')[[1]][1]
  }
  return (as.numeric_version(version))
}


#' Automatically search for the path to SAGA-GIS.
#'
#' Returns the path to the saga_cmd binary. If multiple versions of SAGA-GIS are
#' installed on the system, the path to the newest version is returned.
#' Intended to be called internally by the initSAGA function.
#'
#' @return Full path to installed saga_cmd binary
searchSAGA = function(){
  
  # check to see if saga_cmd is recognized (i.e. has been added to path)
  saga_cmd = ifelse(nchar(Sys.which(names = 'saga_cmd')) > 0, 'saga_cmd', NA)
  
  # otherwise search for saga_cmd in usual install locations
  if (is.na(saga_cmd)) {
    if (Sys.info()["sysname"] == "Windows") {
      saga_cmd = list.files(
        path = 'C:/',
        pattern = 'saga_cmd.exe',
        recursive = TRUE,
        full.names = TRUE
      )
    } else {
      saga_cmd = list.files(
        path = '/usr',
        pattern = 'saga_cmd$',
        recursive = TRUE,
        full.names = TRUE
      )
    }
  }
  
  if (length(saga_cmd) == 0)
    stop(paste('SAGA-GIS installation not found. Need to supply a valid path',
               'to the saga_cmd executable'))
  
  # Automatically use newest version if multiple installations are found
  if (length(saga_cmd) > 1) {
    message('Multiple installations of SAGA-GIS were found at:')
    message(saga_cmd)
    message(paste('Choosing newest version. Manually specify the location when',
                  'calling initSAGA() to use an older version'))
    saga_version = list()
    for (saga_inst in saga_cmd)
      saga_version = append(saga_version, getSAGAversion(saga_inst))
    saga_cmd = saga_cmd[which(saga_version == max(saga_version))]
  }
  return (saga_cmd)
}


#' Parses valid SAGA-GIS libraries and tools into a nested list of functions.
#' 
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing
#' all libraries, tools and options from the help files into a nested list of
#' library, module and options. Intended to be used internally by the
#' initSAGA function
#'
#' @param saga_bin Optional character with known path to saga_cmd binary
#'
#' @return List of saga_cmd path, SAGA-GIS version and nested list of libaries
#'   tools and options
sagaEnv = function(saga_bin = NA) {

  if (is.na(saga_bin)){
    saga_bin = searchSAGA()
  } else {
    # check that supplied saga_bin location is correct
    if (nchar(Sys.which(names = saga_bin)) == 0)
      stop('The supplied path to the saga_cmd binary is not correct', call. = FALSE)
  }

  # detect saga version
  version = getSAGAversion(saga_bin)

  # generate SAGA help files in temporary directory
  temp_dir = tempfile()
  dir.create(temp_dir)
  help.path = temp_dir
  if (version > as.numeric_version('3.0.0')){
    msg = system(
      paste(paste(shQuote(saga_bin), '--create-docs='), help.path, sep = ''),
      intern=T)
  } else {
    setwd(help.path)
    msg = system(
      paste(shQuote(saga_bin), '--docs'), intern=T)
  }
  if (!is.null(attr(msg, "status"))){
    print (msg)
  }
  
  # parse SAGA help files into nested list of libraries, tools and options
  docs_libraries = list.dirs(path = help.path)
  docs_libraries = docs_libraries[2:length(docs_libraries)]
  libraries = list()

  for (libdir in docs_libraries) {
    tool_files = list.files(path = libdir)

    # get module names from file and remove from parameter list
    tool_names_file = tool_files[which.min(nchar(tool_files))]
    tool_files = tool_files[tool_files != tool_names_file]

    for (tool in tool_files) {
      # check to see if file is not emptry
      f <- file(paste(libdir, tool, sep = '/'), open = "rb")
      
      if (length(readLines(f, warn = F)) > 1) {
        # read module options tables
        options = XML::readHTMLTable(
          doc = paste(libdir, tool, sep = '/'), header = T)
        
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
        options['Required'] = TRUE
        options['Feature'] = NA
        options[grep("input", options$Type), 'IO'] = 'Input'
        options[grep("File path", options$Type), 'IO'] = 'Input'
        options[grep("File path", options$Type), 'Required'] = FALSE
        options[grep("output", options$Type), 'IO'] = 'Output'
        options[grep("optional", options$Type), 'Required'] = FALSE
        options[grep("Grid", options$Type), 'Feature'] = 'Grid'
        options[grep("Grid list", options$Type), 'Feature'] = 'Grid list'
        options[grep("Shapes", options$Type), 'Feature'] = 'Shape'
        options[grep("Shapes list", options$Type), 'Feature'] = 'Shapes list'
        options[grep("Table", options$Type), 'Feature'] = 'Table'
        options[grep("Table", options$Type), 'Required'] = FALSE
        options[grep("Static table", options$Type), 'Feature'] = 'Table'
        options[grep("Static table", options$Type), 'Required'] = FALSE
        options[grep("Table list", options$Type), 'Feature'] = 'Table list'
        options[grep("Table list", options$Type), 'Required'] = FALSE
        options[grep("field", options$Type), 'Feature'] = 'Table field'
        options[grep("field", options$Type), 'Required'] = FALSE
        options[grep("Integer", options$Type), 'Required'] = FALSE
        options[grep("Choice", options$Type), 'Required'] = FALSE
        options[grep("Floating point", options$Type), 'Required'] = FALSE
        options[grep("Boolean", options$Type), 'Required'] = FALSE
        options[grep("Long text", options$Type), 'Required'] = FALSE
        options[grep("Text", options$Type), 'Required'] = FALSE

        # exceptions
        if (toolName == 'Export GeoTIFF' | toolName == 'Export Raster'){
          options[grep("File path", options$Type), 'IO'] = 'Output'
          options[grep("File path", options$Type), 'Required'] = TRUE
          options[grep("File path", options$Type), 'Feature'] = 'Raster'
        } else if (toolName == 'Clip Grid with Rectangle'){
          options[grep("Data Object", options$Type), 'Feature'] = 'Grid'
        }
        
        # replace saga tool arguments that start with a numeric
        identifiers = options$Identifier
        numeric_identifiers = which(grepl("[[:digit:]]", substr(identifiers, 1, 1)) == TRUE)
        if (length(numeric_identifiers) > 0)
          levels(identifiers)[levels(identifiers) == identifiers[[numeric_identifiers]]] = sub("^.", "", identifiers[numeric_identifiers])
        identifiers = gsub(" ", "_", identifiers)
        options['identifierR'] = identifiers
        
        # add parameter options to nested list
        libraries[[basename(libdir)]][[valid_toolname]] = list(
          options=options, tool_cmd=toolName)
      }
      
      close(f)
    }
  }
  
  return(list(saga_cmd = saga_bin, version = version, libraries = libraries))
}


#' Saves R objects to temporary files for processing by SAGA.
#'
#' Intended to be used internally. Raster objects are checked to see if the
#' object is linked to a file or exists only in memory. Spatial, sf objects and
#' dataframes are saved to temporary files.
#'
#' @param param A single variable that may be a raster object, sp object, sf object or dataframe
#'
#' @return Character string of filename of saved object
RtoSAGA = function(param){

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
  if (is(param, 'sf') == TRUE){
    temp = tempfile(fileext = '.shp')
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
    sf::st_write(obj = param, dsn = temp, quiet = TRUE)
    param = temp
  
  # Raster objects
  } else if (is(param, 'RasterLayer') |
             is(param, 'RasterStack') | is(param, 'RasterBrick')) {
    
    # Rasters stored as files
    if (raster::inMemory(param) == FALSE) {
      if (param@file@nbands == 1) {
        param = raster::filename(param)
        if (tools::file_ext(param) == 'grd'){
          temp = tempfile(fileext = '.tif')
          raster::filename(raster::writeRaster(param, filename = temp))
          param = temp
        }
      } else {
        if (raster::nlayers(param) == 1) {
          temp = tempfile(fileext = '.tif')
          raster::filename(raster::writeRaster(param, filename = temp))
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
    } else if (raster::inMemory(param) == TRUE){
      if (raster::nlayers(param) == 1) {
        temp = tempfile(fileext = '.tif')
        raster::filename(raster::writeRaster(param, filename = temp))
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
  } else if (is(param, 'SpatialLinesDataFrame') |
             is(param, 'SpatialPolygonsDataFrame') |
             is(param, 'SpatialPointsDataFrame')) {
    
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
  } else if (is(param, "data.frame")) {
    temp = tempfile(fileext = '.txt')
    pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, temp)
    utils::write.table(x = param,
                file = temp,
                sep = "\t")
    param = temp
  }
  
  return(param)
}


#' Main function to execute SAGA-GIS commands through the command line tool.
#'
#' Intended to be used internally, although sagaGeo can be used directly by
#' passing the name of the SAGA-GIS library and tool, along with named
#' arguments and values.
#'
#' @param lib Character string of name of SAGA-GIS library to execute
#' @param tool Character string of name of SAGA-GIS tool to execute
#' @param intern Boolean to load outputs as R objects
#' @param cores Number of physical processing cores to use for computation.
#'   Default uses all available cores
#' @param ... Named arguments and values for SAGA tool
#'
#' @return Output of SAGA-GIS tool loaded as an R object
#'   (RasterLayer/sf/dataframe)
#' @export
#' @examples
#' \dontrun{
#' # initialize Rsagacmd and dynamically generate functions for all SAGA-GIS tools
#' saga = initSAGA()
#'
#' # Example of running a SAGA-GIS tool
#'
#' # Generate random terrain and load as raster object
#' dem = saga$grid_calculus$Random_Terrain()
#' dem = sagaGeo(lib = 'grid_calculus', tool = 'Random Terrain',
#' RADIUS = 3, TARGET_OUT_GRID = 'somegrid.sgrd')
#' }
sagaGeo = function(lib, tool, intern = TRUE, cores = NULL, ...) {
  args = c(...)
  sagatool = pkg.env$senv$libraries[[lib]][[tool]][['options']]

  # match the identifierR to the identifier used by SAGA-GIS
  arg_names = names(args)
  arg_names = merge(
    x=data.frame(arg_names, stringsAsFactors = F),
    y=sagatool, by.x='arg_names', by.y='identifierR',
    sort=FALSE)$Identifier
  args = setNames(args, arg_names)

  # make lists of missing required arguments
  # missing_req = names(args[sapply(args, function(x) is(x, 'name'))])
  # missing_opt = names(args[sapply(args, function(x) is(x, 'logical'))])

  # strip missing arguments and update arg_names
  args = args[sapply(args, function(x) is(x, 'name'))==FALSE]
  args = args[sapply(args, function(x) !is(x, 'logical'))]
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

  # provide error if tool produces no outputs
  if (length(which(sagatool$IO == "Output")) == 0)
    stop(paste('SAGA Tool', tool, 'produces no outputs'), call. = FALSE)

  # determine the SAGA output parameters that have been specified as function args
  spec_ind = which(sagatool$IO == "Output" & sagatool$Identifier %in% arg_names)
  n_outputs = length(spec_ind)
  spec_out = sagatool[spec_ind, ]

  # process the specified arguments
  if (n_outputs > 0){
    # create dataframe of containing tool settings merged with the Rsagacmd
    # function specified arguments
    spec_out = merge(x=spec_out, y=cbind.data.frame(arg_names, args),
                     by.x='Identifier', by.y='arg_names', sort=FALSE)

    # convert factors to character
    spec_out$args = as.character(spec_out$args)
  }

  # determine any required outputs that have not been specified as function args
  req_out = sagatool[which(sagatool$IO == "Output" & sagatool$Required == TRUE), ]
  unspec_ind = which(!(req_out$Identifier %in% spec_out$Identifier))
  n_temps = length(unspec_ind)

  if (n_temps == 0 & n_outputs == 0){
    # some tools have no required outputs - error if no outputs are specified
    stop(
      paste(
        'Selected SAGA tool has no required outputs....',
        'optional outputs must be specified as arguments'
      ),
      call. = FALSE
    )
  }

  # use tempfiles if any required outputs are not specified
  if (n_temps > 0){
    unspec_out = req_out[unspec_ind,]
    unspec_out['args'] = NA

    for (i in 1:nrow(unspec_out)){
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
  if (!is.null(cores)){
    cores = paste('-c', cores, sep='=')
  } else {
    cores = ''
  }

  flags = '--flags=pl'

  # create string with argument values within quotes
  quote_type = ifelse(Sys.info()["sysname"] == "Windows", "cmd", "csh")
  params = shQuote(string = args, type = quote_type)

  # prepare system call
  param_string = paste("-", arg_names, ':', params, sep = "", collapse = " ")
  saga_cmd = paste(
    shQuote(pkg.env$senv$saga_cmd), cores, flags, lib,
    shQuote(pkg.env$senv$libraries[[lib]][[tool]][['tool_cmd']], type = quote_type),
    param_string)

  # execute system call
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    print (saga_cmd)
    message(msg)
    stop()
  }

  # load SAGA results as list of R objects
  saga_results = list()
  for (i in 1:nrow(spec_out)){
    out_i = spec_out[i, 'args']
    out_i = gsub('.sgrd', '.sdat', out_i)
    current_id = spec_out[i, 'Identifier']

    if (intern == TRUE){

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
          paste(
            'No output for', spec_out[i, 'Identifier'],
            '. The tool may require other inputs in order to calculate this output')
          , call. = FALSE)
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


#' Dynamically maps all SAGA-GIS functions to a nested list of functions in R.
#'
#' Dynamically creates R functions to all valid tools and
#' returns them as a nested and named list of SAGA-GIS libraries and tools. 
#' This list is defined as an S3 SAGAgis object, and the enclosed tools
#' represent SAGAtool objects.
#'
#' @param saga_bin Optional path to saga_cmd
#' @return Nested list of functions for SAGA-GIS libraries and tools
#' @export
#' @examples
#' \dontrun{
#' # initialize Rsagacmd and dynamically generate functions for all SAGA-GIS tools
#' saga = initSAGA()
#'
#' # Example of terrain analysis
#'
#' # Generate random terrain and load as raster object
#' dem = saga$grid_calculus$Random_Terrain()
#'
#' # Display help on usage for tool
#' saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(usage=TRUE)
#' 
#' # Use Rsagacmd for to calculate the terrain ruggedness index
#' tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem)
#' plot(tri)
#' 
#' # Do not load output as an R object
#' saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem, intern=FALSE)
#' }
initSAGA = function(saga_bin = NA) {
  
  pkg.env$senv = sagaEnv(saga_bin)
  
  # dynamic creation of RSAGA functions
  saga = list()
  for (lib in names(pkg.env$senv$libraries)) {
    toolnames = list()
    for (tool in names(pkg.env$senv$libraries[[lib]])) {
      # define tool arguments
      required = gsub('FALSE', '=NA', pkg.env$senv$libraries[[lib]][[tool]][['options']]$Required)
      required = gsub('TRUE', '', required)
      args <-
        paste(
          pkg.env$senv$libraries[[lib]][[tool]][['options']]$identifierR,
          required,
          collapse = ',',
          sep = ''
        )
      
      # define function body
      body = paste(
        paste0('args = as.list(environment())'),
        paste0('fname = ', "'", tool, "'"),
        paste0('lib = ', "'", lib, "'"),
        paste0('tool = ', "'", tool, "'"),
        "
        # remove intern and help from saga args list
        if ('intern' %in% names(args))
          args = args[-which(names(args) == 'intern')]
        if ('cores' %in% names(args))
          args = args[-which(names(args) == 'cores')]

        # call the saga geoprocessor
        saga_results = sagaGeo(lib, tool, intern, cores, args)
        return (saga_results)
        ",
        sep = "\n"
      )
      
      # parse function
      tryCatch(
        expr = {
          func_code = paste0(
            'function(', args,', intern = TRUE, cores = NULL', '){',
            '\n', body, '\n', '}')
          func = structure(
            eval(expr = parse(text = func_code)),
            lib = lib,
            tool = tool,
            class = 'SAGAtool')
          
          saga[[lib]] = append(saga[[lib]], func)
          toolnames = append(toolnames, tool)
        },
        error = function(e)
          warning(
            paste0("Problem parsing SAGA library = ", lib, "; and tool = ", tool), call. = FALSE)
      )
      
    }
    names(saga[[lib]]) = toolnames
  }
  
  # S3 implementation
  class(saga) = 'SAGAgis'
  
  # S4 implementation
  # setClass('SAGAgis', representation(libraries='list', options='list', version='character', saga_cmd='character', senv='list'))
  # saga = new('SAGAgis', cmd=senv$cmd, version=as.character(senv$version), options=senv$libraries, libraries=saga, senv=senv)
  
  return(saga)
}

#' Removes temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any required outputs for a SAGA-GIS tool are not specified as arguments.
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
#' #' \dontrun{
#' # remove all temporary files generated by Rsagacmd
#' sagaRemoveTmpFiles(h=0)
#' }
sagaRemoveTmpFiles = function(h=0){
  for (f in pkg.env$sagaTmpFiles){
    if (file.exists(f) == TRUE){
      tdelay = difftime(Sys.time(), file.mtime(f), units='hours')
      if (tdelay > h){
        message(f)
        assoc_files = list.files(
          path = dirname(f),
          pattern = glob2rx(paste0(tools::file_path_sans_ext(basename(f)), '.*')),
          full.names = T)
        file.remove(assoc_files)
        pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
      }
    } else {
      pkg.env$sagaTmpFiles = pkg.env$sagaTmpFiles[pkg.env$sagaTmpFiles != f]
    }
  }
  return (NULL)
}

#' List temporary files created by Rsagacmd
#' 
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any required outputs for a SAGA-GIS tool are not specified as arguments.
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
#'
sagaShowTmpFiles = function(){
  message('Rsagacmd temporary files:')
  for (f in pkg.env$sagaTmpFiles){
    message(f)
  }
  return(pkg.env$sagaTmpFiles)
}


#' Print (display) parameters and options of a SAGAtool object
#' 
#' Used to display more information on the usage of any particular SAGA-GIS
#' tool
#'
#' @param x SAGAtool object
#'
#' @return NULL
#' @export
print.SAGAtool = function(x){
  lib = attr(x, 'lib')
  tool = attr(x, 'tool')
  
  tool_options = pkg.env$senv$libraries[[lib]][[tool]][['options']][, c('identifierR', 'Name', 'Type', 'Description', 'Constraints')]
  tool_options$Constraints = gsub('Available Choices:\n', '', tool_options$Constraints)
  tool_options$Constraints = gsub('\n', ';', tool_options$Constraints)
  
  cat(paste0('Help for library = ', lib, '; tool = ', tool, ':', '\n'))
  for (i in 1:nrow(tool_options)){
    cat(paste0('Name of tool: ', tool_options[i, 'Name']), '\n')
    cat(paste0('Identifier: ', tool_options[i, 'identifierR'], '\n'))
    cat(paste0('Type: ', tool_options[i, 'Type'], '\n'))
    cat(paste0('Description: ', tool_options[i, 'Description'], '\n'))
    cat(paste0('Constraints: ', tool_options[i, 'Constraints'], '\n'))
    cat('\n')
  }
  
}


# local environment to store vector of tempfiles in package namespace
pkg.env = new.env(parent = emptyenv())
pkg.env$sagaTmpFiles = c()