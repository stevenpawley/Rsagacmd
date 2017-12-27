devtools::use_package("XML")
devtools::use_package("raster")
devtools::use_package("tools")
devtools::use_package("rgdal")
devtools::use_package("foreign")
devtools::use_package("sf")

#' Rsagacmd: A package for linking R with the open-source SAGA-GIS.
#'
#' \pkg{Rsagacmd} is intended to provide an R scripting interface to the
#' open-source \href{https://sourceforge.net/projects/saga-gis/}{SAGA-GIS}. The
#' current version has been tested using SAGA-GIS 2.3.2, 5.0.0 and 6.1.0 on
#' Windows (x64), OS X and Linux.
#'
#' This package is not related to the RSAGA package, which provides an
#' alternative interface to SAGA-GIS versions 2.0.4 - 2.2.3. In comparison to
#' RSAGA, Rsagacmd supports newer versions of SAGA-GIS and provides access to
#' SAGA-GIS tools by dynamically generating R functions that map to the html
#' tags associated with every supoorted SAGA-GIS tool. These functions are
#' returned as a named list of SAGA libraries and nested sub-lists of the
#' functions to each library's tools. This facilitates an easier scripting
#' experience by organizing the large number of available SAGA-GIS tools (> 500)
#' by their respective SAGA-GIS library, and each function's syntax is similar
#' to using the SAGA-GIS command line tool directly. Furthermore, because the
#' arguments (called identifiers) for many SAGA-GIS tools are not consistently
#' named, the user can also take advantage of code autocompletion tools (e.g. in
#' \href{http://rstudio.com}{Rstudio}), allowing for each tools' inputs, outputs
#' and options to be more easily recognized.
#'
#' @section Dynamically-created functions to SAGA-GIS tools: Rsagacmd attempts
#'   to facilitate a seamless interface to the open-source SAGA-GIS by providing
#'   access to most SAGA-GIS geoprocessing tools in a 'R-like' manner. By
#'   default, all results from SAGA-GIS tools are automatically loaded as the
#'   appropriate R object: \itemize{ \item Raster-based outputs from SAGA-GIS
#'   tools are loaded as RasterLayer objects \item Vector features from SAGA-GIS
#'   tools in ESRI Shapefile format are loaded into the R environment as simple
#'   features objects \item Tabular data from SAGA-GIS tools are loaded as
#'   dataframes} The results from tools that return multiple outputs are loaded
#'   into the R environment as a named list of the appropriate R objects.
#' @author Steven Pawley, \email{dr.stevenpawley@gmail.com}

#' @docType package
#' @name Rsagacmd
NULL

#' Return the installed version of SAGA-GIS.
#'
#' Intended to be used internally.
#'
#' @param cmd Path of the saga_cmd binary
#'
#' @return A numeric_version of SAGA-GIS installation
#' @export
#' @examples
#' \dontrun{
#' .getSAGAversion(cmd = 'saga_cmd')
#' }
.getSAGAversion = function(cmd){
  
  # detect saga version
  version = system(
    paste(shQuote(cmd), '--version', sep = ' '), intern = T)[1]
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
#'
#' @return Full path to installed saga_cmd binary
#' @export
searchSAGA = function(){
  
  # check to see if saga_cmd is recognized (i.e. has been added to path)
  cmd = ifelse(nchar(Sys.which(names = 'saga_cmd')) > 0, 'saga_cmd', NA)
  
  # otherwise search for saga_cmd in usual install locations
  if (is.na(cmd)) {
    if (Sys.info()["sysname"] == "Windows") {
      cmd = list.files(
        path = 'C:',
        pattern = 'saga_cmd.exe',
        recursive = TRUE,
        full.names = TRUE
      )
    } else {
      cmd = list.files(
        path = '/usr',
        pattern = 'saga_cmd$',
        recursive = TRUE,
        full.names = TRUE
      )
    }
  }
  
  # decide between multiple versions if found
  if (length(cmd) > 1) {
    message('Multiple installations of SAGA-GIS are present. Choosing newest version...')
    saga_version = list()
    for (saga_inst in cmd)
      saga_version = append(saga_version, .getSAGAversion(saga_inst))
    cmd = cmd[which(saga_version == max(saga_version))]
  }
  
  return (cmd)
}

#' Parses valid SAGA-GIS libraries and tools into a nested list of functions.
#' 
#' Establishes the link to SAGA GIS by generating a SAGA help file and parsing
#' all libraries, tools and options from the help files into a nested list of
#' library, module and options
#'
#' @param saga_bin Optional character with known path to saga_cmd binary
#'
#' @return List of saga_cmd path, SAGA-GIS version and nested list of libaries
#'   tools and options
#' @export
#' @examples
#' \dontrun{
#' saga = sagaEnv(saga_bin = 'saga_cmd')
#' }
sagaEnv = function(saga_bin = NA) {

  # use link2GI to find path to saga_cmd unless specified manually
  if (is.na(saga_bin)){
    saga_bin = searchSAGA()
  } else {
    # check that supplied saga_bin location is correct
    if (nchar(Sys.which(names = saga_bin)) == 0)
      stop('The supplied path to the saga_cmd binary is not correct')
  }

  # detect saga version
  version = .getSAGAversion(saga_bin)

  # generate SAGA help files in temporary directory
  temp_dir = tempfile()
  dir.create(temp_dir)
  help.path = temp_dir
  if (version > as.numeric_version('3.0.0')){
    msg = system(
      paste(paste(shQuote(saga_bin), '--create-docs='), help.path, sep = ''), intern=T)
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
  invalid_libs = which(
    basename(docs_libraries) %in%
       c('db_odbc', 'db_pgsql', 'docs_html', 'docs_pdf', 'garden_3d_viewer',
         'garden_fractals', 'garden_games', 'garden_webservices', 'gc_tools',
         'toolchains', 'tta_tools', 'tin_viewer', 'pointcloud_viewer',
         'garden_learn_to_program', 'grid_visualisation', 'group_files'))
  docs_libraries = docs_libraries[-invalid_libs]
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
        options = XML::readHTMLTable(doc = paste(libdir, tool, sep = '/'), header = T)
        
        # create valid tool names
        valid_toolname = colnames(options[[1]])[2]
        valid_toolname = gsub(" ", "_", valid_toolname)
        valid_toolname = gsub("\\(", "", valid_toolname)
        valid_toolname = gsub("\\)", "", valid_toolname)
        valid_toolname = gsub("'", "", valid_toolname)
        valid_toolname = gsub(",", "_", valid_toolname)
        valid_toolname = gsub("/", "_", valid_toolname)
        valid_toolname = gsub("-", "_", valid_toolname)
        valid_toolname = gsub(":", "_", valid_toolname)
        valid_toolname = gsub("\\[", "_", valid_toolname)
        valid_toolname = gsub("\\]", "_", valid_toolname)
        valid_toolname = gsub("&", "_", valid_toolname)
        valid_toolname = gsub("____", "_", valid_toolname)
        valid_toolname = gsub("___", "_", valid_toolname)
        valid_toolname = gsub("__", "_", valid_toolname)
        
        # parse parameter table into a dataframe
        toolName = colnames(options[[1]])[2]
        options = options[[length(options)]] # tool options are last table
        options = options[which(apply(options[, 2:5], 1, function(x)
          any(is.na(x))) == FALSE), ] # strip input, output, options lines in table
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
        options[grep("Table list", options$Type), 'Feature'] = 'Table list'
        options[grep("field", options$Type), 'Feature'] = 'Table field'
        options[grep("field", options$Type), 'Required'] = FALSE
        options[grep("field", options$Type), 'IO'] = 'Input'
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
          levels(identifiers)[levels(identifiers)==identifiers[[numeric_identifiers]]] = sub("^.", "", identifiers[numeric_identifiers])
        identifiers = gsub(" ", "_", identifiers)
        options['validRIdentifier'] = identifiers

        # add parameter options to nested list
        libraries[[basename(libdir)]][[valid_toolname]] = list(options=options, cmd=toolName)
      } # readlines if
      close(f)
    } # tool_file loop
  } # libdir loop

  return(list(
    cmd = saga_bin,
    version = version,
    libraries = libraries
  ))
}


#' Saves R objects to temporary files for processing by SAGA.
#'
#' Intended to be used internally. Raster objects are checked to see if the
#' object is linked to a file or exists only in memory. Spatial, sf objects and
#' dataframes are saved to temporary files.
#'
#' @param param A single variable that may be a raster object, sp object, sf
#'   object or dataframe
#'
#' @return Character string of filename of saved object
#' @export
.RtoSAGA = function(param){

  # rasters
  if (class(param) == "RasterLayer" |
      class(param) == "RasterStack" |
      class(param) == "RasterBrick") {
    if (raster::inMemory(param) == FALSE) {
      # get filename if raster not stored in memory
      param = raster::filename(param)
      
      # but have to rewrite raster if stored in R format
      if (tools::file_ext(param) == '.grd'){
        tmp_raster = tempfile(fileext = '.tif')
        raster::writeRaster(param, filename = tmp_raster)
        param = tmp_raster
      }
    } else {
      # write raster to file if stored in memory
      tmp_raster = tempfile(fileext = '.tif')
      raster::writeRaster(param, filename = tmp_raster)
      param = tmp_raster
    }
  }

  # spatial objects
  if (class(param) == "SpatialLinesDataFrame" |
      class(param) == "SpatialPolygonsDataFrame" |
      class(param) == "SpatialPointsDataFrame") {
    tmp_vector = tempfile(fileext = '.shp')
    rgdal::writeOGR(
      obj = param,
      dsn = tmp_vector,
      layer = 1,
      driver = "ESRI Shapefile"
    )
    param = tmp_vector
  }
  
  # simple features objects
  if (all(class(param) == c("sf", "data.frame"))){
    tmp_vector = tempfile(fileext = '.shp')
    sf::st_write(obj = param, dsn = tmp_vector, quiet = TRUE)
    param = tmp_vector
  }

  # tables
  if (class(param) == "data.frame") {
    tmp_table = tempfile(fileext = '.txt')
    utils::write.table(x = param,
                file = tmp_table,
                sep = "\t")
    param = tmp_table
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
#' @param senv SAGA-GIS environment setting
#' @param intern Boolean to load outputs as R objects
#' @param ... Named arguments and values for SAGA tool
#'
#' @return Output of SAGA-GIS tool loaded as an R object (raster/rasterstack/sf/dataframe)
#' @export
sagaGeo = function(lib, tool, senv, intern = TRUE, ...) {

  # Preprocessing of arguments
  ## split argument names and values
  args = c(...)
  arg_names = names(args)
  arg_vals = args

  ## match the validRidentifier to the actual identifier used by SAGA-GIS
  arg_names = merge(
    x=data.frame(arg_names, stringsAsFactors = F), y=senv$libraries[[lib]][[tool]][['options']],
    by.x='arg_names', by.y='validRIdentifier', sort=FALSE)$Identifier
  sagatool = senv$libraries[[lib]][[tool]][['options']]
  
  # Checking for valid libraries, tools and parameters
  ## save loaded R objects to files for SAGA to access
  for (i in seq_along(arg_vals)) {

    # if list split list into separate files
    if (class(arg_vals[[i]]) == "list"){
      for (j in seq_along(arg_vals[[i]]))
        arg_vals[[i]][[j]] = .RtoSAGA(arg_vals[[i]][[j]])

    # if rasterstack then parse each band separated
    } else if (class(arg_vals[[i]]) == 'RasterStack'){
      arg_vals_parsed = list()
      for (j in 1:raster::nlayers(arg_vals[[i]]))
        arg_vals_parsed[[j]] = .RtoSAGA(arg_vals[[i]][[j]])
      arg_vals[[i]] = unlist(arg_vals_parsed)

    } else {
      arg_vals[[i]] = .RtoSAGA(arg_vals[[i]])
    }
  }
  
  # check to see if inputs are valid SAGA GIS parameters
  for (identifier in arg_names) {
    if (identifier %in% senv$libraries[[lib]][[tool]][['options']]$Identifier == FALSE) {
      stop(paste('Invalid parameter', identifier, 'not present in', tool))
    }
  }
    
  # Prepare saga_cmd string to system
  ## collapse any argument values that are lists
  for (i in seq_along(arg_vals))
    if (class(arg_vals[i]) == "list")
      arg_vals[i] = paste(arg_vals[[i]], collapse = ';')

  ## create character with argument values within quotes
  quote_type = ifelse(Sys.info()["sysname"] == "Windows", "cmd", "csh")
  arg_vals = gsub('.sdat', '.sgrd', arg_vals)
  params = shQuote(string = arg_vals, type = quote_type)

  ## check that the selected tool produces some type of output
  specified_outputs = sagatool[which(sagatool$IO == "Output" & sagatool$Identifier %in% arg_names), ]
  if (nrow(specified_outputs) == 0){
    warning('Selected SAGA tool will not produce any output files')
    saga_results = NULL
  } else {
    names_vals_df = cbind.data.frame(arg_names, arg_vals)
    specified_outputs = merge(specified_outputs, names_vals_df, by.x='Identifier', by.y='arg_names')
    
    # convert factors to character
    specified_outputs$arg_vals = as.character(specified_outputs$arg_vals)
    
    # replace '.sdat' extension if user passes incorrect extension or if passed from RasterLayer
    for (i in 1:nrow(specified_outputs)){
      output = as.character(specified_outputs[i, 'arg_vals'])
      if (tools::file_ext(output) == 'sdat')
        specified_outputs[i, 'arg_vals'] = gsub('.sdat', '.sgrd', output)
    }
    
  }
  
  # Execute the external saga_cmd
  ## add saga_cmd arguments to the command line call:
  param_string = paste("-", arg_names, ':', params, sep = "", collapse = " ")
  saga_cmd = paste(shQuote(senv$cmd), lib, shQuote(senv$libraries[[lib]][[tool]][['cmd']], type = quote_type),
                   param_string, sep = ' ')
  
  ## execute system call
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    print (saga_cmd)
    print (msg)
  }

  # Load SAGA results as list of R objects
  if (nrow(specified_outputs) > 0){
    saga_results = list()
    for (i in 1:nrow(specified_outputs)){
      output = specified_outputs[i, 'arg_vals']
      file_sans_ext = tools::file_path_sans_ext(basename(output))

      if (intern == TRUE){
        # Import GDAL/OGR supported vector data
        if (specified_outputs[i, 'Feature'] == 'Shape' |
            specified_outputs[i, 'Feature'] == 'Shapes list')
          saga_results[[paste(file_sans_ext)]] = sf::st_read(output)

        # Import table data
        if (tools::file_ext(output) == 'txt')
          saga_results[[paste(file_sans_ext)]] = utils::read.table(output, header = T, sep = '\t')
        if (tools::file_ext(output) == 'csv')
          saga_results[[paste(file_sans_ext)]] = utils::read.csv(output)
        if (tools::file_ext(output) == 'dbf')
          saga_results[[paste(file_sans_ext)]] = foreign::read.dbf(output)

        # Import raster data
        if (tools::file_ext(output) == 'sgrd') output = gsub('.sgrd', '.sdat', output)
        if (specified_outputs[i, 'Feature'] == 'Grid' |
            specified_outputs[i, 'Feature'] == 'Grid list' |
            specified_outputs[i, 'Feature'] == 'Raster'){
          saga_results[[paste(file_sans_ext)]] = raster::raster(output)
        }
      } else {
        # if intern=FALSE then only return list of file paths for the sagacmd outputs
        saga_results[[paste(file_sans_ext)]] = output
      }
    }

    # do not embed in list if only one result is returned
    if (length(saga_results) == 1) saga_results = saga_results[[1]]
  }

  return(saga_results)
}


#' Dynamically maps all SAGA-GIS functions to a nested list of functions in R.
#'
#' A simple wrapper that dynamically creates R functions to all valid tools and
#' returns them as a nested and named list of SAGA-GIS libraries and tools.
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
#' dem = saga$grid_calculus$Random_Terrain(TARGET_OUT_GRID = tempfile(fileext='.sgrd'))
#'
#' # Display help on usage for tool
#' saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(usage=TRUE)
#' 
#' # Use Rsagacmd for to calculate the terrain ruggedness index
#' tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem, TRI = tempfile(fileext='.sgrd'))
#' plot(tri)
#' 
#' # Do not load output as an R object
#' saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem, TRI = tempfile(fileext='.sgrd'), intern=FALSE)
#' }
initSAGA = function(saga_bin = NA) {
  # find saga_cmd
  senv = sagaEnv(saga_bin)
  
  # dynamic creation of RSAGA functions
  saga = list()
  for (lib in names(senv$libraries)) {
    toolnames = list()
    for (tool in names(senv$libraries[[lib]])) {
      # define tool arguments
      required = gsub('FALSE', '=NA', senv$libraries[[lib]][[tool]][['options']]$Required)
      required = gsub('TRUE', '', required)
      args <-
        paste(
          senv$libraries[[lib]][[tool]][['options']]$validRIdentifier,
          required,
          collapse = ',',
          sep = ''
        )
      
      # define function body
      body = ""
      body = paste(
        body,
        "
        # get names of function and arguments
        func_call = sys.call()
        fname = as.character(as.list(func_call))[[1]]
        
        lib = strsplit(fname, '\\\\$')[[1]][2]
        tool = strsplit(fname, '\\\\$')[[1]][3]
        
        # optionally display help for selected tool
        if (usage == TRUE){
        print(subset(senv$libraries[[lib]][[tool]][['options']], select=c(validRIdentifier,Name,Type,Description,Constraints)))
        return()
        }
        
        # get argument names and values
        args = as.list(func_call)[2:length(func_call)]
        
        # remove intern and help from saga args list
        if ('intern' %in% names(args))
        args = args[-which(names(args) == 'intern')]
        if ('usage' %in% names(args))
        args = args[-which(names(args) == 'usage')]
        
        # evaluate any arg_vals
        for (i in seq_along(args))
        args[[i]] = eval.parent(args[[i]])
        
        # call the saga geoprocessor
        saga_results = sagaGeo(lib, tool, senv, intern, args)
        ",
        sep = "\n"
      )
      
      # parse function
      tryCatch(
        expr = {
          saga[[lib]] = append(saga[[lib]], eval(expr = parse(
            text = paste(
              paste('.', lib, '.', tool, sep = ''),
              # function name
              '= function(',
              args,
              ', intern = TRUE, usage = FALSE',
              '){',
              '\n',
              body,
              '\n',
              'return(saga_results)}',
              sep = ''
            )
          )))
          toolnames = append(toolnames, tool)
        },
        error = function(e)
          warning(
            paste("Problem parsing SAGA library", lib, "and tool", tool, sep = ' ')
          )
      )
      
    }
    names(saga[[lib]]) = toolnames
    saga[['.env']] = senv
  }
  
  return(saga)
}
