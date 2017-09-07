require(XML)
require(parallel)
require(link2GI)
require(raster)
require(tools)
require(stringdist)
require(rgdal)

sagaEnv = function(saga_bin = NA) {

  # Establishes the link to SAGA GIS
  # Generates SAGA help file and parses all libraries, tools and options
  # from the help files into a nested list of library, module and options
  # Also maps inputs/outputs and establishes which arguments are required or
  # optional
  #
  # Args:
  #   saga_bin: Optional character with known path to saga_cmd binary
  #
  # Returns:
  #   list: List of saga_cmd path, SAGA-GIS version and nested list of libaries
  #         tools and options

  if (is.na(saga_bin)) {
    link2GI::linkSAGA()
    path = sagaPath
    cmd = sagaCmd
  } else {
    path = dirname(saga_bin)
    cmd = saga_bin
  }

  # detect saga version
  version = system(
    paste(shQuote(cmd), '--version', sep = ' '), intern = T)[1]
  version = strsplit(x = version, split = ': ')[[1]][2]
  if (Sys.info()["sysname"] == "Windows") {
    version = strsplit(x = version, split = ' ')[[1]][1]
  }

  # generate SAGA help files in temporary directory
  temp_dir = tempfile()
  dir.create(temp_dir)
  help.path = temp_dir
  msg = system(
    paste(paste(shQuote(cmd), '--create-docs='), help.path, sep = ''),
    intern=T)
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
        options = XML::readHTMLTable(doc = paste(libdir, tool, sep = '/'),
                                     header = T)

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

        # add parameter options to nested list
        libraries[[basename(libdir)]][[toolName]] = options
      } # readlines if
      close(f)
    } # tool_file loop
  } # libdir loop

  return(list(
    cmd = cmd,
    path = path,
    version = version,
    libraries = libraries
  ))
}


.RtoSAGA = function(param){

  # Saves R spatial objects to temporary files for processing by SAGA
  # for raster objects it checks if the object is linked to a file or exists
  # only in memory
  #
  # Args:
  #   param: A single variable that may be a raster object, sp object or
  #          dataframe
  #
  # Returns:
  #   param: Character string of filename of saved object

  # rasters
  if (class(param) == "RasterLayer" |
      class(param) == "RasterStack" |
      class(param) == "RasterBrick") {
    if (raster::inMemory(param) == FALSE) {
      param = filename(param)
    } else {
      tmp_raster = tempfile(fileext = '.sdat')
      raster::writeRaster(param, filename = tmp_raster, format = "SAGA")
      param = tmp_raster
    }
  }

  # vectors
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

  # tables
  if (class(param) == "data.frame") {
    tmp_table = tempfile(fileext = '.txt')
    write.table(x = param,
                file = tmp_table,
                sep = "\t")
    param = tmp_table
  }
  return(param)
}


sagaGeo = function(lib, tool, senv, intern = TRUE, ...) {

  # Main function to execute SAGA-GIS commands through the command line tool
  #
  # Args:
  #   lib: Character string of name of SAGA-GIS library to execute
  #   tool: Character string of name of SAGA-GIS tool to execute
  #   senv: SAGA-GIS environment setting
  #   intern: Boolean to load outputs as R objects
  #   ...: Named arguments and values for SAGA tool
  #
  # Returns:
  #   saga_results: Output of SAGA-GIS tool loaded as an R object
  #                 (raster/rasterstack/sp/dataframe)

  # ---- Preprocessing of arguments ----

  # split argument names and values
  args = c(...)
  arg_names = names(args)
  arg_vals = args

  # match the fixed argument names to actual saga commands and arguments
  tool = names(senv$libraries[[lib]])[stringdist::amatch(
    tool, names(senv$libraries[[lib]]), maxDist=20)]
  arg_names = senv$libraries[[lib]][[tool]]$Identifier[stringdist::amatch(
    arg_names, senv$libraries[[lib]][[tool]]$Identifier, maxDist=20)]

  # ---- Checking for valid libraries, tools and parameters ----

  # save loaded R objects to files for SAGA to access
  for (i in seq_along(arg_vals)) {
    if (class(arg_vals[[i]]) == "list"){
      for (j in seq_along(arg_vals[[i]]))
        arg_vals[[i]][[j]] = .RtoSAGA(arg_vals[[i]][[j]])
    } else {
      arg_vals[[i]] = .RtoSAGA(arg_vals[[i]])
    }
  }

  # check to see if inputs are valid SAGA GIS parameters
  for (identifier in arg_names) {
    if (identifier %in% senv$libraries[[lib]][[tool]]$Identifier == FALSE) {
      stop(paste('Invalid parameter', identifier, 'not present in', tool))
    }
  }

  # ---- Prepare saga_cmd string to system ----

  sagatool = senv$libraries[[lib]][[tool]]

  # argument names
  quote_type = ifelse(Sys.info()["sysname"] == "Windows", "cmd", "csh")

  # argument values
  # collapse any params that are lists
  for (i in seq_along(arg_vals))
    if (class(arg_vals[i]) == "list")
      arg_vals[i] = paste(arg_vals[[i]], collapse = ';')

  arg_vals = as.character(unlist(arg_vals))
  params = shQuote(string = arg_vals, type = quote_type)

  # add saga_cmd arguments to the command line call:
  param_string = paste("-", arg_names, ':', params, sep = "", collapse = " ")
  saga_cmd = paste(shQuote(senv$cmd), lib, shQuote(tool, type = quote_type),
                   param_string, sep = ' ')

  # Execute the external saga_cmd
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    print (msg)
  }

  # ---- Load SAGA results as list of R objects ----

  # check that the selected tool does produce outputs
  specified_outputs=sagatool[which(sagatool$IO == "Output" & sagatool$Identifier %in% arg_names), ]
  if (nrow(specified_outputs) == 0){
    warning('Selected SAGA tool will not produce any output files')
    saga_results = NULL
  } else {
    params = cbind.data.frame(arg_names, arg_vals)
    specified_outputs = merge(specified_outputs, params, by.x='Identifier', by.y='arg_names')

    # iterate through the specified outputs and load as R objects
    saga_results = list()
    for (i in 1:nrow(specified_outputs)){
      output = as.character(specified_outputs[i, 'arg_vals'])
      if (intern == TRUE){
        if (tools::file_ext(output) == 'shp')
          saga_results[[paste(tools::file_path_sans_ext(basename(output)))]] = shapefile(
            output)
        if (tools::file_ext(output) == 'txt')
          saga_results[[paste(tools::file_path_sans_ext(basename(output)))]] = read.table(
            output, header = T, sep = '\t')
        if (tools::file_ext(output) == 'sgrd' | tools::file_ext(output) == 'sdat'){
          if (tools::file_ext(output) == 'sgrd') output = gsub('.sgrd', '.sdat', output)
          saga_results[[paste(tools::file_path_sans_ext(basename(output)))]] = raster(
            output)
        }
      } else {
        saga_results[[paste(tools::file_path_sans_ext(basename(output)))]] = output
      }
    }

    # do not embed in list if only one result is returned
    if (length(saga_results) == 1) saga_results = saga_results[[1]]
  }

  return(saga_results)
}

initSAGA = function(saga_bin = NA){

  # Dynamic creating of equivalent R functions for all SAGA-GIS libraries
  # and tools
  #
  # Args:
  #   saga_bin: Optional string with path of saga_cmd.exe
  #
  # Returns:
  #   saga: Nested list containing functions of SAGA-GIS libraries and tools

  # find saga_cmd
  senv = sagaEnv(saga_bin)

  # dynamic creation of RSAGA functions
  saga = list()
  for (lib in names(senv$libraries)){
    toolnames = list()
    for (tool in names(senv$libraries[[lib]])){

      # replace saga tool arguments that start with a numeric
      identifiers = senv$libraries[[lib]][[tool]]$Identifier
      numeric_identifiers = which(grepl("[[:digit:]]", substr(identifiers, 1, 1)) == TRUE)
      if (length(numeric_identifiers) > 0)
        levels(identifiers)[levels(identifiers)==identifiers[[numeric_identifiers]]] = sub("^.", "", identifiers[numeric_identifiers])
      identifiers = gsub(" ", "_", identifiers)

      # define tool arguments
      required = gsub('FALSE', '=NA', senv$libraries[[lib]][[tool]]$Required)
      required = gsub('TRUE', '', required)
      args <- paste(identifiers, required, collapse=',', sep='')

      # replace invalid characters from tool name
      tool = gsub(" ", "_", tool)
      tool = gsub("\\(", "", tool)
      tool = gsub("\\)", "", tool)
      tool = gsub("'", "", tool)
      tool = gsub(",", "_", tool)
      tool = gsub("/", "_", tool)
      tool = gsub("-", "_", tool)
      tool = gsub(":", "_", tool)
      tool = gsub("\\[", "_", tool)
      tool = gsub("\\]", "_", tool)
      tool = gsub("&", "_", tool)
      tool = gsub("____", "_", tool)
      tool = gsub("___", "_", tool)
      tool = gsub("__", "_", tool)

      # define function body
      body = ""
      body = paste(
        body,
        "
        # get names of function and arguments
        func_call = sys.call()
        fname = as.character(as.list(func_call))[[1]]

        library = strsplit(fname, '\\\\$')[[1]][2]
        tool = strsplit(fname, '\\\\$')[[1]][3]

        # get argument names and values
        args = as.list(func_call)[2:length(func_call)]

        # remove intern from saga args list
        if ('intern' %in% names(args))
          args = args[-which(names(args) == 'intern')]

        # evaluate any arg_vals
        for (i in seq_along(args))
          args[[i]] = eval.parent(args[[i]])

        # call the saga geoprocessor
        saga_results = sagaGeo(library, tool, senv, intern, args)
        ", sep="\n")

      toolnames = append(toolnames, tool)

      # parse function
      saga[[lib]] = append(saga[[lib]], eval(
        expr = parse(
          text = paste(paste('.', lib, '.', tool, sep=''), # function name
                       '= function(', args, ', intern = TRUE', '){', '\n', body, '\n', 'return(saga_results)}',
                       sep=''))))
    }
    names(saga[[lib]]) = toolnames
    saga[['.env']] = senv
  }

  return(saga)
}

viewLibraries = function(senv){
  return(names(senv$.env$libraries))
}

viewTools = function(lib, senv){
  return(names(senv$.env$libraries[[lib]]))
}

viewOptions = function(lib, tool, senv) {
  options = senv$.env$libraries[[lib]][[tool]]
  options = options[c('Name', 'Type', 'Identifier', 'Description', 'Constraints')]
  View(options)
}
