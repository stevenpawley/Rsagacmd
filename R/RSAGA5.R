library(XML)
library(parallel)
library(link2GI)
library(raster)
library(tools)
library(stringdist)

sagaEnv = function(saga_bin = NA) {

  # find SAGA path if not specified
  if (is.na(saga_bin)) {
    linkSAGA()
    path = sagaPath
    cmd = basename(sagaCmd)
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
  invalid_libs = which(basename(docs_libraries) %in%
                         c('db_odbc', 'db_pgsql', 'docs_html', 'docs_pdf', 'garden_3d_viewer', 'garden_fractals', 'garden_games',
                           'garden_webservices', 'gc_tools', 'toolchains', 'tta_tools', 'tin_viewer', 'pointcloud_viewer',
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
        options[grep("Shape", options$Type), 'Feature'] = 'Shape list'
        options[grep("Shape list", options$Type), 'Feature'] = 'Shape'
        options[grep("Table", options$Type), 'Feature'] = 'Table'
        options[grep("Table list", options$Type), 'Feature'] = 'Table list'
        options[grep("field", options$Type), 'Feature'] = 'Table field'
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
    cmd = sagaCmd,
    path = sagaPath,
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
      writeRaster(param, filename = tmp_raster, format = "SAGA")
      param = tmp_raster
    }
  }

  # vectors
  if (class(param) == "SpatialLinesDataFrame" |
      class(param) == "SpatialPolygonsDataFrame" |
      class(param) == "SpatialPointsDataFrame") {
    tmp_vector = tempfile(fileext = '.shp')
    writeOGR(
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


.sagaGeo = function(lib, tool, arg_names, arg_vals, .env) {

  # evaluate any arg_vals
  for (i in seq_along(arg_vals))
    arg_vals[[i]] = eval(arg_vals[[i]])

  # match the fixed argument names to actual saga commands and arguments
  tool = names(.env$libraries[[lib]])[stringdist::amatch(
    tool, names(.env$libraries[[lib]]), maxDist=10)]
  arg_names = .env$libraries[[lib]][[tool]]$Identifier[stringdist::amatch(
    arg_names, .env$libraries[[lib]][[tool]]$Identifier)]

  # Checking for valid libraries, tools and parameters
  # ---------------------------------------------------

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
    if (identifier %in% .env$libraries[[lib]][[tool]]$Identifier == FALSE) {
      stop(paste('Invalid parameter', identifier, 'not present in', tool))
    }
  }

  # Prepare saga_cmd string to system
  # ---------------------------------
  sagatool = .env$libraries[[lib]][[tool]]

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
  saga_cmd = paste(shQuote(.env$cmd), lib, shQuote(tool, type = quote_type),
                   param_string, sep = ' ')

  # check that all required outputs have been specified
  # note that some tools do not have any required outputs - issue a warning
  required_outputs = sagatool[which(sagatool$IO == 'Output' & sagatool$Required == TRUE), ]
  if (nrow(required_outputs) > 0)
    if (nrow(sagatool[which(arg_names %in% required_outputs$Identifier), ]) == 0)
      stop('SAGA command is missing required output arguments')


  # Execute the external saga_cmd
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    print (msg)
  }

  # Load SAGA results

  # check that the selected tool does produce outputs
  if (nrow(sagatool[which(sagatool$IO == "Output"), ]) == 0){
    warning('Selected SAGA tool does not produce any output files')
    saga_results = NULL
  } else {
    specified_outputs = sagatool[which(
      sagatool$IO == 'Output' & sagatool$Identifier %in% arg_names), ]
    specified_outputs['Filename'] = NA
    specified_outputs['Filename'] = arg_vals[
      which(sagatool$IO == 'Output' & sagatool$Identifier %in% arg_names)]

    # replace .sgrd extension with .sdat for loading as raster
    specified_outputs['Filename'] = gsub(
      pattern = '.sgrd', replacement = '.sdat', x = specified_outputs['Filename'])

    # iterate through the specified outputs and load as R objects
    saga_results = list()
    for (output in specified_outputs['Filename']){
      if (file_ext(output) == 'shp')
        saga_results[[paste(file_path_sans_ext(basename(output)))]] = shapefile(output)
      if (file_ext(output) == 'txt')
        saga_results[[paste(file_path_sans_ext(basename(output)))]] = read.table(
          output, header = T, sep = '\t')
      if (file_ext(output) == 'sdat')
        saga_results[[paste(file_path_sans_ext(basename(output)))]] = raster(
          output)
    }

    # do not embed in list if only one result is returned
    if (length(saga_results) == 1) saga_results = saga_results[[1]]
  }

  return(saga_results)
}

.cleanSagaNames = function(.env, lib, tool){
  # replace saga tool arguments that start with a numeric
  identifiers = .env$libraries[[lib]][[tool]]$Identifier
  numeric_identifiers = which(grepl("[[:digit:]]", substr(identifiers, 1, 1)) == TRUE)
  if (length(numeric_identifiers) > 0)
    levels(identifiers)[levels(identifiers)==identifiers[[numeric_identifiers]]] = sub("^.", "", identifiers[numeric_identifiers])
  identifiers = gsub(" ", "_", identifiers)

  # define tool arguments
  required = gsub('FALSE', '=NA', .env$libraries[[lib]][[tool]]$Required)
  required = gsub('TRUE', '', required)
  args <- paste(identifiers, required, collapse=',', sep='')

  # replace invalid characters from tool name
  tool = gsub(' ', '_', tool)
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

  # define function body
  body = ""
  body = paste(
    body,
    "
    # get names of function and arguments
    func_call = match.call()
    fname = func_call[[1]]
    func_call = func_call[2:length(func_call)]
    arg_names = names(func_call)
    arg_vals = unlist(lapply(seq_along(func_call), function(x){func_call[[x]]}))

    fname = strsplit(as.character(fname), '\\\\.')[[1]]
    library = fname[2]
    tool = fname[3]

    # call the saga geoprocessor
    saga_results = .sagaGeo(library, tool, arg_names, arg_vals, .env)
    ",
    sep="\n")

  return (list(tool=tool, args=args, function_body=body))
}

# dynamic creation of RSAGA functions
.env = sagaEnv()

for (lib in names(.env$libraries)){
  for (tool in names(.env$libraries[[lib]])){

    valid_saga_names = .cleanSagaNames(.env, lib, tool)
    function_name = valid_saga_names$tool
    args = valid_saga_names$args
    body = valid_saga_names$function_body

    # parse function
    eval(
      expr = parse(text = paste(paste(lib, '.', function_name, sep=''), # function name
                                ' = function(', args, '){', '\n', body, '\n', 'return(saga_results)}', sep='')))
  }
}
remove(valid_saga_names)
remove(function_name)
remove(args)
remove(body)
#dump(lsf.str(), file="/Users/Steven Pawley/Documents/R Packages/RSAGA5/R/RSAGA_Functions.R")
