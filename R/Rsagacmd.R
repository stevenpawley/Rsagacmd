devtools::use_package("XML")
devtools::use_package("raster")
devtools::use_package("tools")
devtools::use_package("rgdal")
devtools::use_package("foreign")
devtools::use_package("sf")

#' Rsagacmd: A package for linking R with the open-source SAGA-GIS.
#'
#' \pkg{Rsagacmd} is intended to provide an R scripting interface to the
#' open-source \href{https://sourceforge.net/projects/saga-gis/}{SAGA-GIS}
#' software. The current version has been tested using SAGA-GIS 2.3.2, 5.0.0,
#' 6.1.0, and 6.2.0 on Windows (x64), OS X and Linux.
#'
#' This package is not related to the \pkg{RSAGA} package, which provides an
#' alternative interface to SAGA-GIS versions 2.0.4 - 2.2.3. In comparison to
#' RSAGA, Rsagacmd supports newer versions of SAGA-GIS and provides access to
#' SAGA-GIS tools by dynamically generating R functions that map to the html
#' tags associated with every supoorted SAGA-GIS tool. These functions are
#' returned as a named list of SAGA libraries, each of which contain a nested
#' sub-list of functions that are mapped to each SAGA-GIS tool in the library.
#' This facilitates an easier scripting experience by organizing the large
#' number of SAGA-GIS tools (> 500) by their respective library. Each function's
#' syntax is therfore similar to using the SAGA-GIS command line tool directly.
#' Furthermore, because the arguments (called identifiers) for many SAGA-GIS
#' tools are not consistently named, the user can also take advantage of code
#' autocompletion tools (e.g. in \href{http://rstudio.com}{Rstudio}), allowing
#' for each tools' inputs, outputs and options to be more easily recognized.
#'
#' @section Handling of geospatial and tabular data:
#' Rsagacmd aims to facilitate a seamless interface to the open-source
#' SAGA-GIS by providing access to all SAGA-GIS geoprocessing tools in a
#' 'R-like' manner. In addition to mapping R functions to execute SAGA-GIS
#' tools, Rsagacmd automatically handles the passing of geospatial and tabular
#' data contained from the R environment to SAGA-GIS.
#' 
#' Rsagacmd uses the SAGA-GIS command line interface to perform geoprocessing
#' operations. Therefore, all of the Rsagacmd tools allow paths to the input
#' data to be used as arguments, if the data is stored in the appropriate file
#' formats (e.g. GDAL-supported single-band rasters, OGR supported vector data,
#' and comma- or tab-delimited text files for tabular data). In addition,
#' Rsagacmd currently supports the following R object classes to pass data to
#' SAGA-GIS, and to load the results back into the R environment:
#' \itemize{
#' \item Raster data handling is provided by the R \pkg{raster} package
#' Raster-based outputs from SAGA-GIS tools are loaded as RasterLayer objects.
#' For more details, see the 'Handling of raster data'.
#' \item Vector features that result from SAGA-GIS geoprocessing operations are
#' output in ESRI Shapefile format and are loaded into the R environment as
#' simple features objects
#' \item Tabular data from SAGA-GIS tools are loaded as dataframes
#' }
#' The results from tools that return multiple outputs are loaded into the R
#' environment as a named list of the appropriate R object classes.
#'   
#' @section Handling of raster data by Rsagacmd and SAGA-GIS:
#' SAGA-GIS does not handle multi-band rasters and the native SAGA GIS Binary
#' file format (.sgrd) supports only single band data. Therefore when passing
#' raster data to most SAGA-GIS tools using Rsagacmd, the data should represent
#' single raster bands, specified as either the path to the single raster band,
#' or when using the R \pkg{raster} package, a RasterLayer (or less commonly a
#' RasterStack or RasterBrick) object that contains only a single layer.
#' Subsetting of raster data is performed automatically by Rsagacmd in the case
#' of when a single band from a RasterStack or RasterBrick object is passed to a
#' SAGA-GIS tool. This occurs in by either passing the filename of the raster
#' to the SAGA-GIS command line, or by writing the data to a temporary file.
#' However, a few SAGA-GIS functions will accept a list of single
#' band rasters as an input. In this case if this data is in the form of a
#' RasterStack or RasterLayer object, it is recommended to use the unstack
#' function in the \pkg{raster} package, which will return a list of RasterLayer
#' objects, and then Rsagacmd will handle the subsetting automatically.
#' 
#' @section Combining SAGA-GIS commands with pipes:
#' For convenience, non-optional outputs from SAGA-GIS are automatically saved
#' to tempfiles if outputs are not explicitly stated, e.g.
#' 
#' tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM=dem_clipped, RADIUS=3)
#' 
#' Will write the output terrain ruggedness index to a temporary file, and will
#' automatically load the result into the R environment as a RasterLayer object.
#' This was implemented for convenience, and so that the user can also create
#' complex workflows that require very little code. It is also means that you
#' can combine several processing steps with pipes:
#' 
#' # clip dem to shape, resample, and calculate potential incoming solar
#' radiation
#' 
#' prj_bnd = st_read('some_shape.shp')
#' dem = raster('some_dem.tif')
#' 
#' pisr = dem %>%
#' saga$shapes_grid$Clip_Grid_with_Rectangle(SHAPES = prj_bnd)) %>%
#' saga$grid_tools$Resampling(TARGET_USER_SIZE = 100) %>%
#' saga$ta_lighting$Potential_Incoming_Solar_Radiation(
#' LOCATION = 1, PERIOD = 2, DAY='2013-01-01', DAY_STOP = '2014-01-01',
#' DAYS_STEP=10, HOUR_STEP=3, METHOD='Hofierka and Suri',
#' GRD_LINKE_DEFAULT=3)
#' 
#' In the above example, three tools are joined together using pipes, and only
#' the PISR grid is returned as a RasterLayer object. The intermediate
#' processing steps are dealt with automatically by saving the outputs as
#' tempfiles. When dealing with high-resolution and/or larger raster data, these
#' tempfiles can start to consume a significant amount of disk space over a
#' session. If required, temporary files can be cleaned during the session in a
#' similar way to the raster package, using:
#' 
#' saga.removeTmpFiles(h=0).
#' 
#' where h is minimum age (in number of hours) of tempfiles for removal, so h=0
#' will remove all tempfiles that were automatically created by Rsagacmd.
#' 
#' @section Notes:
#' SAGA-GIS compressed .sg-grd-z files are not currently supported, although
#' support may be added in future package updates.
#'
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
  # invalid_libs = which(
  #   basename(docs_libraries) %in%
  #      c('db_odbc', 'db_pgsql', 'docs_html', 'docs_pdf', 'garden_3d_viewer',
  #        'garden_fractals', 'garden_games', 'garden_webservices', 'gc_tools',
  #        'toolchains', 'tta_tools', 'tin_viewer', 'pointcloud_viewer',
  #        'garden_learn_to_program', 'grid_visualisation', 'group_files'))
  # docs_libraries = docs_libraries[-invalid_libs]
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
        options['validRIdentifier'] = identifiers
        
        # add parameter options to nested list
        libraries[[basename(libdir)]][[valid_toolname]] = list(
          options=options, cmd=toolName)
      }
      
      close(f)
    }
  }
  
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
#' @param param A single variable that may be a raster object, sp object, sf object or dataframe
#'
#' @return Character string of filename of saved object
#' @export
.RtoSAGA = function(param){

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
          stop('Raster object contains multiple bands; SAGA-GIS requires single band rasters as inputs')
        }
      }
    
    # Rasters stored in memory
    } else if (raster::inMemory(param) == TRUE){
      if (raster::nlayers(param) == 1) {
        temp = tempfile(fileext = '.tif')
        raster::filename(raster::writeRaster(param, filename = temp))
        param = temp
      } else {
        stop('Raster object contains multiple bands; SAGA-GIS requires single band rasters as inputs')
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
#' @param senv SAGA-GIS environment setting
#' @param intern Boolean to load outputs as R objects
#' @param cores Number of physical processing cores to use for computation. Default uses all available cores
#' @param ... Named arguments and values for SAGA tool
#'
#' @return Output of SAGA-GIS tool loaded as an R object (RasterLayer/sf/dataframe)
#' @export
sagaGeo = function(lib, tool, senv, intern = TRUE, cores, ...) {
  args = c(...)
  sagatool = senv$libraries[[lib]][[tool]][['options']]

  # match the validRidentifier to the identifier used by SAGA-GIS
  arg_names = names(args)
  arg_names = merge(
    x=data.frame(arg_names, stringsAsFactors = F),
    y=sagatool, by.x='arg_names', by.y='validRIdentifier',
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
        args[[i]][[j]] = .RtoSAGA(args[[i]][[j]])
    } else {
      args[[i]] = .RtoSAGA(args[[i]])
    }
  }

  # collapse argument values that are lists into a semi-colon separated strings
  for (i in seq_along(args))
    if (class(args[i]) == "list")
      args[i] = paste(args[[i]], collapse = ';')

  # replace sdat fileext used by raster package with sgrd used by SAGA
  args = gsub('.sdat', '.sgrd', args)
  
  # provide error if tool produces no outputs
  if (length(which(sagatool$IO == "Output")) == 0)
    stop(paste('SAGA Tool', tool, 'produces no outputs'))
  
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
    # some tools have no required outputs - error if no outputs in total are specified
    stop('Selected SAGA tool has no required outputs.... optional outputs must be specified as arguments')
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
    shQuote(senv$cmd), cores, flags, lib,
    shQuote(senv$libraries[[lib]][[tool]][['cmd']], type = quote_type),
    param_string)

  # execute system call
  msg = system(saga_cmd, intern = T)
  if (!is.null(attr(msg, "status"))){
    print (saga_cmd)
    print (msg)
  }

  # load SAGA results as list of R objects
  saga_results = list()
  for (i in 1:nrow(spec_out)){
    out_i = spec_out[i, 'args']
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
  
        # import raster data
        if (spec_out[i, 'Feature'] %in% c('Grid', 'Grid list', 'Raster')){
          if (tools::file_ext(out_i) == 'sgrd') out_i = gsub('.sgrd', '.sdat', out_i)
          if (tools::file_ext(out_i) == 'sg-gds-z'){
            message('Cannot load SAGA Grid Collection as an R raster object - this is not supported')
          } else {
            saga_results[[paste0(current_id)]] = raster::raster(out_i)  
          }
        }
      }, error = function(e){
        warning(paste0('No output for', spec_out[i, 'Identifier'],
                      '. The tool may require other inputs in order to calculate this output'))
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
  #if (class(saga_results) == 'list' & all(sapply(slope, class) == 'RasterLayer'))
  #  saga_results = raster::stif (class(args[[i]]) == "list")ack(saga_results)
  
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
        # get arguments and names
        args = as.list(environment())

        # get names of function
        fname = as.character(sys.call())[[1]]
        lib = strsplit(fname, '\\\\$')[[1]][2]
        tool = strsplit(fname, '\\\\$')[[1]][3]
        
        # optionally display help for selected tool
        if (usage == TRUE){
          print(subset(senv$libraries[[lib]][[tool]][['options']],
                  select=c(validRIdentifier,Name,Type,Description,Constraints)))
          return()
        }
        
        # remove intern and help from saga args list
        if ('intern' %in% names(args))
          args = args[-which(names(args) == 'intern')]
        if ('usage' %in% names(args))
          args = args[-which(names(args) == 'usage')]
        if ('cores' %in% names(args))
          args = args[-which(names(args) == 'cores')]

        # evaluate any arg_vals
        # for (i in seq_along(args))
        #   args[[i]] = eval(args[[i]])

        # call the saga geoprocessor
        saga_results = sagaGeo(lib, tool, senv, intern, cores, args)
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
              ', intern = TRUE, usage = FALSE, cores = NULL',
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

#' Removes temporary files created by Rsagacmd
#'
#' For convenience, functions in the Rsagacmd package create temporary files if
#' any required outputs for a SAGA-GIS tool are not specified as arguments.
#' Temporary files in R are automatically removed at the end of each session.
#' However, when dealing with raster data, these temporary files potentially can
#' consume large amounts of disk space. These temporary files can be observed
#' during a session by using the saga.showTmpFiles function, and can be removed
#' using the saga.removeTmpFiles function. Note that this function also removes
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
#' saga.removeTmpFiles(h=0)
#' }
saga.removeTmpFiles = function(h=0){
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
#' during a session by using the saga.showTmpFiles function, and can be removed
#' using the saga.removeTmpFiles function.
#'
#' @return returns the file names of the files in the temp directory that have
#' been generated by Rsagacmd. Note this list of files only includes the primary
#' file extension, i.e. '.shp' for a shapefile without the accessory files
#' (e.g. .prj, .shx etc.).
#' @export
#'
saga.showTmpFiles = function(){
  message('Rsagacmd temporary files:')
  for (f in pkg.env$sagaTmpFiles){
    message(f)
  }
  return(pkg.env$sagaTmpFiles)
}

# local environment to store vector of tempfiles in package namespace
pkg.env = new.env()
pkg.env$sagaTmpFiles = c()