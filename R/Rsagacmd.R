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
#' returned from the initSAGA function as a named list of SAGA-GIS libraries,
#' each of which contain a nested sub-list of functions that correspond to each
#' SAGA-GIS tool in the library. This facilitates an easier scripting experience
#' by organizing the large number of SAGA-GIS tools (> 500) by their respective
#' library. Each function's syntax is therefore similar to what is encounted
#' when using the SAGA-GIS command line tool directly. Furthermore, because the
#' arguments (called identifiers) for many SAGA-GIS tools are not consistently
#' named, the user can also take advantage of code autocompletion tools (e.g. in
#' \href{http://rstudio.com}{Rstudio}), allowing for each tools' inputs, outputs
#' and options to be more quickly recognized.
#' 
#' @section Handling of geospatial and tabular data:
#' Rsagacmd aims to facilitate a seamless interface to the open-source SAGA-GIS
#' by providing access to all SAGA-GIS geoprocessing tools in a 'R-like' manner.
#' In addition to generating R functions that correspond to each SAGA-GIS tool,
#' Rsagacmd automatically handles the passing of geospatial and tabular data
#' contained from the R environment to SAGA-GIS.
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
#' \item Tabular data from SAGA-GIS tools are loaded as data frames
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
#' A few SAGA-GIS functions will accept a list of single band rasters as an
#' input. In this case if this data is in the form of a RasterStack or
#' RasterLayer object, it is recommended to use pass the output from the unstack
#' function in the \pkg{raster} package, which will return a list of RasterLayer
#' objects, and then Rsagacmd will handle the subsetting automatically.
#' 
#' @section Combining SAGA-GIS commands with pipes:
#' For convenience, non-optional outputs from SAGA-GIS are automatically saved
#' to tempfiles if outputs are not explicitly stated as arguments when calling
#' the function. This was implemented so that the user can create complex
#' workflows based on little code. It is also means that several processing
#' steps can be combined or chained in a convenient manner using pipes from the
#' \pkg{magritrr} package. When using pipes, all of the intermediate processing
#' steps are dealt with automatically by saving the outputs as tempfiles, and
#' then in turn passing the output to the next function in the chain. Note that
#' when dealing with high-resolution and/or larger raster data, these tempfiles
#' can start to consume a significant amount of disk space during a session. If
#' required, these temporary files can be cleaned during the session in a
#' similar way to the raster package, using the saga.removeTmpFiles function.
#' 
#' @section Notes:
#' SAGA-GIS compressed .sg-grd-z files are not currently supported, although
#' support may be added in future package updates.
#' 
#' @examples
#' \dontrun{
#' # initialize a saga.gis class object
#' library(Rsagacmd)
#' saga = initSAGA('C:/SAGA-GIS')
#' 
#' # Generate random terrain and save to file
#' dem = saga$grid_calculus$Random_Terrain(TARGET_OUT_GRID = 'terrain.sgrd')
#' 
#' # Example of automatically using tempfiles by not explicitly stating an
#' # output file for a non-optional SAGA-GIS tool argument
#' dem = saga$grid_calculus$Random_Terrain()
#' tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM=dem, RADIUS=3)
#' 
#' # Example of chaining operations using pipes
#' library(maggritr)
#' tri = saga$grid_calculus$Random_Terrain() %>%
#'     saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(RADIUS=3)
#' 
#' # A more complex use of pipes
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
#' # Remove tempfiles generated by Rsagacmd during a session
#' sagaRemoveTmpFiles(h=0)
#' }
#'
#' @author Steven Pawley, \email{dr.stevenpawley@gmail.com}

#' @docType package
#' @name Rsagacmd
NULL