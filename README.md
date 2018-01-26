Rsagacmd
======

Rsagacmd: A package for linking R with the open-source SAGA-GIS.

Rsagacmd is intended to provide an R scripting interface to the open-source https://sourceforge.net/projects/saga-gis/ SAGA-GIS software. The current version has been tested using SAGA-GIS 2.3.2, 5.0.0 and 6.1.0 on Windows (x64), OS X and Linux.

## Description

This package is not related to the \pkg{RSAGA} package, which provides an alternative interface to SAGA-GIS versions 2.0.4 - 2.2.3. In comparison to RSAGA, Rsagacmd supports newer versions of SAGA-GIS and provides access to SAGA-GIS tools by dynamically generating R functions that map to the html tags associated with every supoorted SAGA-GIS tool. These functions are returned as a named list of SAGA libraries, each of which contain a nested sub-list of functions that are mapped to each SAGA-GIS tool in the library. This facilitates an easier scripting experience by organizing the large number of SAGA-GIS tools (> 500) by their respective library. Each function's syntax is therfore similar to using the SAGA-GIS command line tool directly. Furthermore, because the arguments (called identifiers) for many SAGA-GIS tools are not consistently named, the user can also take advantage of code autocompletion tools (e.g. in http://rstudio.com Rstudio), allowing for each tools' inputs, outputs and options to be more easily recognized.

## Handling of geospatial and tabular data
Rsagacmd aims to facilitate a seamless interface to the open-source SAGA-GIS by providing access to all SAGA-GIS geoprocessing tools in a 'R-like' manner. In addition to mapping R functions to execute SAGA-GIS tools, Rsagacmd automatically handles the passing of geospatial and tabular data contained from the R environment to SAGA-GIS.

Rsagacmd uses the SAGA-GIS command line interface to perform geoprocessing operations. Therefore, all of the Rsagacmd tools allow paths to the input data to be used as arguments, if the data is stored in the appropriate file formats (e.g. GDAL-supported single-band rasters, OGR supported vector data, and comma- or tab-delimited text files for tabular data). In addition, Rsagacmd currently supports the following R object classes to pass data to SAGA-GIS, and to load the results back into the R environment:

- Raster data handling is provided by the R raster package. Raster-based outputs from SAGA-GIS tools are loaded as RasterLayer objects. For more details, see the 'Handling of raster data'.
- Vector features that result from SAGA-GIS geoprocessing operations are output in ESRI Shapefile format and are loaded into the R environment as simple features objects
- Tabular data from SAGA-GIS tools are loaded as dataframes

The results from tools that return multiple outputs are loaded into the R environment as a named list of the appropriate R object classes.

## Handling of raster data by Rsagacmd and SAGA-GIS
SAGA-GIS does not handle multi-band rasters and the native SAGA GIS Binary file format (.sgrd) supports only single band data. Therefore when passing raster data to most SAGA-GIS tools using Rsagacmd, the data should represent single raster bands, specified as either the path to the single raster band, or when using the R \pkg{raster} package, a RasterLayer (or less commonly a RasterStack or RasterBrick) object that contains only a single layer. Subsetting of raster data is performed automatically by Rsagacmd in the case of when a single band from a RasterStack or RasterBrick object is passed to a SAGA-GIS tool. This occurs in by either passing the filename of the raster to the SAGA-GIS command line, or by writing the data to a temporary file. However, a few SAGA-GIS functions will accept a list of single band rasters as an input. In this case if this data is in the form of a RasterStack or RasterLayer object, it is recommended to use the saga.SplitLayers function, which will return a list of RasterLayer objects, and then Rsagacmd will handle the subsetting automatically.
 
## Notes:
SAGA-GIS compressed .sg-grd-z files are not currently supported, although support may be added in future package updates once the format is supported by the rgdal package.

The use of temporary files during raster object subsetting operations will consume additional disk space. If you are dealing with large raster data, then you will need to take care of ensuring that tempfiles do not consume all available disk space, either by ensuing the raster data represent single band, on-disk files, or by manually clearing the contents of the tempdir() location periodically.

Rsagacmd is intended to provide an R-like scripting environment to the open-source SAGA-GIS. The current version has been tested using SAGA-GIS 5.0.0 and 6.1.0 on Windows (x64), OS X and Linux.

This package is not related to the RSAGA package, which provides an alternative method to link with SAGA-GIS versions 2.0.4 - 2.2.3. However, in addition to supporting newer versions of SAGA-GIS, Rsagacmd emphasises access to SAGA-GIS tools by dynamically generating R functions for every SAGA-GIS tool. These functions are embedded within a nested list structure. This facilitates an easier scripting experience because the function's syntax are similar to using the SAGA-GIS command line tool directly, and the user can also take advantage of code autocompletion tools, allowing for each tools' inputs, outputs and options to be more easily recognized.

## Dynamically-created functions to SAGA-GIS tools

Rsagacmd attempts to facilitate a seamless interface to the open-source SAGA-GIS by providing access to most SAGA-GIS geoprocessing tools in a R-like manner. By default, all results from SAGA-GIS tools are loaded as the appropriate R object:
- Raster-based outputs from SAGA-GIS tools are loaded as RasterLayer objects
- Vector features from SAGA-GIS tools in ESRI Shapefile format are loaded into the R environment as simple features objects
- Tabular data from SAGA-GIS tools are loaded as dataframes

## Package installation

#### First install the devtools package:
```
install.packages("devtools")
```

#### Next install Rsagacmd:
```
library(devtools)
install_github("/stevenpawley/Rsagacmd")
```

## Package usage
```
library(Rsagacmd)

# initialize Rsagacmd and return a nested list containing named list entries
# to SAGA-GIS libraries and functions to the library tools
saga = initSAGA()

# Generate random terrain and load as raster object
dem = saga$grid_calculus$Random_Terrain(TARGET_OUT_GRID = tempfile(fileext='.sgrd'))

# Display help on usage for tool
saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(usage=TRUE)

# Use Rsagacmd for to calculate the terrain ruggedness index
tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(
  DEM = dem,
  TRI = tempfile(fileext='.sgrd'))
plot(tri)

# Do not load output as an R object
saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(
  DEM = dem,
  TRI = tempfile(fileext='.sgrd'),
  intern=FALSE)
```
