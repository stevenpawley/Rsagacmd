Rsagacmd
======

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

# initialize Rsagacmd and return a nested list containing named list entries to SAGA-GIS libraries and functions to the library tools
saga = initSAGA()

# Generate random terrain and load as raster object
dem = saga$grid_calculus$Random_Terrain(TARGET_OUT_GRID = tempfile(fileext='.sgrd'))

# Display help on usage for tool
saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(usage=TRUE)

# Use Rsagacmd for to calculate the terrain ruggedness index
tri = saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = dem, TRI = tempfile(fileext='.sgrd'))
plot(tri)

# Do not load output as an R object
saga$ta_morphometry$Terrain_Ruggedness_Index_TRI(DEM = r, TRI = tempfile(fileext='.sgrd'), intern=FALSE)
```
