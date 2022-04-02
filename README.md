<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/Rsagacmd)](https://cran.r-project.org/package=Rsagacmd)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Rsagacmd)
[![Codecov test coverage](https://codecov.io/gh/stevenpawley/Rsagacmd/branch/master/graph/badge.svg)](https://app.codecov.io/gh/stevenpawley/Rsagacmd?branch=master)
[![R-CMD-check](https://github.com/stevenpawley/Rsagacmd/workflows/R-CMD-check/badge.svg)](https://github.com/stevenpawley/Rsagacmd/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

# **Rsagacmd** - a package for linking R with SAGA-GIS

**Rsagacmd** provides an R scripting interface to the open-source SAGA-GIS
(<https://sourceforge.net/projects/saga-gis/>) software. The current
version has been tested using SAGA-GIS 2.3.2, 5.0.0 - 8.0.1 on Windows
(x64), OS X and Linux.

## Contents

-   [Description](#description)
-   [Package installation](#package-installation)
-   [Usage](#usage)
-   [Passing geospatial and tabular data between R and
    SAGA-GIS](#passing-geospatial-and-tabular-data-between-r-and-saga-gis)
-   [Notes on handing multi-band raster datasets by **Rsagacmd** and
    SAGA-GIS](#notes-on-handing-multi-band-raster-datasets-by-**Rsagacmd**-and-saga-gis)
-   [Combining SAGA-GIS tools with
    pipes](#combining-saga-gis-tools-with-pipes)
-   [Notes](#notes)

## Description

**Rsagacmd** provides a complete interface between R and SAGA-GIS. The
package allows all SAGA-GIS tools and libraries to be used from within
R. Further, the most common geospatial datatypes within R (rasters,
simple features, and sp objects) are seamlessly passed between R and
SAGA-GIS during geoprocessing operations.

**Rsagacmd** is unrelated to the `RSAGA` package
(<https://cran.r-project.org/package=RSAGA>), which
provides a command line parser and a subset of pre-defined functions to
interface with SAGA-GIS. In contrast, **Rsagacmd** provides links with
SAGA-GIS by dynamically generating R functions for every SAGA-GIS tool
that is contained in the user's installed SAGA-GIS version. This means
that every SAGA-GIS tool is available for use within R (other than the
interactive tools), and **Rsagacmd** always remains up-to-date with new
versions of SAGA-GIS. Custom tools that have been created using SAGA's
`toolchains` (<https://rohanfisher.wordpress.com/saga-tool-chains/>)
will also be accessible via **Rsagacmd**.

## Package installation

#### CRAN version

**Rsagacmd** is now available on CRAN. To install this version run:

```r
install.packages("Rsagacmd")
```

In your R session.

#### Development version

First install the `devtools` package:

```r
install.packages("devtools")
```

Then install **Rsagacmd** from github:

```r
library(devtools)
install_github("stevenpawley/**Rsagacmd**")
```

## Usage

The primary function in **Rsagacmd** is the `saga_gis` function that returns
an object containing all of the SAGA-GIS libraries and tools in the same
structure as what is accessible within the GIS software itself. Each
tool is nested within its respective library and can be accessed by:

```r
library(Rsagacmd)

# initiate a saga object
saga <- saga_gis()

# access the libraries and tools
saga$ta_morphometry$mass_balance_index()
```

This facilitates an easier scripting experience by organizing the large
number of SAGA-GIS tools (\> 700) by their respective library. Each
function's syntax is the same as when using the SAGA-GIS command line
tool directly except that **Rsagacmd** always uses lowercase arguments).
Furthermore, because the arguments (called identifiers in SAGA-GIS) for
many SAGA-GIS tools are not consistently named, the user can also take
advantage of code autocompletion tools in RStudio, allowing for each
tools' inputs, outputs and options to be more easily recognized.

## Passing geospatial and tabular data between R and SAGA-GIS

**Rsagacmd** aims to facilitate a seamless interface to the open-source
SAGA-GIS by providing access to all SAGA-GIS geoprocessing tools in a
R-like manner. In addition to mapping R functions to execute SAGA-GIS
tools, **Rsagacmd** automatically handles the passing of geospatial and
tabular data contained from the R environment to SAGA-GIS.

**Rsagacmd** uses the SAGA-GIS command line interface to perform
geoprocessing operations. Therefore, all of the **Rsagacmd** tools allow
paths to the input data to be used as arguments, if the data is stored
in the appropriate file formats (GDAL-supported single-band rasters, OGR
supported vector data, and comma- or tab-delimited text files for
tabular data). In addition, **Rsagacmd** currently supports the following R
object classes to pass data to SAGA-GIS, and to load the results back
into the R environment:

-   Raster data handling is provided by the R **raster** package.
    Raster-based outputs from SAGA-GIS tools are loaded as `RasterLayer`
    objects. For more details, see the 'Handling of raster data'.
-   Vector features that result from SAGA-GIS geoprocessing operations
    are output in ESRI Shapefile format and are loaded into the R
    environment as simple features (`sf`) objects.
-   Tabular data from SAGA-GIS tools are loaded as dataframes.

The results from tools that return multiple outputs are loaded into the
R environment as a named list of the appropriate R object classes.

## Notes on handing multi-band raster datasets by **Rsagacmd** and SAGA-GIS

SAGA-GIS does not handle multi-band rasters and native SAGA GIS Binary
file format (.sgrd) supports only single band data. Therefore when
passing raster data to most SAGA-GIS tools using **Rsagacmd**, the data
should represent single raster bands, specified as either the path to
the single raster band, or when using the R **raster** package, a
`RasterLayer` (or less commonly a `RasterStack` or `RasterBrick`) object that
contains on-the-fly a single layer. Subsetting of raster data is
performed automatically by **Rsagacmd** in the case of when a single band
from a `RasterStack` or `RasterBrick` object is passed to a SAGA-GIS tool.
This occurs in by either passing the filename of the raster to the
SAGA-GIS command line, or by writing the data to a temporary file.
However, a few SAGA-GIS functions will accept a list of single band
rasters as an input. In this case if this data is in the form of a
`RasterStack` or `RasterLayer` object, it is recommended to use the unstack
function in the **raster** package, which will return a list of RasterLayer
objects, and then **Rsagacmd** will handle the subsetting automatically.

## Combining SAGA-GIS tools with pipes

For convenience, non-optional outputs from SAGA-GIS are automatically
saved to tempfiles if outputs are not explicitly stated, and then loaded
as the appropriate R object (`RasterLayer`, `sf` object, or a tibble).

This means that **Rsagacmd** can be used with `%>%` to quickly chain
together complex geoprocessing operations:

```r
saga <- saga_gis()

# Generate random terrain and save to file
dem <- saga$grid_calculus$random_terrain(target_out_grid = "terrain.sgrd")

# Terrain ruggedness index and automatically save the result to a tempfile
tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(dem = dem, radius = 3)
```

This example will write the output terrain ruggedness index to a
temporary file, and will automatically load the result into the R
environment as a `RasterLayer` object. This was implemented for
convenience, and so that the user can also create complex workflows that
require very little code. It is also means that you can combine several
processing steps with pipes:

```r
# read project area as a simple features object
prj_bnd <- st_read('some_shape.shp')
dem <- raster('some_dem.tif')

# clip dem to shape, resample, and calculate potential incoming solar radiation
pisr <- dem %>%
    saga$shapes_grid$clip_grid_with_rectangle(shapes = prj_bnd)) %>%
    saga$grid_tools$resampling(target_user_size = 100) %>%
    saga$ta_lighting$potential_incoming_solar_radiation(
        location = 1, 
        period = 2, 
        day = "2013-01-01", 
        day_stop = "2014-01-01",
        days_step = 10, 
        hour_step = 3, 
        method = 'Hofierka and Suri',
        grd_linke_default = 3
    )
```

In the above example, three tools are joined together using pipes, and
only the PISR grid is returned as a `RasterLayer` object. The intermediate
processing steps are dealt with automatically by saving the outputs as
tempfiles. When dealing with high-resolution and/or larger raster data,
these tempfiles can start to consume a significant amount of disk space
over a session. If required, temporary files can be cleaned during the
session in a similar way to the **raster** package, using:

```r
saga_remove_tmpfiles(h = 0)
```

where `h` is minimum age (in number of hours) of tempfiles for removal, so
`h = 0` will remove all tempfiles that were automatically created by
**Rsagacmd**.

The behaviour of automatically outputting results to tempfiles can be
disabled for any tool by using `.all_outputs = FALSE`. In this case, the
output arguments need to be specified manually, e.g.:

```r
tri <- saga$ta_morphometry$terrain_ruggedness_index_tri(
    dem = dem, 
    radius = 3, 
    tri = "somefile.sgrd"
)
```
