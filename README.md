sagacmd
======

sagacmd is an early version of an R package that is intended to provide an R-like scripting environment to the open-source SAGA-GIS. The current version has only been tested using SAGA-GIS 5.0.0 on Windows (x64).

This package is not related to the RSAGA package, which provides an excellent connection to SAGA-GIS versions 2.0.4 - 2.2.3. However, in addition to supporting a newer version of SAGA-GIS, RSAGA5 provides a more functional method of accessing most SAGA-GIS tools and libraries. sagacmd works by dynamically generating R functions for every SAGA-GIS tool and embeds these functions within a nested list structure. This facilitates an easier scripting experience because the function's syntax are similar to using the SAGA-GIS command line tool directly, and the user can also take advantage of code autocompletion in Rstudio allowing for each tools' inputs, outputs and options to be more easily recognized.

## Usage

#### First install the devtools package:
install.packages("devtools")

#### Next install sagacmd:
library(devtools)

install_github("/stevenpawley/sagacmd")

#### Tool usage:
library(sagacmd)

saga = initSAGA()

saga$ta_morphometry$Slope_Aspect_Curvature(ELEVATION = dem, SLOPE = 'Slope', ASPECT = 'Aspect')
