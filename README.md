Rsagacmd
======

Rsagacmd is intended to provide an R-like scripting environment to the open-source SAGA-GIS. The current version has only been tested using SAGA-GIS 5.0.0 and 6.0.0 on Windows (x64), OS X and Linux.

This package is not related to the RSAGA package, which provides an excellent connection to SAGA-GIS versions 2.0.4 - 2.2.3. However, in addition to supporting newer versions of SAGA-GIS, Rsagacmd provides a more functional method of accessing SAGA-GIS tools and libraries. Rsagacmd works by dynamically generating R functions for every SAGA-GIS tool and embeds these functions within a nested list structure. This facilitates an easier scripting experience because the function's syntax are similar to using the SAGA-GIS command line tool directly, and the user can also take advantage of code autocompletion tools (e.g in Rstudio), allowing for each tools' inputs, outputs and options to be more easily recognized.

## Usage

#### First install the devtools package:
install.packages("devtools")

#### Next install Rsagacmd:
library(devtools)

install_github("/stevenpawley/Rsagacmd")

#### Tool usage:
library(Rsagacmd)

saga = initSAGA()

saga$ta_morphometry$Slope_Aspect_Curvature(ELEVATION = dem, SLOPE = 'Slope', ASPECT = 'Aspect')
