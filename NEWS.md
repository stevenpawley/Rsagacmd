# Rsagacmd 0.1.1

New features:

- Included support for the 'stars' raster backend.

Changes:

- Because 'Rsagacmd' now supports multiple raster backends, the 'raster' package
is no longer automatically attached when using Rsagacmd. The raster/terra/stars
objects resulting from geoprocessing operations be still be loaded, but to apply
additional functions to these objects (e.g. plot) then the user needs to
explicitly load the desired backend, e.g. `library(stars)` in their script.

- The spinner is now turned off for geoprocessing operations because to avoid
creating an annoying extra line in Rmarkdown notebooks, jupyter etc.

- Rsagacmd now checks the file extensions of tools to ensure that the correct
file extension is used relative to the `raster_format` or `vector_format`. 
This ensures that outputs of SAGA-GIS tools will be correctly loaded by R.

# Rsagacmd 0.1.0

Rsagacmd 0.1.0 includes many behind-the-scenes changes that organize the
the internal representation of SAGA-GIS tools and parameter settings using a 
similar approach as used by the saga_api.

Most notably, this release provides support for the use of different raster
backend, including the existing 'raster' package, as well as the newer 'terra'
package. Configuration options to use different spatial formats as data is
passed from R to SAGA (and back) is also provided. For rasters, datasets that
are saved from R memory to disk for access by the saga command line (and vice
versa) can be in 'SAGA' format (the default), 'SAGA Compressed', or 'GeoTIFF',
which can potentially save disk space when transferring large volumes of data.
Vector formats now include 'ESRI Shapefile' (the default), 'Geopackage' and
'GeoJSON'. The use of the geopackage format eliminates problems when
transferring spatial datasets which have attribute names that are >10 characters
in length.

Enjoy!
