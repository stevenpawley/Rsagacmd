# Rsagacmd 0.1.0

Rsagacmd 0.1.0 includes many behind-the-scenes changes that organize the
the internal representation of SAGA-GIS tools and parameter settings using a 
similar approach as used by the saga_api.

Most notably, this release provides support for the use of different raster
backend, including the existing 'raster' package, as well as the newer 'terra'
package. Configuration options to use different spatial formats as data is
passed from R to SAGA (and back) is also provided. For rasters, datasets that
are saved from R memory to disk for access by the saga command line (and vice
versa) can be in 'SAGA' format, 'SAGA Compressed', or 'GeoTIFF' (the default),
which can potentially save disk space when transferring large volumes of data.
Vector formats now include 'ESRI Shapefile', 'Geopackage' (the default) and
'GeoJSON'. The default use of the geopackage format eliminates problems when
transferring spatial datasets which have attribute names that are >10 characters
in length.

Enjoy!
