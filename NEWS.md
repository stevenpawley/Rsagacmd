# Rsagacmd 0.43

Important bug fix:

- Fix issue that prevented Rsagacmd working with R 4.4.1 related to
comparisons to numeric versions required to be made on character vectors.

# Rsagacmd 0.42

Minor bug fixes:

- Fixed issue with reading using the terra SpatVector backend.
- Change to the saga_cmd syntax by referring to a tool by its ID rather than 
its name, where possible. This avoids confusion with tools that have interactive
versions.
- Added package-level documentation.

# Rsagacmd 0.41

Minor fix to only pass character vectors to 'numeric_version'

# Rsagacmd 0.4

Changes:

 - Removed dependencies for raster and rgdal apackages

# Rsagacmd 0.2

New features:

- Support for SpatVector and SpatVectorProxy proxy objects for vector spatial
datasets.

- tidy methods to summarize information about the SAGA-GIS libraries and tools
within any library and return these as a tibble.

- Function 'saga_docs' to browse the online documentation for each SAGA-GIS
tool.

Changes:

 - Bug fix for using the `terra` package as a raster backend.

# Rsagacmd 0.1.2

New features:

- The `search_tool` function now returns a `tibble` containing the tools that
contain the matching term along with additional metadata including the author of
the tool, a description and the tool's parameters.

- The generic `tidy` S3 method from the `generics` package has been extended
with a tidy.saga_tool method that summarizes a SAGA-GIS tool object's parameters
into a tibble. This is designed for easy viewing of a tools options.

Changes:

- Fix for difference in syntax/parameters between the SAGA GUI and command line
affecting the TPI tool. The manual/docs list the parameters as specified in the
GUI, but these differ from those on the command line.

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
file extension is used relative to the `raster_format` or `vector_format`. This
ensures that outputs of SAGA-GIS tools will be correctly loaded by R.

- Rsagacmd saga_search should correctly recognize the location of the saga_cmd
binary on macOS that is included within the QGIS.app bundle, if installed from
official sources.

# Rsagacmd 0.1.0

Rsagacmd 0.1.0 includes many behind-the-scenes changes that organize the the
internal representation of SAGA-GIS tools and parameter settings using a similar
approach as used by the saga_api.

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
