# Rsagacmd 0.0.9

Rsagacmd 0.0.9 is a bug fix release that hopefully catches the remaining few bugs before moving onto adding new functionality in future releases. The main changes are:

* Bug fix relating to `search_tools` was broken in previous version
* Bug fix to return a single object (e.g. raster) when a SAGA-GIS 'grid list' contains only a single grid
* Bug fix to remove linking to any tools within SAGA-GIS that are interactive (i.e. are designed to run within the GUI) or do not produce any outputs (i.e. are designed to modify existing grid loaded into memory when using the GUI).

Thanks!
