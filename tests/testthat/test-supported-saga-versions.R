context("test-supported-saga-versions")
library(Rsagacmd)
library(raster)

download_saga = function(surl) {
  # Downloads a saga windows x64 binary from sourceforge
  saga_zipdir = file.path(tempdir(), basename(surl))
  dir.create(saga_zipdir, showWarnings = FALSE)
  download.file(
    url = surl,
    destfile = tempfile(tmpdir = saga_zipdir, fileext = '.zip'), quiet = TRUE)
  unzip(zipfile = list.files(saga_zipdir, full.names = TRUE), exdir = saga_zipdir)
  saga_bin = file.path(list.dirs(path = saga_zipdir, recursive = FALSE), 'saga_cmd.exe')
  
  return(saga_bin)
}

testthat::test_that("initiation of all supported SAGA-GIS versions", {
  testthat::skip_on_cran()
  
  if (Sys.info()["sysname"] == "Windows" & Sys.info()["machine"] == 'x86-64') {
    saga_urls = c(
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%202/SAGA%202.3.1/saga_2.3.1_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%203/SAGA%20-%203.0.0/saga_3.0.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%204/SAGA%20-%204.0.0/saga_4.0.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%204/SAGA%20-%204.0.1/saga-4.0.1_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%204/SAGA%20-%204.1.0/saga-4.1.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%205/SAGA%20-%205.0.0/saga-5.0.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.0.0/saga-6.0.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.1.0/saga-6.1.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.2.0/saga-6.2.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.3.0/saga-6.3.0_x64.zip',
      'https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.4.0/saga-6.4.0_x64.zip'
    )
    
    lapply(saga_urls, function(surl) {
      saga_bin = download_saga(surl)
      saga = sagaGIS(saga_bin)
      testthat::expect_false(is.null(saga))
      testthat::expect_gt(length(saga), 0)
      testthat::expect_is(saga$grid_calculus$Random_Terrain(ITERATIONS = 1, RADIUS = 1), 'RasterLayer')
      unlink(dirname(saga_bin), recursive = TRUE)
    })
  }
  
})
