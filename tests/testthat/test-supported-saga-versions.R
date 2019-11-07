context("test-supported-saga-versions")
library(Rsagacmd)
library(raster)

download_saga <- function(surl) {
  # Downloads a saga windows x64 binary from sourceforge
  saga_zipdir <- file.path(tempdir(), tools::file_path_sans_ext(basename(surl)))
  dir.create(saga_zipdir, showWarnings = FALSE)

  err <- tryCatch({
    download.file(
      url = surl,
      destfile = tempfile(tmpdir = saga_zipdir, fileext = ".zip"),
      quiet = TRUE
    )

    unzip(
      zipfile = list.files(saga_zipdir, full.names = TRUE), 
      exdir = saga_zipdir)
    
  }, error = function(e) {
    return(NULL)
  })

  if (!is.null(err)) {
    saga_bin <- file.path(
      list.dirs(path = saga_zipdir, recursive = FALSE), 
      "saga_cmd.exe")
    
  } else {
    saga_bin <- NULL
  }

  return(saga_bin)
}


testthat::test_that("initiation of SAGA-GIS 2.3.1", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
    Sys.info()["machine"] == "x86-64")
  
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%202/SAGA%202.3.1/saga_2.3.1_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 3.0.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")

  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%203/SAGA%20-%203.0.0/saga_3.0.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 4.0.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")
    
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%204/SAGA%20-%204.0.0/saga_4.0.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 5.0.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")
    
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%205/SAGA%20-%205.0.0/saga-5.0.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 6.0.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")
  
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.0.0/saga-6.0.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 7.0.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")
  
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%207/SAGA%20-%207.0.0/saga-7.0.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})


testthat::test_that("initiation of SAGA-GIS 7.4.0", {
  testthat::skip_on_cran()
  testthat::skip_if_not(
    Sys.info()["sysname"] == "Windows" & 
      Sys.info()["machine"] == "x86-64")
  
  saga_url <-
    "https://sourceforge.net/projects/saga-gis/files/SAGA%20-%207/SAGA%20-%207.4.0/saga-7.4.0_x64.zip"
  saga_bin <- download_saga(saga_url)
  
  if (!is.null(saga_bin)) {
    saga <- saga_gis(saga_bin)
    testthat::expect_false(is.null(saga))
    testthat::expect_gt(length(saga), 0)
    testthat::expect_is(
      saga$grid_calculus$random_terrain(iterations = 1, radius = 1),
      "RasterLayer")
    unlink(dirname(saga_bin), recursive = TRUE)
  }
})