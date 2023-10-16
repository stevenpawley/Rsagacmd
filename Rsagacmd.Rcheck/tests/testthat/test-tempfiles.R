test_that("temporary file usage with all_outputs = TRUE", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))

  # all_outputs = TRUE
  # output should be added to saga tempfile list
  saga <- saga_gis(all_outputs = TRUE)
  saga_remove_tmpfiles()

  dem <- saga$grid_calculus$random_terrain()
  expect_length(saga_show_tmpfiles(), 1)
  saga_remove_tmpfiles()

  # output is given explicitly as argument - not added to tempfiles
  dem <- saga$grid_calculus$random_terrain(target_out_grid = tempfile(fileext = ".sgrd"))
  expect_length(saga_show_tmpfiles(), 0)
  saga_remove_tmpfiles()
})

test_that("temporary file usage with all_outputs = FALSE", {
  testthat::skip_on_cran()
  testthat::skip_if(is.null(search_saga()))

  # outputs are not added to tempfiles
  saga <- saga_gis(all_outputs = FALSE)
  saga_remove_tmpfiles()

  dem <- saga$grid_calculus$random_terrain(target_out_grid = tempfile(fileext = ".sgrd"))
  expect_length(saga_show_tmpfiles(), 0)

  # check multiple outputs
  lsps <- saga$ta_morphometry$slope_aspect_curvature(
    elevation = dem,
    slope = tempfile(fileext = ".sgrd"),
    aspect = tempfile(fileext = ".sgrd"),
    c_prof = tempfile(fileext = ".sgrd")
  )
  expect_length(lsps, 3)
  expect_length(saga_show_tmpfiles(), 0)
})
