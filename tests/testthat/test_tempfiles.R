library(Rsagacmd)
library(testthat)

test_that("temporary file usage", {
  skip_on_cran()
  
  if (!is.null(saga_search())) {
    # all_outputs = TRUE
    # output added to tempfiles
    saga <- saga_gis()
    dem <- saga$grid_calculus$random_terrain()
    expect_length(saga_show_tmpfiles(), 1) 
    saga_remove_tmpfiles()
    
    # output is given explicitly as argument - not added to tempfiles
    dem <- saga$grid_calculus$random_terrain(target_out_grid = tempfile(fileext = ".sgrd"))
    expect_length(saga_show_tmpfiles(), 0)
    saga_remove_tmpfiles()
    
    # all_outputs = FALSE 
    # outputs are not added to tempfiles
    saga <- saga_gis(all_outputs = FALSE)
    dem <- saga$grid_calculus$random_terrain(target_out_grid = tempfile(fileext = ".sgrd"))
    expect_length(saga_show_tmpfiles(), 0)
  }
})

