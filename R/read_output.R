#' Read a spatial vector data set that is output by saga_cmd
#'
#' @param x list, a `options` object that was created by the `create_tool`
#'   function that contains the parameters for a particular tool and its
#'   outputs.
#'
#' @return an `sf` object.
#' 
#' @keywords internal
read_shapes <- function(x) {
  sf::st_read(x$args, quiet = TRUE)
}


#' Read a tabular data set that is output by saga_cmd
#'
#' @param x list, a `options` object that was created by the `create_tool`
#'   function that contains the parameters for a particular tool and its
#'   outputs.
#'
#' @return a `tibble`.
#' 
#' @keywords internal
read_table <- function(x) {
  if (tools::file_ext(x$args) == "txt") {
    object <- 
      utils::read.table(x$args, header = T, sep = "\t") %>%
      tibble::as_tibble()
    
  } else if (tools::file_ext(x$args) == "csv") {
    object <- 
      utils::read.csv(x$args) %>% 
      tibble::as_tibble()
    
  } else if (tools::file_ext(x$args) == "dbf") {
    object <- 
      foreign::read.dbf(x$args) %>%
      tibble::as_tibble()
  }
  
  object
}


#' Read a raster data set that is output by saga_cmd
#'
#' @param x list, a `options` object that was created by the `create_tool`
#'   function that contains the parameters for a particular tool and its
#'   outputs.
#' @param backend character, either "raster" or "terra"
#'
#' @return either a `raster` or `SpatRaster` object
#' 
#' @keywords internal
read_grid <- function(x, backend) {
  if (tools::file_ext(x$args) == "sg-gds-z") {
    warning(paste(
      "Cannot load SAGA Grid Collection as an R raster object",
      "- this is not currently supported"
    ))
  } else {
    if (backend == "raster")
      object <- raster::raster(x$args)
    if (backend == "terra")
      object <- terra::rast(x$args)
  }
  
  object
}


#' Read a semi-colon separated list of grids that are output by saga_cmd
#'
#' @param x list, a `options` object that was created by the `create_tool`
#'   function that contains the parameters for a particular tool and its
#'   outputs.
#' @param backend character, either "raster" or "terra"
#'
#' @return list, containing multile `raster` or `SpatRaster` objects.
#' 
#' @keywords internal
read_grid_list <- function(x, backend) {
  x$args <- strsplit(x$args, ";")[[1]]
  
  if (backend == "raster")
    object <- lapply(x$args, raster::raster)
  
  if (backend == "terra")
    object <- lapply(x$args, terra::rast)
  
  names(object) <- paste(x$alias, seq_along(x$args), sep = "_")
  
  # unlist if grid list but just a single output
  if (length(object) == 1)
    object <- object[[1]]
  
  object
}


#' Primary function to read data sets (raster, vector, tabular) that are output
#' by saga_cmd
#'
#' @param output list, a `options` object that was created by the `create_tool`
#'   function that contains the parameters for a particular tool and its
#'   outputs.
#' @param backend character, either "raster" or "terra"
#' @param .intern logical, whether to load the output as an R object
#'
#' @return the loaded objects, or NULL is `.intern = FALSE`.
#' 
#' @keywords internal
read_output <- function(output, backend, .intern, .all_outputs) {
  output$args <- gsub(".sgrd", ".sdat", output$args)

  if (.intern) {
    object <- tryCatch(expr = {
      
      switch(
        output$feature,
        "Shape" = read_shapes(output),
        "Table" = read_table(output),
        "Grid" = read_grid(output, backend),
        "Raster" = read_grid(output, backend),
        "Grid list" = read_grid_list(output, backend)
      )
      
    }, error = function(e) {
      
      if (.all_outputs)
        message(paste("No geoprocessing output for", 
                      output$alias, 
                      ". Results may require other input parameters to be specified"))

      return(NULL)
    })
    
  } else {
    object <- output$args
  }
  
  object
}
