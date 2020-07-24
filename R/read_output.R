read_shapes <- function(x) {
  # read a vector data output
  sf::st_read(x$args, quiet = TRUE)
}

read_table <- function(x) {
  # read a tabular data output
  
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

read_grid <- function(x, backend) {
  # read a raster grid output

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

read_grid_list <- function(x, backend) {
  # read a semi-colon separated list of grids
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

read_output <- function(output, backend, .intern) {
  # reads different output data types in R
  output$args <- gsub(".sgrd", ".sdat", output$args)

  if (isTRUE(.intern)) {
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
      message(
        paste(
          "No geoprocessing output for", output$alias,
          ". Results may require other input parameters to be specified"
          )
        )
      return(NULL)
    })
    
  } else {
    object <- output$args
  }
  
  object
}

