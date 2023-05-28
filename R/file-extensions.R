get_file_ext <- function(x) {
  # function to get the file extension of a path
  # used instead of tools::file_ext because that does not work with '.sg-grd.z'
  ext <- strsplit(x, "\\.")[[1]]
  ext <- ext[length(ext)]
  paste0(".", ext)
}


quote_file_ext <- function(x) {
  paste0("'", x, "'")
}

#' Check the file extension of the output file to see if it is the same as the
#' `raster_format` or `vector_format` settings. If a raster, such as a GeoTIFF
#' is output directly from a SAGA-GIS tool but the raster format is set to SAGA,
#' then this might work depending on the saga version but Rsagacmd will not
#' know how to read the file.
#'
#' @param x a `parameter` object that is an output parameter of a tool.
#' @param raster_format the raster format.
#' @param vector_format the vector format.
#'
#' @return NULL
#' @keywords internal
check_output_format <- function(x, raster_format, vector_format) {
  if (x$feature == "Grid") {
    ext <- get_file_ext(x$files)

    if (ext != raster_format) {
      msg <- paste(
        "`raster_format` is set to", quote_file_ext(raster_format),
        "but you specified an output with a", quote_file_ext(ext),
        "file extension.",
        "This will prevent Rsagacmd from loading the output",
        "into your R environment"
      )

      stop(msg)
    }
  } else if (x$feature == "Grid list") {
    files <- strsplit(x$files, ";")[[1]]
    ext <- sapply(files, function(f) get_file_ext(f))
    incorrect_ext <- ext[ext != raster_format]

    if (any(ext != raster_format)) {
      msg <- paste(
        "`raster_format` is set to", quote_file_ext(raster_format),
        "but you specified an output with a",
        quote_file_ext(incorrect_ext), "file extension.",
        "This will prevent Rsagacmd from loading the output",
        "into your R environment"
      )
      stop(msg)
    }
  } else if (x$feature == "Shape") {
    ext <- get_file_ext(x$files)

    if (ext != vector_format) {
      msg <- paste(
        "`vector_format` is set to", quote_file_ext(vector_format),
        "but you specified an output with a", quote_file_ext(ext),
        "file extension.",
        "This will prevent Rsagacmd from loading the output",
        "into your R environment"
      )
      stop(msg)
    }
  } else if (x$feature == "Shapes list") {
    files <- strsplit(x$files, ";")[[1]]
    ext <- sapply(files, function(f) get_file_ext(f))
    incorrect_ext <- ext[ext != raster_format]

    if (any(ext != vector_format)) {
      msg <- paste(
        "`raster_format` is set to", quote_file_ext(vector_format),
        "but you specified an output with a",
        quote_file_ext(incorrect_ext), "file extension.",
        "This will prevent Rsagacmd from loading the output",
        "into your R environment"
      )
      stop(msg)
    }
  }
}

#' Ensure that the file extension for the SAGA raster format ends with .sdat for
#' reading or writing SAGA grid objects in R.
#'
#' This is used because the R raster/terra libraries expect to read and write
#' SAGA grid formats using the '.sdat' file extension, not '.sgrd'.
#'
#' @param fp file path to raster writing
#'
#' @return a character vector with the corrected file extensions to read SAGA
#'   sgrd files back into R.
#' @export
#' @keywords internal
convert_sagaext_r <- function(fp) {
  return(gsub(".sgrd", ".sdat", fp))
}
