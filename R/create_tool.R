#' Generates list of options for a SAGA-GIS tool
#'
#' Parses the html table for a SAGA-GIS tool into a list of identifiers,
#' options, defaults and constraints
#'
#' @param tool_information list
#' @param tool_options list
#'
#' @return A `saga_tool` object containing:
#' + `tool_name` A syntactically-correct name for the tool.
#' + `tool_cmd` The command to use for saga_cmd to execute tool.
#' + `parameters` A named list of the tool's parameter objects.
#'
#' @keywords internal
create_tool <- function(tool_information, tool_options) {
  
  # get the command to execute the saga_cmd tool
  saga_tool_cmd <- colnames(tool_information)[2]
  
  # create syntactically-correct name for the tool
  tool_name <- saga_tool_cmd %>%
    stringr::str_replace_all("^[0-9]+", "") %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("\\(", "") %>%
    stringr::str_replace_all("\\)", "") %>%
    stringr::str_replace_all("\\(", "") %>%
    stringr::str_replace_all("\\)", "") %>%
    stringr::str_replace_all("'", "") %>%
    stringr::str_replace_all(",", "_") %>%
    stringr::str_replace_all("/", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    stringr::str_replace_all(":", "_") %>%
    stringr::str_replace_all("\\[", "_") %>%
    stringr::str_replace_all("\\]", "_") %>%
    stringr::str_replace_all("&", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_+", "") %>%
    tolower()
  
  # strip input, output and options lines from table
  tool_options <-
    tool_options[which(apply(tool_options[, 2:5], 1, function(x)
      any(is.na(x))) == FALSE),]
  
  # get the parameters object
  params <- parameters(tool_options)
  
  # apply exceptions for specific saga-gis tools
  if (tool_name == "export_geotiff" | tool_name == "export_raster") {
    params$file$io <- "Output"
    params$file$feature <- "Grid"
    
  } else if (tool_name == "export_shapes" | tool_name == "export_shapes_to_kml") {
    params$file$io <- "Output"
    params$file$feature <- "Shapes"
    
  } else if (tool_name == "clip_grid_with_rectangle") {
    params$output$feature <- "Grid"
  
  } else if (tool_name == "tiling") {
    params$tiles_path$io <- "Output"
  }

  structure(list(
    tool_name = tool_name,
    tool_cmd = saga_tool_cmd,
    params = params
  ),
  class = "saga_tool")
}
