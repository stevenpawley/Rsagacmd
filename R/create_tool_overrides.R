#' Apply manually-defined changes to specific tools
#' 
#' Used to manually alter or add parameters for specific tools outside of what
#' has been defined based on the output of saga_cmd --create-docs
#'
#' @param tool_name character, name of the tool. This is the alias name used by
#' Rsagacmd, i.e. the tool name without spaces, all lowercase etc.
#' @param params the `parameters` object for the tool
#'
#' @return the altered `parameters` object
#' @keywords internal
create_tool_overrides <- function(tool_name, params) {
  if (tool_name == "export_geotiff" | tool_name == "export_raster") {
    params$file$io <- "Output"

  } else if (tool_name == "export_shapes" | tool_name == "export_shapes_to_kml") {
    params$file$io <- "Output"

  } else if (tool_name == "clip_grid_with_rectangle") {
    params$output$feature <- "Grid"
    
  } else if (tool_name == "tiling") {
    params$tiles_path$io <- "Output"
    
  } else if (tool_name == "tpi_based_landform_classification") {
    if (!"radius_a_min" %in% names(params)) {
      names(params)[names(params) == "radius_a"] <- "radius_a_min"
      params$radius_a_min$alias <- "radius_a_min"
      params$radius_a_min$identifier <- "RADIUS_A_MIN"
      params$radius_a_min$default <- 0
      
      names(params)[names(params) == "radius_b"] <- "radius_b_min"
      params$radius_b_min$alias <- "radius_b_min"
      params$radius_b_min$identifier <- "RADIUS_B_MIN"
      params$radius_b_min$default <- 0
      
      params$radius_a_max <- params$radius_a_min
      params$radius_a_max$alias <- "radius_a_max"
      params$radius_a_max$identifier <- "RADIUS_A_MAX"
      params$radius_a_max$default <- 100
      
      params$radius_b_max <- params$radius_b_min
      params$radius_b_max$name <- "Large Scale"
      params$radius_b_max$alias <- "radius_b_max"
      params$radius_b_max$identifier <- "RADIUS_B_MAX"
      params$radius_b_max$default <- 1000
      
      args <- c("dem", "landforms", "radius_a_min", "radius_a_max",
                "radius_b_min", "radius_b_max", "dw_weighting", "dw_idw_power",
                "dw_idw_offset", "dw_bandwidth")
      
      params <- params[args]      
    }
  }
    
  params
}
