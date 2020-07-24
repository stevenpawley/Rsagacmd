#' Convenience function to join together the saga_cmd option:value pairs
#'
#' @param key character, the saga_cmd option such as "DEM".
#' @param value character, the value of the option.
#'
#' @return character, a joined option:value pair such as "-DEM:mygrid.tif"
#' 
#' @keywords internal
parse_options <- function(key, value) {
  option <- paste0("-", key)
  paste(option, value, sep = ":")
}


#' Prepares the statement and runs the external saga_cmd executable
#'
#' @param saga_cmd character, name of the saga_cmd executable or alias.
#' @param saga_config character, path to the saga configuration "ini" file.
#' @param lib character, name of the selected library.
#' @param tool_cmd character, name of the selected tool.
#' @param args named list of tool options, such as list(DEM = "mygrid.tif", RADIUS = 3).
#' @param verbose logical, whether to show all saga_cmd messages on the R console.
#'
#' @return list, output from `processx::run()`
#' 
#' @keywords internal
run_cmd <- function(saga_cmd, saga_config, lib, tool_cmd, args, verbose) {
  cmd <- saga_cmd
  
  # add flag to load projections library
  flags <- "--flags=p"

  # add optional configuration flag
  if (!is.null(saga_config)) {
    saga_config <- paste("-C", saga_config, sep = "=")
  }
    
  # create options:value character vector
  param_string <- mapply(parse_options, names(args), args, USE.NAMES = FALSE)
  
  # execute command  
  msg <- processx::run(
    command = cmd, 
    args = c(flags, saga_config, lib, tool_cmd, param_string), 
    echo_cmd = verbose, 
    echo = verbose, 
    spinner = TRUE, 
    error_on_status = FALSE
  )

  return(msg)
}
