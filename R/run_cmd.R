parse_options <- function(arg, nm) {
  option <- paste0("-", nm)
  paste(option, arg, sep = ":")
}


run_cmd <- function(saga_cmd, saga_config, lib, tool_cmd, args, verbose) {
  cmd <- saga_cmd
  
  # add flag to load projections library
  flags <- "--flags=p"

  # add optional configuration flag
  if (!is.null(saga_config)) {
    saga_config <- paste("-C", saga_config, sep = "=")
  }
    
  # create options:value character vector
  param_string <- mapply(parse_options, args, names(args), USE.NAMES = FALSE)
  
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


