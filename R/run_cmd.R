parse_options <- function(arg, nm) {
  option <- paste0("-", nm)
  paste(option, arg, sep = ":")
}


run_cmd <- function(saga_cmd, saga_config, lib, tool_cmd, args, verbose) {
  cmd <- saga_cmd
  
  # add flag to load projections library
  flags <- "--flags=p"
  cmd <- paste(cmd, flags)

  # add optional configuration flag
  if (!is.na(saga_config)) {
    config <- paste("-C", shQuote(saga_config), sep = "=")
    cmd <- paste(cmd, config)
  }
    
  # create options:value character vector
  param_string <- mapply(parse_options, args, names(args), USE.NAMES = FALSE)
  
  # execute command  
  msg <- processx::run(
    command = saga_cmd, 
    args = c(lib, tool_cmd, param_string), 
    echo_cmd = verbose, 
    echo = verbose, 
    spinner = TRUE, 
    error_on_status = FALSE
  )

  return(msg)
}


