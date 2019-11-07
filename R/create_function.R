create_function <- function(lib, tool) {
  paste(
    paste0("args = as.list(environment())"),
    paste0("lib = ", deparse(lib)),
    paste0("tool = ", deparse(tool)),
    "
        # remove intern and help from saga args list
        if ('.intern' %in% names(args))
            args = args[-which(names(args) == '.intern')]
        
        if ('.all_outputs' %in% names(args))
            args = args[-which(names(args) == '.all_outputs')]
        
        # call the saga geoprocessor
        saga_results = saga_execute(lib, tool, senv, .intern, .all_outputs, args)
        return (saga_results)
        ",
    sep = "\n"
  )
}