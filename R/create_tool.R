#' Generates list of options for a saga-gis tool
#' 
#' Parses the html table for a saga-gis tool into a list of 
#' identifiers, options, defaults and constraints
#'
#' @param tool_information list
#' @param tool_options list
#'
#' @return named list, containing three items:
#' `tool_name` syntactically-correct name for the tool
#' `tool_cmd` command to use for saga_cmd to execute tool
#' `options` named list of the tool's parameters containing:
#'    `type` data type of parameter
#'    `name` long name of the parameter
#'    `alias` synatically correct alias for the SAGA's identifier
#'    `identifier` identifier used by saga_cmd
#'    `description` tool description
#'    `constraints` description of the parameters constraints
#'    `default` default parameter value
#'    `minimum` minimum permissible value
#'    `maximum` maximum permissible value
#'    `feature` type of feature, e.g. Shapes, Shapes List, Grid etc.
#'    `io` whether the parameter represents an input or output
#'        
#' @importFrom stringr str_replace_all str_extract str_detect
#' @importFrom stats setNames
create_tool <- function(tool_information, tool_options) {
  
  # create syntactically valid tool_name
  saga_tool_cmd <- colnames(tool_information)[2]
  
  tool_name <- saga_tool_cmd %>%
    str_replace_all("^[0-9]+", "") %>% # remove digits from start of tool name
    str_replace_all(" ", "_") %>% # replace spaces with underscores
    str_replace_all("\\(", "") %>% # replace spaces with underscores
    str_replace_all("\\)", "") %>% # replace spaces with underscores
    str_replace_all("\\(", "") %>% # remove parenthesis
    str_replace_all("\\)", "") %>% # remove parenthesis
    str_replace_all("'", "") %>% # remove single quotations
    str_replace_all(",", "_") %>% # remove commas
    str_replace_all("/", "_") %>% # replace forward slash with underscore
    str_replace_all("-", "_") %>% # replace minus with underscore
    str_replace_all(":", "_") %>% # replace colon with underscore
    str_replace_all("\\[", "_") %>% # replace square brackets with underscore
    str_replace_all("\\]", "_") %>% # replace square brackets with underscore
    str_replace_all("&", "_") %>% # replace ampersand with underscore
    str_replace_all("_+", "_") %>% # replace multiple underscores due to above with single _
    str_replace_all("^_+", "") %>% # remove underscore from start of tool name
    tolower()
  
  # strip input, output and options lines from table
  tool_options <-
    tool_options[which(apply(tool_options[, 2:5], 1, function(x)
      any(is.na(x))) == FALSE), ]
  
  # replace tool arguments with synatically-correct version
  tool_identifiers <- tool_options$Identifier
  tool_aliases <- sapply(tool_identifiers, function(x)
    if (grepl("^[[:digit:]]", x)) paste0("x", x) else x, USE.NAMES = FALSE)
  tool_aliases <- gsub(" ", "_", tool_aliases)
  
  # check for duplicated arguments that occur in some saga tools
  if (any(duplicated(tool_identifiers))) {
    warning(
      paste(
        "Cannot parse SAGA-GIS tool", 
        tool_name,
        "due to duplicated identifiers being as arguments")
      )
    return()
  }
  
  # convert options table to nested list
  params <- rep(list(NA), nrow(tool_options)) %>% setNames(tool_aliases)
  
  for (i in seq_len(length(tool_aliases))) {
    alias <- tool_aliases[[i]]
    identifier <- tool_identifiers[[i]]
    
    params[[alias]] <- 
      list(
        type = tool_options[tool_options$Identifier == identifier, "Type"],
        name = tool_options[tool_options$Identifier == identifier, "Name"],
        alias = alias,
        identifier = identifier,
        description = tool_options[tool_options$Identifier == identifier, "Description"],
        constraints = tool_options[tool_options$Identifier == identifier, "Constraints"],
        io = NA,
        feature = NA,
        default = NA,
        minimum = NA,
        maximum = NA
      )
    
    # clean constraints
    if (params[[alias]]$constraints == "")
      params[[alias]]$constraints <- NA
    
    params[[alias]]$constraints <-
      params[[alias]]$constraints %>%
      str_replace_all("Available Choices:", "")
    
    params[[alias]]$constraints <-
      params[[alias]]$constraints %>%
      str_replace_all("^\n", "")
    
    params[[alias]]$constraints <-
      params[[alias]]$constraints %>%
      str_replace_all("\n", ";")
    
    params[[alias]]$default <-
      params[[alias]]$constraints %>%
      str_extract("(?<=Default: \\s{0,1})[-0-9.]+")
    
    params[[alias]]$default <-
      suppressWarnings(as.numeric(params[[alias]]$default))
    
    params[[alias]]$minimum <-
      params[[alias]]$constraints %>%
      str_extract("(?<=Minimum: \\s{0,1})[-0-9.]+")
    
    params[[alias]]$minimum <-
      suppressWarnings(as.numeric(params[[alias]]$minimum))
    
    params[[alias]]$maximum <-
      params[[alias]]$constraints %>%
      str_extract("(?<=Maximum: \\s{0,1})[-0-9.]+")
    
    params[[alias]]$maximum <-
      suppressWarnings(as.numeric(params[[alias]]$maximum))
  }
  
  # parse additional parameters
  for (n in names(params)) {
    if (str_detect(params[[n]]$type, "input")) params[[n]]$io <- "Input"
    if (str_detect(params[[n]]$type, "output")) params[[n]]$io <- "Output"
    if (str_detect(params[[n]]$type, "Grid")) params[[n]]$feature <- "Grid"
    if (str_detect(params[[n]]$type, "Grid list")) params[[n]]$feature <- "Grid list"
    if (str_detect(params[[n]]$type, "Shapes")) params[[n]]$feature <- "Shape"
    if (str_detect(params[[n]]$type, "Shapes list")) params[[n]]$feature <- "Shapes list"
    if (str_detect(params[[n]]$type, "Table")) params[[n]]$feature <- "Table"
    if (str_detect(params[[n]]$type, "Static table")) params[[n]]$feature <- "Table"
    if (str_detect(params[[n]]$type, "Table list")) params[[n]]$feature <- "Table list"
    if (str_detect(params[[n]]$type, "File path")) params[[n]]$feature <- "File path"
    if (str_detect(params[[n]]$type, "field")) params[[n]]$feature <- "Table field"
    if (str_detect(params[[n]]$type, "Integer")) params[[n]]$feature <- "Integer"
    if (str_detect(params[[n]]$type, "Choice")) params[[n]]$feature <- "Choice"
    if (str_detect(params[[n]]$type, "Floating point")) params[[n]]$feature <- "numeric"
    if (str_detect(params[[n]]$type, "Boolean")) params[[n]]$feature <- "logical"
    if (str_detect(params[[n]]$type, "Long text")) params[[n]]$feature <- "character"
    if (str_detect(params[[n]]$type, "Text")) params[[n]]$feature <- "character"
  }
  
  # exceptions
  if (tool_name == "export_geotiff" | tool_name == "export_raster") {
    params$FILE$io <- "Output"
    params$FILE$feature <- "Grid"
  } else if (tool_name == "export_shapes" | tool_name == "export_shapes_to_kml") {
    params$FILE$io <- "Output"
    params$FILE$feature <- "Shapes"
  } else if (tool_name == "clip_grid_with_rectangle") {
    params$OUTPUT$feature <- "Grid"
  }
  
  list(
    tool_name = tool_name,
    tool_cmd = saga_tool_cmd, 
    options = params
    )
}

