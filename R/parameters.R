#' Generates a syntactically-correct R name based on a SAGA-GIS identifier
#'
#' SAGA-GIS identifiers sometimes cannot represent syntactically-correct names
#' in R because they start with numbers or have spaces. They are also all in
#' uppercase which is ugly to refer to in code. This function creates an
#' alternative/alias identifier.
#'
#' @param identifier A character with the identifier.
#'
#' @return A character with a syntactically-correct alias.
#'
#' @keywords internal
create_alias <- function(identifier) {
  alias <- identifier
  
  if (grepl("^[[:digit:]]", identifier))
    alias <- paste0("x", identifier)
  
  alias <- gsub(" ", "_", alias)
  alias <- tolower(alias)
  alias <- make.names(alias, unique = TRUE)
  
  alias
}


#' Generates a list of `parameter` objects for a SAGA-GIS tool
#' 
#' Each `parameter` object contains information about the datatype, permissible values
#' and input/output settings associated with each identifier for a SAGA-GIS tool.
#'
#' @param tool_options A data.frame containing the table that refers to the
#'   SAGA-GIS tool parameter options.
#'
#' @return A `parameters` object
#'
#' @keywords internal
parameters <- function(tool_options) {
  
  # replace tool arguments with syntactically-correct version
  tool_identifiers <- tool_options$Identifier
  tool_aliases <- sapply(tool_identifiers, create_alias)
  
  # convert options table to nested list
  params <- rep(list(NA), nrow(tool_options)) %>%
    stats::setNames(tool_aliases)
  
  for (i in seq_len(length(tool_aliases))) {
    alias <- tool_aliases[[i]]
    identifier <- tool_identifiers[[i]]
    
    params[[alias]] <- parameter(
      type = tool_options[tool_options$Identifier == identifier, "Type"],
      name = tool_options[tool_options$Identifier == identifier, "Name"],
      alias = alias,
      identifier = identifier,
      description = tool_options[tool_options$Identifier == identifier, "Description"],
      constraints = tool_options[tool_options$Identifier == identifier, "Constraints"]
    )
  }
  
  class(params) <- "parameters"
  params
}


#' Parameter class
#'
#' Stores metadata associated with each SAGA-GIS tool parameter.
#'
#' @param type A character to describe the data type of the parameter. One of
#'   "input", "output", "Grid", "Grid list", "Shapes", "Shapes list", "Table",
#'   "Static table", "Table list", "File path", "field", "Integer", "Choice",
#'   "Floating point", "Boolean", "Long text", "Text.
#' @param name A character with the long name of the parameter.
#' @param alias A syntactically correct alias for the identifier.
#' @param identifier A character with the identifier of the parameter used by
#'   saga_cmd.
#' @param description A character with the description of the parameter.
#' @param constraints A character describing the parameters constraints.
#'
#' @return A `parameter` class object.
#'
#' @keywords internal
parameter <-
  function(type,
           name,
           alias,
           identifier,
           description,
           constraints) {
    
    # parameter class attributes
    param <- list(
      type = type,
      name = name,
      alias = identifier,
      identifier = identifier,
      description = description,
      constraints = constraints,
      io = NA,
      feature = NA,
      default = NA,
      minimum = NA,
      maximum = NA,
      value = NULL,
      files = NULL
    )
    
    # generate syntactically-correct alias for identifier
    if (grepl("^[[:digit:]]", identifier))
      param$alias <- paste0("x", identifier)
    
    param$alias <- gsub(" ", "_", param$alias)
    param$alias <- tolower(param$alias)
    param$alias <- make.names(param$alias, unique = TRUE)
    
    # parse constraints into default, minimum, and maximum attributes
    if (param$constraints == "")
      param$constraints <- NA
    
    param$constraints <-
      stringr::str_replace_all(param$constraints, "Available Choices:", "")
    
    param$constraints <-
      stringr::str_replace_all(param$constraints, "^\n", "")
    
    param$constraints <-
      stringr::str_replace_all(param$constraints, "\n", ";")
    
    param$default <-
      stringr::str_extract(param$constraints, "(?<=Default: \\s{0,1})[-0-9.]+")
    
    param$default <-
      suppressWarnings(as.numeric(param$default))
    
    param$minimum <-
      stringr::str_extract(param$constraints, "(?<=Minimum: \\s{0,1})[-0-9.]+")
    
    param$minimum <-
      suppressWarnings(as.numeric(param$minimum))
    
    param$maximum <-
      stringr::str_extract(param$constraints, "(?<=Maximum: \\s{0,1})[-0-9.]+")
    
    param$maximum <-
      suppressWarnings(as.numeric(param$maximum))
    
    # parse type into explicit `io` attribute
    if (stringr::str_detect(param$type, "input"))
      param$io <- "Input"
    
    if (stringr::str_detect(param$type, "output"))
      param$io <- "Output"
    
    if (stringr::str_detect(param$type, "Grid"))
      param$feature <- "Grid"
    
    if (stringr::str_detect(param$type, "Grid list"))
      param$feature <- "Grid list"
    
    if (stringr::str_detect(param$type, "Shapes"))
      param$feature <- "Shape"
    
    if (stringr::str_detect(param$type, "Shapes list"))
      param$feature <- "Shapes list"
    
    if (stringr::str_detect(param$type, "Table"))
      param$feature <- "Table"
    
    if (stringr::str_detect(param$type, "Static table"))
      param$feature <- "Table"
    
    if (stringr::str_detect(param$type, "Table list"))
      param$feature <- "Table list"
    
    if (stringr::str_detect(param$type, "File path"))
      param$feature <- "File path"
    
    if (stringr::str_detect(param$type, "field"))
      param$feature <- "Table field"
    
    if (stringr::str_detect(param$type, "Integer"))
      param$feature <- "Integer"
    
    if (stringr::str_detect(param$type, "Choice"))
      param$feature <- "Choice"
    
    if (stringr::str_detect(param$type, "Floating point"))
      param$feature <- "numeric"
    
    if (stringr::str_detect(param$type, "Boolean"))
      param$feature <- "logical"
    
    if (stringr::str_detect(param$type, "Long text"))
      param$feature <- "character"
    
    if (stringr::str_detect(param$type, "Text"))
      param$feature <- "character"

    class(param) <- "parameter"
    param
  }
