#' @description
#' Generates list of options for a SAGA-GIS tool
#'
#' @details
#' Parses the html table for a SAGA-GIS tool into a list of identifiers,
#' options, defaults and constraints
#'
#' @keywords internal
SagaTool <- R6::R6Class(
  classname = "SagaTool",
  
  public = list(
    #' @field tool_name A syntactically-correct name for the tool.
    tool_name = NULL,
    
    #' @field description The tool's description.
    description = NULL,
    
    #' @field author The tool's author.
    author = NULL,
    
    #' @field tool_cmd The command to use for saga_cmd to execute tool.
    tool_cmd = NULL,
    
    #' @field parameters A named list of the tool's `Parameter` objects.
    parameters = NULL,
    
    #' @field html_file The html document name.
    html_file = NULL,
    
    #' @description
    #' Creates a new SagaTool R6 class object.
    #' 
    #' @param tool_information list
    #' @param tool_options list
    #' @param description The description text for the tool that has been 
    #' scraped from the help documentation.
    #' @param html_file the name of the html file for the tool's documentation.
    #' This is stored to help linking with online documentation.
    initialize = function(tool_information, tool_options, description, html_file) {
      # parse the command to execute the saga_cmd tool
      saga_tool_cmd <- tool_information[[2]][1]
      author <- tool_information[[2]][2]
    
      # create syntactically-correct name for the tool
      tool_name <- gsub("^[0-9]+", "", saga_tool_cmd)
      tool_name <- gsub("^[0-9]+", "", tool_name)
      tool_name <- gsub(" ", "_", tool_name)
      tool_name <- gsub("\\(", "", tool_name)
      tool_name <- gsub("\\)", "", tool_name)
      tool_name <- gsub("\\(", "", tool_name)
      tool_name <- gsub("\\)", "", tool_name)
      tool_name <- gsub("'", "", tool_name)
      tool_name <- gsub(",", "_", tool_name)
      tool_name <- gsub("/", "_", tool_name)
      tool_name <- gsub("-", "_", tool_name)
      tool_name <- gsub(":", "_", tool_name)
      tool_name <- gsub("\\[", "_", tool_name)
      tool_name <- gsub("\\]", "_", tool_name)
      tool_name <- gsub("&", "_", tool_name)
      tool_name <- gsub("_+", "_", tool_name)
      tool_name <- gsub("^_+", "", tool_name)
      tool_name <- tolower(tool_name)
    
      # strip input, output and options lines from table
      # (rows in the table that represent headers/section breaks and have same value
      # like 'Input' filled across the row)
      header_rows <- apply(tool_options, 1, function(row) {
        length(unique(unlist(row)))
      })
      header_rows <- which(header_rows > 1)
      tool_options <- tool_options[header_rows, ]
    
      # get the parameters object
      params <- Parameters$new(tool_options)
    
      # apply exceptions for specific saga-gis tools
      params <- create_tool_overrides(tool_name, params)
  
      # update the class fields
      self$tool_name = tool_name
      self$description = description
      self$author = author
      self$tool_cmd = saga_tool_cmd
      self$parameters = params
      self$html_file = html_file
    }
  )
)
