#' @description
#' Generates a list of `Parameter` objects for a SAGA-GIS tool
#' 
#' @details
#' Each `Parameter` object contains information about the datatype, permissible
#' values and input/output settings associated with each identifier for a
#' SAGA-GIS tool.
#' 
#' @keywords internal
Parameters_class = R6::R6Class(
  classname = "parameters_class",
  
  public = list(
    #' @field temp_path A character specifying the tempdir to use for storage
    temp_path = NULL,
    
    #' @description
    #' Create a new Parameters R6 class object.
    #' 
    #' The `Parameters` R6 class is a list of `Parameter` objects. Each 
    #' Parameter represents a blueprint for a single argument that is supplied
    #' to a SAGA-GIS tool, and a Parameters object represents all of the
    #' Parameters that can be supplied to a tool.
    #' 
    #' @param tool_options A data.frame containing that refers to the SAGA-GIS
    #' tool parameter options. This is essentially the SAGA-GIS help page for
    #' a tool that has been parsed into a data.frame.
    initialize = function(temp_path = NULL) {
      self$temp_path = temp_path
    },
    
    #' @description
    #' Generates a syntactically-correct R name based on a SAGA-GIS identifier
    #'
    #' @details
    #' SAGA-GIS identifiers sometimes cannot represent syntactically-correct
    #' names in R because they start with numbers or have spaces. They are also
    #' all in uppercase which is ugly to refer to in code. This function creates
    #' an alternative/alias identifier.
    #'
    #' @param identifier A character with the identifier.
    #' @return A character with a syntactically-correct alias.
    create_alias = function(identifier) {
      alias = identifier
      if (grepl("^[[:digit:]]", identifier)) alias = paste0("x", identifier)
      alias = gsub(" ", "_", alias)
      alias = tolower(alias)
      make.names(alias, unique = TRUE)
    },
    
    #' @description
    #' Updates the `file` attribute of each `Parameter` object.
    #' 
    #' @param raster_format file extension for raster formats.
    #' @param vector_format file extension for vector formats.
    update_parameters_file = function(raster_format, vector_format) {
      for (parameter in self$parameters) {
        parameter$update_parameter_file()
      }
    },
    
    #' @description
    #' Updates the `file` attribute of each `Parameter` object with a path to
    #' a temporary file if an output is required but no path is specificed by
    #' the user.
    #' 
    #' @param raster_format file extension for raster formats
    #' @param vector_format file extension for vector formats
    update_parameters_tempfiles = function(raster_format, vector_format) {
      # select only parameters that represent output types
      parameter_outputs = self$parameters[
        sapply(self$parameters, function(x) {!is.na(x$io)}
        )]
      parameter_outputs = parameter_outputs[
        sapply(parameter_outputs, function(x) {x$io == "Output"})
      ]
      parameter_outputs = names(parameter_outputs)
      
      for (parameter in parameter_outputs) {
        if (parameter$io == "Output" & is.null(parameter$files)) {
          parameter$assign_tempfile(raster_format, vector_format)
        }
      }
    },
    
    #' @description
    #' Drops unused/empty parameters from the `Parameters` object
    drop_parameters = function() {
      self$parameters = self$parameters[
        sapply(self$parameters, function(param) !is.null(param$value))
      ]
    }
  )
)