# Parameters R6 class ----

#' @description
#' Generates a list of `Parameter` objects for a SAGA-GIS tool
#' 
#' @details
#' Each `Parameter` object contains information about the datatype, permissible
#' values and input/output settings associated with each identifier for a
#' SAGA-GIS tool.
#' 
#' @keywords internal
Parameters = R6::R6Class(
  classname = "parameters",
  
  public = list(
    #' @field parameters List of `parameter` objects
    parameters = list(),
    
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
    initialize = function(tool_options, temp_path = NULL) {
      # replace tool arguments with syntactically-correct version
      tool_identifiers = tool_options$Identifier
      tool_aliases = sapply(tool_identifiers, self$create_alias)
      tool_aliases = make.names(tool_aliases, unique = TRUE)
      
      # convert options table to nested list
      params = rep(list(NA), nrow(tool_options))
      params = stats::setNames(params, tool_aliases)
      
      
      alias <- tool_aliases[[1]]
      identifier <- tool_identifiers[[1]]
      
      parameters = Parameter_class$new(
        type = tool_options[tool_options$Identifier == identifier, ][["Type"]],
        name = tool_options[tool_options$Identifier == identifier, ][["Name"]],
        alias = alias,
        identifier = identifier,
        description = tool_options[tool_options$Identifier == identifier, ][["Description"]],
        constraints = tool_options[tool_options$Identifier == identifier, ][["Constraints"]]
      )
      
      for (i in seq_len(length(tool_aliases))) {
        browser()

        alias <- tool_aliases[[i]]
        identifier <- tool_identifiers[[i]]
        
        self$parameters[[alias]] = Parameter$new(
          type = tool_options[tool_options$Identifier == identifier, ][["Type"]],
          name = tool_options[tool_options$Identifier == identifier, ][["Name"]],
          alias = alias,
          identifier = identifier,
          description = tool_options[tool_options$Identifier == identifier, ][["Description"]],
          constraints = tool_options[tool_options$Identifier == identifier, ][["Constraints"]]
        )
      }
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


# Parameter R6 class ----

#' Parameter class
#' 
#' @description
#' Stores metadata associated with each SAGA-GIS tool parameter.
#' 
#' @keywords internal
Parameter = R6::R6Class(
  classname = "parameter",
  public = list(
    #' @field type A character to describe the data type of the parameter such 
    #' as "input", "output", "Grid", "Grid list", "Shapes", etc.
    type = NULL,
    
    #' @field name Character with the long name of the parameter.
    name = NULL,
    
    #' @field alias Syntactically correct alias for the identifier.
    alias = NULL,
    
    #' @field identifier Character with the identifier used by saga_cmd.
    identifier = NULL,
    
    #' @field description A character with the description of the parameter.
    description = NULL,
    
    #' @field constraints A character describing the parameters constraints.
    constraints = NULL,
    
    #' @field io Either purpose of the parameters, either 'input' or 'output'.
    io = NA,
    
    #' @field feature The type of data that the parameter represents. Examples
    #' include 'Grid', 'Grid list', 'Shapes' etc.
    feature = NA,
    
    #' @field default The default value for the parameters.
    default = NA,
    
    #' @field minumum The minimum value for the parameter.
    minimum = NA,
    
    #' @field maximum The maximum value for the parameter.
    maximum = NA,
    
    #' @field value The assigned value for the parameter. This value will be
    #'   updated when the tool is run by the user.
    value = NULL,
    
    #' @field files Any files (potentially after being written to disk) that
    #' are associated with the parameter, for example an input grid.
    files = NULL,
    
    #' @field temp_path Character specifying the tempdir to use for storage
    temp_path = tempdir(),
    
    #' @field raster_format Name of the raster format to use for saving data.
    #' Must match a name in `supported_raster_formats`.
    raster_format = "SAGA",
    
    #' @field vector_format Name of the vector format to use for saving data.
    #' Must match a name in `supported_vector_formats`.
    vector_format = "ESRI Shapefile",
    
    #' @description
    #' Create a new parameter object.
    #' 
    #' @param type A character to describe the data type of the parameter.
    #' @param name A character with the long name of the parameter.
    #' @param alias A syntactically correct alias for the identifier.
    #' @param identifier A character with the identifier of the parameter used
    #' by saga_cmd.
    #' @param description A character with the description of the parameter.
    #' @param constraints A character describing the parameters constraints.
    initialize = function(type, name, alias, identifier, description, 
                          constraints) {
      # assign initial values
      self$type = tolower(type[1])
      self$name = name[1]
      self$alias = alias[1]
      self$identifier = identifier[1]
      self$description = description[1]
      self$constraints = constraints[1]

      # generate syntactically-correct alias for identifier
      if (grepl("^[[:digit:]]", self$identifier)) {
        self$alias = paste0("x", self$identifier)
      }
      self$alias = gsub(" ", "_", self$alias)
      self$alias = tolower(self$alias)
      self$alias = make.names(self$alias, unique = TRUE)
      
      # set description to explicit NA
      self$description =
        ifelse(self$description == "", NA_character_, self$description)
      
      # parse constraints into default, minimum, and maximum attributes
      self$constraints = gsub("Available Choices:", "", self$constraints)
      self$constraints = gsub("^\n", "", self$constraints)
      self$constraints = gsub("\n", ";", self$constraints)
      
      self$default = stringr::str_extract(self$constraints, "(?<=Default: \\s{0,1})[-0-9.]+")
      self$default = suppressWarnings(as.numeric(self$default))
      
      self$minimum = stringr::str_extract(self$constraints, "(?<=Minimum: \\s{0,1})[-0-9.]+")
      self$minimum = suppressWarnings(as.numeric(self$minimum))
      
      self$maximum = stringr::str_extract(self$constraints, "(?<=Maximum: \\s{0,1})[-0-9.]+")
      self$maximum = suppressWarnings(as.numeric(self$maximum))
      
      # convert constraints into lists
      self$constraints = strsplit(self$constraints, "Default: ")[[1]][1]
      
      if (!is.na(self$constraints)) {
        if (self$constraints == "") {
          self$constraints = NA_character_
        }
      }
      
      if (!is.na(self$constraints)) {
        self$constraints = strsplit(self$constraints, "\\[[:digit:]\\]")[[1]]
        self$constraints = self$constraints[self$constraints != ""]
        self$constraints = trimws(self$constraints)
        self$constraints = paste(
          paste0("[", seq_along(self$constraints) - 1, "]"),
          self$constraints
        )
      }
      
      # parse type into explicit `io` attribute
      if (grepl("input", self$type)) self$io = "Input"
      if (grepl("output", self$type)) self$io = "Output"
      if (grepl("grid", self$type)) self$feature = "Grid"
      if (grepl("grid list", self$type)) self$feature = "Grid list"
      if (grepl("shapes", self$type)) self$feature = "Shape"
      if (grepl("shapes list", self$type)) self$feature = "Shapes list"
      if (grepl("table", self$type)) self$feature = "Table"
      if (grepl("static table", self$type)) self$feature = "Table"
      if (grepl("table list", self$type)) self$feature = "Table list"
      if (grepl("file path", self$type)) self$feature = "File path"
      if (grepl("field", self$type)) self$feature = "Table field"
      if (grepl("integer", self$type)) self$feature = "Integer"
      if (grepl("choice", self$type)) self$feature = "Choice"
      if (grepl("floating point", self$type)) self$feature = "numeric"
      if (grepl("boolean", self$type)) self$feature = "logical"
      if (grepl("long text", self$type)) self$feature = "character"
      if (grepl("text", self$type)) self$feature = "character"
    },
    
    #' @description
    #' Updates the `Parameter` object with file paths to the R data objects.
    update_parameter_file = function() {
      # check to see if the 'value' object needs to be written to disk
      if (!is.null(self$files)) {
        self$files = save_object(
          self$value,
          temp_path = self$temp_path,
          raster_format = self$raster_format,
          vector_format = self$vector_format
        )
      }
      # collapse lists into semi-colon separated string
      if (length(self$files) > 1) {
        self$files = paste(self$files, collapse = ";")
      }
    },
  
    #' @description
    #' Update a parameters file field with a path to a temporary file to
    #' collect outputs from SAGA-GIS that are automatically created.
    assign_tempfile = function() {
      # create tempfile path with the correct file extension
      if (self$feature %in% private$grid_features) {
        self$files = tempfile(self$temp_path, self$raster_format)
      } else if (self$feature %in% private$shape_features) {
        self$files = tempfile(self$temp_path, self$vector_format)
      } else if (self$feature == private$table_features) {
        self$files = tempfile(self$temp_path, ".csv")
      } else {
        stop(
          paste(
            "Rsagacmd cannot determine the number of results for list-like outputs.",
            "For Grid/Shapes list outputs, please provide file path(s) to the",
            "tool's output arguments."
          )
        )
      }
      
      # replace value with path to the saved file
      self$value = self$files
      
      # add the created tempfile to the package list of temporary files
      tfiles = self$files
      tfiles = strsplit(tfiles, ";")
      pkg.env$sagaTmpFiles = append(pkg.env$sagaTmpFiles, tfiles)
    }
  ),
  
  private = list(
    #' @field grid_features Character vector of output types that represent grids
    grid_features = c("Grid", "Raster"),
    
    #' @field shape_features Character vector of output types that represent shapes
    shape_features = c("Shape"),
    
    #' @field table_features Character vector of output types that represent tables
    table_features = "Table"
  )
)

