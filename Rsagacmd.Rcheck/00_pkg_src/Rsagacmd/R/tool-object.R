#' Generates list of options for a SAGA-GIS tool
#'
#' Parses the html table for a SAGA-GIS tool into a list of identifiers,
#' options, defaults and constraints
#'
#' @param tool_information list
#' @param tool_options list
#' @param description the description text for the tool that has been scraped
#'   from the help documentation
#' @param html_file the name of the html file for the tool's documentation.
#'   Stored to help linking with online documentation.
#'
#' @return A `saga_tool` object containing:
#' + `tool_name` A syntactically-correct name for the tool.
#' + `description` The tool's description.
#' + `author` The tool's author.
#' + `tool_cmd` The command to use for saga_cmd to execute tool.
#' + `tool_id` The tool's ID.
#' + `parameters` A named list of the tool's parameter objects.
#' + `html_file` The html document name.
#'
#' @keywords internal
create_tool <- function(tool_information, tool_options, description,
                        html_file) {
  # get the command to execute the saga_cmd tool
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
  
  # get the tool ID
  tool_id <- tool_information[tool_information[[1]] == "ID",][[2]]
  suppressWarnings(tool_id <- as.integer(tool_id))
  if (is.na(tool_id)) {
    tool_id = saga_tool_cmd
  }

  # strip input, output and options lines from table
  # (rows in the table that represent headers/section breaks and have same value
  # like 'Input' filled across the row)
  header_rows <- apply(tool_options, 1, function(row) {
    length(unique(unlist(row)))
  })
  header_rows <- which(header_rows > 1)
  tool_options <- tool_options[header_rows, ]

  # get the parameters object
  params <- parameters(tool_options)

  # apply exceptions for specific saga-gis tools
  params <- create_tool_overrides(tool_name, params)

  obj <- list(
    tool_name = tool_name,
    description = description,
    author = author,
    tool_cmd = saga_tool_cmd,
    tool_id = tool_id,
    params = params,
    html_file = html_file
  )
  
  return(obj)
}
