#' Internal function to extract information from a `saga_tool` object
#'
#' @param x a `saga_tool` object
#'
#' @return the intervals of a `saga_tool`
#' @export
#' @keywords internal
extract_tool <- function(x) {
  lib <- attr(x, "lib")
  tool <- attr(x, "tool")

  # get environment of saga_gis object
  env <- environment(x)
  tool_obj <- env$senv$libraries[[lib]][[tool]]

  tool_obj
}

#' Interval function used to summarize a `saga_tool` into a tibble that
#' describes the tools parameters and options
#'
#' @param tool_obj a nested list which constitutes the internals of a saga_tool
#'   object
#'
#' @return a tibble
#' @export
#' @keywords internal
summarize_tool_params <- function(tool_obj) {
  params <- tool_obj[["params"]]

  df <- tibble::tibble(
    parameter = sapply(params, function(x) x$name),
    type = sapply(params, function(x) x$type),
    argument = sapply(params, function(x) x$alias),
    identifier = sapply(params, function(x) x$identifier),
    description = sapply(params, function(x) {
      ifelse(x$description == "", NA_character_, x$description)
    }),
    default = sapply(params, function(x) x$default),
    available_opts = sapply(params, function(param) {
      constraints <- param$constraints[!is.na(param$constraints)]

      ifelse(length(constraints) > 0,
             paste(constraints, collapse = "; "),
             constraints)
    })
  )

  df  
}


#' Generic function to display help and usage information for any SAGA-GIS tool
#'
#' Displays a tibble containing the name of the tool's parameters, the argument
#' name used by Rsagacmd, the identifier used by the SAGA-GIS command line, and
#' additional descriptions, default and options/constraints.
#'
#' @param x A `saga_tool` object.
#' @param ... Additional arguments to pass to print. Currently not used.
#'
#' @return NULL
#' @method print saga_tool
#' @export
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' saga <- saga_gis()
#'
#' # Display usage information on a tool
#' print(saga$ta_morphometry$slope_aspect_curvature)
#'
#' # Or simply:
#' saga$ta_morphometry$slope_aspect_curvature
#' }
print.saga_tool <- function(x, ...) {
  tool_obj <- extract_tool(x)
  lib <- attr(x, "lib")
  tool <- attr(x, "tool")

  author <- tool_obj[["author"]]
  description <- tool_obj[["description"]]

  cat(paste0("Help for library = ", lib, "; tool = ", tool, ":", "\n"))
  cat(paste0("Author: n", author), "\n")
  cat(paste0("Description: ", description), "\n")
  cat("\n")

  df <- summarize_tool_params(tool_obj)
  print(df)
}

#' @export
generics::tidy

#' Summarize the parameters that are available within a SAGA-GIS tool and
#' return these as a tibble.
#'
#' @param x a `saga_tool` object
#' @param ... additional arguments. Currently unused.
#'
#' @return a tibble that describes tools, identifiers used by the saga_cmd
#'   command line tool, the equivalent argument name used by Rsagacmd, and other
#'   options and descriptions.
#' @importFrom generics tidy
#' @export
#' @exportS3Method tidy saga_tool
#'
#' @examples
#' \dontrun{
#' # Initialize a saga object
#' saga <- saga_gis()
#'
#' # tidy the tools parameters into a tibble
#' tidy(saga$ta_morphometry$slope_aspect_curvature)
#' }
tidy.saga_tool <- function(x, ...) {
  tool_obj <- extract_tool(x)
  summarize_tool_params(tool_obj)
}
