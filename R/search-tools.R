#' Search for a SAGA-GIS tool
#'
#' @param x saga object
#' @param pattern character, pattern of text to search for within the tool name
#'
#' @return a tibble containing the libraries, names and parameters of the tools
#'   that match the pattern of the search text and their host library
#' @export
#' @examples
#' \dontrun{
#' # initialize Rsagacmd
#' saga <- saga_gis()
#'
#' # search for a tool
#' search_tools(x = saga, pattern = "terrain")
#' }
search_tools <- function(x, pattern) {
  # get local environment of saga object
  libraries <- environment(x[[1]][[1]])$senv$libraries

  matches <- tibble::tibble(
    library = character(),
    tool = character(),
    author = character(),
    parameters = list(),
    description = character(),
    .rows = 0
  )

  for (lib in names(libraries)) {
    match_text <- grep(
      pattern,
      names(libraries[[lib]]),
      ignore.case = TRUE
    )

    if (length(match_text) > 0) {
      for (idx in match_text) {
        result <- list(
          library = lib,
          tool = names(libraries[[lib]])[idx],
          author = libraries[[lib]][[idx]]$author,
          saga_cmd = libraries[[lib]][[idx]]$tool_cmd,
          parameters = list(names(libraries[[lib]][[idx]]$params)),
          description = libraries[[lib]][[idx]]$description
        )

        matches <- rbind(matches, tibble::as_tibble(result))
      }
    }
  }

  matches
}
