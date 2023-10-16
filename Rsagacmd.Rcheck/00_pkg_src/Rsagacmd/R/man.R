#' Browse the online documentation for a saga_tool
#'
#' @param saga_tool a saga_tool object
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' library(Rsagacmd)
#' 
#' saga <- saga_gis()
#' 
#' saga_docs(saga$ta_morphometry$slope_aspect_curvature)
#' }
saga_docs <- function(saga_tool) {
  env <- environment(saga_tool)
  version <- env$senv$saga_vers
  library <- attr(saga_tool, "lib")
  tool_name <- attr(saga_tool, "tool")
  html_page <- env$senv$libraries[[library]][[tool_name]]$html_file
  
  base_url <- "https://saga-gis.sourceforge.io/saga_tool_doc"
  doc_url <- paste(base_url, version, html_page, sep = "/")

  if (interactive()) {
    utils::browseURL(doc_url)
  } else {
    message(
      paste(
        "Session is not interactive - browser cannot be used",
        "Documentation page is accessible at:",
        doc_url,
        sep = "\n"
      )
    )
  }
}
