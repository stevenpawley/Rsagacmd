
# tidy.saga <- function(x, ...) {
#   env <- environment(x[[1]][[1]])
#   
#   lib_descriptions <- sapply(
#     env$senv$libraries,
#     function(lib) {
#       desc <- attr(lib, "description")
#       if (is.null(desc)) desc <- NA_character_
#       desc
#     })
#   
#   tibble::tibble(
#     libraries = names(x),
#     description = unlist(lib_descriptions),
#     n_tools = sapply(x, length)
#   )
# }


# tidy.saga_library <- function(x, ...) {
#   tool_descriptions <- sapply(x, function(tool) {
#     tool_obj <- extract_tool(tool)
#     lib <- attr(x, "lib")
#     tool <- attr(x, "tool")
#     tool_obj[["description"]]
#   })
#   
#   tool_authors <- sapply(x, function(tool) {
#     tool_obj <- extract_tool(tool)
#     lib <- attr(x, "lib")
#     tool <- attr(x, "tool")
#     tool_obj[["author"]]
#   })
#   
#   tibble::tibble(
#     tools = names(x),
#     description = tool_descriptions,
#     author = tool_authors
#   )
# }