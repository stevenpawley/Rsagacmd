#' Automatic search for the path to a SAGA-GIS installation
#'
#' Returns the path to the saga_cmd executable.
#' 
#' @details
#' On Microsoft Windows, automatic searching will occur first in 'C:/Program
#' Files/SAGA-GIS'; 'C:/Program Files (x86)/SAGA-GIS'; 'C:/SAGA-GIS';
#' 'C:/OSGeo4W'; and 'C:/OSGeo4W64'.
#' 
#' On Linux, saga_cmd is usually included in PATH, if not an automatic search is
#' performed in the '/usr/' folder.
#' 
#' For macOS, since version 8.5, SAGA-GIS is available as an standalone macOS
#' app from \href{https://sourceforge.net/projects/saga-gis/}{SourceForge}. The
#' 'SAGA.app' package is searched first (assuming that it is installed in the
#' '/Applications/' folder). Other macOS locations that are searched include
#' '/usr/local/bin/' (for Homebrew installations) and within the QGIS application
#' (SAGA-GIS is bundled with the QGIS application on macOS by default).
#' 
#' If multiple versions of SAGA-GIS are installed on the system, the path to the
#' newest version is returned.
#'
#' @return The path to installed saga_cmd binary.
#'
#' @export
search_saga <- function() {
  if (!is.null(getOption("saga_cmd"))){
    return(getOption("saga_cmd"))
  } else {
    saga_cmd <- if (nchar(Sys.which(names = "saga_cmd")) > 0) "saga_cmd" else NULL

    if (is.null(saga_cmd)) {

      # define search paths
      if (Sys.info()["sysname"] == "Windows") {
        search_paths <- c(
          "C:/Program Files/SAGA",
          "C:/Program Files/SAGA-GIS",
          "C:/Program Files (x86)/SAGA-GIS",
          "C:/SAGA-GIS",
          "C:/OSGeo4W",
          "C:/OSGeo4W64"
        )
        saga_executable <- "saga_cmd.exe"
      
      } else if (Sys.info()["sysname"] == "Linux") {
        search_paths <- c("/usr")
        saga_executable <- "saga_cmd$"
      
      } else if (Sys.info()["sysname"] == "Darwin") {
        search_paths <- c(
          "/Applications/SAGA.app/Contents/MacOS",
          "/usr/local/bin",
          "/Applications/QGIS.app/Contents/MacOS/bin"
        )

        saga_executable <- "^saga_cmd$"
      }

      # search for saga_cmd executable
      saga_cmd <- c()
      for (f in search_paths) {
        saga_cmd <- c(saga_cmd, list.files(
          path = f,
          pattern = saga_executable,
          recursive = TRUE,
          full.names = TRUE
        ))
      }
    }

    # error is saga_cmd not found
    if (length(saga_cmd) == 0) {
      rlang::abort(
        paste(
          "SAGA-GIS installation not found. Need to supply a valid path",
          "to the saga_cmd executable"
        )
      )

      return(NULL)

      # automatically use newest version if multiple installations are found
    } else if (length(saga_cmd) > 1) {
      message("Multiple installations of SAGA-GIS were found at:")
      message(paste(saga_cmd, collapse = "\n"))
      message(paste(
        "Choosing newest version. Manually specify the location when",
        "calling saga_gis() to use an older version"
      ))

      saga_vers <- list()

      for (saga_inst in saga_cmd) {
        saga_vers <- append(saga_vers, saga_version(saga_inst))
      }

      saga_cmd <- saga_cmd[which(saga_vers == max(saga_vers))]
    }

    return(saga_cmd)
  }
}
