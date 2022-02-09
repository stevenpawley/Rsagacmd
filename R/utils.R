#' Calculate the t_slope value based on DEM resolution for MRVBF
#'
#' Calculates the t_slope value for the Multiresolution Index of Valley Bottom
#' Flatness (Gallant and Dowling, 2003) based on input DEM resolution. MRVBF
#' identified valley bottoms based on classifying slope angle and identifying
#' low areas by ranking elevation in respect to the surrounding topography
#' across a range of DEM resolutions. The MRVBF algorithm was developed using a
#' 25 m DEM, and so if the input DEM has a different resolution then the slope
#' threshold t_slope needs to be adjusted from its default value of 16 in order
#' to maintain the relationship between slope and DEM resolution. This function
#' provides a convenient way to perform that calculation.
#'
#' @param res numeric, DEM resolution
#'
#' @return numeric, t_slope value for MRVBF
#' @export
#'
#' @examples
#' mrvbf_threshold(res = 10)
mrvbf_threshold <- function(res) {
  t_slope <- 116.57 * (res**-0.62)
  return(t_slope)
}
