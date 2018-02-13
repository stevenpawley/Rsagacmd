devtools::use_package("minpack.lm")

#' Calculate the T_SLOPE value based on DEM resolution for MRVBF
#'
#' Calculates the T_SLOPE value for the Multiresolution Index of Valley Bottom 
#' Flatness (Gallant and Dowling, 2003) based on input DEM resolution. MRVBF
#' identified valley bottoms based on classifying slope angle and identifying
#' low areas by ranking elevation in respect to the surrounding topography
#' across a range of DEM resolutions. The MRVBF algorithm was developed using a
#' 25 m DEM, and so if the input DEM has a different resolution then the slope
#' threshold T_SLOPE needs to be adjusted from its default value of 16 in order
#' to maintain the relationship between slope and DEM resolution. This function
#' provides a convenient way to perform that calculation.
#'
#' @param res Input DEM resolution
#' @param plot logical, produce plot of relationship
#'
#' @return Numeric. T_SLOPE value for MRVBF
#' @export
#'
saga.mrvbf.threshold = function(res, plot=FALSE){
  # slope decreases by factor of 2 per every 'step' above a 25 m dem resolution
  # a step consists of a 3 factor increase in the dem cell size
  # Gallant and Dowling 2003
  dem_res = c(6075, 2025, 675, 225, 75, 25, 8, 3, 1)
  mrvbf_slope = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128)
  ds = data.frame(dem_res, mrvbf_slope)
  m = minpack.lm::nlsLM(mrvbf_slope ~ a*I(dem_res^z), data = ds, start = list(a=100, z=1))
  
  if (plot == TRUE){
    # produce nls smooth line
    if (res > max(dem_res))
      predx = seq(min(dem_res), max(res*2, dem_res), 1)
    predy = predict(m, list(dem_res = predx))
    
    # plot
    plot(dem_res, mrvbf_slope, xlab = 'DEM resolution (m)',
         ylab = 'MRVBF Initial Slope', xlim=c(min(dem_res), res*1.1), xaxs="i")
    lines(predx, predy)
    lines(x=c(res,res), y=c(0,predict(m, list(dem_res=res))))
    lines(x=c(0,res), y=c(predict(m, list(dem_res=res)),predict(m, list(dem_res=res))))
    }
  
  predict(m, list(dem_res=res))
}
