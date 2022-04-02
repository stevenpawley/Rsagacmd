library(terra)

x <- rast("~/Downloads/N53W118.hgt")
x <- project(x, "epsg:3402", method = "bilinear")
x <- crop(x, ext(c(310000, 350000, 5880000, 5920000)))

srtm <- resample(x, rast(ext(x), resolution = c(100, 100)), method = "bilinear")
writeRaster(srtm, "inst/extdata/srtm.tif", overwrite = TRUE)
