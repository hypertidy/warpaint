library(vapour)
## really weird example
## 100m REMA DEM
rema <- function(extent, dimension, projection) {
  src <- "/rdsi/PUBLIC/raad/data/ftp.data.pgc.umn.edu/elev/dem/setsm/REMA/mosaic/v1.1/100m/REMA_100m_dem.tif"
  vapour::vapour_warp_raster_dbl(src, extent = extent, dimension = dimension, projection = projection, resample = "bilinear")
}

## global SST
sst <- function(extent, dimension, projection) {
  file0 <- "/rdsi/PUBLIC/raad/data/www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/202205/oisst-avhrr-v02r01.20220501_preliminary.nc"
  src <- vapour_vrt(file0, sds = 1, projection = "OGC:CRS84")
  vapour::vapour_warp_raster_dbl(src, extent = extent, dimension = dimension, projection = projection, resample = "bilinear")
}
## Geoscience topographic basemap
map <- function(extent, dimension, projection) {
  src <- "https://services.ga.gov.au/gis/rest/services/Topographic_Base_Map/MapServer/WMTS/1.0.0/WMTSCapabilities.xml"
  v <- vapour::vapour_warp_raster(src, extent = extent, dimension = dimension, projection = projection, resample = "bilinear", bands = 1:3, band_output_type = "double")
  ##col <- rgb(v[[1]], v[[2]], v[[3]], maxColorValue = 255)
  v[[1]] * 0.2126 + v[[2]]  * 0.7152 + v[[3]] * 0.0722
}


## create a raster
div <- function(x) x[1]/x[2]
ex <- c(110, 190, -80, -10)
dm <- as.integer(sort(c(div(diff(ex)[c(1, 3)]), 1)) * 1024)
rfile <- vapour:::vapour_create("myfile.tif", extent = ex,
                                dimension = dm, projection = "OGC:CRS84")
  #remotes::install_github("hypertidy/grout")
  info <- vapour::vapour_raster_info(rfile)
  index <- grout::tile_index( grout:::grout(info, info$tilesXY))
  res <- diff(ex)[c(1, 3)]/info$dimXY
  replace_na <- function(x) {
    x[is.na(x)] <- 1
    x
  }
  for (i in seq_len(dim(index)[1L])) {
    tile <- index[i, ]
    extent <- c(info$extent[1] + tile$offset_x * res[1],
                info$extent[1] +  (tile$offset_x + tile$ncol) * res[1],
                info$extent[4] +  (tile$offset_y + tile$nrow)     * -res[2],
                info$extent[4] +  tile$offset_y * -res[2])

    vals <- list(scales::rescale(rema(extent, c(tile$ncol, tile$nrow), projection = info$projection), c(0, 255), c(0, 3200)),
                 scales::rescale(sst(extent, c(tile$ncol, tile$nrow), projection = info$projection), c(0, 255), c(-1.8, 28)),
                 map(extent, c(tile$ncol, tile$nrow), projection = info$projection))


    tilevals <- dplyr::coalesce(vals[[1]], vals[[2]], vals[[3]])
    print(i)
    vapour:::vapour_write_raster_block(rfile, tilevals, c(tile$offset_x, tile$offset_y), c(tile$ncol, tile$nrow), overwrite = TRUE)
  }

  library(terra)
  plot(rast("myfile.tif"), col = grey.colors(256))
