# Test coordinates
library(tibble)
coords <- tribble(
  ~place,  ~lon,         ~lat,
  "JGCRI",    -76.92238, 38.97160,
  "Coldfoot", -150.17,   67.25,
  "Thompson", -97.84862, 55.74706
)
coords$ID <- seq_len(nrow(coords))

results <- list()

files <- list.files("esacci-permafrost/", full.names = TRUE,
                    pattern = "\\.nc$", recursive = TRUE)
#fn <- "esacci-permafrost/Permafrost_cci/v05.0/northern_hemisphere/1997/ESACCI-PERMAFROST-L4-GTD-ERA5_MODISLST_BIASCORRECTED-AREA4_PP-1997-fv05.0.nc"

for(fn in files) {
  # Trying a simple terra::rast() with these data gives
  # Error: [rast] could not load multidimensional data. 
  # So use ncdf4 to load; transpose and flip; and convert
  # to a raster, specifying the spatial extent and CRS
  library(ncdf4)
  message("Opening ", basename(fn))
  nc <- nc_open(fn)
  
  ncvars_to_read <- c("T1m")
  #ncvars_to_read <- c("GST", "GST_uncertainty", "T1m", "T1m_uncertainty")
  for(v in ncvars_to_read) {
    message("\tGetting ", v)
    dat_matrix <- ncvar_get(nc, varid = v)
    
    # Transpose and flip the data
    x <- t(dat_matrix)
    x <- x[dim(x)[1]:1,]
    # Convert to spatial raster, specifying lon/lat extent
    dat_rast <- rast(x, 
                     extent = c(-180, 180, 25, 85), 
                     crs="+proj=longlat +datum=WGS84")
    message("\tExtracting points...")
    xdat <- terra::extract(dat_rast, coords[2:3])
    coords[v] <- xdat[2]
  }
  
  # The year is in the filename; extract it (a bit hackily)
  yr <- strsplit(basename(fn), "-", fixed = TRUE)[[1]][7]
  coords$Year <- as.numeric(yr)
  results[[yr]] <- coords
  
  nc_close(nc)
}

library(dplyr)
library(ggplot2)
theme_set(theme_bw())
bind_rows(results) |> 
  ggplot(aes(Year, T1m, color = place)) + geom_line()

