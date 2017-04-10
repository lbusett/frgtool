#' frg_fastzonal
#' 
#' @description aaaa
#'
#' @param in_rts a
#' @param zone_object f
#' @param end_date g
#' @param id_field g
#' @param FUN h
#' @param out_format g
#' @param small h
#' @param small_method h
#' @param na.rm g
#' @param verbose g
#' @param start_band t
#' @param end_band re
#' @param maxchunk f
#' @param mask_object g 
#'
#' @importFrom xts as.xts
#' @importFrom rgdal writeOGR readOGR
#' @importFrom sp proj4string spTransform CRS
#' @importFrom tools file_path_sans_ext
#' @importFrom raster getValues crop extent getZ extract rasterize res nlayers
#' @importFrom tools file_path_sans_ext
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom data.table data.table setkey rbindlist
#' @importFrom dplyr mutate group_by select
#' @importFrom lubridate ymd year
#' @return ts_data data.table containing SVI time series extracted for each burnt 
#'       area from `in_rts` multitemporal SVI raster file
#' @export
#'
#' @examples
#'
frg_fastzonal = function(in_rts,
                         zone_object,
                         clc_object,
                         # envzone_object,
                         mask_object,
                         start_date  = NULL,
                         end_date    = NULL,
                         start_band  = NULL,
                         end_band    = NULL,
                         id_field    = NULL,
                         FUN         = "null",
                         out_format  = "xts",
                         small       = TRUE,
                         small_method = "centroids",
                         na.rm       = TRUE,
                         verbose     = FALSE,
                         maxchunk    = 50E6)
{
  
  dates     <- getZ(in_rts)
  sel_bands <- seq(1, nlayers(in_rts), 1)
  
  # Start cycling on dates/bands -----
  if (length(sel_bands) > 0) {
    
    zone_object <- raster(zone_object) %>% 
      crop(extent(in_rts[[1]]))
    
    # Setup chunks ----
    n_cells   <- nrow(zone_object) * ncol(zone_object)
    n_chunks  <- floor(n_cells / maxchunk)
    ts_data   <- list()
    
    # Start data extraction ----
    for (f in seq(along = sel_bands)) {
       # for (f in 1:1) {  
      full_data <- list()
      date      <- dates[f]
      if (verbose) {message(paste0("Extracting data for year: ",year(date)))}
      
      if (n_chunks > 1) {
        for (chunk in seq_len(n_chunks)) {
          
          # Import data chunk and put it in full_data ----
          if (verbose) {message("---- Processing chunk, ", chunk,  " of:  ", n_chunks, " ----")}
          
          startrow <- ifelse(chunk == 1, 1, (chunk - 1) * ceiling(nrow(zone_object) / n_chunks)) 
          nrows    <- ifelse(chunk != n_chunks, ceiling(nrow(zone_object) / n_chunks),
                             nrow(zone_object))
          endrow   <- ifelse(chunk == 1, nrows - 1, nrows)
          
          full_data[[chunk]] <- data.table(
            value      = getValues(in_rts[[sel_bands[f]]], startrow, endrow),
            NAME       = getValues(zone_object   , startrow, endrow), 
            CLC_Class  = getValues(clc_object    , startrow, endrow), 
            # ENV_ZONE   = getValues(envzone_object, startrow, endrow), 
            mask_data  = getValues(mask_object   , startrow, endrow), 
            Year       = year(date), 
            key = 'NAME') %>% 
            # remove data outside polygons (== zones = 0 )  and of 
            # non-core pixels Eroded mask = 0
            subset(NAME != 0 & mask_data == 1)   
          gc()
        }
        gc()
        
        # Add current chunk to the full data ----
        full_data <- rbindlist(full_data)
      } # end cycle on chunks
      
      setkey(full_data, "NAME")
      if (f == 1) {
        zones <- unique(full_data)$NAME
      }
      
      # Apply the aggregation function if needed, otherwise just extract all pixels
      if (FUN != 'null') {
        ts_data[[f]] <- full_data[, lapply(.SD, match.fun(FUN),
                                           na.rm = na.rm), by = zones]$value
      } else {
        ts_data[[f]] <- group_by(full_data, NAME) %>% 
          mutate(N_PIX = seq(along = value))
      }
      
    } # end cycle on years
    
    # Transform the output list to a data.table
    ts_data <- rbindlist(ts_data) %>% 
      select(NAME, N_PIX, CLC_Class, Year, value) %>% 
      rename(Index = value)
    
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
  
  # Send back the result to the caller
  return(ts_data)
}
