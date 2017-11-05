#' @title Helper function used to extract data from the Med_SNDVI time series
#'   in correspondance of the different burnt areas
#' @description FUNCTION_DESCRIPTION
#' @param in_rts PARAM_DESCRIPTION
#' @param zone_object PARAM_DESCRIPTION
#' @param clc_object PARAM_DESCRIPTION
#' @param mask_object PARAM_DESCRIPTION
#' @param start_date PARAM_DESCRIPTION, Default: NULL
#' @param end_date PARAM_DESCRIPTION, Default: NULL
#' @param start_band PARAM_DESCRIPTION, Default: NULL
#' @param end_band PARAM_DESCRIPTION, Default: NULL
#' @param id_field PARAM_DESCRIPTION, Default: NULL
#' @param FUN PARAM_DESCRIPTION, Default: 'null'
#' @param out_format PARAM_DESCRIPTION, Default: 'xts'
#' @param small PARAM_DESCRIPTION, Default: TRUE
#' @param small_method PARAM_DESCRIPTION, Default: 'centroids'
#' @param na.rm PARAM_DESCRIPTION, Default: TRUE
#' @param verbose PARAM_DESCRIPTION, Default: FALSE
#' @param maxchunk PARAM_DESCRIPTION, Default: 5e+07
#' @return ts_data data.table containing SVI time series extracted for each burnt 
#'       area from `in_rts` multitemporal SVI raster file
#' @rdname frg_fastzonal
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster getZ nlayers raster crop extent nrow ncol getValues
#' @importFrom data.table data.table rbindlist setkey
#' @importFrom lubridate year
#' @importFrom dplyr group_by mutate select rename
#' @importFrom magrittr "%>%"
#'
frg_fastzonal = function(in_rts,
                         zone_object,
                         clc_object,
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
  
  NAME <- mask_data <- .SD <- value <- N_PIX <- CLC_Class <- Year <- NULL
  
  dates     <- raster::getZ(in_rts)
  sel_bands <- seq(1, raster::nlayers(in_rts), 1)
  
  # Start cycling on dates/bands -----
  if (length(sel_bands) > 0) {
    
    zone_object <- raster::raster(zone_object) %>% 
      raster::crop(raster::extent(in_rts[[1]]))
    
    # Setup chunks ----
    n_cells   <- raster::nrow(zone_object) * raster::ncol(zone_object)
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
          
          startrow <- ifelse(chunk == 1,
                             1,
                             (chunk - 1) * ceiling(raster::nrow(zone_object) / n_chunks)) 
          nrows    <- ifelse(chunk != n_chunks, 
                             ceiling(raster::nrow(zone_object) / n_chunks),
                             raster::nrow(zone_object))
          endrow   <- ifelse(chunk == 1, nrows - 1, nrows)
          
          full_data[[chunk]] <- data.table::data.table(
            value      = raster::getValues(in_rts[[sel_bands[f]]], startrow, endrow),
            NAME       = raster::getValues(zone_object   , startrow, endrow), 
            CLC_Class  = raster::getValues(clc_object    , startrow, endrow), 
            # ENV_ZONE   = getValues(envzone_object, startrow, endrow), 
            mask_data  = raster::getValues(mask_object   , startrow, endrow), 
            Year       = lubridate::year(date), 
            key = 'NAME') %>% 
            # remove data outside polygons (== zones = 0 )  and of 
            # non-core pixels Eroded mask = 0
            subset(NAME != 0 & mask_data == 1)   
          gc()
        }
        gc()
        
        # Add current chunk to the full data ----
        full_data <- data.table::rbindlist(full_data)
      } # end cycle on chunks
      
      data.table::setkey(full_data, "NAME")
      if (f == 1) {
        zones <- unique(full_data)$NAME
      }
      
      # Apply the aggregation function if needed, otherwise just extract all pixels
      if (FUN != 'null') {
        ts_data[[f]] <- full_data[, lapply(.SD, match.fun(FUN),
                                           na.rm = na.rm), by = zones]$value
      } else {
        ts_data[[f]] <- dplyr::group_by(full_data, NAME) %>% 
          dplyr::mutate(N_PIX = seq(along = value))
      }
      
    } # end cycle on years
    
    # Transform the output list to a data.table
    ts_data <- data.table::rbindlist(ts_data) %>% 
      dplyr::select(NAME, N_PIX, CLC_Class, Year, value) %>% 
      dplyr::rename(Index = value)
    
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
  
  # Send back the result to the caller
  return(ts_data)
}
