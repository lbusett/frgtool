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
#' @import lubridate
#' @return Time series
#' @export
#'
#' @examples
#'
frg_fastzonal = function(in_rts,
                         zone_object,
                         clc_object,
                         envzone_object,
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
  # Check input types and requesed dates/bands -----
  ras_type   <-  check_spatype(in_rts)
  zone_type  <-  check_spatype(zone_object)
  if (!ras_type %in% "rastobject") {
    stop("Input in_rts is not a RasterStack or RasterBrick object")
  }
  if (zone_type == "none") {
    stop("Input zone_object is not a valid spatial file or object !")
  }
  
  if (!class(getZ(in_rts)) == "Date") {
    message("Input doesn't contain valid dates in its 'Z' attribute\nBand numbers will be used instead on the outputs")
    ts_check = FALSE
  } else {
    dates <- getZ(in_rts)
    ts_check = TRUE
  }
  
  if (is.null(start_date) & is.null(start_band)) {
    if (ts_check) {start_band = ymd(dates[1])} else {start_band = as.integer(1)}
    if (verbose) {message("Starting date/Starting band not provided - Using the first layer in the stack")}
  }
  
  if (is.null(end_date) & is.null(end_band)) {
    if (ts_check) {end_band = ymd(dates[nlayers(in_rts)])} else {end_band = as.integer(nlayers(in_rts))}
    if (verbose) {message("Starting date/Starting band not provided - Using the last layer in the stack")}
  }
  if (!class(start_band) %in% c("Date", "POSIXct", "POSIXlt")) {
    start_date = try(as.Date(start_date), silent = TRUE)
    if (class(start_date) == "try-error") {
      warning("start_date is not a Date object or string cohercible to date - it will be ignored")
      start_date <- as.integer(1)
    }
  }
  if (!class(end_band) %in% c("Date", "POSIXct", "POSIXlt")) {
    end_date = try(as.Date(end_date), silent = TRUE)
    if (class(end_date) == "try-error") {
      warning("end_date is not a Date object or string cohercible to date - it will be ignored")
      end_date <- nlayers(in_rts)
    }
  }
  
  # if (!class(start_band) == "integer") { stop("start_band is not numeric") }
  # if (!class(end_band)   == "integer") { stop("end_band is not numeric")}
  
  if (start_band > end_band) {
    stop("start_date larger than end_date")
  }
  
  if (!small_method %in% c("centroids", "full")) {
    warning("Unknown 'small_method' value - resetting to 'centroids'")
  }
  if (!out_format %in% c("xts", "dframe")) {
    if (verbose)
      message("Unknown 'out_format' value - resetting to 'xts'")
    out_format = "xts"
  }
  
  if (length(id_field) != 0) {
    if (!id_field %in% names(zone_object)) {
      warning(
        "Invalid 'id_field' value - names of output columns will be the record number of the shapefile feature"
      )
      id_field <- NULL
    }
  }
  
  if (!ts_check) {
    dates <- seq(1, nlayers(in_rts), 1)
  }
  
  # if not dates passed, or in_rts doesn't have dates, requested dates
  # replaced by band numbers
  if (class(start_date) == "integer" & class(end_date) == "integer") {
    sel_bands <- seq(start_band, end_band, 1)
  } else {
    sel_bands <- which(dates >= start_band & dates <= end_band)
  }
  
  # Start cycling on dates/bands -----
  if (length(sel_bands) > 0) {
    
    
    if (check_spatype(zone_object) %in% c("spfile", "spobject")) {
      
      if (check_spatype(zone_object) == "spfile") {zone_object <- openshape(zone_object)}
      
      if (proj4string(zone_object) != proj4string(in_rts)) {
        zone_object <- spTransform(zone_object, CRS(proj4string(in_rts[[1]])))
      }
      
      zone_object@data$mdxtnq = seq(1:length(zone_object@data[, 1]))
      zone_cropped = crop(zone_object, extent(in_rts[[1]]))
      
      if (!isTRUE(all.equal(extent(zone_cropped), (extent(zone_object)), scale = 100))) {
        warning(
          "Some features of the spatial object are outside or partially outside\n the extent of the input RasterStack !
Output for features outside rasterstack extent\n will be set to NODATA. Outputs for features only partially inside\n
will be retrieved\n using only the available pixels !"
        )
        if (!setequal(zone_object$mdxtnq, zone_cropped$mdxtnq)) {
          outside_feat = setdiff(zone_object$mdxtnq, zone_cropped$mdxtnq)
        }
      }
      
      # Extraction on points  or lines -----
      if (class(zone_cropped) %in% c("SpatialPointsDataFrame","SpatialPoints","SpatialLines", 
                                     "SpatialLinesDataFrame")) {
        if (verbose) { message("On point and lines shapefiles, the standard `extract` function is used. 
              This could be slow !")}
        
        ts <- matrix(nrow = length(sel_bands), ncol = length(zone_cropped[, 1]))
        
        for (f in seq(along = sel_bands)) {
          if (verbose == TRUE) {
            message(paste0("Extracting data from ", ifelse(ts_check, "date: ", "band: "),
                           dates[sel_bands[f]]))
          }
          ts[f,] <- extract(in_rts[[sel_bands[f]]], zone_cropped, fun = FUN)
        }
        ts <- as.data.frame(ts)
        if (length(id_field) == 1) {
          all_feats <- as.character(zone_cropped@data[, eval(id_field)])
          names(ts) <- c(all_feats)
        }
        else {
          names(ts) <- 1:length(zone_cropped[, 1])
          all_feats <- as.character(names(ts))
        }
        if (out_format == "dframe") {
          ts <- cbind(date = dates[sel_bands], ts)
        }
      } else {   
        # Extraction on polygons: raterize shape ----
        
        if (verbose) { message("Rasterizing shape")}
        if (verbose) { message("Writing temporary shapefile")}
        
        tempshape = tempfile(tmpdir = tempdir(), fileext = ".shp")
        writeOGR(zone_cropped, dsn = dirname(tempshape),layer = basename(file_path_sans_ext(tempshape)),
                 driver = "ESRI Shapefile", overwrite_layer = TRUE,verbose = FALSE)
        
        if (verbose) {(message("Writing temporary rasterized shapefile"))}
        tempraster = tempfile(tmpdir = tempdir(), fileext = ".tiff")
        
        if (max(zone_cropped@data$mdxtnq) <= 255) {
          ot = "Byte"
        } else {
          ot <- ifelse(max(zone_cropped@data$mdxtnq) <= 65536, "Int16", "Int32")
        }
        gdal_rasterize(tempshape, tempraster, tr = raster::res(in_rts), te = extent(in_rts)[c(1, 3, 2, 4)], a = "mdxtnq", ot = ot)
        zone_object <- raster(tempraster)
      }
    } else {
      # Extraction on raster: crop raster on shape ----
      if (check_spatype(zone_object) == "rastfile") { zone_object = raster(zone_object)}
      zone_object = crop(zone_object, extent(in_rts[[1]]))
      
    }
    
    # Setup chunks ----
    n_cells   <- nrow(zone_object) * ncol(zone_object)
    ncols     <- ncol(zone_object)
    n_chunks  <- floor(n_cells / maxchunk)
    ts_data   <- list()
    
    # Start data extraction ----
    for (f in seq(along = sel_bands)) {
      
      full_data <- list()
      date = dates[f]
      print(year(date))
      if (verbose == TRUE) {
        message(paste0("Extracting data from date: ",
                       dates[sel_bands[f]]))
      }
      
      if (n_chunks > 1) {
        for (chunk in seq_len(n_chunks)) {
          
          # Import data chunk ----
          startrow <- ifelse(chunk == 1, 1, (chunk - 1) * ceiling(nrow(zone_object) / n_chunks)) 
          nrows    <- ifelse(chunk != n_chunks, ceiling(nrow(zone_object) / n_chunks),
                             nrow(zone_object))
          message(chunk, " ", startrow, " ",  startrow + (nrows - 1))
          
          # put current chunk in "full_data
          
          temp_data <- data.table(
            value      = getValues(in_rts[[sel_bands[f]]], startrow, ifelse(chunk == 1, nrows - 1, nrows)),
            NAME       = getValues(zone_object   , startrow, ifelse(chunk == 1, nrows - 1, nrows)), 
            CLC_Class  = getValues(clc_object    , startrow, ifelse(chunk == 1, nrows - 1, nrows)), 
            ENV_ZONE   = getValues(envzone_object, startrow, ifelse(chunk == 1, nrows - 1, nrows)), 
            mask_data  = getValues(mask_object   , startrow, ifelse(chunk == 1, nrows - 1, nrows)), 
            Year       = year(date), 
            key = 'NAME') %>% 
            subset(NAME != 0 & mask_data == 1)   # remove data outside polygons (== zones = 0 ) 
          full_data[[chunk]] <- temp_data
          
          gc()
        }
        gc()
        # Add to the full data ----
        
        full_data <- rbindlist(full_data)
        # full_data[ , mask_data:=NULL]     # remove mask_data column
        
      } 
      
      setkey(full_data, "NAME")
      
      if (f == 1) {
        zones <- unique(full_data)$NAME
      }
      
      # Apply the aggregation function if needed, otherwise just extract all pixrels
      if (FUN != 'null') {
        ts_data[[f]] <- full_data[, lapply(.SD, match.fun(FUN),
                                           na.rm = na.rm), by = zones]$value
      } else {
        
        ts_data[[f]] <- group_by(full_data, NAME) %>% 
          mutate(N_PIX = seq(along = value))
      }
    }
    ts_data <- rbindlist(ts_data) %>% 
      select(NAME, N_PIX, CLC_Class, ENV_ZONE, Year, value) %>% 
      rename(Index = value)
    
  } else {
    warning("Selected time range does not overlap with the one of the rasterstack input dataset !")
  }
  gc()
  ts_data
}
