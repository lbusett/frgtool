#' @title frg_compmean
#' @description Accessory function to compute average summer NDVI for each year. 
#' @details Accessory function to compute average summer NDVI for each year using
#'  MODIS data downloaded by frg_moddownload.
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param yy `character` year of the analysis
#' @param UI_check `logical` If TRUE, use usefulness index data to filter-out 
#'  low quality data, Default: TRUE
#' @param max_UI `numeric` Maximum UI value of high-quality NDVI data. Values 
#'  of pixels with UI > max_UI are treated as noisy and not considered in average
#'  computation, Default: 5
#' @param force_update `logical` If TRUE, recompute the average files even if
#'  it already exist, Default: FALSE
#' @return `character` If all goes well, the function returns "DONE!"
#' @rdname frg_compmean
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom raster raster blockSize writeStart getValues writeValues writeStop
#'  stack
#' @author Lorenzo Busetto, PhD (2012)
#'         email: lbusett@gmail.com

frg_compmean <- function(opts,
                         yy,
                         UI_check = TRUE,
                         max_UI = 5, 
                         force_update = FALSE) {
  
  # Crete output folders if necessary ----
  out_dir_avg      <- file.path(opts$out_origpath,'NDVI','Averages')
  outfilename_avg  <- file.path(out_dir_avg, paste0("NDVI_Average_",yy,".tif"))
  dir.create(out_dir_avg, showWarnings = FALSE, recursive = T)
  
  if (!file.exists(outfilename_avg) | force_update == TRUE) {
    
    # Get file names for NDVI, UI and Reliability for the selected year ----
    NDVI_Dir <- file.path(opts$out_origpath,"time_series/VI_16Days_250m_v6/NDVI")
    UI_Dir   <- file.path(opts$out_origpath,"time_series/VI_16Days_250m_v6/QA_usef")
    Rely_Dir <- file.path(opts$out_origpath,"time_series/VI_16Days_250m_v6/Rely")
    
    NDVI_files_names <- list.files(NDVI_Dir,pattern = "tif$", full.names = TRUE)
    NDVI_files_names <- NDVI_files_names[grep(yy, NDVI_files_names)]
    
    UI_files_names   <- list.files(UI_Dir,pattern = "tif$", full.names = TRUE)
    UI_files_names   <- UI_files_names[grep(yy, UI_files_names)]
    
    RELY_files_names <- list.files(Rely_Dir,pattern = "*.tif$", full.names = TRUE)
    RELY_files_names <- RELY_files_names[grep(yy, RELY_files_names)]
    
    # Accessory function for average computation in chunks ----
    frg_maskmeanNDVI <- function(NDVI, UI, Rely, max_UI, filename) {
      out <- raster::raster(NDVI)
      bs  <- raster::blockSize(out)
      
      out <- raster::writeStart(out, filename, overwrite = TRUE, NAflag = 32767, 
                                options = c("COMPRESS=NONE"))
      for (i in 1:bs$n) {
        vi_data_1 <- raster::getValues(NDVI[[1]], row = bs$row[i], nrows = bs$nrows[i]) *
          ifelse((raster::getValues(UI[[1]], row = bs$row[i], nrows = bs$nrows[i]) <= max_UI) &
                   (raster::getValues(Rely[[1]], row = bs$row[i], nrows = bs$nrows[i]) <= 1), 1, NA) 
        
        vi_data_2 <- raster::getValues(NDVI[[2]], row = bs$row[i], nrows = bs$nrows[i]) *
          ifelse((raster::getValues(UI[[2]], row = bs$row[i], nrows = bs$nrows[i]) <= max_UI) &
                   (raster::getValues(Rely[[2]], row = bs$row[i], nrows = bs$nrows[i]) <= 1), 1, NA)   
        
        vi_data   <- rowMeans(cbind(vi_data_1, vi_data_2), na.rm = TRUE) 
        raster::writeValues(out, vi_data, bs$row[i])
        gc()
      }
      raster::writeStop(out)
    }
    
    # Setup data and compute average ----
    NDVI      <- raster::stack(NDVI_files_names) 
    UI        <- raster::stack(UI_files_names) 
    Rely      <- raster::stack(RELY_files_names)
    max_UI    <- max_UI 
    filename  <- outfilename_avg 
    frg_maskmeanNDVI(NDVI, UI, Rely, max_UI, filename)
    gc() 
  } else {
    message("---- Average NDVI file for ", yy, " already exists - Skipping. ----")
  }
  return('DONE')
}