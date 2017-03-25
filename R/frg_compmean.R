#' frg_compmean
#' @inheritParams frg_moddownload 
#' @param ReProc 
#' @param UI_check 
#' @param max_UI 
#' @importFrom raster mean
#' @author Lorenzo Busetto, PhD (2012)
#'         email: lbusett@gmail.com

frg_compmean <- function(OutOrig_Path, 
                         ReProc, 
                         yy, 
                         UI_check,
                         max_UI) {
  
  # Crete output folders if necessary ----
  out_dir_avg      <- file.path(OutOrig_Path,'NDVI','Averages')				
  outfilename_avg  <- file.path(out_dir_avg, paste0("NDVI_Average_",yy,".tif"))
  dir.create(out_dir_avg, showWarnings = FALSE, recursive = T)
  
  if (!file.exists(outfilename_avg) | ReProc == TRUE) {
    
    # Get file names for NDVI, UI and Reliability for the selected year ----
    NDVI_Dir <- file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/NDVI")
    UI_Dir   <- file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/QA_usef")
    Rely_Dir <- file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/Rely")
    
    NDVI_files_names <- list.files(NDVI_Dir,pattern = "tif$", full.names = TRUE)
    NDVI_files_names <- NDVI_files_names[grep(yy, NDVI_files_names)]
    
    UI_files_names   <- list.files(UI_Dir,pattern = "tif$", full.names = TRUE)
    UI_files_names   <- UI_files_names[grep(yy, UI_files_names)]
    
    RELY_files_names <- list.files(Rely_Dir,pattern = "*.tif$", full.names = TRUE)
    RELY_files_names <- RELY_files_names[grep(yy, RELY_files_names)]
    
    # Accessory function for average computation in chunks ----
    frg_maskmeanNDVI <- function(NDVI, UI, Rely, max_UI, filename) {
      out <- raster(NDVI)
      bs <- blockSize(out)
      out <- writeStart(out, filename, overwrite = TRUE, NAflag = 32767)
      for (i in 1:bs$n) {
        vi_data_1 <- getValues(NDVI[[1]], row = bs$row[i], nrows = bs$nrows[i]) *
          ifelse((getValues(UI[[1]], row = bs$row[i], nrows = bs$nrows[i]) <= max_UI) &
                   (getValues(Rely[[1]], row = bs$row[i], nrows = bs$nrows[i]) <= 1), 1, NA) 
        
        vi_data_2 <- getValues(NDVI[[2]], row = bs$row[i], nrows = bs$nrows[i]) *
          ifelse((getValues(UI[[2]], row = bs$row[i], nrows = bs$nrows[i]) <= max_UI) &
                   (getValues(Rely[[2]], row = bs$row[i], nrows = bs$nrows[i]) <= 1), 1, NA)   
        
        vi_data   <- rowMeans(cbind(vi_data_1, vi_data_2), na.rm = TRUE) 
        writeValues(out, vi_data, bs$row[i])
        gc()
      }
      writeStop(out)
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


