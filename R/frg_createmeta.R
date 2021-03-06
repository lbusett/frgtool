#' @title frg_createmeta
#' @description Create ENVI META files allowing access to time series 
#'  of Med_SVI files
#' @return The function is called for its side effects
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param out_files `character` names of raster files to be used to create the META
#' @param force_update `logical` If TRUE, recreate the ROI file even if it already
#'  exist, default: FALSE
#' @rdname frg_createmeta
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' 
frg_createmeta <- function(opts,
                           out_files, 
                           force_update = FALSE) {
  
  # Get files dimensions from the header of the first file ----
  head_file   <- paste(out_files[1], ".hdr", sep = "")
  fileConn_hd <- file(head_file)
  nsamp       <- (strsplit(readLines(fileConn_hd)[4], "=")[[1]])[2]
  nrow        <- (strsplit(readLines(fileConn_hd)[5], "=")[[1]])[2]
  close(fileConn_hd)
  
  # Define META FILE name ----
  meta_filename <- paste("Med_S", opts$index, "_",
                         opts$start_year, "_",
                         opts$end_year, "_META.dat",
                         sep = "")
  if (!file.exists(meta_filename) | force_update){
    meta_filename <- file.path(file.path(opts$scaled_dir,
                                         paste0("Med_S", opts$index),
                                         meta_filename))
    
    # Write the META file ----
    fileConn_meta <- file(meta_filename, "w")
    writeLines(c("ENVI META FILE"), fileConn_meta)  # Write first line
    
    # Write the lines of the META file corresponding to each input file ----
    for (ff in out_files) {
      writeLines(c(paste0("File : ", ff, sep = ""), 
                   paste0("Bands: 1"), 
                   paste0("Dims: 1-", nsamp, " , 1-", nrow))
                 , fileConn_meta)
    }
    close(fileConn_meta)  # Close connection to META file
  } else {
    message("frg_createmeta --> ", meta_filename, "already exists. Skipping")
  }
}

