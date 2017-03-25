#' frg_createmeta
#'
#' @inheritParams frg_compSVI
#' @return
#' @export
#'
#' @examples
frg_createmeta <- function(Index, Start_Year, End_Year, Method, 
                           Scaled_Folder, Out_Files) {
  
  # Get files dimensions from the header of the first file ----
  head_file   <- paste(Out_Files[1], ".hdr", sep = "")
  fileConn_hd <- file(head_file)
  nsamp       <- (strsplit(readLines(fileConn_hd)[4], "=")[[1]])[2]
  nrow        <- (strsplit(readLines(fileConn_hd)[5], "=")[[1]])[2]
  close(fileConn_hd)
  
  # Define META FILE name ----
  meta_filename <- paste("Med_S", Index, "_", Start_Year, "_", End_Year, "_META.dat", sep = "")
  meta_filename <- file.path(file.path(Scaled_Folder, paste0("Med_S", Index), meta_filename))
  
  # Write the META file ----
  fileConn_meta <- file(meta_filename, "w")
  writeLines(c("ENVI META FILE"), fileConn_meta)  # Write first line
  
  # Write the lines of the META file corresponding to each input file ----
  for (ff in Out_Files) {
    writeLines(c(paste0("File : ", ff, sep = ""), 
                 paste0("Bands: 1"), 
                 paste0("Dims: 1-", nsamp, " , 1-", nrow))
               , fileConn_meta)
  }
  close(fileConn_meta)  # Close connection to META file
}

