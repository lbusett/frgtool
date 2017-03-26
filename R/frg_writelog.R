#' frg_start_log
#'
#' @return NULL

frg_startlog <- function() {
  
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = FALSE)
  cat("--- Processing Summary", file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat("", file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Processing Date: "), file = ff$OutFile_Conn, sep = "\n", 
      append = TRUE)
  cat("", file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- Processing PArameters ---"), file = ff$OutFile_Conn, sep = "\n", 
      append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat("", file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Start Year for MODIS Data: ", Start_Year), file = ff$OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- End Year for MODIS Data: ", End_Year), file = ff$OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Moving Window  Size: ", NKer, " Km"), file = ff$OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Number of Before Fire Years used as reference : ", MedWdt), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  if (SNDVI == 1) {
    cat(paste("--- NDVI Analysis: Yes"), file = ff$OutFile_Conn, sep = "\n", 
        append = TRUE)
  } else if (SNDVI == 1) {
    cat(paste("--- NDVI Analysis: No"), file = ff$OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  if (SNDVI == 1) {
    cat(paste("--- NDVI Analysis: Yes"), file = ff$OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  if (SNDVI == 1) {
    cat(paste("--- Minimum Percentages for significant difference: NDVI--> ", 
              perc_diff), file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  }
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- Input Data ---"), file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Input ShapeFile of Burnt Areas: ", Shape_File), file = ff$OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Input Corine Map: ", CLC_File_00), file = ff$OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = ff$OutFile_Conn, sep = "\n", append = TRUE)
}
