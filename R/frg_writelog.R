#' Title
#'
#' @return
#' @export
#'
#' @examples
start_log <- function(){
  
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = FALSE)
  cat("--- Processing Summary", file = OutFile_Conn, sep = "\n", 
      append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat("", file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Processing Date: "), file = OutFile_Conn, sep = "\n", 
      append = TRUE)
  cat("", file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- Processing PArameters ---"), file = OutFile_Conn, sep = "\n", 
      append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat("", file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Start Year for MODIS Data: ", Start_Year), file = OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- End Year for MODIS Data: ", End_Year), file = OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Moving Window  Size: ", NKer, " Km"), file = OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Number of Before Fire Years used as reference : ", 
            MedWdt), file = OutFile_Conn, sep = "\n", append = TRUE)
  if (SNDVI == 1) {
    cat(paste("--- NDVI Analysis: Yes"), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  } else if (SNDVI == 1) {
    cat(paste("--- NDVI Analysis: No"), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  if (SRDVI == 1) {
    cat(paste("--- RDVI Analysis: Yes"), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  } else if (SNDVI == 1) {
    cat(paste("--- RDVI Analysis: No"), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  if (SNDVI == 1) {
    cat(paste("--- Minimum Percentages for significant difference: NDVI--> ", 
              perc_diffs[["NDVI"]]), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  if (SRDVI == 1) {
    cat(paste("--- Minimum Percentages for significant difference: RDVI--> ", 
              perc_diffs[["RDVI"]]), file = OutFile_Conn, sep = "\n", 
        append = TRUE)
  }
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- Input Data ---"), file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Input ShapeFile of Burnt Areas: ", Shape_File), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
  cat(paste("--- Input Corine Map: ", CLC_File_00), file = OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(paste("--- Input EcoZones Map: ", ENV_Zones_File), file = OutFile_Conn, 
      sep = "\n", append = TRUE)
  cat(c("--- -------------------------------------------------- ---"), 
      file = OutFile_Conn, sep = "\n", append = TRUE)
}
