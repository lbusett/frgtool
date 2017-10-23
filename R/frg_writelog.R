#' #' frg_start_log
#' #'
#' #' @return NULL
#' 
#' frg_startlog <- function(opts) {
#'   
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = FALSE)
#'   cat("--- Processing Summary", file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat("", file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(paste("--- Processing Date: ", Sys.Date()), file = opts$outfile_conn, sep = "\n", 
#'       append = TRUE)
#'   cat("", file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- Processing Parameters ---"), file = opts$outfile_conn, sep = "\n", 
#'       append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat("", file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(paste("--- Start Year for MODIS Data: ", opts$start_year), file = opts$outfile_conn, 
#'       sep = "\n", append = TRUE)
#'   cat(paste("--- End Year for MODIS Data: ", opts$end_year), file = opts$outfile_conn, 
#'       sep = "\n", append = TRUE)
#'   cat(paste("--- Moving Window  Size: ", opts$nker, " Km"), file = opts$outfile_conn, 
#'       sep = "\n", append = TRUE)
#'   cat(paste("--- Number of Before Fire Years used as reference : ", opts$MedWdt), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   if (opts$SNDVI == 1) {
#'     cat(paste("--- NDVI Analysis: Yes"), file = opts$outfile_conn, sep = "\n", 
#'         append = TRUE)
#'   } 
#'   if (opts$SNDVI == 1) {
#'     cat(paste("--- Minimum Percentages for significant difference: NDVI--> ", 
#'               opts$perc_diff), file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   }
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- Input Data ---"), file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#'   cat(paste("--- Input ShapeFile of Burnt Areas: ", Shape_File), file = opts$outfile_conn, 
#'       sep = "\n", append = TRUE)
#'   cat(paste("--- Input Corine Map: ", CLC_File_00), file = opts$outfile_conn, 
#'       sep = "\n", append = TRUE)
#'   cat(c("--- -------------------------------------------------- ---"), 
#'       file = opts$outfile_conn, sep = "\n", append = TRUE)
#' }
