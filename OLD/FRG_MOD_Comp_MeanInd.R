#'@title FRG_MOD_Comp_RDVI
#'@description Accessory function used to compute  the average DVI, RDVI and NDVI maps, starting from DOY 209 and 225  data of each year and accounting for NODATA and bad quality data  				
#'@details This function is used simply to call the appropriate IDL function used to compute average VI values, starting from DOY 209 and 225 data 
#'  and accounting for NODATA and bad quality data issues. See the IDL function "FRG_MEAN_Indexes.pro" documentation for further info.  
#'
#' @param OutOrig_Path string  Folder where the original MODIS images are being processed (Selected in FRG_MOD_Sel)
#' @param ReProc logical If TRUE, UI is recalculated even if appropriate files are already found in the file system.  
#' @param yy numeric Year of the processing
#' @param UI_Check flag If 1 , then check on usefulness index is performed when averaging MODIS data. default = 1
#' @param max_UI Max UI value kept when averaging MODIS data. default = 5 
#' @param no_data_in numeric NODATA value of input files
#'
#' @return string 'DONE' if processing completed correctly
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 23, 2012
#' @export


# ---- Accessory function used to compute  the average RDVI and NDVI values, starting from DOY 209 and 225 
#       data of each year and accounting for NODATA and bad quality data												         ----  #

FRG_MOD_Comp_MeanInd <- function(OutOrig_Path= OutOrig_Path, ReProc = ReProc, yy = yy, UI_check = UI_check,
                                 max_UI = max_UI, nodata_in = nodata_in) {
  
  NDVI_Dir = file.path(OutOrig_Path,'NDVI/Single_Dates')
  RDVI_Dir = file.path(OutOrig_Path,'RDVI/Single_Dates')
  UI_Dir = file.path(OutOrig_Path,'UI/Single_Dates')
  RELY_Dir = file.path(OutOrig_Path,'RELY/Single_Dates')
  
  NDVI_files_names = list.files(NDVI_Dir,pattern = ".tif$")				# Get file names for NDVI for the selected year 
  NDVI_files_names = NDVI_files_names[grep(yy, NDVI_files_names)]
  RDVI_files_names = list.files(RDVI_Dir,pattern = ".tif$")					# Get file names for DVI for the selected year 
  RDVI_files_names = RDVI_files_names[grep(yy, RDVI_files_names)]
  UI_files_names = list.files(UI_Dir,pattern = "*.hdr$" )					# Get file names for UI for the selected year 
  UI_files_names =UI_files_names[grep(yy, UI_files_names)]
  RELY_files_names = list.files(RELY_Dir,pattern = "*.tif$" )  				# Get file names for RELY for the selected year 
  RELY_files_names =RELY_files_names[grep(yy, RELY_files_names)]
  
  out_files_names_NDVI = paste("Mosaic_NDVI_Average_",yy, sep = '')		# Crete output file names. 
  out_files_names_RDVI = paste("Mosaic_RDVI_Average_",yy, sep = '')
  out_dir_NDVI = file.path(OutOrig_Path,'NDVI','Average')							# Crete output folders if necessary
  out_dir_RDVI = file.path(OutOrig_Path,'RDVI','Average')
  dir.create(out_dir_NDVI, showWarnings = FALSE, recursive = T)																	
  dir.create(out_dir_RDVI, showWarnings = FALSE, recursive = T)
  
  exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')	# Expand IDL path to the folder containing the scripts to be executed
  
  #	Launch computation for mean NDVI ----
  if (length(NDVI_files_names) ==2 ) {
    File_1 = file.path(NDVI_Dir, NDVI_files_names[grep(209, NDVI_files_names)])  ;   File_2 = file.path(NDVI_Dir, NDVI_files_names[grep(225, NDVI_files_names)])		
    UI_File1= file.path(UI_Dir,UI_files_names[grep(209, UI_files_names)])			;   	UI_File2= file.path(UI_Dir, UI_files_names[grep(225, UI_files_names)])	
    RELY_File1= file.path(RELY_Dir,RELY_files_names[grep(209, RELY_files_names)])  		;   	RELY_File2= file.path(RELY_Dir, RELY_files_names[grep(225, RELY_files_names)])	
    out_file = file.path(out_dir_NDVI, out_files_names_NDVI)
    
    if (file.exists(out_file) == F | ReProc == 1) {
      
      str_idl = paste(	'res = FRG_MEAN_Indexes(',	'File_1 = \'',File_1,
                       '\' , ',	'File_2 = \'',File_2, 
                       '\' , ',	'Out_File= \'',out_file, 
                       '\' , ',	'UI_File1 = \'',UI_File1,
                       '\' , ',	'UI_File2 = \'',UI_File2,
                       '\' , ',  'RELY_File1 = \'',RELY_File1,
                       '\' , ',	'RELY_File2 = \'',RELY_File2,
                       '\' , ',	'nodata_in = \'',nodata_in, 
                       '\' , ',	'UI_check = \'',UI_check,
                       '\' , ',	'max_UI = \'',max_UI, 
                       '\' )',	sep = ''	)
      # ------------------------------------------------------------------- #
      # Build the IDL batch file 
      # ------------------------------------------------------------------- #
      
      batch_file = file.path(FRG_Options$IDL_Dir,'FRG_ComputeMeanIndexes.pro')
      fileConn<-file(batch_file)
      writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
      close(fileConn)
      print(paste('--- Computing Mean NDVI for year ',  yy, ' ---'))    # Update progress window
      out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)   # Launch external process
      if (out != 0) {
         print('An error occurred while Mean NDVI ! Processing stopped')
      stop()
      }
    }
  }
  #	Launch computation for mean RDVI ----
  
#   if (length(RDVI_files_names) ==2 ) {
#     File_1 = file.path(RDVI_Dir, RDVI_files_names[grep(209, RDVI_files_names)])  ;   File_2 = file.path(RDVI_Dir, RDVI_files_names[grep(225, RDVI_files_names)])		
#     UI_File1= file.path(UI_Dir,UI_files_names[grep(209, UI_files_names)])			;   	 UI_File2= file.path(UI_Dir, UI_files_names[grep(225, UI_files_names)])	
#     RELY_File1= file.path(RELY_Dir,RELY_files_names[grep(209, RELY_files_names)])    	;   	RELY_File2= file.path(RELY_Dir, RELY_files_names[grep(225, RELY_files_names)])
#     out_file = file.path(out_dir_RDVI, out_files_names_RDVI)
#     
#     if (file.exists(out_file) == F | ReProc == 1) {
#       
#       str_idl = paste(	'res = FRG_MEAN_Indexes(',	'File_1 = \'',File_1,
#                        '\' , ',	'File_2 = \'',File_2, 
#                        '\' , ',	'Out_File= \'',out_file, 
#                        '\' , ',	'UI_File1 = \'',UI_File1,
#                        '\' , ',	'UI_File2 = \'',UI_File2,
#                        '\' , ',  'RELY_File1 = \'',RELY_File1,
#                        '\' , ',	'RELY_File2 = \'',RELY_File2,
#                        '\' , ',	'nodata_in = \'',nodata_in, 
#                        '\' , ',	'UI_check = \'',UI_check,
#                        '\' , ',	'max_UI = \'',max_UI, 
#                        '\' )',	sep = ''	)
#       # ------------------------------------------------------------------- #
#       # Build the IDL batch file 
#       # ------------------------------------------------------------------- #
#       
#       batch_file = file.path(FRG_Options$IDL_Dir,'FRG_ComputeMeanIndexes.pro')
#       fileConn<-file(batch_file)
#       writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
#       close(fileConn)
#       print(paste('--- Computing Mean RDVI for year ',  yy, ' ---'))    # Update progress window
#       out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)
#       if (out != 0) {
#          print('An error occurred while Mean RDVI ! Processing stopped')
#       stop()
#       }
#     }
#   }
  return('DONE')
}


