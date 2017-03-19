#' frg_compmean
#' @inheritParams frg_moddownload 
#' @param ReProc 
#' @param UI_check 
#' @param max_UI 
#'
#' @examples
frg_compmean <- function(OutOrig_Path, 
                         ReProc, 
                         yy, 
                         UI_check,
                         max_UI) {
  
  # Get file names for NDVI, UI and Reliability for the selected year ----
  NDVI_Dir = file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/NDVI")
  UI_Dir = file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/QA_usef")
  Rely_Dir = file.path(OutOrig_Path,"time_series/VI_16Days_250m_v6/pixel_reliability")
  
  NDVI_files_names = list.files(NDVI_Dir,pattern = "tif$", full.names = TRUE)
  NDVI_files_names = NDVI_files_names[grep(yy, NDVI_files_names)]
  
  UI_files_names = list.files(UI_Dir,pattern = "tif$", full.names = TRUE)
  UI_files_names =UI_files_names[grep(yy, UI_files_names)]
  
  RELY_files_names = list.files(Rely_Dir,pattern = "*.tif$", full.names = TRUE)
  RELY_files_names = RELY_files_names[grep(yy, RELY_files_names)]
  
  # Crete output folders if necessary
  out_dir_avg = file.path(OutOrig_Path,'NDVI','Average')				
  dir.create(out_dir_avg, showWarnings = FALSE, recursive = T)																	
  
  # Create raster stacks ----
  NDVI_rast <- raster::stack(NDVI_files_names)
  UI_rast   <- raster::stack(UI_files_names)
  Rely_rast <- raster::stack(RELY_files_names)
  
  # Set unreliable VIs to NODATA
  
  frg_maskNDVI <- function(NDVI, UI, Rely) {
    
    NDVI = NDVI * (UI > UI_check) * (Rely < 1)
    # NDVI[[y]] = NA
    return(NDVI)
    
  }
  NDVI_stack <- raster(NDVI)
  for (fileind in seq(along = length(NDVI_files_names))) {
    NDVI        <- raster(NDVI_files_names[fileind])
    UI          <- raster(UI_files_names[fileind])
    Rely        <- raster(RELY_files_names[fileind])
    NDVI_masked <- raster::overlay(NDVI, UI, Rely, fun = frg_maskNDVI)
    NDVI_stack  <- raster::addLayer(NDVI_stack, NDVI_masked)
    gc()
  }
  
  NDVI_avg <- mean(NDVI_stack)
  
  
  
  
  
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


