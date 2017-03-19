#'@title FRG_MOD_Comp_RDVI
#'@description Accessory function used to compute RDVI from RED and NIR images exploiting IDL external function
#'@details This function is used simply to call the appropriate IDL function used to compute RDVI values from MODIS RED and NIR bands data
#' values. See documentation of "FRG_COMPUTE_RDVI.pro" IDL script for additional details
#'
#' @param OutOrig_Path string  Folder where the original MODIS images are being processed (Selected in FRG_MOD_Sel)
#' @param ReProc logical If TRUE, UI is recalculated even if appropriate files are already found in the file system.  
#' @param mess_lab Identifier of the gWidget usd for processing status reporting
#' @param yy numeric Year of the processing
#'
#' @return string 'DONE' if processing completed correctly
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 23, 2012
#' @export

FRG_MOD_Comp_RDVI <- function(OutOrig_Path= OutOrig_Path, ReProc = ReProc, yy = yy) {
  
  exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')	# Expand IDL path to the folder containing the scripts to be executed
  
  RED_Dir = file.path(OutOrig_Path,'Red','Single_Dates')			# Folder containing RED Data
  NIR_Dir = file.path(OutOrig_Path,'NIR','Single_Dates')				# Folder containing NIR Data
  RDVI_Dir = file.path(OutOrig_Path,'RDVI','Single_Dates')				# Folder where to put RDVI data
  dir.create (RDVI_Dir, showWarnings = FALSE, recursive = T) 							# Create output folder if necessary
  
  RED_files_names = list.files(RED_Dir,pattern = ".tif$")										# List of input RED files
  RED_files_names = RED_files_names[grep(yy, RED_files_names)]
  
  if (length(RED_files_names) > 0) {
    
    NIR_files_names =  gsub('Red','NIR',RED_files_names, fixed = TRUE)		# List of input NIR files
    out_files_names = gsub('Red','RDVI',RED_files_names, fixed = TRUE)			# List of output RDVI files
    
    for (file in 1:length(RED_files_names)) {
      
      RED_file = file.path(RED_Dir, RED_files_names[file])			;   NIR_file = file.path(NIR_Dir, NIR_files_names[file])	; out_file = file.path(RDVI_Dir, out_files_names[file])
      
      if (file.exists(out_file) == F | ReProc == 1) {				# Check for output file existance or REproc FLAG
        
        # Create IDL processing string
        str_idl = paste(	'res = FRG_COMPUTE_RDVI(',	'NIR_file = \'',NIR_file, 
                         '\' , ',	'RED_File =\'',  RED_file,
                         '\' ,',	'Out_File=\'',  out_file,
                         '\' ,','nodata_in=\'', FRG_Options$No_Data_Out_Rast,
                         '\' ,','nodata_out  =\'',  FRG_Options$No_Data_Out_Rast,
                         '\' )',	sep = ''	)
        
        # ------------------------------------------------------------------- #
        # Build the IDL batch file 
        # ------------------------------------------------------------------- #
        
        batch_file = file.path(FRG_Options$IDL_Dir,'FRG_CreateRDVI_batch.pro')
        fileConn<-file(batch_file)
        writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
        close(fileConn)
        print(paste('--- Computing RDVI for date ',  out_files_names[file], ' ---'))    # Update progress window
         
        out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)		# Call IDL processing 
        if (out != 0) {
           print('An error occurred while  RDVI ! Processing stopped')
        stop()
        }        
      } # End if on file existance and ReProc flag
    }   #  End cycle on input files
  } 
  
  return('DONE')
  
}


