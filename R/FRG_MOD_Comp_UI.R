#'@title FRG_MOD_Comp_UI
#'@description Accessory function used to perform Convertion of QA to UI values  exploiting IDL external function
#'@details This function is used simply to call the appropraite IDL function used to convert MODIS QA data to UI (Usefulness Index)
#' values. See documentation of "FRG_CONVERT_QA.pro" IDL script for additional details
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
FRG_MOD_Comp_UI <- function(OutOrig_Path= OutOrig_Path, ReProc = ReProc, yy = yy) {
  
  exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')	# Expand IDL path to the folder containing the scripts to be executed
  
  QA_Dir = file.path(OutOrig_Path,'QA','Single_Dates')		# Create QA folder name
  UI_Dir = file.path(OutOrig_Path,'UI','Single_Dates')			# Create UI folder name
  dir.create (UI_Dir, recursive = T,showWarnings = FALSE)   # Create UI folder if necessary
  
  in_files_names = list.files(QA_Dir,pattern = ".tif$")
  in_files_names = in_files_names[grep(yy, in_files_names)]				# List of file names in QA folder corresponding to year yy
  
  if (length(in_files_names) > 0) {
    out_files_names = gsub('QA','UI',file_path_sans_ext(in_files_names, compression = FALSE), fixed = TRUE)		# Define output UI file names
    
    for (file in 1:length(in_files_names)) {											# Cycle on QA files
      
      in_file = file.path(QA_Dir, in_files_names[file])			; out_file = file.path(UI_Dir, out_files_names[file])			# In and out file names
      
      if (file.exists(out_file) == F | ReProc == 1) {				# Check for existance of output files and ReProc flag
        
        # Create IDL "launche" string
        str_idl = paste(	'res = FRG_CONVERT_QA(',	'file_in = \'',in_file, '\' , ',	'file_out =\'',  out_file,'\' ,',	'nodata_in=\'', FRG_Options$No_Data_Out_Rast,'\' ,',
                         'nodata_out  =\'',  FRG_Options$No_Data_Out_Rast,	'\' )',	sep = ''	)
        # ------------------------------------------------------------------- #
        # Build the IDL batch file and launch the processing
        # ------------------------------------------------------------------- #
        
        batch_file = file.path(FRG_Options$IDL_Dir,'FRG_ConvertQA_batch.pro')
        fileConn<-file(batch_file)
        writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
        close(fileConn)
        print(paste('--- Converting QA to UI for file ',  in_files_names[file], ' ---'))    # Update progress window
        out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)    # Launch external process
        if (out != 0) {
          print('An error occurred while computing MODIS UI ! Processing stopped')
          stop()
        }  
      } # End if on outfile existance and ReProc flag
    }	# # End cycle on in_files
  } else { stop('Input QA files not found ! Quitting') }    # End If on file existence
  
  return('DONE')
  
}
