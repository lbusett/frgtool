# - Accessory function for Eroded Mask creation. Calls the FRG_CREATE_MASK_Eroded.pro IDL function

FRG_Create_Mask_Eroded <- function(ROI_File = ROI_File, FireMask_File = FireMask_File, exp_path_str = exp_path_str, Intermediate_Folder = Intermediate_Folder) {
  
  dir.create(file.path(Intermediate_Folder,'ENVI_Mask'), showWarnings = FALSE, recursive = TRUE)
  FireMask_File_Eroded = file.path(Intermediate_Folder,'ENVI_Mask', paste(sub("[.][^.]*$", "", basename(ROI_File)),'_ENVI_Mask_Eroded', sep = ''))
  selection = 'yes'
  #     if (file.exists(FireMask_File)) {
  #   		selection = tk_messageBox(caption = 'Overwrite Warning', type = c ("yesno"), message = 
  #   						"A Mask file for the selected ROI file already exists !\n Do you want to overwrite it ? ", default = 'no')}
  #   	
  if (!file.exists(FireMask_File_Eroded) | selection == 'yes') {
    
    print(paste('--- Creating Eroded Burnt Areas Mask File: ',FireMask_File_Eroded,' ---'))    							# Update status bar
    str_idl = paste('res = FRG_Create_Mask_Eroded(',	'Mask_File = \'',FireMask_File,
                    '\' , ',	'Eroded_Mask_File = \'',FireMask_File_Eroded, 
                    '\' )',	sep = ''	)
    batch_file = file.path(FRG_Options$IDL_Dir,'FRG_CreateMask_Eroded_Batch.pro')
    fileConn<-file(batch_file)
    writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
    close(fileConn)
    out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)					# Launch computation in IDL 
    if (out != 0) {
      print('An error occurred while creating Envi eroded areas mask  ! Processing stopped')
      stop()
    }  
  }
  return(FireMask_File_Eroded)
}