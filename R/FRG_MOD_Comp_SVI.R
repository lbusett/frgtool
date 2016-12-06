#' FRG_MOD_Comp_SVI
#'@description Function used to call the IDL functiosn used for computation of SRDVI and SNDVI yearly images ( FRG_Compute_MedScaled_VI.pro)
#'@details This function is used to call the IDL function used for computation of SDVI and SNDVI yearly images.\cr
#' Also calls the IDL routines needed to create the ENVI burnt areas ROI, and the ENVI burnt areas MASKS (both total and eroded!) \cr
#'  Yearly SRDVI (or SNDVI) images are computed starting from the corresponding Yearly Average mosaics. Computation of the scaled indexes for each pixel is done as follows:\cr
#' \itemize{
#' \item The pixels belonging to the same class of the target included in a window of width specified by the user centred on the target are identified on the basis of the CLC00 recoded map
#' \item Pixels showing NODATA values in the input VI image, or corresponding to areas classified as "burned" on the input ROI files are removed from the above selection
#' \Item Remaining pixels (i.e., same class, unburned and with good data) are used to compute the MOMENTS of the distribution of VI values of the target CLC class in the window centred on the target pixel
#' \Item Computed MOMENTS are use to determine an ESTIMATE of the 5th, 50th (median) 95th percentile of the distribution, using the Cornish-Fisher fourth-order Expansion method (http://www.nematrian.com/R.aspx?p=CornishFisherDerivation))
#' \Item Scaled indexes for the target pixel is computed exploiting this method:
#'		  'Percentage Deviation to Kernel Median': For each pixel, the algorithm identifies all pixels of the image corresponding to the same CLC class contained in a window of width NKer
#' 			It then computes the median of the distribution of values of the analyzed Vegetation Index in the selecteds pixels. Scaled Index for the pixel are then computed as:
#' 			sVI = 100 * (VI_pix - VI_median)/(VI_median).
#' }#' \cr
#'(For further details, see the documentation for the IDL functions (FRG_Compute_MedScaled_VI.pro))
#'
#' @param MOD_Dir string Main Folder containing MODIS data (i.e., the one above the "Originals" folder)
#' @param Shape_File string ENVI ROI file containing info on burnt areas (derived from the EFFIS shapefile)
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param Out_Folder string Main Output folder for the yearly Scaled Indexes images
#' @param Start_Year numeric Starting year for the analysis
#' @param End_Year numeric Ending Year for the analysis
#' @param NKer numeric width (in Km) of the moving window used for computation of scaled indexes
#' @param SRDVI numeric. 1 = compute SDVI; 2 =  Don't Compute SDVI
#' @param SNDVI numeric. 1 =  compute SNDVI; 2 = Don't Compute SNDVI
#' @param Method numeric. 1 =  0-100 ReScaling; 2 = Percentage Median difference computation \cr
#' @param nodata_out numeric nodata value of input data
#' @param ReProcIm numeric 1 =  Reprocess already available images; 2 = Don't Reprocess already available images
#' @param Intermediate_Folder string Folder where ROI files and Masks files will be placed
#'
#' @return 'DONE' if all went OK, otherwise error message to be treated by "try.catch"
#'
#'
#' @author Lorenzo Busetto (2013)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2013
#' @export

FRG_MOD_Comp_SVI = function(MOD_Dir = MOD_Dir, Shape_File = Shape_File, CLC_File_00 = CLC_File_00,
                            Out_Folder = Out_Folder, Start_Year = Start_Year, End_Year = End_Year,
                            NKer = NKer, Method = Method, SRDVI = SRDVI ,SNDVI = SNDVI,
                            nodata_out = nodata_out,ReProcIm = ReProcIm, Intermediate_Folder = Intermediate_Folder) {


  addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(TRUE)})			# Prevent MAIN_GUI from closing

  # Initialize processing variables
  low_perc = 0.05    ;    high_perc = 0.95      ;    out_5th = 0 		; 		out_95th = 0 				; 			out_NPix = 0					; lowperc_zero = 0
  NKer = (NKer*1000/250)+1				# Compute number of pixels in Kernel on the basis of the extent in KM of the kenel
  exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')	# Expand IDL path to the folder containing the scripts to be executed

  # Initialize message window
  print('--- Checking and building ROI and MAsk files ---')

  # ---------------------------------------------------------------------------------------------------------------------------------  #
  # Ceate a ROI file on the basis of the ORIGINAL SHAPEFILE specified by the user and of the INPUT CLC_00_File (used to determine extent !)
  # ---------------------------------------------------------------------------------------------------------------------------------  #

  ROI_File = FRG_ROI_Build(Shape_File = Shape_File, CLC_File_00 = CLC_File_00,exp_path_str = exp_path_str, Intermediate_Folder= Intermediate_Folder)

  # ---------------------------------------------------------------------------------------------------------------------------------  #
  # create a "Mask" envi file using the ROI File specified by the user. (The mask file is successively used to exclude burned areas
  # from computation of statistics needed for the calculation of the scaled indexes.
  # ---------------------------------------------------------------------------------------------------------------------------------  #

  FireMask_File = FRG_Create_Mask(ROI_File = ROI_File, CLC_File_00 = CLC_File_00,exp_path_str = exp_path_str, Intermediate_Folder= Intermediate_Folder)

  # ---------------------------------------------------------------------------------------------------------------------------------  #
  # create an ERODED "Mask" envi file using the ROI File specified by the user. (The mask file is successively used to determine which of
  # the ROI pixels are CORE pixels (i.e., not on the borders)
  # ---------------------------------------------------------------------------------------------------------------------------------  #


  FireMask_File_Eroded = FRG_Create_Mask_Eroded(ROI_File = ROI_File,FireMask_File = FireMask_File,exp_path_str = exp_path_str, Intermediate_Folder= Intermediate_Folder)

  # Define indexes to be computed and start cycling on indexes ----
  comp_ind = NULL
  if (SNDVI == 1) comp_ind = c(comp_ind, 'NDVI')
  if (SRDVI == 1) comp_ind = c(comp_ind, 'RDVI')
  for (Index in comp_ind) {

    Out_Files = NULL				# Initialize "Out_Files vector" . Used to store output file names to be used for creation of the output ENVI META file

    if (Method == 2 | Method == 3) {   # control on selected method (now obsolete !)

      # ---------------------------------- #
      # Start Cycle on selected years
      # ---------------------------------- #

      for (yy in Start_Year:End_Year) {

        In_Dir = file.path(MOD_Dir, 'Originals',Index,'Average')							# Folder containing the Mean yearly VI images
        In_File = list.files(In_Dir, pattern = '*.hdr$')											# Get List of ENVI header files
        In_File = file.path(In_Dir,In_File[grep(yy, In_File)])								# Find File for the selected year

        # ---- Define output folder and file name ----

        Out_Dir = file.path(Out_Folder, paste('Med_S',Index, sep = ''),'Yearly_Images')					# Output Folder and File Name
        dir.create(Out_Dir, showWarnings = FALSE, recursive = TRUE)
        Out_File = file.path(Out_Dir, paste('Med_S',Index,'_',yy, sep = ''))

        # If output file for selected year and index  doesn't exist, or ReProc = Yes, start the processing
        if (file.exists(Out_File) == F | ReProcIm == 1) {

          print(paste('Computing Med_S',Index,'- Year ',yy, sep = ''))  								# Update status bar
          # Build IDL shell string

          str_idl = paste( 'res = FRG_Compute_MedScaled_VI(',	'CLC_File_00 = \'',CLC_File_00,
                           '\' , ',	'In_File = \'',In_File,
                           '\' , ',	'FireMask_File= \'',FireMask_File,
                           '\' , ',	'Out_File = \'',Out_File,
                           '\' , ',	'nodata_out = \'',nodata_out,
                           '\' , ',	'N_Ker = \'',NKer,
                           '\' , ',	'Index = \'',Index,
                           '\' , ',	'Year = \'',yy,
                           '\' )',	sep = ''	)

          # ------------------------------------------------------------------- #
          # Build the IDL batch file
          # ------------------------------------------------------------------- #
          batch_file = file.path(FRG_Options$IDL_Dir,'FRG_Compute_Med_SVI_batch.pro')
          fileConn<-file(batch_file)
          writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
          close(fileConn)
          out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)					# Launch computation in IDL
          if (out != 0) {
             print('An error occurred while computing scaled indexes  ! Processing stopped')
          stop()
        }
        }	# End of "if" condition on file existence

        if (file.exists(Out_File) == T) {
          Out_Files = c(Out_Files,Out_File)    								# Update list of available sVI files to be used for META file creation
        }	else {stop(" An Error occurred while computing Scaled Indexes")}

      }   # End of Cycle on Years

      # -------------------------------------------- #
      # --- Write the ENVI META text file --- #
      # -------------------------------------------- #
      FRG_Create_Meta(Index = Index, Start_Year = Start_Year, End_Year = End_Year , Method = 2, Out_Folder = Out_Folder, Out_Files = Out_Files)

    }

  }		# End cycle SNDVI vs SRDVI

  addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})			# Prevent MAIN_GUI from closing
  return('DONE')
}

# -------------------------------------------- #
# ---- ACCESSORY FUNCTIONS !!!! ---------
# -------------------------------------------- #

# - Accessory function used to create the ENVI METADATA file ----

FRG_Create_Meta <- function(Index = Index, Start_Year = Start_Year, End_Year = End_Year, Method = Method ,
                            Out_Folder = Out_Folder, Out_Files = Out_Files) {

  # Get files dimensions from the header of the first file
  head_file = paste(Out_Files[1],'.hdr', sep = '')
  fileConn_hd<-file(head_file)
  nsamp = (strsplit(readLines(fileConn_hd)[4], '=')[[1]])[2]
  nrow = (strsplit(readLines(fileConn_hd)[5], '=')[[1]])[2]
  close(fileConn_hd)

  # Define META FILE name
  if (Method ==1 | Method == 3) {
    meta_filename = paste('S',Index,'_',Start_Year,'_',End_Year,'_META', sep = '')
    meta_filename = file.path(Out_Dir = file.path(Out_Folder, paste('S',Index, sep = ''),meta_filename),fsep = '/')
  }

  if (Method ==2 | Method == 3) {
    meta_filename = paste('Med_S',Index,'_',Start_Year,'_',End_Year, '_META', sep = '')
    meta_filename = file.path(Out_Dir = file.path(Out_Folder, paste('Med_S',Index, sep = ''),meta_filename),fsep = '/')
  }

  fileConn_meta<-file(meta_filename, 'w')					# Open connection
  writeLines(c('ENVI META FILE'), fileConn_meta)		# Write first line

  # Write the lines of the META file corresponding to each input file
  for (ff in Out_Files) {
    writeLines(c(paste('File : ', ff, sep = ''),
                 paste('Bands: 1', sep = ''),
                 paste('Dims: 1-',nsamp,' , 1-',nrow, sep = ''), ''),
               fileConn_meta)
  }
  close(fileConn_meta)		# Close connection to META file
}


# Old Stuff ! Previously used to compute scaled indexes using percentages methos ! ----
# if (Method ==1 | Method ==3 ) {
#
#   for (yy in Start_Year:End_Year) {
#
#
#     In_Dir = file.path(MOD_Dir, 'Originals',Index,'Average')  										# Folder containing the Mean yearly VI images
#     In_File = list.files(In_Dir, pattern = '*.hdr')
#     In_File = file.path(In_Dir,In_File[grep(yy, In_File)])																	# File for the selected year
#
#     if (FRG_Options$Use_Temp_Folder == 1) {
#       File_Dir_Temp = file.path(FRG_Options$Temp_Folder,'Temp_Index_Files')
#       dir.create(File_Dir_Temp, recursive = T)
#       In_File_no_hdr = strsplit(In_File,'.hdr')[[1]]
#       file.copy(c(In_File,In_File_no_hdr),to = File_Dir_Temp,overwrite = FALSE, recursive = FALSE,
#                 copy.mode = TRUE)
#       In_File = file.path(File_Dir_Temp,basename(In_File))
#       In_File_no_hdr = strsplit(In_File,'.hdr')[[1]]
#
#
#     }
#
#
#     Out_Dir = file.path(Out_Folder, paste('S',Index, sep = ''),'Yearly_Images')					# Output Folder and File Name
#     dir.create(Out_Dir, showWarnings = FALSE, recursive = TRUE)
#     Out_File = file.path(Out_Dir, paste('S',Index,'_',yy, sep = ''))
#
#     # If output file for selected year and index  doesn't exist, or ReProc = Yes, start the processing
#     if (file.exists(Out_File) == F | ReProc == 1) {
#       svalue(mess_lab) = paste('Computing S',Index,'- Year ',yy, sep = '')									# Update status bar
#       # Build IDL shell string
#       str_idl = paste(	'res = FRG_Compute_Scaled_VI(',	'CLC_File_00 = \'',CLC_File_00,
#                        '\' , ',	'In_File = \'',In_File,
#                        '\' , ',	'FireMask_File= \'',FireMask_File,
#                        '\' , ',	'Out_File = \'',Out_File,
#                        '\' , ',	'nodata_out = \'',nodata_out,
#                        '\' , ',	'N_Ker = \'',NKer,
#                        '\' , ',	'Index = \'',Index,
#                        '\' , ',	'Year = \'',yy,
#                        '\' , ',	'low_perc = \'',low_perc,
#                        '\' , ',	'high_perc = \'',high_perc,
#                        '\' , ',	'out_5th = \'',out_5th,
#                        '\' , ',	'out_95th = \'',out_95th,
#                        '\' , ',	'out_NPix = \'',out_NPix,
#                        '\' , ',	'lowperc_zero = \'',lowperc_zero,
#                        '\' )',	sep = ''	)
#
#       # ------------------------------------------------------------------- #
#       # Build the IDL batch file
#       # ------------------------------------------------------------------- #
#       batch_file = file.path(FRG_Options$IDL_Dir,'FRG_Compute_SVI_batch.pro')
#       fileConn<-file(batch_file)
#       writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
#       close(fileConn)
#
#       out = system ((paste('idl ',batch_file,sep = '')),  invisible = TRUE, show.output.on.console = TRUE)					# Launch computation in IDL
#
#     }	# End of "if" condition on file existence
#     if (file.exists(Out_File) == T) {Out_Files = c(Out_Files,Out_File)}										# Update list of available sVI files
#
#
#     if (FRG_Options$Use_Temp_Folder == 1) {
#       file.remove(In_File)
#       file.remove(In_File_no_hdr)
#
#
#     }
#   }
#   # -------------------------------------------- #
#   # --- Write the ENVI META text file --- #
#   # -------------------------------------------- #
#   FRG_Create_Meta(Index, Start_Year, End_Year,1, Out_Folder, Out_Files)
#
# }
