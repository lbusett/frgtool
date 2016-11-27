#'@title FRG_MOD_Comp_SVI
#'@description Function used to extract Scaled VIs time series of the different pixels of analyzed burnt areas, associate info form the burnt areas \cr
#'             shapefile and save as an RData file to be used for statistical analysis
#'@details This function is used to extract Scaled VIs time series of the different pixels of analyzed burnt areas from the raster images created by \cr
#'         "FRG_Compute_SVI.R, associate ancillary info derived form the burnt areas shapefile (e.g., Area, FireYear, ...) and save results as an RData file \cr
#'         (in the "/TS_Extraction subfolder) to be used later for statistical analysis
#'
#' @param SVI_File string ENVI Meta File associated to time serie to be analyzed
#' @param Shape_File string Burnt Areas Shapefile (Must Correspond to the "burned_once" shafile if Overlap == 'Single'
#'                                                 Must Correspond to the "burned_once" shafile if Overlap == 'Multiple')
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param ENV_Zones_File string Tiff file containing EcoZones (No longer used...)
#' @param Out_File numeric BaseName of the output RData and CSV files
#' @param erode flag if 1, then perfom analysis on eroded ROIS (default to 1)
#' @param erode_file string Filename of the ENVI mask containing the ERODED ROIS (generated automatically in FRG_Compute_SVI.R)
#' @param Intermediate_Folder string name of the Folder containing the intermediate processing results
#' @param Overlap string If == 'Single', extract time series for areas burned once.
#'        if =='Multiple, extract time series for areas burned multiple times
#' @param Shape_File_Orig string Filename of the original input BAs shapefile
#' @param LUT_File_Multiple string Filename of the LUT table created by FRG_Process_Shapefile.py
#'
#' @return 'DONE' if all went OK, otherwise error message
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2012
#' @export
#'
FRG_Extr_Stats = function(SVI_File = SVI_File, Shape_File = Shape_File, CLC_File_00 = CLC_File_00,
                          ENV_Zones_File = ENV_Zones_File, Out_File = Out_File,erode = erode, erode_file = erode_file,
                          Intermediate_Folder = Intermediate_Folder, Overlap = Overlap, Shape_File_Orig = '', LUT_File_Multiple = '') {

  if (Overlap == 'Single') {
    Out_RData =  paste(Out_File, 'RData.RData', sep = '_')																		# Filenames used for output saving RData Matrix
    Out_IDL_CSV = paste(Out_File, 'IDL_Matrix.csv', sep = '_')
  } else {
    Out_RData =  paste(Out_File, 'Multiple_RData.RData', sep = '_')  													# Filenames used for output saving RData Matrix
    Out_IDL_CSV = paste(Out_File, 'Multiple_IDL_Matrix.csv', sep = '_')
  }

  # Get Year names from the name of the input MODIS SVI file

  Split_Year = unlist(strsplit(basename(SVI_File), '_'))
  Start_Year = Split_Year[length(Split_Year)-2]
  End_Year = Split_Year[length(Split_Year)-1]

  # Retrieve ROI file name on the bases of the Shape_File name for which TS are to be extracted
  ROI_File = file.path(Intermediate_Folder,'ENVI_ROI', paste(sub("[.][^.]*$", "", basename(Shape_File)),'.ROI', sep = ''))

  # Check for existance of IDL CSV file
  selection = 'yes'

#   if (file.exists(Out_IDL_CSV)) {
#     selection = tk_messageBox(caption = 'Overwrite Warning', type = c("yesno"), message =
#                                 "Outpur CSV file already exists !\n Do you want to re-compute and overwrite it ?\n", defaul = 'no')}
  if (!file.exists(Out_IDL_CSV) | selection == 'yes') {

# ------------------------------------------------------------------- #
# Launch  IDL processing (See FRG_ROI_STAT_ERODE) --------------
# ------------------------------------------------------------------- #

    print('------------------------------------------------------')
    print(paste('-> Extracting Time Series for burnt areas from file: ',basename(SVI_File)))
    print('------------------------------------------------------')
    sp = '\' , '

    str_idl = paste('res = FRG_ROI_STAT_ERODE(','ROI_File = \'', ROI_File, sp,
                    'Erode_File =\'',  	 erode_file,sp,
                    'SVI_File =\'',		 SVI_File,sp,
                    'CLC_File_00=\'', CLC_File_00, sp,
                    'ENV_Zones_File=\'',  ENV_Zones_File, sp,
                    'Out_File =\'', 	 Out_IDL_CSV,sp,
                    'no_data_in = \'', FRG_Options$No_Data_Out_Rast,sp,
                    'Start_Year =\'', 	 Start_Year, sp,
                    'End_Year =\'',    End_Year,
                    '\' )',
                    sep = ''	)

    # ------------------------------------------------------------------- #
    # Buil the IDL batch file and call the IDL process
    # ------------------------------------------------------------------- #
    exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')
    fileConn<-file(file.path(FRG_Options$IDL_Dir,'FRG_Comp_Stats_batch.pro'))
    writeLines(c(exp_path_str,'envi, /restore_base_save_files  ', "ENVI_batch_init",str_idl,'exit'), fileConn)
    close(fileConn)
    batch_file = file.path(FRG_Options$IDL_Dir,'FRG_Comp_Stats_batch.pro')

    out = system ((paste(FRG_Options$idl_exe,batch_file,sep = ' ')),  invisible = TRUE, show.output.on.console = TRUE)  # Call processing

    if (out != 0) {
      print('An error occurred while Extracting time series data ! Processing stopped')
      stop()
    }

  } else { print(paste('-> Time series file already existing: ',basename(SVI_File)))}

  # ------------------------------------------------------------------- #
  # Create the RData matrixes, by joining info with the burnt areas shapefile.
  # ------------------------------------------------------------------- #

  if (file.exists(Out_RData)) {
    selection = tk_messageBox(caption = 'Overwrite Warning', type = c ("yesno"), message =
                                "Outpur RData files already exists !\n Do you want to re-compute and overwrite them ?\n", defaul = 'no')}
  if (!(file.exists(Out_RData)) | selection == 'yes') {

    print(paste('-> Creating RData file joining MODIS and Shapefile info'))

    #----------------------------------------------------------------------------------------------------------#
    # Load the burned Areas shapefile and save the attribute table as a data frame
    #-----------------------------------------------------------------------------------------------------------------#
    BAreas_Name = strsplit(basename(Shape_File_Orig),'.shp')[[1]]
    BAreas_Dir = dirname(Shape_File_Orig)
    BAreas_shp = readOGR(BAreas_Dir, BAreas_Name)
    Data_Shape = BAreas_shp@data										# Get attributes data from the shp
    Data_Shape = arrange(Data_Shape,desc(Area_HA))

    # Reassign FireSeason according to FireDate: If FireDate exists, check if the fire happened after DOY 235: in that case reassign FireSeason
    # as FireSeason + 1 (needed because images used to compute SDVI are of DOYS 209 and 225: if the fire happened after this date, the "effect" will be visible only on the next
    # year ! -- OBSOLETE AND REMOVED !!!!

    #     doys = as.POSIXlt(Data_Shape$FireDate)$yday
    #     Data_Shape$YearSeason[which(doys >= 245 )] =Data_Shape$YearSeason[which(doys >= 245 )] + 1
    #

  #----------------------------------------------------------------------------------------------------------#
  # Processing conducted when analyzing areas burned only once
  #----------------------------------------------------------------------------------------------------------#

    if (Overlap == 'Single') {
      # Load the statistics file computed by IDL  (Derived from Application of FRG_ROI_STAT_ERODE.pro)

      Data = read.csv(Out_IDL_CSV,header = TRUE, na.strings = FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE)
      Data[Data == FRG_Options$No_Data_Out_Rast] = NA						# Put the NODATA to "R" NA
      n_Years = (length(names(Data)) - 4)/2
      names(Data)[1] = 'OBJECTID'
      names(Data)[5:(5+n_Years-1)] =  as.character(seq(as.numeric(Start_Year),as.numeric(End_Year)))    # Correct columns names
      names(Data)[(5+n_Years):(5+2*n_Years-1)] =  paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))), sep = '_')    # Correct columns names

      # Remove from Data_Shape records not found in the MODIS ROI data

      Data_Shape = Data_Shape[which(Data_Shape$OBJECTID %in% unique(Data$OBJECTID)) ,]

      # Compute the FireYear and Area variable for each fire (From the sahpefile data) and add it to the DataFrame

      FireYear = numeric(length(Data$OBJECTID))
      Area_All = numeric(length(Data$OBJECTID))
      Area_Forest = numeric(length(Data$OBJECTID))
      IDS = unique(Data_Shape$OBJECTID)

      estimate_Area = function(Data_tmp) {   #Accessory function used to compute Burnt area for each CLC class from the number of pixels in the ROI
                                             # Used only if Info is not already present in the shapefile
        full_Area  =250*250*(length(Data_tmp$N_PIX))/10000.0
        tot_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(2,3,4,5,6))]))/10000.0
        bro_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(2))]))/10000.0
        con_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(3))]))/10000.0
        mix_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(4))]))/10000.0
        scler_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(5))]))/10000.0
        trans_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(6))]))/10000.0
        est_Area = data.frame (tot_Area = tot_Area,bro_Area=bro_Area ,con_Area=con_Area,mix_Area=mix_Area,trans_Area=trans_Area,scler_Area=scler_Area, full_Area = full_Area)
      }

      for(FireID in 1:length(IDS)){
        FireRows = which(Data$OBJECTID == IDS[FireID])                           # Rows of the dataframe corresponding to the selected fire
        YY = Data_Shape$YearSeason[Data_Shape$OBJECTID == IDS[FireID]][1]        # Retrieve Fire Year
        Area_For = Data_Shape[Data_Shape$OBJECTID == IDS[FireID],'BroadLeave']+  # Retrieve Total Area burned in considered CLC_Classes
                   Data_Shape[Data_Shape$OBJECTID == IDS[FireID],'Coniferous']+
                   Data_Shape[Data_Shape$OBJECTID == IDS[FireID],'MixedFores']+
                   Data_Shape[Data_Shape$OBJECTID == IDS[FireID],'Sclerophyl']+
                   Data_Shape[Data_Shape$OBJECTID == IDS[FireID],'Transition']
        if (Area_For == 0 ){                    # If Area_Forest = 0 then, noinfo in shape and area estimated from number of pixels.
          est_Area = estimate_Area(Data[FireRows,])
          Area_For = est_Area$tot_Area
          Data_Shape$BroadLeave[Data_Shape$OBJECTID == IDS[FireID]] = est_Area$bro_Area
          Data_Shape$Coniferous[Data_Shape$OBJECTID == IDS[FireID]] = est_Area$con_Area
          Data_Shape$MixedFores[Data_Shape$OBJECTID == IDS[FireID]] = est_Area$mix_Area
          Data_Shape$Sclerophyl[Data_Shape$OBJECTID == IDS[FireID]] = est_Area$scler_Area
          Data_Shape$Transition[Data_Shape$OBJECTID == IDS[FireID]] = est_Area$trans_Area
        }
        Area_Tot = Data_Shape$Area_HA[Data_Shape$OBJECTID == IDS[FireID]][1]       # Retrieve Total Area burned
        if (Area_Tot == 0) {Area_All= est_Area$full_Area}                  # If Area_Tot = 0 then, noinfo in shape and area estimated from number of pixels.
        FireYear [FireRows] = YY          # Assign FireYear
        Area_Forest [FireRows] = Area_For           # Assign total area burnt in forest land cover types
        Area_All [FireRows] = Area_Tot           # Assign total area burnt in forest land cover types
      }
      Data$FireYear = FireYear          # Assign FireYear
      Data$Area_All = Area_All          # Assign total area of the intersectr
      Data$Area_Forest = Area_Forest            # Assign total area burnt in forest land cover types

      Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1, ncol(Data),5:(ncol(Data)-3))]       													# Reorder Columns

      # Reclass the values of CORINE using the same scheme used in the EFFIS data base

      Data$CLC_Class  = factor(Data$CLC_Class, labels = c('Artificial Surfaces','Agricultural Areas','Broadleaved Forests',
                                                          'Coniferous Forests','Mixed Forests','Schlerophyllus Vegetation',	'Transitional Vegetation','Other Natural Land','Wetlands','Water Bodies','Other'))

      Data$ENV_ZONE  = recode(Data$ENV_ZONE ,  # Recode ENV_ZONE according to legend
                              "1 = 'ALN';	2 = 'BOR';	3 = 'NEM';	4 = 'ATN';	5 = 'ALS';
              							6 = 'CON';	7 = 'ATC';	8 = 'PAN';	9 = 'LUS';	10 = 'ANA';
              							11 = 'MDM'; 12 = 'MDN'; 13 = 'MDS'"
                              , as.factor.result = TRUE)

      # Add attributes to the Data Frame (Useful to keep track of processing options!)

      attr(Data, "SVI_File") = SVI_File					;    attr(Data, "Shape_File") = Shape_File   ; 	attr(Data, "CSV_File") = Out_IDL_CSV
      attr(Data, "CLC_File") = CLC_File_00			;    attr(Data, "Processing Date") = Sys.Date()
      attr(Data, "Start_Year") = Start_Year     ;    attr(Data, "End_Year") = End_Year
      if (length(grep( 'RDVI',SVI_File)) > 0) {attr(Data, "Index") ='RDVI'}     # to be removed !
      if (length(grep( 'NDVI',SVI_File)) > 0) {attr(Data, "Index") ='NDVI'}     # to be removed !
      if (length(grep( 'SNDVI',SVI_File) > 0)) {attr(Data, "Index") ='SNDVI'}
      if (length(grep( 'SRDVI',SVI_File) > 0)) {attr(Data, "Index") ='SRDVI'}
      if (length(grep( 'Med_SNDVI',SVI_File) > 0)) {attr(Data, "Index") ='Med_SNDVI'}
      if (length(grep( 'Med_SRDVI',SVI_File)) > 0) {attr(Data, "Index") ='Med_SRDVI'}

      # Save the computed Data Frames

      print(paste('-> Saving TS info RData file'))
      save(Data, Data_Shape, file = Out_RData)

      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(Data, BAreas_shp)
      gc()
    } else {

#----------------------------------------------------------------------------------------------------------#
# Processing conducted when analyzing areas burned multiple times
#----------------------------------------------------------------------------------------------------------#

      BAreas_Name = strsplit(basename(Shape_File),'.shp')[[1]]   # Open shapefile of areas burned multiple times
      BAreas_Dir = dirname(Shape_File)                           # (Used to determine burnt surface in the overlaps )
      BAreas_shp_mult = readOGR(BAreas_Dir, BAreas_Name)
      Data_Shape_mult = BAreas_shp_mult@data  									# Get attributes data from the shp

      # Load the statistics file computed by IDL  (Derived from Application of FRG_ROI_STAT_ERODE.pro)
      Data = read.csv(Out_IDL_CSV,header = TRUE, na.strings = FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE)
      Data[Data == FRG_Options$No_Data_Out_Rast] = NA  					# Put the NODATA to "R" NA
      n_Years = (length(names(Data)) - 4)/2
      names(Data)[1] = 'OVERLAP_ID'
      names(Data)[5:(5+n_Years-1)] =  as.character(seq(as.numeric(Start_Year),as.numeric(End_Year)))    # Correct columns names
      names(Data)[(5+n_Years):(5+2*n_Years-1)] =  paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))), sep = '_')    # Correct columns names

      # Load the LUT data relating each overlap with the corresponding original BAs
      Data_LUT = read.csv2(file=LUT_File_Multiple, stringsAsFactors = FALSE, header = TRUE, sep = ';')   # Restore the LUT
      # Create a new empty Data_Shape object. This will be filled using info from the ROIS
      Data_Shape  = droplevels(Data_Shape[0,])
      names(Data_Shape)[1] = 'OVERLAP_ID'
      names = names(Data_Shape)
      n_recs = length(unique(Data$OVERLAP_ID))
      tmp_df = as.data.frame(array(NA, dim = c(n_recs,length(names))))
      names(tmp_df)=names
      Data_Shape = tmp_df
      #Set the Data_Shape OVERLAPID to the values derived from the ROIS
      Data_Shape$OVERLAP_ID = unique(Data$OVERLAP_ID)
      Data_Shape$FireYear = 'Multiple'
      Data_Shape$YearSeason = 'Multiple'
      Data_Shape$FireDate = 'Multiple'
      # Compute the FireYear and Area variable for each fire (From the sahpefile data) and add it to the DataFrame
      # FireYear corresponds to the FIreYear of the earlier fire that originated the overlap
      FireYear = numeric(length(Data$OBJECTID))
      Area_All = numeric(length(Data$OVERLAP_ID))
      Area_Forest = numeric(length(Data$OVERLAP_ID))
      IDS = unique(Data$OVERLAP_ID)

      estimate_Area = function(Data_tmp) {   #Accessory function used to compute Burnt area for each CLC class from the number of pixels in the ROI
        # Used ALWAYS for areas burned multiple times, since no Info is already available regarding the area burned in different classes within an overlap
        full_Area  =250*250*(length(Data_tmp$N_PIX))/10000.0
        Area_For = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(2,3,4,5,6))]))/10000.0
        bro_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(2))]))/10000.0
        con_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(3))]))/10000.0
        mix_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(4))]))/10000.0
        scler_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(5))]))/10000.0
        trans_Area = 250*250*(length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% c(6))]))/10000.0
        est_Area = data.frame (Area_For = Area_For,bro_Area=bro_Area ,con_Area=con_Area,mix_Area=mix_Area,trans_Area=trans_Area,scler_Area=scler_Area, full_Area = full_Area)
      }

      for(FireID in 1:length(IDS)){
        FireRows = which(Data$OVERLAP_ID == IDS[FireID])                           # Rows of the dataframe corresponding to the selected fire
        min_FireYear = min(Data_LUT[which(Data_LUT$OVERLAP_ID == IDS[FireID]),]$YearSeason)   # Get fire year of the first fire originating the overlap
        YY = min_FireYear   # Set Fire Year to the year of the first fire
        est_Area = estimate_Area(Data[FireRows,])    # Estimate burned area in the different LC classes, as the product of the number of pixel*250*250
        Data_Shape$Area_HA[Data_Shape$OVERLAP_ID == IDS[FireID]] = Data_Shape_mult$Area_Int[BAreas_shp_mult$OVERLAP_ID == IDS[FireID]][1]  # Area burned in all land cover types
        Data_Shape$BroadLeave[Data_Shape$OVERLAP_ID == IDS[FireID]] = est_Area$bro_Area
        Data_Shape$Coniferous[Data_Shape$OVERLAP_ID == IDS[FireID]] = est_Area$con_Area
        Data_Shape$MixedFores[Data_Shape$OVERLAP_ID == IDS[FireID]] = est_Area$mix_Area
        Data_Shape$Sclerophyl[Data_Shape$OVERLAP_ID == IDS[FireID]] = est_Area$scler_Area
        Data_Shape$Transition[Data_Shape$OVERLAP_ID == IDS[FireID]] = est_Area$trans_Area
        FireYear [FireRows] = YY          # Assign FireYear
        Area_Forest [FireRows] = est_Area$Area_For          # Assign total area burnt in forest land cover types
        Area_All [FireRows] = Data_Shape_mult$Area_Int[BAreas_shp_mult$OVERLAP_ID == IDS[FireID]][1]           # Assign total area burnt in forest land cover types
      }

      Data$FireYear = FireYear          # Assign FireYear
      Data$Area_All = Area_All          # Assign total area of the intersect
      Data$Area_Forest = Area_Forest            # Assign total area burnt in forest land cover types

      Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1, ncol(Data),5:(ncol(Data)-3))]  																# Reorder Columns

      # Reclass the values of CORINE using the same scheme used in the EFFIS data base

      Data$CLC_Class  = factor(Data$CLC_Class, labels = c('Artificial Surfaces','Agricultural Areas','Broadleaved Forests',
                                                          'Coniferous Forests','Mixed Forests','Schlerophyllus Vegetation',	'Transitional Vegetation','Other Natural Land','Wetlands','Water Bodies','Other'))

      Data$ENV_ZONE  = recode(Data$ENV_ZONE ,  # Recode ENV_ZONE according to legend
                              "1 = 'ALN';	2 = 'BOR';	3 = 'NEM';	4 = 'ATN';	5 = 'ALS';
              							6 = 'CON';	7 = 'ATC';	8 = 'PAN';	9 = 'LUS';	10 = 'ANA';
              							11 = 'MDM'; 12 = 'MDN'; 13 = 'MDS'"
                              , as.factor.result = TRUE)

      # Add attributes to the Data Frame (Useful to keep track of processing options!)

      attr(Data, "SVI_File") = SVI_File					;    attr(Data, "Shape_File") = Shape_File   ; 	attr(Data, "CSV_File") = Out_IDL_CSV
      attr(Data, "CLC_File") = CLC_File_00			;    attr(Data, "Processing Date") = Sys.Date()
      attr(Data, "Start_Year") = Start_Year     ;    attr(Data, "End_Year") = End_Year
      if (length(grep( 'RDVI',SVI_File)) > 0) {attr(Data, "Index") ='RDVI'}     # to be removed !
      if (length(grep( 'NDVI',SVI_File)) > 0) {attr(Data, "Index") ='NDVI'}     # to be removed !
      if (length(grep( 'SNDVI',SVI_File) > 0)) {attr(Data, "Index") ='SNDVI'}
      if (length(grep( 'SRDVI',SVI_File) > 0)) {attr(Data, "Index") ='SRDVI'}
      if (length(grep( 'Med_SNDVI',SVI_File) > 0)) {attr(Data, "Index") ='Med_SNDVI'}
      if (length(grep( 'Med_SRDVI',SVI_File)) > 0) {attr(Data, "Index") ='Med_SRDVI'}

      # Save the computed Data Frames

      print(paste('-> Saving TS info RData file'))
      save(Data, Data_Shape, file = Out_RData)

      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(Data, BAreas_shp)
      gc()

    }
  } else { print(paste('-> RData file joining MODIS and Shapefile info already existing: ',basename(Out_RData)))}

  # Completed
  return('DONE')
}
