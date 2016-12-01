#' @title FRG_Full_Processing
#' @description Function used to apply all FRG processing steps \cr
#'
#' @param MOD_Dir string Folder where the original and preprocessed image will be stored (i.e., the one above the "Originals" folder)
#' @param Shape_File string Input Shapefile of BAs
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param Out_Folder string Main Results Folder where results of the analysis will be saved
#' @param Start_Year numeric Starting year of the analysis
#' @param End_Year numeric Ending Year of the analysis
#' @param Method numeric. 2 = Percentage Median difference computation \cr
#' @param SRDVI numeric. 1 = compute SRDVI; 2 =  Don't Compute SRDVI
#' @param SNDVI numeric. 1 =  compute SNDVI; 2 = Don't Compute SNDVI
#' @param ReProc numeric if = 1, already existing Scaled Indexes will be recomputed
#' @param ReDown numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and 
#'     		 already existing mosaics will be overwritten 
#' @param ReProcIm numeric if = 1, already existing MODIS mosaics will be reprocessed
#' @param erode flag if = 1, analysis is conducted only on core pixels (defaults to 1)
#' @param min_pix numeric minimum number of core pixels needed for a BA to be processed (default to 10)
#' @param NKer numeric width (in Km) of the moving window used for computation of scaled indexes
#' @param sig_level numeric Significance level for Wilcoxon test. Default to 0.05
#' @param sub_zones Obsolete
#' @param perc_diffs numeric hash table Reduction Threshold on NDVIR reduction used in significance reduction analysis with wilcoxon test
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@jrc.ec.europa.eu
#' @export
#' 
  # Shape_File     <- "D:/Documents/temp/frgdata/Input_Shapefiles/Burned_Areas_00_15.shp"
  # Erode_File     <- "D:/Documents/temp/frgdata/output_2015/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask_Eroded"
  # sVI_Folder     <- "D:/Documents/temp/frgdata/output_2015/Scaled_Indexes/Med_SNDVI/Yearly_Images/"
  # CLC_File_00    <- "data/CLC_00/CLC_00_250_ENVI"
  # ENV_Zones_File <- "data/ENV_Zones/ENV_Zones.tif"
  # Out_Folder     <- "D:/Documents/temp/frgdata/output_2015/"
  # no_data_in     <- -999
  # Start_Year     <- 2003
  # End_Year       <- 2015
  # csv_file_single <- "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/Burned_Once/TS_Extraction_Med_SNDVI_2000_2015_META_IDL_Matrix.csv"
  # RData_file_single <- "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/Burned_Once/TS_Extraction_Med_SNDVI_2000_2015_META_IDL_Matrix.RData"
  # perc_diff      <- 9.5
  # min_pix        <- 20
  # MedWdt         <- 3
  # Method = 2
  # NKer = 200
  # SNDVI = 1




FRG_Full_Processing = function(MOD_Dir= MOD_Dir, Shape_File = Shape_File, CLC_File_00 = CLC_File_00, 
                               Out_Folder = Out_Folder, Start_Year = Start_Year, End_Year = End_Year,
                               Method = Method, SRDVI = 1, SNDVI = 2,ReProc = ReProc, ReDown = ReDown, ReProcIm = ReProcIm, 
                               erode = 1, min_pix = 10, MedWdt = 3, NKer = NKer , sig_level = 0.05,
                               sub_zones = 0,  perc_diffs = hash( c(NDVI=9.5, RDVI=11.5))      ) {
  
  SRDVI = 1
  #- ------------------------------------------------------------------------------------------------- - #
  #- Initialize Processing: Set processing folders on the basis of user's choice and set some processing parameters 
  #- ------------------------------------------------------------------------------------------------- - #
  
  perc_diff = perc_diffs[['NDVI']]
  
  Out_Folder = file.path(Out_Folder,paste('Results', Start_Year,End_Year, paste('Ker',NKer,sep ='_'),
                                          paste('Perc_Diff',perc_diff,sep ='_'), sep = '_'))  # Set Main Statistical Results folder
  
  selection = 'yes'  # Check if Main Results folder already exist and ask if overwrite
  
  if (file.exists(Out_Folder)) { selection = tk_messageBox(caption = 'Overwrite Warning', type = c ("yesno"), message = 
                            "A results folder for the same analysis already exists ! All results will be overwritten !\n Do you want to continue ? ", default = 'no')}
  if (selection == 'yes'){
    
    Out_Summary_File = file.path(Out_Folder,paste('Processing_Summary','.txt', sep = ''))  # Set Processing log file
    OutFile_Conn<-Out_Summary_File      # Open log file
    
    Intermediate_Folder = file.path(Out_Folder,'Intermediate_Processing') # Set folder for storing intermediate processing results
    dir.create(Intermediate_Folder, recursive = T, showWarnings = FALSE)
    
    Results_Summary_Folder = file.path(Out_Folder,'Summaries_for_EFFIS')  # Set folder for storing EFFIS summaries results
    dir.create(Results_Summary_Folder, recursive = T, showWarnings = FALSE)
    
    Scaled_Folder = file.path(Out_Folder,'Scaled_Indexes')     # Define folder were Scaled Indexes time series will be saved
    dir.create(Scaled_Folder, recursive = TRUE, showWarnings  = FALSE)   #create folder
    
    # ENV_Zones_File = file.path(FRG_Options$Main_Dir,'Ancillary_Data','ENV_Zones','ENV_Zones.tif') # Input file of EcoZOnes (Obsolete and unused, now)

   # Flags to skip some processing steps in debugging phase - Set all to T for complete processing - proceed with caution !
    MOD_dwl = F  ;   Comp_SVI = F  ;  Extr_Stat = T   ;  Sig_Anal = T;  Perc_Anal = T
    
    # Set flags indicating which rescaled indexes to calculate
    comp_ind = NULL
    if (SNDVI == 1) comp_ind = c(comp_ind, 'NDVI')
    if (SRDVI == 1) comp_ind = c(comp_ind, 'RDVI')
    
    # Check if Out Folder for similar processing parameters already esist and ask if it is ok to overwrite. 
    
    
    # Write first lines of processing summary text file
    
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=FALSE)
    cat("--- Processing Summary",file = OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat("", file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(paste("--- Processing Date: "), file =OutFile_Conn,sep="\n",append=TRUE)  
    cat("", file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- Processing PArameters ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat("", file =OutFile_Conn,sep="\n",append=TRUE)
    cat(paste("--- Start Year for MODIS Data: ",Start_Year), file =OutFile_Conn,sep="\n",append=TRUE)             
    cat(paste("--- End Year for MODIS Data: ",End_Year), file =OutFile_Conn,sep="\n",append=TRUE)             
    cat(paste("--- Moving Window  Size: ",NKer,' Km'), file =OutFile_Conn,sep="\n",append=TRUE)   
    cat(paste("--- Number of Before Fire Years used as reference : ",MedWdt), file =OutFile_Conn,sep="\n",append=TRUE)   
    if(SNDVI == 1){cat(paste('--- NDVI Analysis: Yes'), file =OutFile_Conn,sep="\n",append=TRUE)} else if(SNDVI == 1){cat(paste('--- NDVI Analysis: No'), file =OutFile_Conn,sep="\n",append=TRUE)}
    if(SRDVI == 1){cat(paste('--- RDVI Analysis: Yes'), file =OutFile_Conn,sep="\n",append=TRUE)} else if(SNDVI == 1){cat(paste('--- RDVI Analysis: No'), file =OutFile_Conn,sep="\n",append=TRUE)}
    if(SNDVI == 1){cat(paste("--- Minimum Percentages for significant difference: NDVI--> ",perc_diffs[['NDVI']]), file =OutFile_Conn,sep="\n",append=TRUE)} 
    if(SRDVI == 1){ cat(paste("--- Minimum Percentages for significant difference: RDVI--> ",perc_diffs[['RDVI']]), file =OutFile_Conn,sep="\n",append=TRUE)}
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- Input Data ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(paste("--- Input ShapeFile of Burnt Areas: ",Shape_File), file =OutFile_Conn,sep="\n",append=TRUE)             
    cat(paste("--- Input Corine Map: ",CLC_File_00), file =OutFile_Conn,sep="\n",append=TRUE)     
    cat(paste("--- Input EcoZones Map: ",ENV_Zones_File), file =OutFile_Conn,sep="\n",append=TRUE)     
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    
    
    #- ------------------------------------------------------------------------------------------------- - #
    #- Call Routines for download and preprocessing ----
    #- ------------------------------------------------------------------------------------------------- - #
    
    if (MOD_dwl == T) {
      
      print('----------------------------------------------------------')
      print('---------- MODIS Download and PreProcessing --------------')
      print('----------------------------------------------------------')
      print(c('-> Main MODIS Folder: ',MOD_Dir))
      
      dir.create(MOD_Dir, recursive = TRUE, showWarnings = FALSE)
      er =(FRG_MOD_Proc(MOD_Dir= MOD_Dir, Start_Year= Start_Year, End_Year= End_Year, ReProcIm = ReProcIm, ReDown = ReDown))
      print('-> MODIS PreProcessing Completed')
      print('----------------------------------------------------------')
      
    } # End if on  MODIS processing
    
    #- ------------------------------------------------------------------------------------------------- - #
    #- Call Routines for Scaled Indexes Computation ----
    #- ------------------------------------------------------------------------------------------------- - #
    
    if (Comp_SVI == T) {
      print('----------------------------------------------------------')
      print('------------- Computation of Scaled Indexes --------------')
      print('----------------------------------------------------------')
      print(c('-> Scaled Indexes Output Folder: ',Scaled_Folder))
      er = 'DONE'
      er = FRG_MOD_Comp_SVI(MOD_Dir = MOD_Dir, Shape_File = Shape_File, CLC_File_00 = CLC_File_00, 
                          Out_Folder = Scaled_Folder, Start_Year = Start_Year, End_Year = End_Year, 
                          NKer = NKer, Method = Method, SRDVI = SRDVI ,SNDVI = SNDVI,
                          nodata_out = FRG_Options$No_Data_Out_Rast,ReProcIm = ReProcIm, Intermediate_Folder = Intermediate_Folder)
      print(' -> Computation of Scaled Indexes Completed')
      print('----------------------------------------------------------')
      if (er != 'DONE') {stop ('An Error occurred while computing Scaled Indexes !')}
    } # End if on "Comp NDVIR
    
    #- ------------------------------------------------------------------------------------------------- - #
    #- Start Call Routines for Statistical analysis ----
    #- ------------------------------------------------------------------------------------------------- - #
    
    print('----------------------------------------------------------')
    print('------------------ Statistical Analysis ------------------')
    print('----------------------------------------------------------')
    
    Out_Stat_Dir = file.path(Out_Folder)
    dir.create(Out_Stat_Dir, recursive = TRUE, showWarnings  = FALSE)
    print(paste('-> Statistical Results Main Folder: ',Out_Stat_Dir))
    print('----------------------------------------------------------')
    
    for (Index in comp_ind) {
      
      # ----------------------------------------------------------------------------------#
      #   Call routines for Time Series Extraction ----
      # ----------------------------------------------------------------------------------#
      
      TS_filename = paste('Med_S',Index,'_',Start_Year,'_',End_Year, '_META', sep = '')         # Set Input file of MODIS data for Time Series Extraction
      TS_filename = file.path(Out_Dir = file.path(Scaled_Folder, paste('Med_S',Index, sep = ''),TS_filename),fsep = '/')
      
      # ------------------------------------------------------------------- #
      # --- Process the Burnt area shapefile to create the shapefile of areas burnt once
      # --- and that of areas burnt multiple times and the corresponding ROIs
      # ------------------------------------------------------------------- #  
      
      # Shape_Files_Inter = FRG_Process_Shapefile(Shape_File = Shape_File, Intermediate_Folder = Intermediate_Folder,
                                                # CLC_File_00=CLC_File_00)
      Shape_Files_Inter = data.frame(Shape_File_Single = "/home/lb/Google_Drive/Intermediate_Processing/Shapefiles//Burned_Areas_00_15_Single_Fires.shp",
                                     Shape_File_Multiple = "/home/lb/Google_Drive/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp",
                                     LUT_File_Multiple    = "/home/lb/Google_Drive/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Intersect_LUT_csv.csv")
      # retrieve the ROI file name and the name of the ENVI mask file of eroded ROIS (created automatically in FRG_Compute_SVI)
      ROI_File = file.path(Intermediate_Folder,'ENVI_ROI', paste(sub("[.][^.]*$", "", basename(Shape_File)),'.ROI', sep = '')) # Define ROI file name
      erode_file = file.path(Intermediate_Folder,'ENVI_Mask',
                             paste(sub("[.][^.]*$", "", basename(ROI_File)),'_ENVI_Mask_Eroded', sep = ''))
      
      if (Extr_Stat == T) {
        
        #       Perform TS extraction on the shapefile of areas burned once
        out_dir= file.path(Out_Stat_Dir,paste('Med_S',Index, sep = ''),'TS_Extraction','Burned_Once')       # Set output folder
        dir.create(out_dir,recursive = T, showWarnings = F)
        
        ExtTS_File = file.path(out_dir, paste('TS_Extraction_',basename(TS_filename), sep = ''))            # Set Output file of Time Series Extraction
        
        message('----------------------------------------------------------')
        message('------ Extraction of sVI time series for burnt areas - Areas Burnt Once -----')
        message('----------------------------------------------------------')
        message(paste('-> In File for TS extraction: ',TS_filename))
        message(paste('-> Out File for TS extraction: ',ExtTS_File))
        
        er = FRG_Extr_Stats_new(SVI_File = TS_filename, Shape_File = as.character(Shape_Files_Inter$Shape_File_Single),    # Call the processing routine
                            CLC_File_00 = CLC_File_00 , ENV_Zones_File = ENV_Zones_File, Out_File = ExtTS_File, 
                            erode = 1, erode_file = erode_file, Intermediate_Folder = Intermediate_Folder, Overlap = 'Single',
                            Shape_File_Orig = Shape_File, LUT_File_Multiple = '')  
        
#       Perform TS extraction on the shapefile of areas burned more than once
        out_dir = file.path(Out_Stat_Dir,paste('Med_S',Index, sep = ''),'TS_Extraction','Burned_Multiple')    # Set output folder
        dir.create(out_dir,recursive = T, showWarnings = F)
        ExtTS_File_Multiple = file.path(out_dir, paste('TS_Extraction_',basename(TS_filename), sep = ''))  # Set Output file of Time Series Extraction
        
        print('----------------------------------------------------------')
        print('------ Extraction of sVI time series for burnt areas - Areas Burnt Multiple Times -----')
        print('----------------------------------------------------------')
        print(paste('-> In File for TS extraction: ',TS_filename))
        print(paste('-> Out File for TS extraction: ',ExtTS_File_Multiple))
        
        er = FRG_Extr_Stats_new(SVI_File = TS_filename, Shape_File = as.character(Shape_Files_Inter$Shape_File_Multiple),    # Call the processing routine
                            CLC_File_00 = CLC_File_00 , ENV_Zones_File = ENV_Zones_File, Out_File = ExtTS_File_Multiple, 
                            erode = 1, erode_file = erode_file,Intermediate_Folder = Intermediate_Folder, Overlap = 'Multiple',
                            Shape_File_Orig = Shape_File, LUT_File_Multiple = as.character(Shape_Files_Inter$LUT_File_Multiple))    
        
        if (er == 'DONE') {
          print('--- TS Extraction Completed ---')
          print('----------------------------------------------------------')
        } else { stop('An Error Occurred while extracting the Time Series')}      
        
      } # End If on Extr_Stat
      
      # ----------------------------------------------------------------------------------#
      #   Call routines for extraction of plotting data and for Significance Analysis (The latter, only on the areas burnt once !) ----
      # ----------------------------------------------------------------------------------#
      
      if (Sig_Anal == T) {
        
        # Initialize and define output file names and folders
        ExtTS_RData_File =  paste(ExtTS_File, 'RData.RData', sep = '_')										# Recreate name of RData TS Extraction output
        Out_Stats_Folder_Single = file.path(dirname(dirname(dirname(ExtTS_RData_File))),'Stat_Analysis','Burned_Once',sep = '')
        Out_Stats_File_Single = file.path(Out_Stats_Folder_Single,gsub('TS_Extraction','Stat_Analysis',basename(ExtTS_RData_File)))  # Set Basename for statistics output file
        dir.create(Out_Stats_Folder_Single,recursive = T, showWarnings = F)
        print('----------------------------------------------------------')
        print('------ Extract plotting data and perform Statistical Analysis of NDVIR reductions ----')
        print('------ on areas burned once                                                        ----')
        print('----------------------------------------------------------')
        print('')
        print(paste('-> In File for Analysis: ',ExtTS_RData_File))
        print(paste('-> Out File for Analysis: ',Out_Stats_File_Single))
        
        # ----------------------------------------------------------------------------------#        
        # Extract plotting data for areas burnt once, and perform significance analysis     #
        # - for areas burnt once                                                            #
        # ----------------------------------------------------------------------------------#        
        
        er = FRG_Significance_Matrix(In_File = ExtTS_RData_File, Out_File = Out_Stats_File_Single,min_pix = min_pix,   # Call processing routine
                                     perc_diff = perc_diff,MedWdt = MedWdt,sub_zones = sub_zones,
                                     sig_level = sig_level ,erode =erode)
        
        # ----------------------------------------------------------------------------------#        
        # Extract plotting data for areas burnt multiple times                              #
        # ----------------------------------------------------------------------------------#  
        
        ExtTS_RData_File_Multiple =  paste(ExtTS_File_Multiple, 'Multiple_RData.RData', sep = '_')    								# Recreate name of RData TS Extraction output
        Out_Stats_Folder_Multiple = file.path(dirname(dirname(dirname(ExtTS_RData_File_Multiple))),'Stat_Analysis','Burned_Multiple',sep = '')
        Out_Stats_File_Multiple = file.path(Out_Stats_Folder_Multiple,gsub('TS_Extraction','Stat_Analysis',basename(ExtTS_RData_File_Multiple)))  # Set Basename for statistics file
        dir.create(Out_Stats_Folder_Multiple,recursive = T, showWarnings = F)
        
        print('----------------------------------------------------------')
        print('------ Extract plotting data on areas burned multiple times                                                        ----')
        print('----------------------------------------------------------')
        print('')
        print(paste('-> In File for Analysis: ',ExtTS_RData_File_Multiple))
        print(paste('-> Out File for Analysis: ',Out_Stats_File_Multiple))
        er = FRG_Comp_Plot_Stat_Multiple(In_File = ExtTS_RData_File_Multiple , Out_File = Out_Stats_File_Multiple,min_pix = min_pix, # Call processing routine
                                         sub_zones = sub_zones, erode = erode)
        
        print('-> Plotting data extraction and statistical analysis Completed')
        print('----------------------------------------------------------')
        
        #----------------------------------------------------------------------------------#        
        # Copy the main processing results csv files to the "summaries" folder and create the 
        # summary subsetted shapefiles  
        # ------------------------------------------------------------------------------ ----  
        
        print('----------------------------------------------------------')
        print('-> Create Final output summary tables and shapefiles')
        print('----------------------------------------------------------')
        # Copy the plot data and recovery statistics for areas burned once 
        
        load (Out_Stats_File_Single)
        out_csv_file_plot = file.path(Out_Stats_Folder_Single,paste("PLOT_DATA_",basename(TS_filename),'.csv', sep = ''))
        write.table(plot_stat, file = out_csv_file_plot , row.names = FALSE, sep = ";")  
        file.copy(from=out_csv_file_plot, to =  Results_Summary_Folder, recursive = FALSE, overwrite = TRUE)
        
        out_csv_file_recov = file.path(Out_Stats_Folder_Single,paste("RECOV_DATA_",basename(TS_filename),'.csv', sep = ''))
        write.table(recov_stat, file = out_csv_file_recov , row.names = FALSE, sep = ";")  
        file.copy(from=out_csv_file_recov, to =  Results_Summary_Folder, recursive = FALSE, overwrite = TRUE)
        
        # Remove from the "Single Fires" shapefile the polygons not processed (i.e., the ones below 10 core pixels, plus the
        # ones outside the study areas, plus the ones before 2003 and later than end-year -1)
        # Then save the subsetted shapefile to the "summaries" folder
        
        # Open the "single areas" shape
#         setCPLConfigOption("SHAPE_ENCODING", "UTF-8")
        BAreas_Name_Single = strsplit(basename(as.character(Shape_Files_Inter$Shape_File_Single)),'.shp')[[1]]
        BAreas_Dir_Single = dirname(as.character(Shape_Files_Inter$Shape_File_Single))
        BAreas_shp_Single = readOGR(BAreas_Dir_Single, BAreas_Name_Single)
#         setCPLConfigOption("SHAPE_ENCODING", NULL)

        Analyzed_OBIDs = levels(plot_stat$OBJECTID)          # Identify the objectids of the processed areas from the "plot_stat" data frame of single-fire areas
        subshape = BAreas_shp_Single[BAreas_shp_Single$OBJECTID %in% Analyzed_OBIDs,]     # Subset the original single areas shape on the basis of analyzed OBJECTIDs
        
        # Save the new subsetted shapefile

        # Recode placename and province to UTF8

#         subshape@data$Place_Name = as.character(subshape@data$Place_Name)
#         subshape@data$Place_Name = iconv(subshape@data$Place_Name,'UTF-8','windows-1252')
# # Encoding(subshape@data$Place_Name) = "UTF-8"
# # 
# #         subshape@data$Province = as.character(subshape@data$Province)
# #         Encoding(subshape@data$Province) = 'UTF-8'
# #         
        Out_Shape_Dir = file.path(Results_Summary_Folder,'Shapefiles')
        dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
        writeOGR(subshape,Out_Shape_Dir,paste(BAreas_Name_Single,'_Processed', sep = ''),
                  "ESRI Shapefile", overwrite_layer=TRUE)
        
        # Copy the plot data for areas burned multiple times to the summary folder
        
        load(Out_Stats_File_Multiple)
        out_csv_file_plot_multiple = file.path( Out_Stats_Folder_Multiple,paste("PLOT_DATA_MULTIPLE_",basename(TS_filename),'.csv', sep = ''))
        write.table(plot_stat, file = out_csv_file_plot_multiple , row.names = FALSE, sep = ";")  
        file.copy(from=out_csv_file_plot_multiple, to =  Results_Summary_Folder, recursive = FALSE, overwrite = TRUE)
        
        # Remove from the "Multiple Fires" shapefile the polygons not processed (i.e., the ones below 10 core pixels, plus the
        # ones outside the study areas, plus the ones before 2003 and later than end-year -1) and save them in the "Summary Results" folder
        
        # Open the "single areas shape
        BAreas_Name_Multiple = strsplit(basename(as.character(Shape_Files_Inter$Shape_File_Multiple)),'.shp')[[1]]
        BAreas_Dir_Multiple = dirname(as.character(Shape_Files_Inter$Shape_File_Multiple))
        BAreas_shp_Multiple = readOGR(BAreas_Dir_Multiple, BAreas_Name_Multiple)
        
        Analyzed_OVERLAP_FIDs = levels(plot_stat$OVERLAP_ID)          # Identify the overlapIDs of the processed areas from the "plot_stat" data frame of multiple-fires
        subshape = BAreas_shp_Multiple[BAreas_shp_Multiple$OVERLAP_ID %in% Analyzed_OVERLAP_FIDs,]     # Subset the original single areas shape on the basis of processed OBJECTIDs
        
        # Save the new subsetted shapefile
        Out_Shape_Dir = file.path(Results_Summary_Folder,'Shapefiles')
        dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
        writeOGR(subshape,Out_Shape_Dir, paste(BAreas_Name_Multiple,'_Processed', sep = ''),"ESRI Shapefile", overwrite_layer=TRUE)
        
        # Remove from the original burnt areas shapefile the polygons not processed (i.e., the ones below 10 core pixels, plus the
        # ones outside the study areas, plus the ones before 2003 and later than end-year -1) and save them in the "Summary Results" folder
        
        # Open the "original burnt areas shape
        BAreas_Name_Orig = strsplit(basename(Shape_File),'.shp')[[1]]
        BAreas_Dir_Orig = dirname(Shape_File)
        BAreas_shp_Orig = readOGR(BAreas_Dir_Orig, BAreas_Name_Orig)
        
        Data_LUT = read.csv2(file=as.character(Shape_Files_Inter$LUT_File_Multiple), stringsAsFactors = FALSE, header = TRUE, sep = ';')   # Restore the LUT
        Analyzed_OVERLAP_OBIDs = unique(droplevels(subset(Data_LUT,OVERLAP_ID %in% Analyzed_OVERLAP_FIDs))$OBJECTID) # Find which "original" fires are included in at least one overlap area
        Analyzed_OBID_full = unique((c(as.character(Analyzed_OBIDs),as.character(Analyzed_OVERLAP_OBIDs)))) # Create a "full" array containing the OBJECTIDs of the analyze 
                                                                                                            # "single fire" areas and of the analyzed "multiple fires" areas and remove duplicates   
        subshape = BAreas_shp_Orig[BAreas_shp_Orig$OBJECTID %in% Analyzed_OBID_full,]     # Subset the original single areas shape on the basis of analyzed OBJECTIDs
        
        # Save the new subsetted shapefile
        Out_Shape_Dir = file.path(Results_Summary_Folder,'Shapefiles')
        dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
        writeOGR(subshape,Out_Shape_Dir,paste(BAreas_Name_Orig,'_Full_Processed', sep = ''),"ESRI Shapefile", overwrite_layer=TRUE)
        
        # Copy the Overlap BA LUT to the summary folder
        out_file_LUT = file.path(as.character(Shape_Files_Inter$LUT_File_Multiple))
        file.copy(from=out_file_LUT, to =  Results_Summary_Folder, recursive = FALSE, overwrite = TRUE)
        
        # ------------------------------------------------------------------------------ ----       
      } # End if on Sig_anal
      
      # Write output lines for the considered index in the processing summary txt file
      cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
      cat(c("--- Output Folders and Files ---"), file =OutFile_Conn,sep="\n",append=TRUE)
      cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
      cat(paste("--- Main MODIS Output Folder: ",MOD_Dir), file =OutFile_Conn,sep="\n",append=TRUE)             
      cat(paste("--- Main Results Output Folder ",Out_Folder), file =OutFile_Conn,sep="\n",append=TRUE)     
      cat(paste("--- Output ShapeFile of Burnt Areas burnt once: ",Shape_Files_Inter$Shape_File_Single), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output ShapeFile of Burnt Areas burnt multiple times: ",Shape_Files_Inter$Shape_File_Multiple), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output Folder for results summaries: ",Results_Summary_Folder), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output File for Time series of areas burnt once: ",basename(out_csv_file_plot)), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output File for Recovery Stats of areas burnt once: ",basename(out_csv_file_recov)), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output File for Time series of areas burnt multiple times: ",basename(out_csv_file_plot_multiple)), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output ShapeFile of Processed single-fire Burnt Areas: ",paste(BAreas_Name_Single,'_Processed', sep = '')), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output ShapeFile of Processed multiple-fire Burnt Areas: ",paste(BAreas_Name_Multiple,'_Processed', sep = '')), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(paste("--- Output ShapeFile of Processed full Burnt Areas: ",paste(BAreas_Name_Orig,'_Processed', sep = '')), file =OutFile_Conn,sep="\n",append=TRUE)  
      cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
      
    } # End cycle on index
    
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- ALL PROCESSING COMPLETE ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    cat(c("--- -------------------------------------------------- ---"), file =OutFile_Conn,sep="\n",append=TRUE)
    
    print('----------------------------------------------------------')
    print('------------ ALL PROCESSING COMPLETE ! -------------------')
    print('----------------------------------------------------------')
    
    # Check if all output files were created in the "Summaries for EFFIS" folder
    
    out_files_list = c(file.path(Results_Summary_Folder, basename(out_csv_file_plot)),      # List of the foreseen output files
                       file.path(Results_Summary_Folder, basename(out_csv_file_recov)),		
                       file.path(Results_Summary_Folder, basename(out_csv_file_plot_multiple)),		
                       file.path (Out_Shape_Dir, paste(BAreas_Name_Single,'_Processed.shp'  , sep = '')),
                       file.path (Out_Shape_Dir, paste(BAreas_Name_Multiple,'_Processed.shp', sep = '')),
                       file.path (Out_Shape_Dir, paste(BAreas_Name_Orig,'_Full_Processed','.shp', sep = '')),
                       file.path(Results_Summary_Folder, basename(out_file_LUT)))
    
    # Check if all output files exist and were created on the same date and hour
    err = 'OK'
    Dates = NULL     ; hours = NULL
    for(file_out in out_files_list) {
      check = file.exists(file_out)
      Dates = c(Dates,(as.Date(file.info(file_out)$mtime)))
      hours = c(hours,(as.POSIXlt(file.info(file_out)$mt)$hour))
      if (check == FALSE) {err ='File_Error' ; break} 
    }
    if (err != 'File_Error') {
      if ((length(unique(Dates)) != 1) | ((max(hours) - min(hours)) > 1)) { err = 'Date_Error'}
    }
    
    #Send final output messages
    
    if (err == 'OK') {mess = gmessage(paste('Processing Completed Successfully !\n 
                      All Outputs were created successfully',sep = ''), title = 'Message', icon = 'info')}    # Completion message
    
    if (err == 'File_Error') {mess = gmessage(paste('Some of the outputs files are missing !\n 
                    Please Check the processing chain !',sep = ''), title = 'Message', icon = 'info')}    # Completion message
    
    if (err == 'Date_Error') {mess = gmessage(paste('Output files seem to have been created on different dates/hours !\n 
                    Please Check the processing chain !',sep = ''), title = 'Message', icon = 'info')}    # Completion message
    
  } else {gmessage("Output Folder already exists. Please Correct !", title="Warning", icon = 'warning')}                             # End if on existing out folder
  
} # END


#           if (Method ==1 | Method == 3) { 
#             
#             # ----------------------------------------------------------------------------------#
#             #    TS Extraction
#             # ----------------------------------------------------------------------------------#
#             TS_filename = paste('S',Index,'_',Start_Year,'_',End_Year,'_META', sep = '')
#             out_dir= file.path(Out_Stat_Dir,paste('S',Index, sep = ''),'TS_Extraction')
#             dir.create(out_dir,recursive = T, showWarnings = F)
#             Out_File = file.path(out_dir,paste('TS_Extraction_',TS_filename, sep = ''))
#             
#             TS_filename = file.path(Out_Dir = file.path(Scaled_Folder, paste('S',Index, sep = ''),TS_filename),fsep = '/')
#             print(TS_filename)
#             print(Out_File)
#             # OKKIO ! Da rimodificare !!!!### 
#             Shape_File = 'Z:/WORKING/Fire_Regeneration/Data/Burned_Areas/Burned_Areas_Single_Fire_00_12.shp'
#             erode_file = 'E:/Projects_Data/Fire_Regeneration_Data/Temp_Files/ROIS_Dilate/Burned_Areas_Single_Fire_00_12_ENVI_Mask_Erode_3'
#             
#             
#             # OKKIO ! Da rimodificare !!!!###  
#             mess = gwindow(title =paste('Extraction of sVI time series for burnt areas'), container = TRUE, width = 400, height = 40)
#             addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})  
#             
#             er = FRG_Extr_Stats(TS_filename, Shape_File,CLC_File_00,ENV_Zones_File, Out_File,erode, erode_file, mess )			# Call the processing routine
#             if (er != 'DONE') {return}
#             #        dispose(mess)
#             
#             
#             # ----------------------------------------------------------------------------------#
#             #    Statistical Analysis
#             # ----------------------------------------------------------------------------------#
#             RData_File =  paste(Out_File, 'RData.RData', sep = '_')																		#RData Matrix
#             out_dir= file.path(Out_Stat_Dir,paste('S',Index, sep = ''),'Stat_Analysis')
#             dir.create(out_dir,recursive = T, showWarnings = F)
#             Out_File = file.path(out_dir, paste('Stat_Analysis_',basename(TS_filename), sep = ''))
#             
#             print(paste('In File for TS extraction: ',RData_File))
#             print(paste('Out File for TS extraction: ',Out_File))
#             #        mess = gwindow(title =paste('Statistical Analysis of burnt areas Time Series'), container = TRUE, width = 400, height = 40)
#             #        
#             #        #  			OKKIO ! Da modificare usando min_pix invece di 20 !!!!###
#             #        
#             #        addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})	
#             #        er = try(FRG_Comp_Regr_Matrix(RData_File, Out_File,min_pix, perc,MedWdt, mess))			# Call the processing routine
#             #        
#             #        if (check_err_Regr(er = er, mess = mess,Main_GUI) == 'DONE') {  																	# Check for errors. In case, call error handling function
#             #          addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
#             #          dispose(mess)																											# Analysis finished - dispose message lab
#             #          addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
#             #          enabled(Main_GUI) = TRUE								#Re-enable the Main GUI														
#             #        }
#             #        if (er != 'DONE') {return}
#             
#           }   #Tutto eliminabile ! Eliminare anche possibilita' di selezione del metodo !!!!


# Space for additional analysis on significance analysis results. Empty for now !!! ----

#       if (Perc_Anal == T) {
#         
#         for (erode in c(0,1)) {
#           
#           print('----------------------------------------------------------')
#           print('--------- Percentages computation and plotting -----------')
#           print('----------------------------------------------------------')
#           if (erode == 0 ){print(paste('-> In File for Percentages: ',Out_Stats_File_Normal))}
#           if (erode == 1 ){print(paste('-> In File for Percentages: ',Out_Stats_File_Eroded))}
#           
#           for (use_subset in c(0,1)) {
#             
#             res = FRG_Compute_Percentages_Matrix_CatArea(Out_Stats_File_Normal,Out_Stats_File_Eroded, NKer, sig_level, use_subset, max_N_Years_after,perc_diff,End_Year, sub_zones,erode,min_pix)
#                  
#             print('----------------------------------------------------------')
#             print('-> Percentages computation and plotting Completed')
#             print('----------------------------------------------------------')
#             print('')
#           }
#         }
#         
#       }
# ----     
# ou = FRG_Full_Processing()