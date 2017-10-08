#' frg_fullprocessing
#' @description Function used to apply all FRG processing steps
#'
#' @param mod_dir     string Folder where the original and preprocessed image will be stored (i.e., the one above the 'Originals' folder)
#' @param Shape_File  string Input Shapefile of BAs
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param Out_Dir  string Main Results Dir where results of the analysis will be saved
#' @param Start_Year  numeric Starting year of the analysis
#' @param End_Year    numeric Ending Year of the analysis
#' @param Method      numeric. 2 = Percentage Median difference computation
#' @param SRDVI       numeric. 1 = compute SRDVI; 2 =  Don't Compute SRDVI
#' @param SNDVI       numeric. 1 =  compute SNDVI; 2 = Don't Compute SNDVI
#' @param ReProc      numeric if = 1, already existing Scaled Indexes will be recomputed
#' @param ReDown      numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and 
#'                    already existing mosaics will be overwritten 
#' @param ReProcIm    numeric if = 1, already existing MODIS mosaics will be reprocessed
#' @param erode       flag if = 1, analysis is conducted only on core pixels (defaults to 1)
#' @param min_pix     numeric minimum number of core pixels needed for a BA to be processed (default to 10)
#' @param NKer        numeric width (in Km) of the moving window used for computation of scaled indexes
#' @param sig_level   numeric Significance level for Wilcoxon test. Default to 0.05
#' @param sub_zones   Obsolete
#' @param MedWdt 
#' @param perc_diffs  numeric hash table Reduction Threshold on NDVIR reduction used in significance reduction analysis 
#'                    with wilcoxon test
#' @import     dplyr
#' @importFrom hash hash
#' @importFrom tcltk tk_messageBox
#' @importFrom tools file_path_sans_ext
#' @importFrom utils read.csv2 write.table
#' @import     gWidgetsRGtk2
#' @importFrom rgdal setCPLConfigOption
#' @export

frg_fullprocessing <- function(mod_dir     ,
                               Shape_File  ,
                               CLC_File_00 ,
                               Out_Dir     , 
                               Start_Year  ,
                               End_Year    ,
                               Method      ,
                               SNDVI    = 1, 
                               ReProc      ,
                               ReDown      ,
                               ReProcIm    ,
                               erode    = 1, 
                               min_pix = 10,
                               MedWdt = 3  ,
                               NKer        ,
                               sig_level = 0.05, 
                               sub_zones= 0,
                               perc_diff  , 
                               # Flags to skip some processing steps in debugging phase - Set all to T
                               # for complete processing - proceed with caution !
                               MOD_dwl   = FALSE,
                               Comp_SVI  = TRUE,
                               Extr_Stat = TRUE,
                               Sig_Anal  = TRUE, 
                               force_update) {
  
  #- Initialize Processing ---- 
  #: Set processing Dirs on the basis of user's choice and set some processing parameters 
  
  selection <- "yes"  # Check if Main Results Dir already exist and ask if overwrite
  
  if (file.exists(Out_Dir) & selection == "No") {
    selection <- tk_messageBox(
      caption = "Overwrite Warning", type = c("yesno"), 
      message = "A results Dir for the same analysis already exists ! 
      All results will be overwritten !\n Do you want to continue ? ", 
      default = "no")
  }
  
  if (selection == "yes") {
    
    # Runf helper to create file names and folder
    ff <- frg_def_files(Out_Dir, Start_Year, End_Year, mod_dir, Shape_File)
    # Write first lines of processing summary text file -----
    
    environment(frg_startlog) <- environment()
    frg_startlog()
    
    #- Step 1: Download and preprocessing MODIS data ---------
    #- Substituted with calls to MODIStsp package in v1.0
    browser()
    if (MOD_dwl == TRUE) {
      
      er <- frg_modproc(ff$OutOrig_Path, 
                        Start_Year, 
                        End_Year, 
                        ReProcIm, 
                        ReDown, 
                        force_update = force_update)
      
    }  # End if on  MODIS processing
    
    # Step 2: Compute scaled indexes (Percentage difference with respect -----
    # to median of surrounding non-burned pixels  
    
    if (Comp_SVI == T) {
      
      er <- frg_compSVI(mod_dir     = ff$mod_dir,  Shape_File    = Shape_File, 
                        CLC_File_00 = CLC_File_00, Scaled_Folder = ff$Scaled_Dir, 
                        Start_Year  = Start_Year,  End_Year      = End_Year, 
                        NKer = NKer , 
                        SRDVI = 0, SNDVI = SNDVI, 
                        nodata_out       = FRG_Options$No_Data_Out_Rast,
                        roi_file         = ff$roi_file,
                        FireMask_File    = ff$FireMask_File, 
                        FireMask_File_Er = ff$FireMask_File_Er, 
                        force_update = force_update)
      
      message(" -> Computation of Scaled Indexes Completed")
      message("----------------------------------------------------------")
      if (er != "DONE") {
        stop("An Error occurred while computing Scaled Indexes !")
      }
    }  # End if on 'Comp NDVIR
    
    # Step 3: Call routines for Time Series Extraction ----    
    if (Extr_Stat == TRUE) {
      message("----------------------------------------------------------")
      message("------------------ Statistical Analysis ------------------")
      message("----------------------------------------------------------")
      
      message(paste("-> Statistical Results Main Dir: ", ff$Out_Stat_Dir))
      message("----------------------------------------------------------")
      
      # Process the Burnt area shapefile to create the shapefile of areas ----
      # burnt once of areas burnt multiple times 
      
      Shape_File_Single = file.path(ff$Intermed_Dir, "Shapefiles/",
                                    paste0(tools::file_path_sans_ext(Shape_File), 
                                           "_Single_Fires.shp"))
      Shape_File_Multiple = file.path(ff$Intermed_Dir, "Shapefiles/",
                                      paste0(tools::file_path_sans_ext(Shape_File), 
                                             "_Multiple_Fires.shp"))
      LUT_File_Multiple = file.path(ff$Intermed_Dir, "Shapefiles/",
                                    paste0(tools::file_path_sans_ext(Shape_File), 
                                           "_Intersect_LUT_csv.csv"))
      
      if (!((file_exists(Shape_File_Single) & 
             file_exists(Shape_File_Multiple) & 
             file_exists(LUT_File_Multiple)) | 
            force_update)) {
        
        Shape_Files_Inter = FRG_Process_Shapefile(Shape_File   = Shape_File,
                                                  Intermed_Dir = ff$Intermed_Dir,
                                                  CLC_File_00  = CLC_File_00)
      } else {
        Shape_Files_Inter <- data.frame(Shape_File_Single = Shape_File_Single, 
                                        Shape_File_Multiple = Shape_File_Multiple, 
                                        LUT_File_Multiple = LUT_File_Multiple)
      }
      
      # Set Output files for Time Series Extraction on single and multiple ----
      # burnt areas
      
      # Perform TS extraction on the shapefile of areas burned once ----
      er <- frg_extract_svi(SVI_File   = ff$TS_filename, 
                            Shape_File = as.character(Shape_Files_Inter$Shape_File_Single), 
                            CLC_File_00, ENV_Zones_File, 
                            Out_File   = ff$ExtTS_File_Single, 
                            erode      = 1, ff$FireMask_File_Er, 
                            ff$Intermed_Dir, 
                            Overlap    = "Single", 
                            Shape_File_Orig = Shape_File, 
                            LUT_File_Multiple = "")
      
      # Perform TS extraction on the shapefile of areas burned more than once ----
      
      er <- frg_extract_svi(SVI_File   = ff$TS_filename, 
                            Shape_File = as.character(Shape_Files_Inter$Shape_File_Multiple), 
                            CLC_File_00, ENV_Zones_File, 
                            Out_File   = ff$ExtTS_File_Multiple, 
                            erode      = 1, ff$FireMask_File_Er, 
                            ff$Intermed_Dir, 
                            Overlap = "Multiple", 
                            Shape_File_Orig = Shape_File, 
                            LUT_File_Multiple = as.character(Shape_Files_Inter$LUT_File_Multiple))
      if (er == "DONE") {
        message("--- TS Extraction Completed ---")
        message("----------------------------------------------------------")
      } else {
        stop("An Error Occurred while extracting the Time Series")
      }
      
    } # End If on Extr_Stat
    
    # Call routines for extraction of plotting data and for Significance ---- 
    # Analysis (The latter, only on the areas burnt once !) 
    
    if (Sig_Anal == T) {
      # ----------------------------------------------------------------------------------#
      # Extract plotting data for areas burnt once, and perform significance
      # analysis # - for areas burnt once #
      # ----------------------------------------------------------------------------------#
      
      # Call processing routine
      er <- frg_sigmatrix(In_File  = paste(ff$ExtTS_File_Single, "RData.RData", sep = "_"),
                          Out_File = ff$Stats_File_Single,
                          min_pix,
                          perc_diff,
                          MedWdt,
                          sub_zones,
                          sig_level,
                          erode)
      
      
      # Extract plotting data for areas burnt multiple times ----
      
      er <- frg_plotstat_multiple(In_File   = paste(ff$ExtTS_File_Multiple, "RData.RData", sep = "_"),
                                  Out_File  = ff$Stats_File_Multiple,
                                  min_pix,
                                  sub_zones,
                                  erode)
      
    }  # End if on Sig_anal
    
    # Copy the main processing results csv files to the 'summaries' Dir -----
    # and create the summary subsetted shapefiles
    
    # Shape_Files_Inter <- data.frame(Shape_File_Single = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Single_Fires.shp", 
    #                                 Shape_File_Multiple = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp", 
    #                                 LUT_File_Multiple = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Intersect_LUT_csv.csv")
    # 
    # TODO : Extract this to a function !!!!
    message("----------------------------------------------------------")
    message("-> Create Final output summary tables and shapefiles  ----")
    message("----------------------------------------------------------")
    # Copy the plot data and recovery statistics for areas burned once
    
    load(ff$Stats_File_Single)
    out_csv_file_plot <- file.path(dirname(ff$Stats_File_Single), 
                                   paste("PLOT_DATA_", basename(ff$TS_filename), ".csv", 
                                         sep = ""))
    write.table(plot_stat, file = out_csv_file_plot, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot, to = ff$Summary_Dir, recursive = FALSE, overwrite = TRUE)
    
    out_csv_file_recov <- file.path(dirname(ff$Stats_File_Single), 
                                    paste0("RECOV_DATA_", basename(ff$TS_filename), ".csv"))
    write.table(recov_stat, file = out_csv_file_recov, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_recov, to = ff$Summary_Dir, recursive = FALSE, overwrite = TRUE)
    
    # Remove from the 'Single Fires' shapefile the polygons not processed ----
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) Then
    # save the subsetted shapefile to the 'summaries' Dir
    
    # Open the 'single areas' shape 
    setCPLConfigOption('SHAPE_ENCODING', 'UTF-8')
    BAreas_shp_Single <- as(st_read(as.character(Shape_Files_Inter$Shape_File_Single[[1]])), "Spatial")
    # setCPLConfigOption('SHAPE_ENCODING', NULL)
    
    Analyzed_OBIDs <- levels(plot_stat$OBJECTID)  # Identify the objectids of the processed areas from the 'plot_stat' data frame of single-fire areas
    subshape <- BAreas_shp_Single[BAreas_shp_Single$OBJECTID %in% 
                                    Analyzed_OBIDs, ]  # Subset the original single areas shape on the basis of analyzed OBJECTIDs
    
    # Save the new subsetted shapefile
    
    Out_Shape_Dir <- file.path(ff$Summary_Dir, "Shapefiles")
    dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
    BAreas_Name_Single <- basename(file_path_sans_ext(as.character(Shape_Files_Inter$Shape_File_Single[[1]])))
    writeOGR(subshape, Out_Shape_Dir, paste(BAreas_Name_Single, "_Processed", sep = ""), 
             "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the plot data for areas burned multiple times to the summary
    # Dir
    
    load(ff$Stats_File_Multiple)
    out_csv_file_plot_multiple <- file.path(dirname(ff$Stats_File_Multiple), 
                                            paste("PLOT_DATA_MULTIPLE_", basename(ff$TS_filename), 
                                                  ".csv", sep = ""))
    write.table(plot_stat, file = out_csv_file_plot_multiple, 
                row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot_multiple, to = ff$Summary_Dir, 
              recursive = FALSE, overwrite = TRUE)
    
    # Remove from the 'Multiple Fires' shapefile the polygons not processed
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) and save
    # them in the 'Summary Results' Dir
    
    # Open the 'single areas shape
    BAreas_Name_Multiple <- strsplit(basename(as.character(Shape_Files_Inter$Shape_File_Multiple)), 
                                     ".shp")[[1]]
    BAreas_Dir_Multiple <- dirname(as.character(Shape_Files_Inter$Shape_File_Multiple))
    BAreas_shp_Multiple <- readOGR(BAreas_Dir_Multiple, BAreas_Name_Multiple)
    
    Analyzed_OVERLAP_FIDs <- levels(plot_stat$OVERLAP_ID)  # Identify the overlapIDs of the processed areas from the 'plot_stat' data frame of multiple-fires
    subshape            <- BAreas_shp_Multiple[BAreas_shp_Multiple$OVERLAP_ID %in% 
                                                 Analyzed_OVERLAP_FIDs, ]  # Subset the original single areas shape on the basis of processed OBJECTIDs
    
    # Save the new subsetted shapefile
    Out_Shape_Dir <- file.path(ff$Summary_Dir, "Shapefiles")
    dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
    writeOGR(subshape, Out_Shape_Dir, paste(BAreas_Name_Multiple, 
                                            "_Processed", sep = ""), "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Remove from the original burnt areas shapefile the polygons not
    # processed (i.e., the ones below 10 core pixels, plus the ones outside
    # the study areas, plus the ones before 2003 and later than end-year
    # -1) and save them in the 'Summary Results' Dir
    
    # Open the 'original burnt areas shape
    BAreas_Name_Orig <- strsplit(basename(Shape_File), ".shp")[[1]]
    BAreas_Dir_Orig  <- dirname(Shape_File)
    BAreas_shp_Orig  <- readOGR(BAreas_Dir_Orig, BAreas_Name_Orig)
    
    Data_LUT <- read.csv2(file = as.character(Shape_Files_Inter$LUT_File_Multiple), 
                          stringsAsFactors = FALSE, header = TRUE, sep = ";")  # Restore the LUT
    Analyzed_OVERLAP_OBIDs <- unique(droplevels(subset(Data_LUT, 
                                                       OVERLAP_ID %in% Analyzed_OVERLAP_FIDs))$OBJECTID)  # Find which 'original' fires are included in at least one overlap area
    Analyzed_OBID_full <- unique((c(as.character(Analyzed_OBIDs), 
                                    as.character(Analyzed_OVERLAP_OBIDs))))  # Create a 'full' array containing the OBJECTIDs of the analyze 
    # 'single fire' areas and of the analyzed 'multiple fires' areas and
    # remove duplicates
    subshape <- BAreas_shp_Orig[BAreas_shp_Orig$OBJECTID %in% 
                                  Analyzed_OBID_full, ]  # Subset the original single areas shape on the basis of analyzed OBJECTIDs
    
    # Save the new subsetted shapefile
    Out_Shape_Dir <- file.path(ff$Summary_Dir, "Shapefiles")
    dir.create(Out_Shape_Dir, recursive = T, showWarnings = F)
    writeOGR(subshape, Out_Shape_Dir, paste(BAreas_Name_Orig, 
                                            "_Full_Processed", sep = ""), "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the Overlap BA LUT to the summary Dir
    out_file_LUT <- file.path(as.character(Shape_Files_Inter$LUT_File_Multiple))
    file.copy(from = out_file_LUT, to = ff$Summary_Dir, 
              recursive = FALSE, overwrite = TRUE)
    
    # Write output lines for the considered index in the processing summary
    #   # txt file
    #   cat(c("--- -------------------------------------------------- ---"), 
    #       file = OutFile_Conn, sep = "\n", append = TRUE)
    #   cat(c("--- Output Folders and Files ---"), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(c("--- -------------------------------------------------- ---"), 
    #       file = OutFile_Conn, sep = "\n", append = TRUE)
    #   cat(paste("--- Main MODIS Output Folder: ", mod_dir), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Main Results Output Folder ", Out_Folder), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output ShapeFile of Burnt Areas burnt once: ", 
    #             Shape_Files_Inter$Shape_File_Single), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output ShapeFile of Burnt Areas burnt multiple times: ", 
    #             Shape_Files_Inter$Shape_File_Multiple), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output Folder for results summaries: ", Results_Summary_Folder), 
    #       file = OutFile_Conn, sep = "\n", append = TRUE)
    #   cat(paste("--- Output File for Time series of areas burnt once: ", 
    #             basename(out_csv_file_plot)), file = OutFile_Conn, sep = "\n", 
    #       append = TRUE)
    #   cat(paste("--- Output File for Recovery Stats of areas burnt once: ", 
    #             basename(out_csv_file_recov)), file = OutFile_Conn, sep = "\n", 
    #       append = TRUE)
    #   cat(paste("--- Output File for Time series of areas burnt multiple times: ", 
    #             basename(out_csv_file_plot_multiple)), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output ShapeFile of Processed single-fire Burnt Areas: ", 
    #             paste(BAreas_Name_Single, "_Processed", sep = "")), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output ShapeFile of Processed multiple-fire Burnt Areas: ", 
    #             paste(BAreas_Name_Multiple, "_Processed", sep = "")), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(paste("--- Output ShapeFile of Processed full Burnt Areas: ", 
    #             paste(BAreas_Name_Orig, "_Processed", sep = "")), file = OutFile_Conn, 
    #       sep = "\n", append = TRUE)
    #   cat(c("--- -------------------------------------------------- ---"), 
    #       file = OutFile_Conn, sep = "\n", append = TRUE)
    #   
    # }  # End cycle on index
    # 
    # cat(c("--- -------------------------------------------------- ---"), 
    #     file = OutFile_Conn, sep = "\n", append = TRUE)
    # cat(c("--- ALL PROCESSING COMPLETE ---"), file = OutFile_Conn, 
    #     sep = "\n", append = TRUE)
    # cat(c("--- -------------------------------------------------- ---"), 
    #     file = OutFile_Conn, sep = "\n", append = TRUE)
    # 
    message("----------------------------------------------------------")
    message("------------ ALL PROCESSING COMPLETE ! -------------------")
    message("----------------------------------------------------------")
    
  }
}