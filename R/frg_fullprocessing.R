#' frg_fullprocessing
#' @description Function used to apply all FRG processing steps
#'
#' @param mod_dir     string Folder where the original and preprocessed image will be stored (i.e., the one above the 'Originals' folder)
#' @param opts$shape_file  string Input Shapefile of BAs
#' @param opts$clc_file string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param opts$out_dir  string Main Results Dir where results of the analysis will be saved
#' @param Start_Year  numeric Starting year of the analysis
#' @param End_Year    numeric Ending Year of the analysis
#' @param Method      numeric. 2 = Percentage Median difference computation
#' @param SRDVI       numeric. 1 = compute SRDVI; 2 =  Don't Compute SRDVI
#' @param SNDVI       numeric. 1 =  compute SNDVI; 2 = Don't Compute SNDVI
#' @param ReProc      numeric if = 1, already existing Scaled Indexes will be recomputed
#' @param opts$redown      numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and 
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

frg_fullprocessing <- function(opts, 
                               force_update, 
                               # Flags to skip some processing steps in debugging phase - Set all to T
                               # for complete processing - proceed with caution !
                               MOD_dwl   = FALSE,
                               Comp_SVI  = TRUE,
                               Extr_Stat = TRUE,
                               Sig_Anal  = TRUE) {
  
  #- Initialize Processing ---- 
  #: Set processing Dirs on the basis of user's choice and set some processing parameters 
  
  selection <- "yes"  # Check if Main Results Dir already exist and ask if overwrite
  
  if (file.exists(opts$out_dir) & selection == "No") {
    selection <- tk_messageBox(
      caption = "Overwrite Warning", type = c("yesno"), 
      message = "A results Dir for the same analysis already exists ! 
      All results will be overwritten !\n Do you want to continue ? ", 
      default = "no")
  }
  
  if (selection == "yes") {
    
    # Runf helper to create file names and folder
    opts <- frg_def_files(opts)
    # Write first lines of processing summary text file -----
    # 
    # environment(frg_startlog) <- environment()
    # frg_startlog()
    
    #- Step 1: Download and preprocessing MODIS data ---------
    #- Substituted with calls to MODIStsp package in v1.0
    # browser()
    if (MOD_dwl == TRUE) {
      
      er <- frg_process_modis(opts, 
                              UI_check = TRUE,
                              max_UI = 5, 
                              force_update)
      
    }  # End if on  MODIS processing
    
    # Step 2: Compute scaled indexes (Percentage difference with respect -----
    # to median of surrounding non-burned pixels  
    
    if (Comp_SVI == T) {
      
      er <- frg_compSVI(opts, force_update)
      
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
      
      message(paste("-> Statistical Results Main Dir: ", opts$out_stat_dir))
      message("----------------------------------------------------------")
      
      # Process the Burnt area shapefile to create the shapefile of areas ----
      # burnt once of areas burnt multiple times 
      
      if (!(file.exists(opts$shapefile_single) & 
             file.exists(opts$shapefile_multiple) & 
             file.exists(opts$lut_file_multiple)) | 
            force_update) {
        browser()
         proc_shapes <- frg_process_shapefile(opts)
      }
      
      # Set Output files for Time Series Extraction on single and multiple ----
      # burnt areas
      
      # Perform TS extraction on the shapefile of areas burned once ----
      er <- frg_extract_svi(SVI_file   = opts$ts_filename, 
                            shape_file = opts$shapefile_single, 
                            out_file   = opts$stats_file_single, 
                            overlap    = "Single", 
                            shape_file_orig = opts$orig_shapefile, 
                            lut_file_multiple = "",
                            opts,
                            force_update)

      
      # Perform TS extraction on the shapefile of areas burned more than once ----
      
      er <- frg_extract_svi(SVI_file   = opts$ts_filename, 
                            shape_file = opts$shapefile_multiple, 
                            out_file   = opts$stats_file_multiple, 
                            overlap = "Multiple", 
                            shape_file_orig = opts$orig_shapefile, 
                            lut_file_multiple = opts$lut_file_multiple,
                            opts,
                            force_update)
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
      er <- frg_sigmatrix(in_file  = paste(opts$ts_file_single, "RData.RData", sep = "_"),
                          out_file = opts$stats_file_single,
                          opts)
      
      
      # Extract plotting data for areas burnt multiple times ----
      
      er <- frg_plotstat_multiple(in_file   = paste(opts$ts_file_multiple, "RData.RData", sep = "_"),
                                  out_file  = opts$stats_file_multiple,
                                  opts)
      
    }  # End if on Sig_anal
    
    # Copy the main processing results csv files to the 'summaries' Dir -----
    # and create the summary subsetted shapefiles
    
    # opts$shape_files_Inter <- data.frame(opts$shape_file_single = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Single_Fires.shp", 
    #                                 opts$shape_file_multiple = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp", 
    #                                 lut_file_multiple = "D:/Temp/tempfrg/Intermed_Proc/Shapefiles/Burned_Areas_00_15_Intersect_LUT_csv.csv")
    # 
    # TODO : Extract this to a function !!!!
    message("----------------------------------------------------------")
    message("-> Create Final output summary tables and shapefiles  ----")
    message("----------------------------------------------------------")
    # Copy the plot data and recovery statistics for areas burned once
    
    load(opts$stats_file_single)
    out_csv_file_plot <- file.path(dirname(opts$stats_file_single), 
                                   paste("PLOT_DATA_", basename(opts$ts_filename), ".csv", 
                                         sep = ""))
    write.table(plot_stat, file = out_csv_file_plot, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot, to = opts$summary_dir, recursive = FALSE, overwrite = TRUE)
    
    out_csv_file_recov <- file.path(dirname(opts$stats_file_single), 
                                    paste0("RECOV_DATA_", basename(opts$ts_filename), ".csv"))
    write.table(recov_stat, file = out_csv_file_recov, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_recov, to = opts$summary_dir, recursive = FALSE, overwrite = TRUE)
    
    # Remove from the 'Single Fires' shapefile the polygons not processed ----
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) Then
    # save the subsetted shapefile to the 'summaries' Dir
    
    
    browser()
    #TODO REPLACE PROCEsSING BASED ON sp with one based on sf
    # Open the 'single areas' shape 
    setCPLConfigOption('SHAPE_ENCODING', 'UTF-8')
    bareas_shp_single <- as(st_read(opts$shape_file_single), "Spatial")
    # setCPLConfigOption('SHAPE_ENCODING', NULL)
    
    Analyzed_OBIDs <- levels(plot_stat$OBJECTID)  # Identify the objectids of the processed areas from the 'plot_stat' data frame of single-fire areas
    subshape <- bareas_shp_single[bareas_shp_single$OBJECTID %in% 
                                    Analyzed_OBIDs, ]  # Subset the original single areas shape on the basis of analyzed OBJECTIDs
    
    # Save the new subsetted shapefile
    
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    BAreas_Name_Single <- basename(file_path_sans_ext(as.character(opts$shape_files_Inter$opts$shape_file_single[[1]])))
    writeOGR(subshape, out_shape_dir, paste(BAreas_Name_Single, "_Processed", sep = ""), 
             "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the plot data for areas burned multiple times to the summary
    # Dir
    
    load(opts$stats_file_multiple)
    out_csv_file_plot_multiple <- file.path(dirname(opts$stats_file_multiple), 
                                            paste("PLOT_DATA_MULTIPLE_", basename(opts$ts_filename), 
                                                  ".csv", sep = ""))
    write.table(plot_stat, file = out_csv_file_plot_multiple, 
                row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot_multiple, to = opts$summary_dir, 
              recursive = FALSE, overwrite = TRUE)
    
    # Remove from the 'Multiple Fires' shapefile the polygons not processed
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) and save
    # them in the 'Summary Results' Dir
    
    # Open the 'multipel areas shape
    bareas_shp_multiple <- as(sf::st_read(opts$shape_file_multiple), "Spatial")
    
    # Identify the overlapIDs of the processed areas from the 'plot_stat' data
    #frame of multiple-fires
    Analyzed_OVERLAP_FIDs <- levels(plot_stat$OVERLAP_ID)  
    # Subset the original single areas shape on the basis of processed OBJECTIDs
    subshape            <- bareas_shp_multiple[bareas_shp_multiple$OVERLAP_ID %in% 
                                                 Analyzed_OVERLAP_FIDs, ]
    
    # Save the new subsetted shapefile
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    writeOGR(subshape, out_shape_dir, paste(BAreas_Name_Multiple, 
                                            "_Processed", sep = ""), "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Remove from the original burnt areas shapefile the polygons not
    # processed (i.e., the ones below 10 core pixels, plus the ones outside
    # the study areas, plus the ones before 2003 and later than end-year
    # -1) and save them in the 'Summary Results' Dir
    
    # Open the 'original burnt areas shape
    
    bareas_shp_orig  <- sf::st_read(opts$orig_shape_file)
    
     # Restore the LUT
    Data_LUT <- read.csv2(file = as.character(opts$shape_files_Inter$lut_file_multiple), 
                          stringsAsFactors = FALSE, header = TRUE, sep = ";") 
    Analyzed_OVERLAP_OBIDs <- unique(droplevels(subset(Data_LUT, 
                                                       OVERLAP_ID %in% Analyzed_OVERLAP_FIDs))$OBJECTID)  # Find which 'original' fires are included in at least one overlap area
    Analyzed_OBID_full <- unique((c(as.character(Analyzed_OBIDs), 
                                    as.character(Analyzed_OVERLAP_OBIDs)))) 
    
    # Create a 'full' array containing the OBJECTIDs of the analyze 
    # 'single fire' areas and of the analyzed 'multiple fires' areas and
    # remove duplicates
    subshape <- bareas_shp_orig[bareas_shp_orig$OBJECTID %in% 
                                  Analyzed_OBID_full, ]
    
    # Save the new subsetted shapefile
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    writeOGR(subshape, out_shape_dir, paste(BAreas_Name_Orig, 
                                            "_Full_Processed", sep = ""), "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the Overlap BA LUT to the summary Dir
    out_file_LUT <- file.path(as.character(opts$shape_files_Inter$lut_file_multiple))
    file.copy(from = out_file_LUT, to = opts$summary_dir, 
              recursive = FALSE, overwrite = TRUE)
    
    frg_writelog(opts)
   
     message("----------------------------------------------------------")
    message("------------ ALL PROCESSING COMPLETE ! -------------------")
    message("----------------------------------------------------------")
    
  }
}