#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param opts PARAM_DESCRIPTION
#' @param force_update PARAM_DESCRIPTION
#' @param MOD_dwl PARAM_DESCRIPTION, Default: FALSE
#' @param Comp_SVI PARAM_DESCRIPTION, Default: TRUE
#' @param Extr_Stat PARAM_DESCRIPTION, Default: TRUE
#' @param Sig_Anal PARAM_DESCRIPTION, Default: TRUE
#' @rdname frg_fullprocessing
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tcltk tk_messageBox
#' @importFrom utils write.table read.csv2
#' @importFrom sf st_read
#' @importFrom tools file_path_sans_ext
#' @importFrom rgdal writeOGR setCPLConfigOption
#' @importFrom methods as

frg_fullprocessing <- function(opts, 
                               force_update, 
         # Flags to skip some processing steps in debugging phase - Set all to T
         # for complete processing - proceed with caution !
                               MOD_dwl   = TRUE,
                               Comp_SVI  = TRUE,
                               Extr_Stat = TRUE,
                               Sig_Anal  = TRUE) {
  
  plot_stat <- recov_stat <- OVERLAP_ID <- NULL
  #- Initialize Processing ---- 
  #: Set processing Dirs on the basis of user's choice and set some processing
  #  parameters 
  
  if (file.exists(opts$out_dir)) {
    selection <- tcltk::tk_messageBox(
      caption = "Overwrite Warning", type = c("yesno"), 
      message = "A results Dir for the same analysis already exists ! 
      All results will be overwritten !\n Do you want to continue ? ", 
      default = "no")
  } else {
    selection = "yes"
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
      message(" ")
      message("- ------------------------------------------------------ -")
      message("- Computation of Scaled Indexes Completed                -")
      message("- ------------------------------------------------------ -")
      message("")
      if (er != "DONE") {
        stop("An Error occurred while computing Scaled Indexes! Aborting!")
      }
    }  # End if on 'Comp NDVIR
    
    # Step 3: Call routines for Time Series Extraction ----    
    if (Extr_Stat == TRUE) {
      message("- ------------------------------------------------------ -")
      message("- Statistical Analysis of SVI time series                -")
      message("- ------------------------------------------------------ -")
      message("")
      
      # Process the Burnt area shapefile to create the shapefile of areas ----
      # burnt once of areas burnt multiple times 
      
      if (!(file.exists(opts$shapefile_single) & 
            file.exists(opts$shapefile_multiple) & 
            file.exists(opts$lut_file_multiple)) | 
          force_update) {
        # browser()
        proc_shapes <- frg_process_shapefile(opts)
      }
      
      # Set Output files for Time Series Extraction on single and multiple ----
      # burnt areas
      
      # Perform TS extraction on the shapefile of areas burned once ----
      er <- frg_extract_svi(SVI_file   = opts$ts_filename, 
                            shape_file = opts$shapefile_single, 
                            out_file   = opts$ts_file_single, 
                            overlap    = "Single", 
                            shape_file_orig = opts$orig_shapefile, 
                            lut_file_multiple = "",
                            opts,
                            force_update)
      
      
      # Perform TS extraction on the shapefile of areas burned more than once ----
      
      er <- frg_extract_svi(SVI_file   = opts$ts_filename, 
                            shape_file = opts$shapefile_multiple, 
                            out_file   = opts$ts_file_multiple, 
                            overlap = "Multiple", 
                            shape_file_orig = opts$orig_shapefile, 
                            lut_file_multiple = opts$lut_file_multiple,
                            opts,
                            force_update)
      if (er == "DONE") {
        message("")
        message("- ------------------------------------------------------ -")
        message("- TS Extraction Completed ")
        message("- ------------------------------------------------------ -")
        message("")
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
      
      
      # Just Extract plotting data for areas burnt multiple times ----
      
      er <- frg_plotstat_multiple(in_file   = paste(opts$ts_file_multiple, "RData.RData", sep = "_"),
                                  out_file  = opts$stats_file_multiple,
                                  opts)
      
    }  # End if on Sig_anal
    
    # Copy the main processing results csv files to the 'summaries' Dir -----
    # and create the summary subsetted shapefiles
    
    message("----------------------------------------------------------")
    message("-> Create Final output summary tables and shapefiles  ----")
    message("----------------------------------------------------------")
    # Copy the plot data and recovery statistics for areas burned once
    # browser()
    load(opts$stats_file_single)
    out_csv_file_plot <- file.path(dirname(opts$stats_file_single), 
                                   paste("PLOT_DATA_", basename(opts$ts_filename), ".csv", 
                                         sep = ""))
    utils::write.table(plot_stat, file = out_csv_file_plot, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot, to = opts$summary_dir, recursive = FALSE,
              overwrite = TRUE)
    
    out_csv_file_recov <- file.path(dirname(opts$stats_file_single), 
                                    paste0("RECOV_DATA_", basename(opts$ts_filename), ".csv"))
    
    utils::write.table(recov_stat, file = out_csv_file_recov, row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_recov, to = opts$summary_dir, recursive = FALSE,
              overwrite = TRUE)
    
    # Remove from the 'Single Fires' shapefile the polygons not processed ----
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) Then
    # save the subsetted shapefile to the 'summaries' Dir
    
    # Open the 'single areas' shape 
    rgdal::setCPLConfigOption('SHAPE_ENCODING', 'UTF-8')
    bareas_shp_single <- methods::as(sf::st_read(opts$shapefile_single, quiet = TRUE),
                            "Spatial")
    # setCPLConfigOption('SHAPE_ENCODING', NULL)
    
    # Identify the objectids of the processed areas from the 'plot_stat' data
    # frame of single-fire areas
    Analyzed_OBIDs <- levels(plot_stat$OBJECTID)  
    
    # Subset the original single areas shape on the basis of analyzed OBJECTIDs
    subshape <- bareas_shp_single[bareas_shp_single$OBJECTID %in% 
                                    Analyzed_OBIDs, ]
    
    # Save the new subsetted shapefile
    
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    bareas_name_single <- basename(tools::file_path_sans_ext(
      as.character(opts$shapefile_single[[1]])))
    rgdal::writeOGR(subshape, out_shape_dir, 
                    paste(bareas_name_single, "_Processed", sep = ""), 
                    "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the plot data for areas burned multiple times to the summary
    # Dir
    
    load(opts$stats_file_multiple)
    out_csv_file_plot_multiple <- file.path(
      dirname(opts$stats_file_multiple), 
      paste("PLOT_DATA_MULTIPLE_", basename(opts$ts_filename), 
            ".csv", sep = ""))
    utils::write.table(plot_stat, file = out_csv_file_plot_multiple, 
                row.names = FALSE, sep = ";")
    file.copy(from = out_csv_file_plot_multiple, to = opts$summary_dir, 
              recursive = FALSE, overwrite = TRUE)
    
    # Remove from the 'Multiple Fires' shapefile the polygons not processed
    # (i.e., the ones below 10 core pixels, plus the ones outside the study
    # areas, plus the ones before 2003 and later than end-year -1) and save
    # them in the 'Summary Results' Dir
    
    # Open the 'multipel areas shape
    bareas_shp_multiple <- methods::as(sf::st_read(opts$shapefile_multiple, quiet = TRUE), 
                              "Spatial")
    
    # Identify the overlapIDs of the processed areas from the 'plot_stat' data
    #frame of multiple-fires
    Analyzed_OVERLAP_FIDs <- levels(plot_stat$OVERLAP_ID)  
    # Subset the original single areas shape on the basis of processed OBJECTIDs
    subshape  <- bareas_shp_multiple[bareas_shp_multiple$OVERLAP_ID %in% 
                                       Analyzed_OVERLAP_FIDs, ]
    
    # Save the new subsetted shapefile
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    bareas_name_multiple <- basename(tools::file_path_sans_ext(
      as.character(opts$shapefile_multiple[[1]])))
    rgdal::writeOGR(subshape, out_shape_dir, paste(bareas_name_multiple, 
                                            "_Processed", sep = ""),
             "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Remove from the original burnt areas shapefile the polygons not
    # processed (i.e., the ones below 10 core pixels, plus the ones outside
    # the study areas, plus the ones before 2003 and later than end-year
    # -1) and save them in the 'Summary Results' Dir
    
    # Open the 'original burnt areas shape
    
    bareas_shp_orig  <-  methods::as(sf::st_read(opts$orig_shapefile, quiet = TRUE),
                            "Spatial")
    
    # Restore the LUT
    Data_LUT <- utils::read.csv2(file = as.character(opts$lut_file_multiple), 
                          stringsAsFactors = FALSE, header = TRUE, sep = ";") 
    # Find which 'original' fires are included in at least one overlap area
    Analyzed_OVERLAP_OBIDs <- unique(droplevels(
      subset(Data_LUT, 
             OVERLAP_ID %in% Analyzed_OVERLAP_FIDs))$OBJECTID)  
    Analyzed_OBID_full <- unique((c(as.character(Analyzed_OBIDs), 
                                    as.character(Analyzed_OVERLAP_OBIDs)))) 
    
    # Create a 'full' array containing the OBJECTIDs of the analyze 
    # 'single fire' areas and of the analyzed 'multiple fires' areas and
    # remove duplicates
    subshape <- bareas_shp_orig[bareas_shp_orig$OBJECTID %in% Analyzed_OBID_full, ]
    
    # Save the new subsetted shapefile
    out_shape_dir <- file.path(opts$summary_dir, "Shapefiles")
    dir.create(out_shape_dir, recursive = T, showWarnings = F)
    bareas_name_orig <- basename(tools::file_path_sans_ext(
      as.character(opts$orig_shapefile[[1]])))
    
    rgdal::writeOGR(subshape, out_shape_dir, paste(bareas_name_orig, 
                                            "_Full_Processed.shp", sep = ""), 
             "ESRI Shapefile", overwrite_layer = TRUE)
    
    # Copy the Overlap BA LUT to the summary Dir
    out_file_LUT <- file.path(as.character(opts$lut_file_multiple))
    file.copy(from = out_file_LUT, to = opts$summary_dir, 
              recursive = FALSE, overwrite = TRUE)
    
    # frg_writelog(opts)
    
    message("----------------------------------------------------------")
    message("------------ ALL PROCESSING COMPLETE ! -------------------")
    message("----------------------------------------------------------")
    
  }
}