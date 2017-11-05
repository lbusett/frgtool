#' @title frg_fullproc_gui
#' @description Function used to create the GUI for selecting the parameters for 
#' full FRG processing.
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param force_update `logical` If TRUE, intermediate processing outputs (e.g., 
#'   creation of burnt areas shapefile, computation of SVI, etc. are re-run 
#'   even if intermecdiate processing outputs are already present in the 
#'   specified output folders. Useful to make a "clean run" of the tool, 
#'   Default: FALSE) 
#' @details Main GUI of the tool - used only to set the processing options and 
#'  call the appropraite routines for updating results (FRG_Full_Processing.R)
#'  If the user chooses 'Start' and all processing parameters are OK, the selected
#'  parameters are saved in the 'FRG_Full_Previous.RData' file and processing is 
#'  started
#' @return NULL
#' @import gWidgetsRGtk2
#' @rdname frg_fullproc_gui
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' 
frg_fullproc_gui <- function(opts, 
                             force_update = FALSE) {
  
  if (file.exists(file.path(opts$prev_dir, "frg_previous.RData"))) {
    opts <- get(load(file.path(opts$prev_dir, "frg_previous.RData")))
  } else {
    opts$mod_dir        <- ""
    opts$out_dir     <- ""
    opts$start_year     <- 2001 
    opts$end_year       <- 2016 
    opts$redown         <- 1 
    opts$reproc         <- 1 
    opts$reproc_images  <- 1 
    opts$orig_shapefile     <- "" 
    opts$clc_file       <- "" 
    opts$nker           <- 200 
    opts$method         <- 2 
    opts$SDVI           <- 1 
    opts$SNDVI          <- 1 
  }
  
  # Build Main Widgets ----------------------------------------------------- ###
  
  Main_W       <- gbasicdialog("Fire Regeneration Monitoring Tool", horizontal = FALSE, 
                               do.buttons = FALSE, spacing = 10)
  Files_Group  <- gframe(text = "Select the Processing Files - Folders", 
                         horizontal = FALSE, container = Main_W)
  Fake_Group   <- glabel(text = "", container = Main_W)
  Proc_Group   <- gframe(text = "Select the Processing Options", horizontal = FALSE, 
                         container = Main_W)
  Fake_Group2  <- glabel(text = "", container = Main_W)
  reproc_Group <- gframe(text = "Select the reprocessing Options", horizontal = FALSE, 
                         container = Main_W)
  Fake_Group3  <- glabel(text = "", container = Main_W)
  
  # Build Widgets for opts of Input ShapeFile (Used to mask out ####
  # burned areas when computing statistics !)
  
  Shape_SelGroup   <- ggroup(horizontal = TRUE, container = Files_Group)
  Shape_Lab        <- glabel(text = "Input Burnt Areas Shape File", container = Shape_SelGroup)
  size(Shape_Lab)  <- c(280, 8)
  shape_file       <- gedit(text = format(opts$orig_shapefile, justify = "right"), 
                            container = Shape_SelGroup, anchor = c(1, 1))
  size(shape_file) <- c(600, 20)
  Shape_Choose     <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "open", 
                    text = "Select the Input Burnt Areas Shape file...", 
                    filter = list(Shapfiles = list(patterns = c("*.shp"))))
    if (!is.na(choice)) {
      svalue(shape_file) <- choice  ## Set value of the label widget
      opts$orig_shapefile <- choice  # Set value of the selected variable\t
    }
  }, container = Shape_SelGroup)
  
  # Build Widgets for opts of the folder where to place original  ####
  # and pre-processed MODIS satellite data
  
  # Main group
  MOD_SelGroup  <- ggroup(horizontal = TRUE, container = Files_Group)  
  # Label
  Out_Lab       <- glabel(text = "Main Folder for download of MODIS data", 
                          container = MOD_SelGroup, editable = FALSE)  
  size(Out_Lab) <- c(280, 8)  
  # Selected file 
  mod_dir       <- gedit(text = format(opts$mod_dir, justify = "right"), 
                         container = MOD_SelGroup)
  size(mod_dir) <- c(600, 20)
  MOD_Choose    <- gbutton("Browse", handler = function(h, ...) {
    # File selection widget
    choice <- gfile(type = "selectdir", 
                    text = "Select the Main Folder for MODIS data...")  
    if (!is.na(choice)) {
      ## On new selection, set value of the label widget
      svalue(mod_dir) <- choice  
      # On new selection,  Set value of the selected variable\t
      opts$mod_dir <- format(choice, justify = "right")  
    }
  }, container = MOD_SelGroup)
  addSpace(MOD_SelGroup, 20)
  
  # Build Widgets for opts of CORINE 2000 image ---------------------
  
  CLC_SelGroup_00   <- ggroup(horizontal = TRUE, container = Files_Group)
  CLC_Lab_00        <- glabel(text = "Input Corine Land Cover File (2000)", 
                              container = CLC_SelGroup_00)
  size(CLC_Lab_00)  <- c(280, 8)  # Set label width
  clc_file       <- gedit(text = format(opts$clc_file, justify = "right"), 
                             container = CLC_SelGroup_00, anchor = c(1, 1))
  size(clc_file) <- c(600, 20)  # Set field width
  CLC_Choose_00     <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "open", 
                    text = "Select the Input CLC 2000 file...")
    if (!is.na(choice)) {
      svalue(clc_file) <- choice  ## Set value of the label widget
      opts$clc_file <- choice  # Set value of the selected variable\t
    }
  }, container = CLC_SelGroup_00)
  
  
  # Build Widgets for opts of Output Folder --------------------------
  
  Out_SelGroup     <- ggroup(horizontal = TRUE, container = Files_Group)
  Out_Lab          <- glabel(text = "Output Folder for time series statistical analysis", 
                             container = Out_SelGroup, width = 400)
  size(Out_Lab)    <- c(280, 8)
  out_dir       <- gedit(text = format(opts$out_dir, justify = "right"), 
                            container = Out_SelGroup, anchor = c(1, 1))
  size(out_dir) <- c(600, 20)
  Out_Choose       <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "selectdir", 
                    text = "Select the Output Folder for the results of the statistical analysis...")
    if (!is.na(choice)) {
      svalue(out_dir) <- choice  ## Set value of the label widget
      opts$out_dir <- choice  # Set value of the selected variable\t
    }
  }, container = Out_SelGroup)
  
  # Build Widgets for opts of Processing Years -----------------------
  
  Year_SelGroup     <- ggroup(horizontal = TRUE, container = Proc_Group)
  StartYear_Lab     <- glabel(text = "Starting Year", container = Year_SelGroup, 
                              font.attr = list(style = "bold", size = "big"), editable = FALSE)
  start_year        <- gspinbutton(from = 2000, to = 2020, by = 1, value = opts$start_year, 
                                   container = Year_SelGroup, anchor = c(1, 1))
  Fake_Lab          <- glabel(Text = "", container = Year_SelGroup)
  EndYear_Lab       <- glabel(text = "     Ending Year", container = Year_SelGroup, 
                              font.attr = list(style = "bold", size = "big"), editable = FALSE)
  size(EndYear_Lab) <- c(130, 20)
  end_year          <- gspinbutton(from = 2000, to = 2020, by = 1, 
                                   value = opts$end_year, 
                                   container = Year_SelGroup, anchor = c(1, 1))
  size(StartYear_Lab) <- c(130, 20)
  # ------------------------------------------------------------------- #
  # Widgets for opts of Kernel Dimensions
  # ------------------------------------------------------------------- #
  
  Kern_Lab <- glabel(text = "     Kernel Dimensions (Km)       ", container = Year_SelGroup, 
                     font.attr = list(style = "bold", size = "big"), editable = FALSE)
  nker     <- gspinbutton(from = 10, to = 200, by = 1, value = opts$nker, 
                          horizontal = TRUE, container = Year_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for opts of Percentages
  # ------------------------------------------------------------------- #
  
  perc_Lab  <- glabel(text = "     Minimum NDVI Difference       ", container = Year_SelGroup, 
                      font.attr = list(style = "bold", size = "big"), editable = FALSE)
  perc_diff <- gspinbutton(from = 0, to = 30, by = 0.1, value = 9.5, 
                           horizontal = TRUE, container = Year_SelGroup)
  
  # Build Widgets for opts of redownload and reprocess options -------
  
  Re_SelGroup <- ggroup(horizontal = TRUE, container = reproc_Group)
  redown_Lab  <- glabel(text = "Re-Download existing MODIS data", container = Re_SelGroup, 
                        font.attr = list(style = "bold", size = "big"), editable = FALSE)
  size(redown_Lab) <- c(200, 20)
  # Fake_Lab = glabel(Text = '', container = Year_SelGroup)
  # size(Fake_Lab) = c(100,20)
  redown       <- gradio(items = c("Yes", "No"), container = Re_SelGroup, 
                         horizontal = T, anchor = c(1, 1), selected = 2)
  reproc_Lab   <- glabel(text = "reprocess existing MODIS data", container = Re_SelGroup, 
                         font.attr = list(style = "bold", size = "big"), editable = FALSE)
  
  reproc       <- gradio(items = c("Yes", "No"), container = Re_SelGroup, horizontal = T, 
                         anchor = c(1, 1), selected = 2)
  reproc_images_lab <- glabel(text = "           reprocess Existing Scaled Indexes", 
                         container = Re_SelGroup, font.attr = list(style = "bold", size = "big"), 
                         editable = FALSE)
  reproc_images     <- gradio(items = c("Yes", "No"), container = Re_SelGroup, 
                         horizontal = T, anchor = c(1, 1), selected = 2)
  size(reproc_Lab) <- c(200, 20)
  # Build Command Buttons ------------------------------------------------------
  
  But_Group <- gframe(horizontal = FALSE, container = Main_W)
  Start_But <- gbutton("Start", handler = function(button, ...) {
    
    # If Start selected, retrieve widgets values
    opts$mod_dir     <- svalue(mod_dir)  
    opts$orig_shapefile  <- svalue(shape_file)  
    opts$clc_file    <- svalue(clc_file) 
    opts$out_dir     <- svalue(out_dir)  
    opts$start_year  <- svalue(start_year)
    opts$end_year    <- svalue(end_year)
    opts$nker        <- svalue(nker)
    opts$perc_diff   <- svalue(perc_diff)
    opts$reproc      <- svalue(reproc, index = TRUE)
    opts$redown      <- svalue(redown, index = TRUE)
    opts$reproc_images    <- svalue(reproc_images, index = TRUE)
    #  "Legacy" options (method always set to 2, which means analysis based on 
    #  percentage difference to kernel median. (In oldere versions a second 
    #  method was considered, but it was stripped)
    #  (SNDVI always set to 1, which means compute recovery based on changes in 
    #  SNDVI - A different index was considered in previous version (SRDVI) ,
    #  but it was stripped)
    #  TODO REMOVE THESE OUTDATED OPTIONS !!!!
    opts$method    <- 2  
    opts$SNDVI     <- 1
    
    if (opts$end_year >= opts$start_year) {
      # Save  widgets values in the frg_previous.RData file
      save(opts, file = file.path(opts$prev_dir, 
                                            "frg_previous.RData"))
      # Start the processing
      dispose(Main_W)  # Selection finished - close the GUI
      # enabled(Main_GUI) <- FALSE
      
      frg_fullprocessing(opts,
                         force_update =  force_update)
      
    } else {
      gmessage("Error in selected Years. Please Correct", title = "Warning", 
               icon = "warning")
    }
  }, container = But_Group)
  
  Quit_But <- gbutton("Quit", handler = function(button, ...) {
    dispose(Main_W)  # Processing Canceled - close the GUI
    print("Quit")
  }, container = But_Group)
  
  visible(Main_W, set = TRUE)  ## show the selection GUI
}
