#' @title frg_fullproc_gui
#' @description Function used to create the GUI for selecting the parameters for 
#' full FRG procesing
#' @details Main GUI of the tool - used only to set the processing options and 
#' call the appropraite routines for updating results (FRG_Full_Processing.R)
#' If the user chooses 'Start' and all processing parameters are OK, the selected
#' parameters are saved in the 'FRG_Full_Previous.RData' file and processing is 
#' started
#' @return NULL
#' @author Lorenzo Busetto (2012 - 2017)
#'         email: lbusett@gmail.com
#' @export
#' @import gWidgetsRGtk2
#' @importFrom hash hash values
#' 
frg_fullproc_gui <- function() {
  
  if (file.exists(file.path(FRG_Options$Previous_Dir, "frg_previous.RData"))) {
    load(file.path(FRG_Options$Previous_Dir, "frg_previous.RData"))
  } else {
    Full_Selection <- data.frame(MOD_Dir = "", 
                                 Start_Year = 2001, 
                                 End_Year = 2013, 
                                 ReDown = 1, 
                                 ReProc = 1, 
                                 ReProcIm = 1, 
                                 Shape_File = "", 
                                 CLC_File_00 = "", 
                                 NKer = 200, 
                                 Method = 1, 
                                 SDVI = 1, 
                                 SNDVI = 1, 
                                 nodata_out = FRG_Options$No_Data_Out_Rast)
  }
  
  # Build Main Widgets ---------------------------------------------------------------
  
  Main_W       <- gbasicdialog("Fire Regeneration Monitoring Tool", horizontal = FALSE, 
                               do.buttons = FALSE, spacing = 10)
  Files_Group  <- gframe(text = "Select the Processing Files - Folders", 
                         horizontal = FALSE, container = Main_W)
  Fake_Group   <- glabel(text = "", container = Main_W)
  Proc_Group   <- gframe(text = "Select the Processing Options", horizontal = FALSE, 
                         container = Main_W)
  Fake_Group2  <- glabel(text = "", container = Main_W)
  ReProc_Group <- gframe(text = "Select the ReProcessing Options", horizontal = FALSE, 
                         container = Main_W)
  Fake_Group3  <- glabel(text = "", container = Main_W)
  
  # Build Widgets for Full_Selection of Input ShapeFile (Used to mask out ------------
  # burned areas when computing statistics !)
  
  Shape_SelGroup   <- ggroup(horizontal = TRUE, container = Files_Group)
  Shape_Lab        <- glabel(text = "Input Burnt Areas Shape File", container = Shape_SelGroup)
  size(Shape_Lab)  <- c(280, 8)
  Shape_File       <- gedit(text = format(Full_Selection$Shape_File, justify = "right"), 
                            container = Shape_SelGroup, anchor = c(1, 1))
  size(Shape_File) <- c(600, 20)
  Shape_Choose     <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "open", 
                    text = "Select the Input Burnt Areas Shape file...", 
                    filter = list(Shapfiles = list(patterns = c("*.shp"))))
    if (!is.na(choice)) {
      svalue(Shape_File) <- choice  ## Set value of the label widget
      Full_Selection$Shape_File <- choice  # Set value of the selected variable\t
    }
  }, container = Shape_SelGroup)
  
  # Build Widgets for Full_Selection of the folder where to place original and ------------
  # pre-processed MODIS satellite data
  
  MOD_SelGroup  <- ggroup(horizontal = TRUE, container = Files_Group)  # Main group
  Out_Lab       <- glabel(text = "Main Folder for download of MODIS data", 
                          container = MOD_SelGroup, editable = FALSE)  # Label
  size(Out_Lab) <- c(280, 8)  # Set label width
  MOD_Dir       <- gedit(text = format(Full_Selection$MOD_Dir, justify = "right"), 
                         container = MOD_SelGroup)  # Selected file 
  size(MOD_Dir) <- c(600, 20)  # Set field width
  MOD_Choose    <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "selectdir", 
                    text = "Select the Main Folder for MODIS data...")  # File selection widget
    if (!is.na(choice)) {
      svalue(MOD_Dir) <- choice  ## On new selection, set value of the label widget
      Full_Selection$MOD_Dir <- format(choice, justify = "right")  # \tOn new selection,  Set value of the selected variable\t
    }
  }, container = MOD_SelGroup)
  addSpace(MOD_SelGroup, 20)
  
  # Build Widgets for Full_Selection of CORINE 2000 image ---------------------
  
  CLC_SelGroup_00   <- ggroup(horizontal = TRUE, container = Files_Group)
  CLC_Lab_00        <- glabel(text = "Input Corine Land Cover File (2000)", 
                              container = CLC_SelGroup_00)
  size(CLC_Lab_00)  <- c(280, 8)  # Set label width
  CLC_File_00       <- gedit(text = format(Full_Selection$CLC_File_00, justify = "right"), 
                             container = CLC_SelGroup_00, anchor = c(1, 1))
  size(CLC_File_00) <- c(600, 20)  # Set field width
  CLC_Choose_00     <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "open", 
                    text = "Select the Input CLC 2000 file...")
    if (!is.na(choice)) {
      svalue(CLC_File_00) <- choice  ## Set value of the label widget
      Full_Selection$CLC_File_00 <- choice  # Set value of the selected variable\t
    }
  }, container = CLC_SelGroup_00)
  
  
  # Build Widgets for Full_Selection of Output Folder --------------------------
  
  Out_SelGroup     <- ggroup(horizontal = TRUE, container = Files_Group)
  Out_Lab          <- glabel(text = "Output Folder for time series statistical analysis", 
                             container = Out_SelGroup, width = 400)
  size(Out_Lab)    <- c(280, 8)
  Out_Folder       <- gedit(text = format(Full_Selection$Out_Folder, justify = "right"), 
                            container = Out_SelGroup, anchor = c(1, 1))
  size(Out_Folder) <- c(600, 20)
  Out_Choose       <- gbutton("Browse", handler = function(h, ...) {
    choice <- gfile(type = "selectdir", 
                    text = "Select the Output Folder for the results of the statistical analysis...")
    if (!is.na(choice)) {
      svalue(Out_Folder) <- choice  ## Set value of the label widget
      Full_Selection$Out_Folder <- choice  # Set value of the selected variable\t
    }
  }, container = Out_SelGroup)
  
  # Build Widgets for Full_Selection of Processing Years -----------------------
  
  Year_SelGroup     <- ggroup(horizontal = TRUE, container = Proc_Group)
  StartYear_Lab     <- glabel(text = "Starting Year", container = Year_SelGroup, 
                              font.attr = list(style = "bold", size = "big"), editable = FALSE)
  Start_Year        <- gspinbutton(from = 2000, to = 2020, by = 1, value = Full_Selection$Start_Year, 
                                   container = Year_SelGroup, anchor = c(1, 1))
  Fake_Lab          <- glabel(Text = "", container = Year_SelGroup)
  EndYear_Lab       <- glabel(text = "     Ending Year", container = Year_SelGroup, 
                              font.attr = list(style = "bold", size = "big"), editable = FALSE)
  size(EndYear_Lab) <- c(130, 20)
  End_Year          <- gspinbutton(from = 2000, to = 2020, by = 1, 
                                   value = Full_Selection$End_Year, 
                                   container = Year_SelGroup, anchor = c(1, 1))
  size(StartYear_Lab) <- c(130, 20)
  # ------------------------------------------------------------------- #
  # Widgets for Full_Selection of Kernel Dimensions
  # ------------------------------------------------------------------- #
  
  Kern_Lab <- glabel(text = "     Kernel Dimensions (Km)       ", container = Year_SelGroup, 
                     font.attr = list(style = "bold", size = "big"), editable = FALSE)
  NKer     <- gspinbutton(from = 10, to = 200, by = 1, value = Full_Selection$NKer, 
                          horizontal = TRUE, container = Year_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for Full_Selection of Percentages
  # ------------------------------------------------------------------- #
  
  perc_Lab  <- glabel(text = "     Minimum NDVI Difference       ", container = Year_SelGroup, 
                      font.attr = list(style = "bold", size = "big"), editable = FALSE)
  perc_diff <- gspinbutton(from = 0, to = 30, by = 0.1, value = 9.5, 
                           horizontal = TRUE, container = Year_SelGroup)
  
  # Build Widgets for Full_Selection of ReDownload and ReProcess options -------
  
  Re_SelGroup <- ggroup(horizontal = TRUE, container = ReProc_Group)
  ReDown_Lab  <- glabel(text = "Re-Download existing MODIS data", container = Re_SelGroup, 
                        font.attr = list(style = "bold", size = "big"), editable = FALSE)
  size(ReDown_Lab) <- c(200, 20)
  # Fake_Lab = glabel(Text = '', container = Year_SelGroup)
  # size(Fake_Lab) = c(100,20)
  ReDown       <- gradio(items = c("Yes", "No"), container = Re_SelGroup, 
                         horizontal = T, anchor = c(1, 1), selected = 2)
  ReProc_Lab   <- glabel(text = "ReProcess existing MODIS data", container = Re_SelGroup, 
                         font.attr = list(style = "bold", size = "big"), editable = FALSE)
  
  ReProc       <- gradio(items = c("Yes", "No"), container = Re_SelGroup, horizontal = T, 
                         anchor = c(1, 1), selected = 2)
  ReProcIm_lab <- glabel(text = "           Reprocess Existing Scaled Indexes", 
                         container = Re_SelGroup, font.attr = list(style = "bold", size = "big"), 
                         editable = FALSE)
  ReProcIm     <- gradio(items = c("Yes", "No"), container = Re_SelGroup, 
                         horizontal = T, anchor = c(1, 1), selected = 2)
  size(ReProc_Lab) <- c(200, 20)
  # Build Command Buttons ------------------------------------------------------
  
  But_Group <- gframe(horizontal = FALSE, container = Main_W)
  Start_But <- gbutton("Start", handler = function(button, ...) {
    
    # If Start selected, retrieve widgets values
    Full_Selection$MOD_Dir     <- svalue(MOD_Dir)  
    Full_Selection$Shape_File  <- svalue(Shape_File)  
    Full_Selection$CLC_File_00 <- svalue(CLC_File_00) 
    Full_Selection$Out_Dir     <- svalue(Out_Folder)  
    Full_Selection$Start_Year  <- svalue(Start_Year)
    Full_Selection$End_Year    <- svalue(End_Year)
    Full_Selection$NKer        <- svalue(NKer)
    Full_Selection$perc_diff   <- svalue(perc_diff)
    #  Always set to 2, which means analysis based on percentage difference
    #  to kernel median. (In oldere versions A second method was considered, 
    #  but it was stripped
    Full_Selection$Method   <- 2  
    Full_Selection$ReProc   <- svalue(ReProc, index = TRUE)
    Full_Selection$ReDown   <- svalue(ReDown, index = TRUE)
    Full_Selection$ReProcIm <- svalue(ReProcIm, index = TRUE)
    
    if (Full_Selection$End_Year >= Full_Selection$Start_Year) {
      save(Full_Selection, file = file.path(FRG_Options$Previous_Dir, 
                                            "frg_previous.RData"))  # Save  widgets values in the FRG_CS_Previous.RData file
      # Start the processing
      dispose(Main_W)  # Selection finished - close the GUI
      # enabled(Main_GUI) <- FALSE
      
      frg_fullprocessing(MOD_Dir     = Full_Selection$MOD_Dir, 
                         Shape_File  = Full_Selection$Shape_File, 
                         CLC_File_00 = Full_Selection$CLC_File_00, 
                         Out_Dir     = Full_Selection$Out_Dir, 
                         Start_Year  = Full_Selection$Start_Year, 
                         End_Year    = Full_Selection$End_Year, 
                         NKer        = Full_Selection$NKer, 
                         perc_diff  = Full_Selection$perc_diff, 
                         Method      = Full_Selection$Method,
                         SNDVI       = 1, 
                         ReProc      = Full_Selection$ReProc, 
                         ReDown      = Full_Selection$ReDown, 
                         ReProcIm    = Full_Selection$ReProcIm)
      
      enabled(Main_GUI) <- TRUE
      
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
