#' frg_main
#' @description Main Function of the Fire Regeneration Monitoring Tool
#' @details This is the main function of the Fire Regeneration Monitoring Tool. 
#' It builds the main GUI and handles events corresponding to the different buttons, calling the corresponding functions.
#' Also loads all required libraries, sources necessary routines and sets the main processing frg_conf.
#' The main processing functions of the FRG Tool can be accessed from the Main GUI. In particular:
#' 1. **Create Burned Area Shapefile**:  Allows creating the input burnt areas shapefile, 
#'   starting from data present in the EFFIS database (esposito).
#'   
#' 2. **Analyse burned ares**: Allows performing all the processing needed for the 
#'   extraction and analysis of time series of rescaled indexes for the burnt areas 
#'   present in the input shapefile.
#'  
#' 3. **Update Oracle tables**: Update the Oracle database tables regarding Regeneration 
#'    Monitoring using the results of the latest processing.
#'
#' @return Opens the Main GUI of the Tool and allow selection of the processing step
#' @author Lorenzo Busetto, PhD (2012)
#'         email: lbusett@gmail.com
#' @import gWidgetsRGtk2
#' @import gWidgets
#' @export

frg_main <- function() {
  
  # Accessory functions to get the the execution folder of the Main
  # script
  
  # rscript.stack <- function() { # Returns the stack of RScript files
  # Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile)) }
  # rscript.current <- function() { ## Returns the current RScript file
  # path stack <- rscript.stack() as.character(stack[length(stack)]) }
  # pkgTest <- function(x) {if (!require(x,character.only = TRUE))
  # {install.packages(x,dep=TRUE) if(!require(x,character.only = TRUE))
  # stop('Package not found') } } #-
  # -------------------------------------------------------------------------------
  # -# # Initialize project #-
  # -------------------------------------------------------------------------------
  # -# # Check if needed packages are present. Install them otherwise
  # pkgList =
  # c('tools','debug','gWidgets','gWidgetsRGtk2','RCurl','rgdal','reshape2','ggplot2','tcltk',
  # 'gdata','abind', 'data.table','tcltk',
  # 'hash','plyr','car','lubridate','stringr','raster','gdalUtils','dplyr')
  # for (pkg in pkgList) {pkgTest(pkg) } options('guiToolkit'='RGtk2')
  
  memory.limit(6000)  # Increase maximum allocable memory
  
  # setup the processing folders
  main_dir       <- getwd()
  src_dir_idl    <- file.path(getwd(), "IDL") 
  idl_exe_dir    <- system2("where", args = "idl.exe", stdout = TRUE)
  if (!is.null(attributes(idl_exe_dir)$status)) {
    stop("idl.exe was not found on your PATH. Exiting. Please add the folder containing idl.exe to your
         Windows Path variable. ")
  }
   src_dir_python <- file.path(getwd(), "python")
   cfg_dir        <- file.path(getwd(), "config")
   prev_dir       <- file.path(getwd(), "previous")
   dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
  
  # run project configuration if frg_congif.RData is not present
  while (!file.exists(file.path(cfg_dir, "frg_config.RData"))) {
    frg_config(cfg_dir)
  }
  
  # Load Configuration parameters from 'FRG_Config.txt'
  
  frg_conf <- get(load(file.path(cfg_dir, "frg_config.RData")))
  
  # paths to required scripts
  Create_ShapeFile_script <- file.path(main_dir, "Create_ShapeFile_script", 
                                       "FRG_Create_Shape.exe")
  # # Set Main Processing frg_conf
  FRG_Options <<- data.frame(main_dir            = main_dir, 
                             Previous_Dir        = prev_dir, 
                             No_Data_In_Rast     = 32767, 
                             No_Data_Out_Rast    = 32767, 
                             src_dir_idl         = src_dir_idl, 
                             arcpython           = as.character(frg_conf$python_exe), 
                             Create_Shape_Script = file.path(main_dir, 
                                                             "Create_ShapeFile_Script", "FRG_Update_Shapefile.exe"),
                             effis_dir           = frg_conf$effis_dir, 
                             stringsAsFactors    = FALSE)
  
  # Build the Main GUI -------------------
  
  # Helper function to quit if 'Exit' pressed
  FRG_Dispose <- function(main_gui) {
    dispose(main_gui)
    return()
  }
  
  main_gui         <- gbasicdialog(title = "Fire Regeneration Monitoring Tool", 
                                    do.buttons = FALSE, horizontal = FALSE, anchor = c(0, 0), width = 200, 
                                    height = 200)
  but_group        <- ggroup(horizontal = FALSE, container = main_gui)
  create_shape_but <- gbutton(" Create Burned Areas Shapefile", cont = but_group, 
                               handler = function(h, ...) {
                                 res <- try(system(as.character(FRG_Options$Create_Shape_Script), 
                                                   wait = FALSE, invisible = FALSE))
                                 message("An error occurred while creating the burned areas shapefile (")
                               })
  
  process_but      <- gbutton("Process Burnt Areas Data", cont = but_group, 
                               handler = function(h, ...) {
                                 res <- frg_fullproc_gui()
                               })
  
  update_oracle_but <- gbutton("Update Oracle tables", cont = but_group, 
                               handler = function(h, ...) {
                                 res <- FRG_Update_Oracle()
                               })
  
  glabel(text = "---------------------------------------------------------------", 
         markup = FALSE, editable = FALSE, handler = NULL, container = but_group)
  
  acc_group <- ggroup(horizontal = TRUE, container = but_group)
  exit_but  <- gbutton("Quit ", cont = acc_group, handler = function(h, ...) {
                      res <- FRG_Dispose(main_gui)})
  help_but  <- gbutton("Help ", cont = acc_group, handler = function(h, ...) {
                      system2("open", file.path(main_dir, "frgtool_manual.pdf"))})
  Main_GUI  <<- main_gui
  visible(main_gui, TRUE)
}