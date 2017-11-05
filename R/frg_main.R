#' frg_main
#' @description Main Function of the Fire Regeneration Monitoring Tool
#' @param force_update `logical` If TRUE, intermediate processing outputs (e.g., 
#'   creation of burnt areas shapefile, computation of SVI, etc. are re-run 
#'   even if intermecdiate processing outputs are already present in the 
#'   specified output folders. Useful to make a "clean run" of the tool, 
#'   Default: FALSE) 
#' @details This is the main function of the Fire Regeneration Monitoring Tool. 
#' It builds the main GUI and handles events corresponding to the different buttons,
#' calling the corresponding functions. Also loads all required libraries,
#' sources necessary routines and sets the main processing variables.
#' The main processing functions of the FRG Tool can be accessed from the Main GUI.
#' In particular:
#' 1. **Create Burned Area Shapefile**:  Allows creating the input burnt areas
#'   shapefile, starting from data present in the EFFIS database (esposito).
#'   
#' 2. **Analyse burned ares**: Allows performing all the processing needed for the 
#'   extraction and analysis of time series of rescaled indexes for the burnt areas 
#'   present in the input shapefile.
#'  
#' 3. **Update Oracle tables**: Update the EFFIS Oracle database tables regarding 
#'   Regeneration Monitoring using the results of the latest processing.
#'
#' @return Opens the Main GUI of the Tool and allow selection of the processing
#'   step to be run.
#' @author Lorenzo Busetto, PhD (2017)
#'         email: lbusett@gmail.com
#' @import gWidgetsRGtk2
#' @import gWidgets
#' @export

frg_main <- function(force_update = FALSE) {
  
  
  # - Initialize project                                                    ####
  # ----------------------------------------------------------------------------
  
  # __________________________________________________________________________
  # Create and initilize the `opts` list , which will contain all folder    ####
  # / file names required for the processing, plus some additional
  # processing options
  
  opts <- list()   
  
  # General processing options - do not touch unless you want to try        ####
  # something "new" !
  opts$erode     <- 1 
  opts$min_pix   <- 10
  opts$MedWdt    <- 3  
  opts$sig_level <- 0.05
  opts$sub_zones <- 0
  opts$index     <- "NDVI"
  
  
  
  # path to frgtool IDL scripts
  opts$src_dir_idl      <- system.file("IDL_scripts", package = "frgtool")
  
  # path to frgtool python scripts #nolint
  opts$src_dir_python   <- system.file("python_scripts", package = "frgtool") 
  
  # path to frgtool previous dir #nolint
  
  opts$prev_dir         <- system.file("ExtData/previous", package = "frgtool") 
  
  # paths to the exe used to create the update shapefile (runs on EFFIS
  # oracle tables to generate the up-to-date shapefile of burnt areas
  # at the end of each fire season)
  opts$create_shape_exe <- system.file("Create_Shapefile_exe",
                                     "frg_update_shapefile.exe", package = "frgtool")
  
  #   ________________________________________________________________________
  #   Find and set path to IDL.exe                                        ####
  
  if (Sys.info()['sysname'] == "Linux") {
    idl_exe_dir    <- system2("which", args = "idl", stdout = TRUE)
  } else {
    idl_exe_dir    <- system2("where", args = "idl.exe", stdout = TRUE)
  }
  
  if (!is.null(attributes(idl_exe_dir)$status)) {
    stop("idl.exe was not found on your PATH. Exiting. Please add the folder", 
         "containing idl.exe to your Windows Path variable. ")
  } else {
    opts$idl_exe_dir <- idl_exe_dir
  }
  
  # #   __________________________________________________________________________
  # #   check if effispath was already set. Otherwise ask to set it           ####
  # opts$effispath <- frg_set_effispath()
  
  #   __________________________________________________________________________
  #   check if arcpypath was already set. Otherwise ask to set it           ####
  opts$arcpypath <- frg_set_arcpypath()
  
  opts$nodata_in    <- 32767
  opts$nodata_out   <- 32767
  opts$force_update <- force_update
  
  
  #   __________________________________________________________________________
  #   Build and handle the main GUI                                         ####
  
  # Helper function to quit if 'Exit' pressed
  FRG_Dispose <- function(main_gui) {
    gWidgets::dispose(main_gui)
    return()
  }
  
  main_gui <- gWidgets::gbasicdialog(
    title = "Fire Regeneration Monitoring Tool", 
    do.buttons = FALSE, horizontal = FALSE, anchor = c(0, 0), width = 200, 
    height = 200)
  
  # creat buttons to call different processing steps ----
  but_group        <- gWidgets::ggroup(horizontal = FALSE, container = main_gui)
  
  # create_shape --> Calls the function to update burnt areas shapefile ----
  create_shape_but <- gWidgets::gbutton(
    " Create Burned Areas Shapefile",
    container = but_group, 
    handler = function(h, ...) {
      res <- try(system(as.character(opts$create_shape_exe), 
                        wait = FALSE, invisible = FALSE))
      message("An error occurred while creating the burned areas shapefile (")
    })
  
  # process_but --> Open the processing GUI ----
  process_but <- gWidgets::gbutton(
    "Process Burnt Areas Data",
    container = but_group, 
    handler = function(h, ...) {
      gWidgets::dispose(main_gui)
      res <- frgtool::frg_fullproc_gui(opts, 
                                       force_update = force_update)
    })
  
  # update_oracle_but --> Call frg_update_oracle to update oracle tables ----
  # using last results
  
  update_oracle_but <- gWidgets::gbutton(
    "Update Oracle tables",
    container = but_group, 
    handler = function(h, ...) {
      res <- frgtool::frg_update_oracle(opts, 
                                        force_update = force_update)
    })
  
  gWidgets::glabel(
    text = "---------------------------------------------------------------", 
    markup = FALSE, editable = FALSE, handler = NULL, container = but_group)
  
  # exit_but --> Quit the program ----
  acc_group <- gWidgets::ggroup(horizontal = TRUE, container = but_group)
  exit_but  <- gWidgets::gbutton("Quit ",
                                 container = acc_group,
                                 handler = function(h, ...) {
                                   res <- FRG_Dispose(main_gui)})
  Main_GUI  <<- main_gui
  gWidgets::visible(main_gui, TRUE)
}