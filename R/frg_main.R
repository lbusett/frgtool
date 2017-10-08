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
  
  # -------------------------------------------------------------------------------
  # -# # Initialize project #-
  # -------------------------------------------------------------------------------

  utils::memory.limit(6000)  # Increase maximum allocable memory
  
  # path to frgtool IDL scripts
  src_dir_idl    <- system.file("IDL_Scripts", package = "frgtool")
  # path to frgtool python scripts #nolint
  src_dir_python <- system.file("python_Scripts", package = "frgtool") 
  # path to frgtool previous dir #nolint
  prev_dir       <- system.file("ExtData/previous", package = "frgtool") 
  
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
  }
  
  #   __________________________________________________________________________
  #   check if effispath was already set. Otherwise ask to set it            ####
  
  if (is.null(options("frgtool_effispath")$frgtool_effispath)) {
    check = FALSE
    while (!check) {
      effispath <- readline(
        prompt = "Specify the path to the effis folder where the files to be used to update the ORACLE tables should be copied: ") #nolint
      if (!dir.exists(effispath)) {
        message("The selected folder is not valid. Please try again!")
      } else {
        message("frgtool --> `effispath` set to: ", effispath)
        check = TRUE
      }
    }
    effispath <- normalizePath(effispath)
    options(frgtool_effispath =  effispath)
  } else {
    effispath <- options("frgtool_effispath")[[1]] 
  }
  
  #   __________________________________________________________________________
  #   check if arcpypath was already set. Otherwise ask to set it            ####
  
  if (is.null(options("frgtool_arcpypath")$frgtool_arcpypath)) {
    check = FALSE
    while (!check) {
      arcpypath <- readline(
        prompt = strwrap("Specify the path to  the folder containing 
                         `python.exe` LINKED TO YOUR ARCGIS INSTALLATION: ")
      ) #nolint
      if (!file.exists(file.path(arcpypath, "python.exe"))) {
        message("The selected folder is not valid.", 
                " Please try again!")
      } else {
        message("frgtool --> `arcpypath` set to: ", arcpypath)
        check = TRUE
      }
    }
    arcpypath <- normalizePath(arcpypath)
    options(frgtool_arcpypath =  arcpypath)
  } else {
    arcpypath <- options("frgtool_arcpypath")[[1]] 
  }
  # paths to update shapefile script by Roberto Boca (runs on EFFIS
  # oracle tables to generate the up-to-date shapefile of burnt areas
  # at the end of each fire season!!!!)
  Create_Shape_Script <- system.file("Create_ShapeFile_exe", 
                                       "FRG_Update_Shapefile.exe", 
                                       package = "frgtool")
  # # Set Main Processing frg_conf
  FRG_Options <<- data.frame( 
    Previous_Dir        = prev_dir, 
    No_Data_In_Rast     = 32767, 
    No_Data_Out_Rast    = 32767, 
    src_dir_idl         = src_dir_idl, 
    src_dir_python      = src_dir_python, 
    arcpython           = arcpypath, 
    Create_Shape_Script = Create_Shape_Script,
    effis_dir           = effispath, 
    stringsAsFactors    = FALSE
  )
  
  # Build the Main GUI -------------------
  
  # Helper function to quit if 'Exit' pressed
  FRG_Dispose <- function(main_gui) {
    gWidgets::dispose(main_gui)
    return()
  }
  
  main_gui <- gWidgets::gbasicdialog(
    title = "Fire Regeneration Monitoring Tool", 
    do.buttons = FALSE, horizontal = FALSE, anchor = c(0, 0), width = 200, 
    height = 200)
  but_group        <- gWidgets::ggroup(horizontal = FALSE, container = main_gui)
  create_shape_but <- gWidgets::gbutton(
    " Create Burned Areas Shapefile",
    container = but_group, 
    handler = function(h, ...) {
      res <- try(system(as.character(FRG_Options$Create_Shape_Script), 
                        wait = FALSE, invisible = FALSE))
      message("An error occurred while creating the burned areas shapefile (")
    })
  
  process_but <- gWidgets::gbutton(
    "Process Burnt Areas Data",
    container = but_group, 
    handler = function(h, ...) {
      gWidgets::dispose(main_gui)
      res <- frgtool::frg_fullproc_gui(force_update = force_update)
    })
  
  update_oracle_but <- gWidgets::gbutton(
    "Update Oracle tables",
    container = but_group, 
    handler = function(h, ...) {
      res <- frgtool::FRG_Update_Oracle()
    })
  
  gWidgets::glabel(
    text = "---------------------------------------------------------------", 
    markup = FALSE, editable = FALSE, handler = NULL, container = but_group)
  
  acc_group <- gWidgets::ggroup(horizontal = TRUE, container = but_group)
  exit_but  <- gWidgets::gbutton("Quit ",
                                 container = acc_group,
                                 handler = function(h, ...) {
                                   res <- FRG_Dispose(main_gui)})
  Main_GUI  <<- main_gui
  gWidgets::visible(main_gui, TRUE)
}