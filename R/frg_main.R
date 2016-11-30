#' @title FRG_Main
#' @description Main Function of the Fire Regeneration Monitoring Tool
#' @details This is the main function of the Fire Regeneration Monitoring Tool. \cr
#'It builds the main GUI and handles events corresponding to the different buttons, calling the corresponding functions. \cr
#'Also loads all required libraries, sources necessary routines and sets the main processing frg_conf.  \cr
#'The main processing functions of the FRG Tool can be accessed from the Main GUI. In particular:
#'
#'\itemize {
#'\item Create Burned Area Shapefile: \cr
#'             Allows creating the input burnt areas shapefile, starting from data present in the EFFIS database (esposito) \cr
#'\item Analyse burned ares: \cr
#'             Allows performing all the processing needed for the extraction and analysis of time series of rescaled indexes \cr
#'             for the burnt areas present in the input shapefile
#'\item Update Oracle tables: \cr
#'            Update the Oracle database tables regarding Regeneration Monitoring using the results of the latest processing   \cr
#'}
#'
#'IMPORTANT NOTE: The FRG tool requires the NASA MODIS Reprojection Tool (MRT -https://lpdaac.usgs.gov/tools/modis_reprojection_tool) )
#'and th gdal an proj4 libraries to be properly installed and configured in the system. Moreover, ENVI+IDL and ARCGIS must be installed on the system ! See the "Installation Instructions.pdf" file
#'distributed with the source code for further info !!!
#' @return Main GUI of the Tool
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @import gWidgetsRGtk2
#' @import gWidgets
#' @import ireaRscripts
#' @import gdalUtils
#' @export
#'
frg_main = function() {

  #	Accessory functions to get the  the execution folder of the Main script

    rscript.stack <- function() {					#	Returns the stack of RScript files
      Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
    }

    rscript.current <- function() {				## Returns the current RScript file path
      stack <- rscript.stack()
      as.character(stack[length(stack)])
    }

    pkgTest <- function(x)
    {if (!require(x,character.only = TRUE))
    {install.packages(x,dep=TRUE)
     if(!require(x,character.only = TRUE)) stop("Package not found")
    }
    }
    #- ------------------------------------------------------------------------------- -#
    #  Initialize project
    #- ------------------------------------------------------------------------------- -#

    # Check if needed packages are present. Install them otherwise
    pkgList = c('tools','debug','gWidgets','gWidgetsRGtk2','RCurl','rgdal','reshape2','ggplot2','tcltk',
                'gdata','abind', 'data.table','hash','plyr','car','lubridate','stringr','raster','gdalUtils','dplyr')
    for (pkg in pkgList) {pkgTest(pkg)	}
    options("guiToolkit"="RGtk2")

  memory.limit(6000)							# Increase maximum allocable memory

  # setup the processing folders
  main_dir        <- getwd()
  src_dir_r       <- file.path(getwd(),"R")
  src_dir_idl     <- file.path(getwd(),"idl")
  src_dir_python  <- file.path(getwd(),"python")
  cfg_dir         <- file.path(getwd(),"config")
  prev_dir         <- file.path(getwd(),"previous")
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)

  # run project configuration if frg_congif.RData is not present
  while (!file.exists(file.path(cfg_dir, 'frg_config.RData'))) {
    frg_config(cfg_dir)
  }

  # Load Configuration parameters from "FRG_Config.txt"

  frg_conf = get(load(file.path(cfg_dir, "frg_config.RData")))

  # paths to required scripts
  Create_ShapeFile_script = file.path(main_dir,'Create_ShapeFile_script','FRG_Create_Shape.exe')
  #   # Set Main Processing frg_conf
  FRG_Options <<- data.frame(main_dir = main_dir,
                             Previous_Dir = prev_dir,
                             No_Data_In_Rast = as.numeric((frg_conf$nodata)),
                             No_Data_Out_Rast = as.numeric((frg_conf$nodata)),
                             src_dir_idl = src_dir_idl,
                             idl_exe = frg_conf$idl_exe,
                             use_temp_folder = 0,
                             arcpython = as.character(frg_conf$python_exe),
                             Create_Shape_Script = file.path(main_dir, 'Create_ShapeFile_Script','FRG_Update_Shapefile.exe'),
                             effis_dir  = frg_conf$effis_dir,
                             stringsAsFactors = FALSE
  )

  #- ------------------------------------------------------------------------------- -#d
  #  Build the Main GUI
  #- ------------------------------------------------------------------------------- -#

  # Helper function to quit if 'Exit' pressed
  FRG_Dispose = function(main_gui) {
    dispose(main_gui)
    return()
  }

  main_gui <- gbasicdialog(title = "Fire Regeneration Monitoring Tool", do.buttons = FALSE,
                           horizontal = FALSE, anchor = c(0,0), width = 200, height = 200)
  but_group = ggroup(horizontal = FALSE, container = main_gui)
  create_shape_but = gbutton(" Create Burned Areas Shapefile", cont = but_group,
                             handler = function(h,...) {res =
                               try(system (as.character(FRG_Options$Create_Shape_Script) , wait = FALSE,invisible = FALSE))
                               message("An error occurred while creating the burned areas shapefile (")}
                             )

  process_but = gbutton("Process Burnt Areas Data", cont = but_group,
                        handler = function(h,...) {res = FRG_Full_Proc_GUI()})

  update_oracle_but = gbutton("Update Oracle tables", cont = but_group,
                              handler = function(h,...) {res = FRG_Update_Oracle()})

  glabel(text = "---------------------------------------------------------------", markup = FALSE, editable = FALSE, handler = NULL,
         container = but_group)
  acc_group = ggroup(horizontal = TRUE, container = but_group)
  exit_but = gbutton("Quit ", cont = acc_group, handler = function(h,...) {res = FRG_Dispose(main_gui)})
  help_but = gbutton("Help ", cont = acc_group, handler = function(h,...) {system2('open',file.path(main_dir, 'frgtool_manual.pdf'))})
  Main_GUI  <<- main_gui
  visible(main_gui, TRUE)
  

}

# Launch MAIN GUI

# a = frg_main()
