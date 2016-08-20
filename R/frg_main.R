#' @title FRG_Main
#' @description Main Function of the Fire Regeneration Monitoring Tool
#' @details This is the main function of the Fire Regeneration Monitoring Tool. \cr
#'It builds the main GUI and handles events corresponding to the different buttons, calling the corresponding functions. \cr
#'Also loads all required libraries, sources necessary routines and sets the main processing options.  \cr
#'The main processing functions of the FRG Tool can be accessed from the Main GUI. In particular:
#'
#'\Itemize {
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
#'
#' @return Main GUI of the Tool
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#'
frg_main = function() {

  #	Accessory functions to get the  the execution folder of the Main script

#   rscript.stack <- function() {					#	Returns the stack of RScript files
#     Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
#   }
#
#   rscript.current <- function() {				## Returns the current RScript file path
#     stack <- rscript.stack()
#     as.character(stack[length(stack)])
#   }
#
#   pkgTest <- function(x)
#   {if (!require(x,character.only = TRUE))
#   {install.packages(x,dep=TRUE)
#    if(!require(x,character.only = TRUE)) stop("Package not found")
#   }
#   }
#   #- ------------------------------------------------------------------------------- -#
#   #  Initialize project
#   #- ------------------------------------------------------------------------------- -#
#
#   # Check if needed packages are present. Install them otherwise
#   pkgList = c('tools','debug','gWidgets','gWidgetsRGtk2','RCurl','rgdal','reshape2','ggplot2','tcltk',
#               'gdata','abind', 'data.table','hash','plyr','car')
#   for (pkg in pkgList) {pkgTest(pkg)	}
#   options("guiToolkit"="RGtk2")
#
    memory.limit(6000)							# Increase maximum allocable memory

  # setup the processing folders

  src_dir_r       <- file.path(getwd(),"R")
  src_dir_idl     <- file.path(getwd(),"idl")
  src_dir_python  <- file.path(getwd(),"python")
  cfg_dir         <- file.path(getwd(),"config")
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)

  # run project configuration if frg_congif.RData is not present
  while (!file.exists(file.path(cfg_dir, 'frg_config.RData'))) {
      frg_config(cfg_dir)
  }

  frg_conf = get(load(file.path(cfg_dir, "frg_config.RData")))

#   # Folder initialization
#   rscript.current()
#   Src_Dir = dirname(rscript.current())
#   print(Src_Dir)
#   setwd(file.path(Src_Dir,'../..')); 	Main_Dir = getwd()
#   Previous_Dir = file.path(Main_Dir,'R-FRG/Previous')
#   Config_Dir = file.path(Main_Dir,'Config')
#   IDL_Dir = file.path(Main_Dir,'IDL-FRG')
# #   Create_ShapeFile_script = file.path(Main_Dir,'Create_ShapeFile_script','FRG_Create_Shape.exe')
#
#   # Load Configuration parameters from "FRG_Config.txt"
#   Config_File = file.path(Config_Dir,"FRG_Config.txt")
#   options = read.table(Config_File, sep = '=',strip.white = T )
#
#   options <- data.frame( MRTpath=options[1,2],idl_exe=options[2,2],MOD_FTP=options[3,2],
#                          arcpython = options[4,2],No_Data_In_Rast = options[5,2], No_Data_Out_Rast = options[6,2],
#                          effis_folder = options[7,2])
#
#   # Sourcing of needed R scripts (Remove when building package !!!!)
#
# #   source(file.path(Src_Dir,'GUI','FRG_MOD_GUI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_RDVI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_UI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_MeanInd.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Download.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Proc.R'))
#
# #   source(file.path(Src_Dir,'GUI','FRG_SVI_GUI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_MOD_Comp_SVI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_ROI_Build.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Create_Mask.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Create_Mask_Eroded.R'))
#
# #   source(file.path(Src_Dir,'GUI','FRG_Extr_Stats_GUI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Extr_Stats.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Process_Shapefile.R'))
#
# #   source(file.path(Src_Dir,'GUI','FRG_Regr_GUI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Significance_Matrix.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Significance_Analysis.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Comp_Sig_Matrix.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Comp_Plot_Stat_Multiple.R'))
#
#   source(file.path(Src_Dir,'Processing','FRG_Full_Proc_GUI.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Full_Processing.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Create_Shape.R'))
#   source(file.path(Src_Dir,'Processing','FRG_Update_Oracle.R'))
#
#   # Set Main Processing Options
#   FRG_Options <<- data.frame(Main_Dir = Main_Dir,
#                              Previous_Dir = Previous_Dir,
#                              No_Data_In_Rast = as.numeric(as.character(options$No_Data_In_Rast)),
#                              No_Data_Out_Rast = as.numeric(as.character(options$No_Data_Out_Rast)),
#                              IDL_Dir = IDL_Dir,
#                              MRTpath=as.character(options$MRTpath),
#                              idl_exe = as.character(options$idl_exe),
#                              stringsAsFactors=FALSE,
#                              MOD_FTP = as.character(options$MOD_FTP),
#                              Use_Temp_Folder = 0,
#                              arcpython = as.character(options$arcpython),
#                              Create_Shape_Script = file.path(Main_Dir, 'Create_ShapeFile_Script','FRG_Update_Shapefile.exe'),
# #                              Create_Shape_Script =as.character(options$Create_Shape_Script),
#                              effis_folder  = as.character(options$effis_folder)
#                             )
#
#   #- ------------------------------------------------------------------------------- -#d
#   #  Build the Main GUI
#   #- ------------------------------------------------------------------------------- -#
#
#   # Helper function to quit if 'Exit' pressed
#   FRG_Dispose = function(Main_W) {
#     dispose(Main_W)
#     return()
#   }
#
#   Main_W = gwindow(title = "Fire Regeneration Monitoring Tool", do.buttons = FALSE, anchor = c(0,0), width = 200, height = 200)
#   But_Group = ggroup(horizontal = FALSE, container=Main_W)
#   Create_Shape_But = gbutton(" Create Burned Areas Shapefile", cont = But_Group, handler = function(h,...) {res= FRG_Create_Shape() })
#   Process_But = gbutton("Process Burnt Areas Data", cont = But_Group, handler = function(h,...) {res= FRG_Full_Proc_GUI()})
#   Update_Oracle_But = gbutton("Update Oracle tables", cont = But_Group, handler = function(h,...) {res= FRG_Update_Oracle()})
#   glabel(text = "---------------------------------------------------------------", markup = FALSE, editable = FALSE, handler = NULL,
#          container = But_Group,)
#   Exit_But = gbutton("Quit ", cont = But_Group, handler = function(h,...) { res = FRG_Dispose(Main_W)})
#
#   Main_GUI <<- Main_W

}

# Launch MAIN GUI

# a = FRG_Main()
