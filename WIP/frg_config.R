#' @title Set frg condfiguration options
#' @description Helper function to set some `frgtool` configuration options and 
#'   save them to `frg_config.RData`:
#'   1. 
#' @param cfg_dir `character` path were the config file should be saved. If NULL, 
#'   the file is saved in the main folder of the package as frg_config.RData, 
#'   Default: NULL 
#' @return The function is called for its side effects
#' @rdname frg_config
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
frg_config = function(cfg_dir = system.file(package = "frgtool")) {
  
  idl_exe <- arcpy_exe <- effis_dir <- nodata_value <- NULL
  # Check if configuration file exists
  test_config <- file.exists(file.path(cfg_dir, 'frg_config.RData'))
  choice <- TRUE
  # if exists, ask if overwrite
  if (test_config) {
    choice <- gWidgets::gconfirm(
      "Config file already existing - do you want to modify it ? ",
      title = "Confirm", do.buttons = TRUE
    )
  }
  
  # set up a GUI for configuration
  if (choice == TRUE) {
    
    conf_w <-  gWidgets::gbasicdialog(
      title   = "FRG configuration",
      horizontal = FALSE, 
      do.buttons = TRUE #, 
      # spacing    = 10,
      # handler    = function(h,...) {
      #   nodata = as.integer(gWidgets::svalue(nodata_value))
      #   if (file.exists(gWidgets::svalue(idl_exe))   == FALSE | 
      #       file.exists(gWidgets::svalue(arcpy_exe)) == FALSE |
      #       dir.exists(gWidgets::svalue(effis_dir))  == FALSE | 
      #       is.na(nodata) == TRUE) {
      #     gwidgets::gmessage("Some choices are invalid - Saving aborted!",
      #                        title = "warning")
      #   } else {
      #     # On success, save the config file
      #     frgconf <- data.frame(idl_exe    = gWidgets::svalue(idl_exe),
      #                           python_exe = gWidgets::svalue(arcpy_exe),
      #                           effis_dir  = gWidgets::svalue(effis_dir),
      #                           nodata     = nodata,
      #                           stringsAsFactors = FALSE
      #     )
      #     save(frgconf, file = file.path(cfg_dir,"frg_config.RData"))
      #   }
      # }
  )
    
    # widgets for idl executable selection
    main_group    <- gWidgets::gframe(
      text = "Please insert the followig paths relative to your PC",
      horizontal = FALSE, 
      container = conf_w)
    idl_group     <- gWidgets::ggroup(container = main_group, horizontal = TRUE)
    # Label
    idl_lab       <- gWidgets::glabel(text = 'Path to IDL.exe',
                                      container = idl_group,
                                      editable = FALSE)
    gWidgets::size(idl_lab) <- c(250, 8) 
    # Selected file
    idl_exe       <- gWidgets::gedit(
      text = "", justify = "right", container = idl_group,  expand = TRUE)			
    gWidgets::size(idl_exe) <- c(380,20)																			
    idl_choose    <- gWidgets::gbutton(
      text = "Browse",
      handler = function(h,...) {
        choice <- gWidgets::gfile(type = "open",
                                  text = "Select the IDL.exe file...")		
        if (length(choice) != 0) {
          if (basename(choice) == "idl.exe") {
            gWidgets::svalue(idl_exe) <- choice
          } else {
            gWidgets::gmessage("Invalid choice for IDL executable - please revise ! ") #nolint
          }
        }}, container = idl_group)
    
    # widgets for ARCPYTHON executable
    arcpy_group     <- gWidgets::ggroup(container = main_group, 
                                        horizontal = TRUE)
    arcpy_lab       <- gWidgets::glabel(text = 'Path to ARCGIS python.exe', 
                                        container = arcpy_group, 
                                        editable = FALSE)  
    gWidgets::size(arcpy_lab) <- c(250,8)																				
    arcpy_exe       <- gWidgets::gedit(text = "", justify = "right",
                                       container = arcpy_group,  expand = TRUE)			
    gWidgets::size(arcpy_exe) <- c(380,20)
    # File selection widget
    arcpy_choose    <- gWidgets::gbutton(
      text = "Browse",
      handler = function(h,...) {
        
        choice <- gWidgets::gfile(type = "open", 
                                  text = "Select the IDL.exe file...")		
        if (length(choice) != 0) {
          if (basename(choice) == "python.exe") {
            ## On new selection, set value of the label widget
            gWidgets::svalue(arcpy_exe) <- choice
          } else {
            gWidgets::gmessage(
              "Invalid choice for python executable - please revise ! ")
          }
        }}, container = arcpy_group)
    
    # widgets for EFFIS folder server
    effis_group     <- gWidgets::ggroup(container = main_group,
                                        horizontal = TRUE)
    effis_lab       <- gWidgets::glabel(text = 'Path to effis folder',
                                        container = effis_group,
                                        editable = FALSE)
    gWidgets::size(effis_lab) <- c(250,8)													
    effis_dir       <- gWidgets::gedit(text = "", justify = "right",
                                       container = effis_group,  expand = TRUE)	
    gWidgets::size(effis_dir) <- c(380,20)																			
    effis_choose    <- gWidgets::gbutton(
      text = "Browse",
      handler = function(h,...) {
        choice <- gWidgets::gfile(type = "selectdir",
                                  text = "Select the IDL.exe file...")
        if (length(choice) != 0) {svalue(effis_dir) <- choice}
      }, container = effis_group)
    
    # NODATA values
    nodata_group     <- gWidgets::ggroup(container = main_group, 
                                         horizontal = TRUE)
    nodata_lab       <- gWidgets::glabel(text = 'NODATA value (in and out)', 
                                         container = nodata_group, 
                                         editable = FALSE)
    nodata_value     <- gWidgets::gedit(text = "", 
                                        width = 25, container = nodata_group)
    gWidgets::size(nodata_lab) <- c(250,8)				
    gWidgets::visible(conf_w)
    
  }
  
}


