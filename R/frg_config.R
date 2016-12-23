frg_config = function(cfg_dir) {

  # Check if configuration file exists
  test_config <- file.exists(file.path(cfg_dir, 'frg_config.RData'))
  choice <- TRUE
  browser()
  # if exists, ask if overwrite
  if (test_config) {
    choice <- gconfirm('Config file already existing - do you want to modify it ? ',
                       title = 'Confirm', do.buttons = TRUE)
  }

  # set up a GUI for configuration
  if (choice == TRUE) {

    conf_w <-  gbasicdialog(title   = "FRG configuration", horizontal = FALSE, do.buttons = TRUE, spacing = 10,
                            handler = function(h,...) {
                              nodata = as.integer(svalue(nodata_value))
                              if (file.exists(svalue(idl_exe))   == FALSE | file.exists(svalue(arcpy_exe)) == FALSE |
                                  dir.exists(svalue(effis_dir))  == FALSE | is.na(nodata) == TRUE) {
                                    gmessage("Some choices are invalid - Saving aborted !", title = "warning")
                                  } else {
                                    frgconf <- data.frame(idl_exe    = svalue(idl_exe),
                                                          python_exe = svalue(arcpy_exe),
                                                          effis_dir  = svalue(effis_dir),
                                                          nodata     = nodata,
                                                          stringsAsFactors = FALSE
                                    )
                                    save(frgconf, file = file.path(cfg_dir,"frg_config.RData"))	# 	On new selection,  Set value of the selected variable
                                  }
                            })

    # widgets for idl executable selection
    main_group    <- gframe(text = "Please insert the followig paths relative to your PC", horizontal = FALSE, container = conf_w)
    idl_group     <- ggroup(container = main_group, horizontal = TRUE)
    idl_lab       <- glabel(text = 'Path to IDL.exe', container = idl_group, editable = FALSE)  # Label
    size(idl_lab) <- c(250,8)																				# Set label width
    idl_exe       <- gedit(text = "", justify = "right", container = idl_group,  expand = TRUE)			# Selected file
    size(idl_exe) <- c(380,20)																			# Set field width
    idl_choose    <- gbutton(text = "Browse", handler = function(h,...) {
      choice <- gfile(type = "open", text = "Select the IDL.exe file...")		# File selection widget
      if (length(choice) != 0) {
        if (basename(choice) == "idl.exe") {
          svalue(idl_exe) <- choice
        } else {
          gmessage("Invalid choice for IDL executable - please revise ! ")
        }
      }}, container = idl_group)## On new selection, set value of the label widget

    # widgets for ARCPYTHON executable
    arcpy_group     <- ggroup(container = main_group, horizontal = TRUE)
    arcpy_lab       <- glabel(text = 'Path to ARCGIS python.exe', container = arcpy_group, editable = FALSE)  # Label
    size(arcpy_lab) <- c(250,8)																				# Set label width
    arcpy_exe       <- gedit(text = "", justify = "right", container = arcpy_group,  expand = TRUE)			# Selected file
    size(arcpy_exe) <- c(380,20)																			# Set field width
    arcpy_choose    <- gbutton(text = "Browse", handler = function(h,...) {choice <- gfile(type = "open", text = "Select the IDL.exe file...")		# File selection widget
    if (length(choice) != 0) {
      if (basename(choice) == "python.exe") {
        svalue(arcpy_exe) <- choice
      } else {
        gmessage("Invalid choice for python executable - please revise ! ")
      }
    }}, container = arcpy_group)## On new selection, set value of the label widget

    # widgets for EFFIS folder server
    effis_group     <- ggroup(container = main_group, horizontal = TRUE)
    effis_lab       <- glabel(text = 'Path to effis folder', container = effis_group, editable = FALSE)  # Label
    size(effis_lab) <- c(250,8)																				# Set label width
    effis_dir       <- gedit(text = "", justify = "right", container = effis_group,  expand = TRUE)			# Selected file
    size(effis_dir) <- c(380,20)																			# Set field width
    effis_choose    <- gbutton(text = "Browse", handler = function(h,...) {choice <- gfile(type = "selectdir", text = "Select the IDL.exe file...")		# File selection widget
    if (length(choice) != 0) {svalue(effis_dir) <- choice}}, container = effis_group)## On new selection, set value of the label widget

    # NODATA values
    nodata_group     <- ggroup(container = main_group, horizontal = TRUE)
    nodata_lab       <- glabel(text = 'NODATA value (in and out)', container = nodata_group, editable = FALSE)  # Label
    nodata_value     <- gedit(text = "", width = 25, container = nodata_group)
    size(nodata_lab) <- c(250,8)																				# Set label width

    visible(conf_w, TRUE)

  }

}


