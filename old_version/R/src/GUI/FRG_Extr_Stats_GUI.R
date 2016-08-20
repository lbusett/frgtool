#' 
#' @title FRG_Extr_Stats_GUI
#'
#' This routine builds the GUI for the selection of files to be used for extracting time series information from MODIS scaled
#' vegetation indexes files.  
#'
#' @return 
#'
#' @returnType 
#'
#' @author Lorenzo Busetto
#' Created Date: Feb 17, 2012
#' @export

FRG_Extr_Stats_GUI = function() {
  
  # Load previous selection from FRG_CS_Previous.RData
  
  if (file.exists(file.path(FRG_Options$Previous_Dir,'FRG_EX_Previous.RData'))) {load (file.path(FRG_Options$Previous_Dir,'FRG_EX_Previous.RData'))}
  else (EX_Selection = data.frame(SVI_File = '', ROI_File = '', CLC_File_00 = '', Out_File = ''))
  
  # ------------------------------------------------------------------- #
  # Build Main Widgets
  # ------------------------------------------------------------------- #
  Main_W =  gbasicdialog("Fire Regeneration Monitoring Tool", horizontal = FALSE, do.buttons = FALSE, spacing = 10)
  Files_Group = gframe(text = "Select Processing Files", horizontal = FALSE, container=Main_W)
  
  # ------------------------------------------------------------------- #
  # Widgets for EX_Selection of input satellite data
  # ------------------------------------------------------------------- #
  
  SVI_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)					# Main group
  SVI_Lab <- glabel(text ='Input Scaled VI Time Series META File ', container=SVI_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)  # Label
  size(SVI_Lab) <- c(250,8)																				# Set label width
  SVI_File <- gedit(text = format(EX_Selection$SVI_File, justify = "right") , container=SVI_SelGroup,  expand = TRUE)			# Selected file 
  size(SVI_File) <- c(400,20)																			# Set field width
  SVI_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the Input SVI time serie META file...", filter = '*.RData')				# File selection widget
                                                           if(! is.na(choice)){svalue(SVI_File)<-choice						## On new selection, set value of the label widget
                                                                               EX_Selection$SVI_File = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable	
                                                           }}, container=SVI_SelGroup)
  addSpace(SVI_SelGroup, 20)
  
  # -------------------------------------------------------------------------------------------------------------------- #
  # Widgets for EX_Selection of ShapeFile (Used to mask out burned areas when computing statistics !) 
  # -------------------------------------------------------------------------------------------------------------------- #
  
  Shape_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
  Shape_Lab <- glabel(text ='Input Burnt Areas Shape File', container=Shape_SelGroup)
  size(Shape_Lab) <- c(250,8)
  Shape_File <- gedit(text = format(EX_Selection$Shape_File, justify = "right"), container=Shape_SelGroup, anchor=c(1,1))
  size(Shape_File) <- c(400,20)
  Shape_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the Input Burnt Areas Shape file...", filter =list("Shapefiles" =  list(patterns = c("*.shp"))))
                                                             if(! is.na(choice)){svalue(Shape_File)<-choice				## Set value of the label widget
                                                                                 EX_Selection$Shape_File = choice								# Set value of the selected variable	
                                                             }}, container=Shape_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for EX_Selection of CORINE 2000
  # ------------------------------------------------------------------- #
  CLC_SelGroup_00 = ggroup(horizontal = TRUE, container=Files_Group)
  CLC_Lab_00 <- glabel(text ='Input recoded Corine Land Cover File (2000)' , container=CLC_SelGroup_00)
  size(CLC_Lab_00) <- c(250,8)
  CLC_File_00 <- gedit(text = format(EX_Selection$CLC_File_00, justify = "right"), container=CLC_SelGroup_00, anchor=c(1,1))
  size(CLC_File_00) <- c(400,20)
  CLC_Choose_00 <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="open", text="Select the Input CLC 2000 file...")
                                                               if(! is.na(choice)){svalue(CLC_File_00)<-choice				## Set value of the label widget
                                                                                   EX_Selection$CLC_File_00 = choice								## Set value of the selected variable	
                                                               }}, container=CLC_SelGroup_00)
  
  # -------------------------------------------------------------------------------------------------------------------- #
  # Widgets for EX_Selection of Ecoregions file (Raster File - 250m resolution - Byte values corresponding to different ecoregions )
  # -------------------------------------------------------------------------------------------------------------------- #
  
  ENV_Zones_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
  ENV_Zones_Lab <- glabel(text ='Input EcoRegions ENV_Zones File', container=ENV_Zones_SelGroup)
  size(ENV_Zones_Lab) <- c(250,8)
  ENV_Zones_File <- gedit(text = format(EX_Selection$ENV_Zones_File, justify = "right"), container=ENV_Zones_SelGroup, anchor=c(1,1))
  size(ENV_Zones_File) <- c(400,20)
  ENV_Zones_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the Input Burnt Areas ENV_Zones file...", filter =list("TIFF Files" =  list(patterns = c("*.tif"))))
                                                                 if(! is.na(choice)){svalue(ENV_Zones_File)<-choice				## Set value of the label widget
                                                                                     EX_Selection$ENV_Zones_File = choice								# Set value of the selected variable	
                                                                 }}, container=ENV_Zones_SelGroup)
  
  
  # ------------------------------------------------------------------- #
  # Widgets for EX_Selection of Output file 
  # ------------------------------------------------------------------- #
  Out_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
  Out_Lab <- glabel(text ='ROOT NAME for output files (NO extension)', container=Out_SelGroup,width = 400)
  size(Out_Lab) <- c(250,8)
  Out_File <- gedit(text = format(EX_Selection$Out_File, justify = "right"), container=Out_SelGroup, anchor=c(1,1))
  size(Out_File) <- c(400,20)
  Out_Choose <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="save", text="Select the ROOT NAME for output files...", intialfilename = EX_Selection$Out_File )
                                                            if(! is.na(choice)){svalue(Out_File)<-choice				## Set value of the label widget
                                                                                EX_Selection$Out_File = choice			# Set value of the selected variable	
                                                            }}, container=Out_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Command Buttons
  # ------------------------------------------------------------------- #
  But_Group = gframe(horizontal = FALSE, container=Main_W)
  
  Start_But <- gbutton("Start", handler=function(button,...){ 
    EX_Selection$SVI_File = svalue(SVI_File)																		# If Start selected, retrieve widgets values
    EX_Selection$Shape_File = svalue(Shape_File)
    EX_Selection$CLC_File_00 = svalue(CLC_File_00)
    EX_Selection$ENV_Zones_File = svalue(ENV_Zones_File)
    EX_Selection$Out_File = svalue(Out_File)
    save (EX_Selection, file = file.path(FRG_Options$Previous_Dir,'FRG_EX_Previous.RData'))		# Save  widgets values in the FRG_CS_Previous.RData file
    
    dispose(Main_W)											# Selection finished - close the GUI
    enabled(Main_GUI) = FALSE							# Disable MAIN_GUI
    addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(TRUE)})	
    print(paste('Extraction of sVI time series for burnt areas'))
    
    # retrieve the ROI file name and the ENVI mask of eroded ROIS
    ROI_File = file.path(dirname( EX_Selection$Shape_File),'ENVI_ROI', paste(sub("[.][^.]*$", "", basename( EX_Selection$Shape_File)),'.ROI', sep = '')) # Define ROI file name
    erode_file = file.path(dirname(dirname(ROI_File)),'ENVI_Mask',
                           paste(sub("[.][^.]*$", "", basename(ROI_File)),'_ENVI_Mask_Eroded', sep = ''))
    
    er = with(ex_Selection, FRG_Extr_Stats(SVI_File = SVI_File, Shape_File = Shape_File, CLC_File_00 = CLC_File_00,
                                           ENV_Zones_File = ENV_Zones_File, Out_File = Out_File, ,erode = erode, erode_file = erode_file  ))			# Call the processing routine 
        
    if (er == 'DONE') { 
      print ('--- Time Series Extraction Complete !!!')
      addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
      enabled(Main_GUI) = TRUE								#Re-enable the Main GUI		
    } else {stop('An Error Occured During Time Series extraction')}
    
  }, container=But_Group)
  
  Quit_But <- gbutton("Quit", handler=function(button, ...){ 
    dispose(Main_W)															# Processing Canceled - close the GUI
    print('Quit')}, container=But_Group)	
  
  visible(Main_W, set=TRUE) ## show the selection GUI
}
