
#'@title FRG_SVI_Sel
#'@description Function used to create the GUI for selecting files and options for computation of Scaled Indexes images from MODIS yearly data
#'@details 
#' Allows to select:
#' \itemize{
#' \item MOD_Dir The Main Folder containing the preprocessed MODIS images. This is the folder selected by the user in the Downloading/preprocessing phase (e.g., MODIS_Data)
#' \item CLC_File_00 File corresponding to the recoded CORINE LAND COVER 2000 map (ENVI format !)
#' \item Shape_File Shapefile of burnt areas to be considered in the analysis
#' \item Start_Year Starting year for the analysis
#' \item End_Year End year for the analysis
#' \item NKer Width in Km of the moving window considered for computation of scaled indexes
#' \item Method Method used for Scaled indexes computation. Options are: \cr
#' 		  Forced to "2" = 'Percentage Deviation to Kernel Median': For each pixel, the algorithm identifies all pixels of the image corresponding to the same CLC class contained in a window of width NKer
#' 			It then computes the median of the distribution of values of the analyzed Vegetation Index in the selecteds pixels. Scaled Index for the pixel are then computed as: 
#' 			sVI = 100 * (VI_pix - VI_median)/(VI_median). Previous methods no longer used !
#' \item SNDVI, SRDVI checkboxes used to select which vegetation indexes are to be used in the analysis
#' \item ReProc Checkbox used to specify if already present images are to be reprocessed. Defaults to FALSE
#' }
#' @return
#' If the user chooses 'Start' and all processing parameters are OK, the selected parameters are saved in the 'FRG_SVI_Previous.RData' file and the computation starts \cr
#' Results are saved in appropriate subfolders of the MOD_Dir folder.
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Oct 26, 2012
#' @export

FRG_SVI_GUI = function() {
  
  # Load previous selection from FRG_CS_Previous.RData
  check_prev =  file.info(file.path(FRG_Options$Previous_Dir,'FRG_SVI_Previous.RData'))
  if (is.finite(check_prev$size) )  {load (file.path(FRG_Options$Previous_Dir,'FRG_SVI_Previous.RData'))
  }	else {SDVI_Selection = data.frame(SDVI_File = ' ', CLC_File = ' ',Out_Folder = ' ', Shape_File = ' ', NKer = 801, Start_Year = 2000, End_Year = 2011,
                                      Method =2, SDVI = 1, SNDVI = 1)}
  
  # ------------------------------------------------------------------- #
  # Build Main Widgets
  # ------------------------------------------------------------------- #
  Main_W =  gbasicdialog("Fire Regeneration Monitoring Tool - SDVI Computation", horizontal = FALSE, do.buttons = FALSE, spacing = 10)
  Files_Group = gframe(text = "Select Processing Files", horizontal = FALSE, container=Main_W)
  Proc_Group = gframe(text = "Select Processing Options", horizontal = FALSE, container=Main_W)
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of input folder *("Originals" folder !) 
  # ------------------------------------------------------------------- #
  MOD_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)					# Main group
  Out_Lab <- glabel(text ='    Input Folder for MODIS data ("MODIS Data" folder)', container=MOD_SelGroup, editable =FALSE)  # Label
  size(Out_Lab) <- c(280,8)																				# Set label width
  MOD_Dir <- gedit(text = format(SDVI_Selection$MOD_Dir, justify = "right") , container=MOD_SelGroup)			# Selected file 
  size(MOD_Dir) <- c(600,20)																			# Set field width
  MOD_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Main Folder for MODIS data...")		# File selection widget
                                                           if(! is.na(choice)){svalue(MOD_Dir)<-choice						## On new selection, set value of the label widget
                                                                               SDVI_Selection$MOD_Dir = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable	
                                                           }}, container=MOD_SelGroup)
  addSpace(MOD_SelGroup, 20)
  
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of CORINE 2000 image
  # ------------------------------------------------------------------- #
  CLC_SelGroup_00 = ggroup(horizontal = TRUE, container=Files_Group)
  CLC_Lab_00 <- glabel(text ='Input Corine Land Cover File (2000)' , container=CLC_SelGroup_00)
  size(CLC_Lab_00) <- c(280,8)
  CLC_File_00 <- gedit(text = format(SDVI_Selection$CLC_File_00, justify = "right"), container=CLC_SelGroup_00, anchor=c(1,1))
  size(CLC_File_00) <- c(600,20)
  CLC_Choose_00 <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="open", text="Select the Input CLC 2000 file...")
                                                               if(! is.na(choice)){svalue(CLC_File_00)<-choice				## Set value of the label widget
                                                                                   SDVI_Selection$CLC_File_00 = choice			# Set value of the selected variable	
                                                               }}, container=CLC_SelGroup_00)
  
  # -------------------------------------------------------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of ShapeFile (Used to mask out burned areas when computing statistics !) 
  # -------------------------------------------------------------------------------------------------------------------- #
  
  Shape_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
  Shape_Lab <- glabel(text ='Input Burnt Areas Shape File', container=Shape_SelGroup)
  size(Shape_Lab) <- c(280,8)
  Shape_File <- gedit(text = format(SDVI_Selection$Shape_File, justify = "right"), container=Shape_SelGroup, anchor=c(1,1))
  size(Shape_File) <- c(600,20)
  Shape_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the Input Burnt Areas Shape file...", filter =list("Shapfiles" =  list(patterns = c("*.shp"))))
                                                             if(! is.na(choice)){svalue(Shape_File)<-choice				## Set value of the label widget
                                                                                 SDVI_Selection$Shape_File = choice								# Set value of the selected variable	
                                                             }}, container=Shape_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of Output file 
  # ------------------------------------------------------------------- #
#   Out_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
#   Out_Lab <- glabel(text ='Output Folder for SDVI and SNDVI ("Scaled Folder")', container=Out_SelGroup,width = 400)
#   size(Out_Lab) <- c(280,8)
#   Out_Folder <- gedit(text = format(SDVI_Selection$Out_Folder, justify = "right"), container=Out_SelGroup, anchor=c(1,1))
#   size(Out_Folder) <- c(600,20)
#   Out_Choose <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="selectdir", text="Select the Output Folder for SDVI and SNDVI...")
#                                                             if(! is.na(choice)){svalue(Out_Folder)<-choice				## Set value of the label widget
#                                                                                 SDVI_Selection$Out_Folder = choice			# Set value of the selected variable	
#                                                             }}, container=Out_SelGroup)
#   
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of Processing Years
  # ------------------------------------------------------------------- #
  Year_SelGroup = ggroup(horizontal = TRUE, container=Proc_Group)
  
  StartYear_Lab <- glabel(text ='Starting Year', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(StartYear_Lab) <- c(130,20)	
  Start_Year <- gspinbutton(from=2000,to=2012,by=1,value=SDVI_Selection$Start_Year, container=Year_SelGroup, anchor=c(1,1))
  Fake_Lab = glabel(Text = '', container = Year_SelGroup)
  #	size(Fake_Lab) = c(30,20)
  EndYear_Lab <- glabel(text ='     Ending Year', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(EndYear_Lab) <- c(130,20)	
  End_Year <- gspinbutton(from=2000,to=2012,by=1,value=SDVI_Selection$End_Year, container=Year_SelGroup, anchor=c(1,1))
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of Kernel Dimensions
  # ------------------------------------------------------------------- #
  
  Kern_Lab <- glabel(text ='     Kernel Dimensions (Km)       ', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  NKer = gspinbutton(from = 10, to = 200, by = 1,value =  SDVI_Selection$NKer, horizontal = TRUE, container = Year_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of Processing Method
  # ------------------------------------------------------------------- #
  Meth_Lab <- glabel(text ='     Processing Method ', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  Meth_opt = c('Percentage Deviation to Kernel Median ')
  
  Method= gdroplist(Meth_opt, selected = SDVI_Selection$Method, container = Year_SelGroup)
  
  # ------------------------------------------------------------------- #
  # Widgets for SDVI_Selection of Indexes to be analyzed
  # ------------------------------------------------------------------- #	
  
  Ind_SelGroup = ggroup(horizontal = TRUE, container=Proc_Group)
  
  SNDVI_lab <- glabel(text ='                               Compute Scaled NDVI (SNDVI)', container=Ind_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  SNDVI <- gradio(items = c('Yes','No'),container = Ind_SelGroup, horizontal = T, anchor=c(1,1), selected =SDVI_Selection$SNDVI)
  SRDVI_lab <- glabel(text ='           Compute Scaled RDVI (SRDVI)', container=Ind_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  SRDVI <- gradio(items = c('Yes','No'),container = Ind_SelGroup, horizontal = T, anchor=c(1,1), selected = SDVI_Selection$SDVI)
  ReProc_lab <- glabel(text ='           Reprocess Existing Images ', container=Ind_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  ReProc <- gradio(items = c('Yes','No'),container = Ind_SelGroup, horizontal = T, anchor=c(1,1), selected = 2)
  
  # ------------------------------------------------------------------- #
  # Command Buttons
  # ------------------------------------------------------------------- #
  But_Group = gframe(horizontal = FALSE, container=Main_W)
  
  Start_But <- gbutton("Start", handler=function(button,...){ 
    SDVI_Selection$MOD_Dir = svalue(MOD_Dir)																		# If Start selected, retrieve widgets values
    SDVI_Selection$Shape_File = svalue(Shape_File)
    SDVI_Selection$CLC_File_00 = svalue(CLC_File_00)
    SDVI_Selection$Out_Folder = svalue(Out_Folder)
    SDVI_Selection$Start_Year = file.path( svalue(MOD_Dir),'Scaled_Indexes')
    SDVI_Selection$End_Year = svalue(End_Year)
    SDVI_Selection$NKer = svalue(NKer)
    SDVI_Selection$Method = 2   # Forced to 2, since now it's the only method used !!!!
    SDVI_Selection$SNDVI = svalue(SNDVI, index = TRUE)
    SDVI_Selection$SRDVI = svalue(SRDVI, index = TRUE)
    SDVI_Selection$ReProc = svalue(ReProc, index = TRUE)
    
    if (SDVI_Selection$End_Year >= SDVI_Selection$Start_Year) {
      save (SDVI_Selection, file = file.path(FRG_Options$Previous_Dir,'FRG_SVI_Previous.RData'))		# Save  widgets values in the FRG_CS_Previous.RData file
      # Start the processing
      dispose(Main_W)											# Selection finished - close the GUI
      enabled(Main_GUI) = FALSE
      
      print('--- Starting Computation of Scaled Indexes ---')
      
      er =with(SDVI_Selection,FRG_MOD_Comp_SVI(MOD_Dir = MOD_Dir, Shape_File = Shape_File, CLC_File_00 = CLC_File_00, 
                                               Out_Folder = Out_Folder, Start_Year = Start_Year, End_Year = End_Year, 
                                               NKer = NKer, Method = Method, SRDVI = SRDVI ,SNDVI = SNDVI,
                                               nodata_out = FRG_Options$No_Data_Out_Rast,ReProc = ReProc))
      
      if (er == 'DONE') { 
        
        print('--- Scaled Indexes Computation Complete ! ---')		# Completion message
        addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
        enabled(Main_GUI) = TRUE																				#Re-enable the Main GUI
      } else {stop('An Error Occurred while computing Scaled Indexes ! ')}
    } else gmessage("Error in selected Years. Please Correct", title="Warning", icon = 'warning')
  }, container=But_Group)
  
  
  Quit_But <- gbutton("Quit", handler=function(button, ...){ 
    dispose(Main_W)															# Processing Canceled - close the GUI
    print('Quit')}, container=But_Group)	
  
  visible(Main_W, set=TRUE) ## show the selection GUI
}
