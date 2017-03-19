
#'@title FRG_MOD_GUI
#'@description Function used to create the GUI for selecting the MODIS images download and preprocessing options
#'@details 
#' Allows to select the folder where MODIS original and preprocessed files will be saved, the starting and ending processing years and 
#' the reprocessing and redownloading options. Also calls the MOD_Proc routine if the user selects "Start".
#'
#' @return
#' If the user chooses 'Start' and all processing parameters are OK, the selected parameters are saved in the 'FRG_MOD_Previous.RData' file
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Oct 26, 2012
#' @export

FRG_MOD_GUI = function() {
  
  # Load previous selection from FRG_MOD_Previous.RData
  if (file.exists(file.path(FRG_Options$Previous_Dir,'FRG_MOD_Previous.RData'))) {load (file.path(FRG_Options$Previous_Dir,'FRG_MOD_Previous.RData'))}
  else (MOD_Selection = data.frame(MOD_Dir = '', Start_Year = 2000, End_Year = 2012, ReDown = 1, ReProcIm = 1))
  
  # ------------------------------------------------------------------- #
  # Build Main Widgets
  # ------------------------------------------------------------------- #
  Main_W =  gbasicdialog("Fire Regeneration Monitoring Tool", horizontal = FALSE, do.buttons = FALSE, spacing = 10)
  MOD_Group = gframe(text = "Select the Processing Files - Folders", horizontal = FALSE, container=Main_W)
  Proc_Group = gframe(text = "Select the Processing Options", horizontal = FALSE, container=Main_W)
  
  # ------------------------------------------------------------------- #
  # Widgets for MOD_Selection of the folder where to place original and pre-processed MODIS satellite data 
  # ------------------------------------------------------------------- #
  
  MOD_SelGroup = ggroup(horizontal = TRUE, container=MOD_Group)					# Main group
  Out_Lab <- glabel(text ='Main Output Folder for MODIS data', container=MOD_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)  # Label
  size(Out_Lab) <- c(250,8)																				# Set label width
  MOD_Dir <- gedit(text = format(MOD_Selection$MOD_Dir, justify = "right") , container=MOD_SelGroup,  expand = TRUE)			# Selected file 
  size(MOD_Dir) <- c(380,20)																			# Set field width
  MOD_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="selectdir", text="Select the Main Folder for MODIS data...")		# File selection widget
                                                           if(! is.na(choice)){svalue(MOD_Dir)<-choice						## On new selection, set value of the label widget
                                                                               MOD_Selection$MOD_Dir = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable	
                                                           }}, container=MOD_SelGroup)
  addSpace(MOD_SelGroup, 20)
  
  # ------------------------------------------------------------------- #
  # Widgets for MOD_Selection of Processing Years
  # ------------------------------------------------------------------- #
  Year_SelGroup = ggroup(horizontal = TRUE, container=Proc_Group)
  
  StartYear_Lab <- glabel(text ='Starting Year', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(StartYear_Lab) <- c(200,20)	
  Start_Year <- gspinbutton(from=2000,to=2012,by=1,value=MOD_Selection$Start_Year, container=Year_SelGroup, anchor=c(1,1))
  Fake_Lab = glabel(Text = '', container = Year_SelGroup)
  size(Fake_Lab) = c(30,20)
  EndYear_Lab <- glabel(text ='Ending Year', container=Year_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(EndYear_Lab) <- c(200,20)	
  End_Year <- gspinbutton(from=2000,to=2012,by=1,value=MOD_Selection$End_Year, container=Year_SelGroup, anchor=c(1,1))
  
  # ------------------------------------------------------------------- #
  # Widgets for MOD_Selection of ReDownload and ReProcess options
  # ------------------------------------------------------------------- #	
  
  Re_SelGroup = ggroup(horizontal = TRUE, container=Proc_Group)
  
  ReDown_Lab <- glabel(text ='Re-Download existing MODIS data', container=Re_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(ReDown_Lab) <- c(200,20)	
  ReDown <- gradio(items = c('Yes','No'),container = Re_SelGroup, horizontal = T, anchor=c(1,1), selected =2)
  ReProc_Lab <- glabel(text ='ReProcess existing MODIS data', container=Re_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)
  size(ReProc_Lab) <- c(200,20)	
  ReProcIm <- gradio(items = c('Yes','No'),container = Re_SelGroup, horizontal = T, anchor=c(1,1), selected = 2)
  
  # ------------------------------------------------------------------- #
  # Command Buttons
  # ------------------------------------------------------------------- #
  But_Group = gframe(horizontal = FALSE, container=Main_W)
  
  Start_But <- gbutton("Start", handler=function(button,...){ 
    
    MOD_Selection$MOD_Dir = svalue(MOD_Dir)											# If Start selected, retrieve widgets values
    MOD_Selection$Start_Year = svalue(Start_Year)										# If Start selected, retrieve widgets values
    MOD_Selection$End_Year = svalue(End_Year)						     				# If Start selected, retrieve widgets values
    MOD_Selection$ReProcIm = svalue(ReProcIm, index = TRUE)
    MOD_Selection$ReDown= svalue(ReDown, index = TRUE)
    
    if (MOD_Selection$End_Year >= MOD_Selection$Start_Year) {
      save (MOD_Selection, file = file.path(FRG_Options$Previous_Dir,'FRG_MOD_Previous.RData'))		# Save  widgets values in the FRG_CS_Previous.RData file
      # Start the processing
      dispose(Main_W)											# Selection finished - close the GUI
      enabled(Main_GUI) = FALSE
      with(MOD_Selection, FRG_MOD_Proc(MOD_Dir= MOD_Dir, Start_Year= Start_Year, End_Year= End_Year, ReProcIm = ReProcIm, ReDown = ReDown))
      enabled(Main_GUI) = TRUE
    } else gmessage("Error in selected Years. Please Correct", title="Warning", icon = 'warning')
  }, container=But_Group)
  
  Quit_But <- gbutton("Quit", handler=function(button, ...){ 
    dispose(Main_W)															# Processing Canceled - close the GUI
    print('Quit')}, container=But_Group)	
  
  visible(Main_W, set=TRUE) ## show the selection GUI
}