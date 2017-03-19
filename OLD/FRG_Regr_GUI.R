#'FRG_Regr_GUI
#'
#' @description This routine builds the GUI for the selection of files to be used for statistical analysis of time series information derived for burnt
#' areas from MODIS scaled vegetation indexes files.
#'
#' @return
#'
#'
#' @author Lorenzo Busetto
#' Created Date: Feb 17, 2012
#'
#' @export
FRG_Regr_GUI = function() {

	# Load previous selection from FRG_CS_Previous.RData

	if (file.exists(file.path(FRG_Options$Previous_Dir,'FRG_Regr_Previous.RData'))) {load (file.path(FRG_Options$Previous_Dir,'FRG_Regr_Previous.RData'))}
	else (Regr_Selection = data.frame(RData_File = '', Out_File = '', min_pix = 20))

	# ------------------------------------------------------------------- #
	# Build Main Widgets
	# ------------------------------------------------------------------- #
	Main_W =  gbasicdialog("Fire Regeneration Monitoring Tool", horizontal = FALSE, do.buttons = FALSE, spacing = 10)
	Files_Group = gframe(text = "Select Processing Files", horizontal = FALSE, container=Main_W)
	Proc_Group = gframe(text = "Select Processing Options", horizontal = FALSE, container=Main_W)

	# ------------------------------------------------------------------- #
	# Widgets for Regr_Selection of input satellite data
	# ------------------------------------------------------------------- #

	RData_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)					# Main group
	RData_Lab <- glabel(text ='Input RData file containg SVI time series data ', container=RData_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)  # Label
	size(RData_Lab) <- c(250,8)																				# Set label width
	RData_File <- gedit(text = format(Regr_Selection$RData_File, justify = "right") , container=RData_SelGroup,  expand = TRUE)			# Selected file
	size(RData_File) <- c(400,20)																			# Set field width
	RData_Choose <- gbutton("Browse", handler=function(h,...) {choice<-gfile(type="open", text="Select the Input SVIer META file...", filter = '*.RData')				# File selection widget
				if(! is.na(choice)){svalue(RData_File)<-choice						## On new selection, set value of the label widget
					Regr_Selection$RData_File = format(choice, justify = "right")	# 	On new selection,  Set value of the selected variable
				}}, container=RData_SelGroup)
	addSpace(RData_SelGroup, 20)

	# ------------------------------------------------------------------- #
	# Widgets for Regr_Selection of Output file (RData file, containing different data frames)
	# ------------------------------------------------------------------- #
	Out_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)
	Out_Lab <- glabel(text ='Output file ', container=Out_SelGroup,width = 400)
	size(Out_Lab) <- c(250,8)
	Out_File <- gedit(text = format(Regr_Selection$Out_File, justify = "right"), container=Out_SelGroup, anchor=c(1,1))
	size(Out_File) <- c(400,20)
	Out_Choose <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="save", text="Select the output file...", intialfilename = Regr_Selection$Out_File )
				if(! is.na(choice)){svalue(Out_File)<-choice				## Set value of the label widget
					Regr_Selection$Out_File = choice			# Set value of the selected variable
				}}, container=Out_SelGroup)

	min_pix_group = ggroup(horizontal = TRUE, container=Proc_Group)
	min_pix_Lab <- glabel(text ='Minimum Pixels Number	', container=min_pix_group,font.attr=list(style="bold",size = 'big'), editable =FALSE)
	min_pix = gspinbutton(from = 0, to = 400, by = 1,value =  Regr_Selection$min_pix, horizontal = TRUE, container = min_pix_group)

#	min_perc_group = ggroup(horizontal = TRUE, container=Proc_Group)
	min_perc_Lab <- glabel(text ='     Minimum Significant percentage Difference       ', container=min_pix_group,font.attr=list(style="bold",size = 'big'), editable =FALSE)
	perc = gspinbutton(from = 0, to = 100, by = 1,value =  Regr_Selection$perc, horizontal = TRUE, container = min_pix_group)

	med_width_Lab <- glabel(text ='     Maximum width of Median Window       ', container=min_pix_group,font.attr=list(style="bold",size = 'big'), editable =FALSE)
	MedWdt = gspinbutton(from = 0, to = 10, by = 1,value =  Regr_Selection$MedWdt, horizontal = TRUE, container = min_pix_group)

	# ------------------------------------------------------------------- #
	# Command Buttons
	# ------------------------------------------------------------------- #
	But_Group = gframe(horizontal = FALSE, container=Main_W)

	Start_But <- gbutton("Start", handler=function(button,...){

				Regr_Selection$RData_File = svalue(RData_File)																		# If Start selected, retrieve widgets values
				Regr_Selection$Out_File = svalue(Out_File)
#				browser()
				Regr_Selection$min_pix = svalue(min_pix)
				Regr_Selection$perc = svalue(perc)
				Regr_Selection$MedWdt = svalue(MedWdt)

				save (Regr_Selection, file = file.path(FRG_Options$Previous_Dir,'FRG_Regr_Previous.RData'))		# Save  widgets values in the FRG_CS_Previous.RData file

				dispose(Main_W)											# Selection finished - close the GUI
				enabled(Main_GUI) = FALSE							# Disable MAIN_GUI
				addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(TRUE)})
				mess = gwindow(title =paste('Statistical Analysis of burnt areas Time Series'), container = TRUE, width = 400, height = 40)
				addHandlerUnrealize(mess, handler = function(h,...) {return(TRUE)})

				er = try(FRG_Comp_Regr_Matrix (Regr_Selection$RData_File, Regr_Selection$Out_File,Regr_Selection$min_pix, Regr_Selection$perc,Regr_Selection$MedWdt, mess ))			# Call the processing routine

				if (check_err_Regr(er = er, mess = mess,Main_GUI) == 'DONE') {																		# Check for errors. In case, call error handling function
					mess2 = gmessage('Regression analysis  Complete !', title = 'Message', icon = 'info')		# Completion message
					addHandlerUnrealize(mess, handler = function(h,...) {return(FALSE)})
					dispose(mess)																											# Analysis finished - dispose message lab
					addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(FALSE)})		# Allow Main GUI to be closed since processing ended .
					enabled(Main_GUI) = TRUE								#Re-enable the Main GUI
				}
			}, container=But_Group)

	Quit_But <- gbutton("Quit", handler=function(button, ...){
				dispose(Main_W)															# Processing Canceled - close the GUI
				print('Quit')}, container=But_Group)

	visible(Main_W, set=TRUE) ## show the selection GUI
}
