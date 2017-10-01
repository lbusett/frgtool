#'@title FRG_Create_Shape
#'@description Function used to create the input shapefile for the FRG processing, starting from Oracle database tables
#'@details This function is used to call the external function used to create the input shapefile for the FRG processing, starting from Oracle database tables\cr 
#'
#' @return Shape_File : Name of the created shapefile#'
#' @returnType 
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2012
#' @export

FRG_Create_Shape = function(){
  
  out = system(as.character(FRG_Options$Create_Shape_Script) , wait = FALSE,invisible = FALSE)
}

#   Main_W =  gbasicdialog("Create Shapefile from Oracle Tables", horizontal = FALSE, do.buttons = FALSE, spacing = 10)
#   Files_Group = gframe(text = "Select Name for Output Shapefile", horizontal = FALSE, container=Main_W)
#   
#   Shape_SelGroup = ggroup(horizontal = TRUE, container=Files_Group)  				# Main group
# 	Shape_Lab <- glabel(text ='Output Name for Burnt Areas ShapeFile ', container=Shape_SelGroup,font.attr=list(style="bold",size = 'big'), editable =FALSE)  # Label
# 	size(Shape_Lab) <- c(250,8)																				# Set label width
# 	Shape_File <- gedit(container=Shape_SelGroup,  expand = TRUE)			# Selected file 
# 	size(Shape_File) <- c(400,20)																			# Set field width
#   Out_Choose <- gbutton("Browse",handler = function(h,...) {choice<-gfile(type="save", text="Select the output file name (No extension)...")
# 				if(! is.na(choice)){svalue(Shape_File)<-choice				## Set value of the label widget
# 					# Set value of the selected variable	
# 				}}, container=Shape_SelGroup)
#   
#   But_Group = gframe(horizontal = FALSE, container=Main_W)
#   
# 	Start_But <- gbutton("Start", handler=function(button,...){ 
# 				    
#         enabled(Main_GUI) = FALSE
#         Out_File = paste ( file_path_sans_ext(svalue(Shape_File)),'.shp', sep = '')	
#         
# #         out = system(Create_ShapeFile_script)
# #       if (out != 0) {
# #          print('Error while creating the input Shapefile of burnt Areas from Oracle. Processing stopped !')
# #          stop()
# #       }
#         print(Out_File)
#         dispose(Main_W)      								# Selection finished - close the GUI
#          enabled(Main_GUI) = TRUE
# 			}, container=But_Group)
# 	
# 	Quit_But <- gbutton("Quit", handler=function(button, ...){ 
# 				dispose(Main_W)															# Processing Canceled - close the GUI
# 				print('Quit')}, container=But_Group)	
#   
#   visible(Main_W, set=TRUE) ## show the selection GUI