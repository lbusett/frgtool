#' @title frg_update_oracle
#' @description Function used to update the Oracle database tables using 
#'  processing results of FRG
#' @return The function is called for its side effects
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom rgdal readOGR writeOGR
#' @importFrom tools file_path_sans_ext
#' @import gWidgetsRGtk2
#' @export

frg_update_oracle = function(){
  
  Main_W =  gbasicdialog("Update FRG Oracle Tables", horizontal = FALSE,
                         do.buttons = FALSE, spacing = 10)
  Files_Group = gframe(text = "Select Folder containing FRG processing results to be used to update ORACLE tables",
                       horizontal = FALSE, container = Main_W)
  # Main group
  Fol_SelGroup = ggroup(horizontal = TRUE, container = Files_Group)
  # Label
  Fol_Lab <- glabel(text = 'Folder containing FRG processing results ', 
                    container = Fol_SelGroup, font.attr = list(style = "bold", size = 'big'),
                    editable = FALSE)
  # Set label width
  size(Fol_Lab) <- c(250,8)
  # Selected file 
  Res_Fold <- gedit(container = Fol_SelGroup,  expand = TRUE)
  # Set field width
  size(Res_Fold) <- c(400,20)
  Out_Choose <- gbutton("Browse",
                        handler = function(h,...) {
                          choice <- gfile(type = "selectdir", 
                                          text = "Select the folder containing the FRG Processing Results...")
                          ## Set value of the label widget
                          if(!is.na(choice)){ svalue(Res_Fold) <- choice
                          # Set value of the selected variable
                          }}, container = Fol_SelGroup)
  
  outfol_Group = gframe(text = "Select the folder where processing results are to be copied to allow update of ORACLE tables",
                        horizontal = FALSE, container = Main_W)
  # Main group
  outfol_SelGroup = ggroup(horizontal = TRUE, container = outfol_Group)
  # Label
  outfol_Lab <- glabel(text = 'EFFIS folder ', 
                       container = outfol_SelGroup,font.attr = list(style = "bold", size = 'big'),
                       editable = FALSE)
  # Set label width
  size(outfol_Lab) <- c(250,8)
  # Selected file 
  Out_Fold <- gedit(container = outfol_SelGroup,  expand = TRUE)
  # Set field width
  size(Out_Fold) <- c(400,20)
  Out_Choose <- gbutton("Browse",
                        handler = function(h,...) {
                          choice <- gfile(type = "selectdir", 
                                          text = "Select the folder where processing results are to be copied (effis folder)...")
                          ## Set value of the label widget
                          if(!is.na(choice)){svalue(Out_Fold) <- choice
                          # Set value of the selected variable	
                          }}, container = outfol_SelGroup)
  
  
  
  But_Group = gframe(horizontal = FALSE, container = Main_W)
  
  Start_But <- gbutton("Start", handler = function(button,...){ 
    
    err = 'OK'
    effis_folder = svalue(Out_Fold)
    enabled(Main_GUI) = FALSE
    
    
    # Copy the processing log file
    log_file = list.files(paste(svalue(Res_Fold), sep = ''),
                          pattern = ".txt$", full.names = T)
    copy = file.copy(log_file,
                     file.path(effis_folder,'Data','Processing_log.txt'),
                     overwrite = T)
    if (copy == 'FALSE') {err = 'Error'}
    
    # Check for existance of output tables and set the names
    
    In_Folder = file.path(paste(svalue(Res_Fold), sep = ''),
                          'Summaries_for_EFFIS')
    Files_list = basename(list.files(In_Folder, full.names = T,
                                     include.dirs = FALSE))
    
    chk_LUT = 0 ; chk_Plot = 0 ; chk_Plot_M = 0 ; chk_Recov = 0
    for (file in Files_list) {
      if (length(grep("Intersect_LUT_csv.csv",file)) == 1 ) {LUT_File = file ; chk_LUT = 1}
      if (length(grep("PLOT_DATA",file)) == 1 &
          length(grep("PLOT_DATA_MULTIPLE",file)) != 1) {
        Plot_File = file
        chk_Plot = 1
      } 
      if (length(grep("PLOT_DATA_MULTIPLE",file)) != 0) {
        Plot_File_Multiple = file
        chk_Plot_M = 1
      } 
      if (length(grep("RECOV_DATA",file)) == 1) {
        Recov_File = file 
        chk_Recov = 1
      } 
    }
    
    if (min(c(chk_LUT,chk_Plot,chk_Plot_M,chk_Recov)) == 0) {err = 'Error'}
    # Check for existance of output shapefiles and set the names
    In_Folder_Shapes = file.path(In_Folder, 'Shapefiles')
    Files_list_Shapes= basename(list.files(In_Folder_Shapes,
                                           full.names = T,
                                           pattern = '.shp$'))
    
    chk_shp_single = 0 ; chk_shp_M = 0 ; chk_shp_Full = 0 
    
    for (file in Files_list_Shapes) {
      
      if (length(grep("Single_Fires_Processed.shp",file)) == 1 ) {
        Shape_Single_File = file 
        chk_shp_single = 1
      }
      if (length(grep("Multiple_Fires_Processed.shp",file)) == 1)  {
        Shape_Multiple_File = file
        chk_shp_M = 1
      } 
      if (length(grep("Full_Processed.shp",file)) == 1) {
        Shape_Full_File = file 
        chk_shp_Full = 1
      } 
      
    }
    
    if (min(c(chk_shp_single,chk_shp_M,chk_shp_Full)) == 0) {err ='Error'}
    
    if (err != 'Error') { # If all files found, start the copy to the effis/regeneration/data/ folder
      
      copy = file.copy(file.path(In_Folder,LUT_File),
                       file.path(effis_folder,'Data','reg_intersect_lut.csv'),
                       overwrite = T)
      if (copy == 'FALSE') {err = 'Error'}
      copy = file.copy(file.path(In_Folder,Plot_File),
                       file.path(effis_folder,'Data','reg_time_series.csv'), 
                       overwrite = T)
      if (copy == 'FALSE') {err = 'Error'}
      copy = file.copy(file.path(In_Folder,Plot_File_Multiple), 
                       file.path(effis_folder,'Data','reg_time_series_overlap.csv'),
                       overwrite = T)
      if (copy == 'FALSE') {err = 'Error'}
      copy = file.copy(file.path(In_Folder,Recov_File),
                       file.path(effis_folder,'Data','reg_recov_stat.csv'),
                       overwrite = T)
      if (copy == 'FALSE') {err = 'Error'}
      
      BAreas_shp_Single = try(rgdal::readOGR(In_Folder_Shapes,
                                      tools::file_path_sans_ext(Shape_Single_File)))
      if (class(BAreas_shp_Single) != 'try-error') {
        rgdal::writeOGR(BAreas_shp_Single, 
                 file.path(effis_folder,'Data'),
                 'reg_single_fires',"ESRI Shapefile",
                 overwrite_layer = TRUE)
      } else {(err = 'Error')}
      
      BAreas_shp_Multiple = try(rgdal::readOGR(In_Folder_Shapes,
                                        tools::file_path_sans_ext(Shape_Multiple_File)))
      if (class(BAreas_shp_Multiple) != 'try-error') {
        rgdal::writeOGR(BAreas_shp_Multiple,
                 file.path(effis_folder,'Data'),'reg_multiple_fires',"ESRI Shapefile", 
                 overwrite_layer=TRUE)
      } else {
        err = 'Error'
      }
      
      BAreas_shp_Full = try(rgdal::readOGR(In_Folder_Shapes,
                                    tools::file_path_sans_ext(Shape_Full_File)))
      if (class(BAreas_shp_Full) != 'try-error') {
        rgdal::writeOGR(BAreas_shp_Full,
                 file.path(effis_folder,'Data'), 'reg_full_fires',"ESRI Shapefile",
                 overwrite_layer=TRUE)
      } else {
        err = 'Error'}
    }
    
    # If error occurred delete everything and send error message !!!!
    
    if (err == 'Error') {
      # Error message
      mess_err = gmessage(paste('Error Occurred while updating EFFIS Oracle table !\n 
                            Returning to main\n',sep = ''), title = 'Message', icon = 'info')   
      unlink(file.path(effis_folder,'Data','*'))
    } else {
      # Create an update log file 
      # Set Processing log file
      Update_log_file = file.path(file.path(effis_folder,'Data','Oracle_Update_log.txt'))  
      OutFile_Conn <- Update_log_file      # Open log file
      
      cat(c("--- -------------------------------------------------- ---"), 
          file = OutFile_Conn, sep = "\n", append = FALSE)
      cat("--- Update Log ---",
          file = OutFile_Conn, sep = "\n", append = TRUE)
      cat(c("--- -------------------------------------------------- ---"),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original LUT FILE: ", file.path(In_Folder, LUT_File),
                "Copied to: reg_intersect_lut.csv", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Time Series File (Single Fires): ",
                file.path(In_Folder, Plot_File),
                "Copied to: reg_time_series.csv", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Time Series File (Multiple Fires): ",
                file.path(In_Folder),Plot_File_Multiple,
                "Copied to: reg_time_series_overlap.csv", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Recovery Stat File: ",
                file.path(In_Folder,Recov_File),
                "Copied to: reg_recov_stat.csv", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Single Fires Shapefile: ",
                file.path(In_Folder, "Shapefiles", Shape_Single_File),
                "Copied to: reg_single_fires.shp", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Multiple Fires Shapefile: ",
                file.path(In_Folder, "Shapefiles", Shape_Multiple_File), 
                "Copied to: reg_multiple_fires.shp", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      cat(paste("Original Full Fires: ", 
                file.path(In_Folder,"Shapefiles",Shape_Full_File),
                "Copied to: reg_full_fires.shp", sep = ''),
          file = OutFile_Conn,sep = "\n", append = TRUE)
      # Completion message
      mess = gmessage(paste('Update Successfull !\n Returning to main\n', sep = ''),
                      title = 'Message', icon = 'info')    
      
    }
    # Selection finished - close the GUI
    dispose(Main_W)
    enabled(Main_GUI) = TRUE
  }, container = But_Group)
  
  Quit_But <- gbutton("Quit", handler=function(button, ...){ 
    # Processing Canceled - close the GUI
    dispose(Main_W)
    print('Quit')}, container=But_Group)
  
  visible(Main_W, set=TRUE) ## show the selection GUI
}
