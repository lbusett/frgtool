#'frg_extract_svi
#'@description Function used to extract Scaled VIs time series of the different pixels of analyzed burnt areas, 
#'  associate info form the burnt areas shapefile and save as an RData file to be used for statistical analysis
#'@details This function is used to extract Scaled VIs time series of the different pixels of analyzed burnt 
#'  areas from the raster images created by 'FRG_Compute_SVI.R, associate ancillary info derived form the burnt 
#'  areas shapefile (e.g., Area, FireYear, ...) and save results as an RData file (in the '/TS_Extraction subfolder) 
#'  to be used later for statistical analysis
#'
#' @param SVI_File string ENVI Meta File associated to time serie to be analyzed
#' @param Shape_File string Burnt Areas Shapefile (Must Correspond to the 'burned_once' shafile if Overlap == 'Single'
#'                                                 Must Correspond to the 'burned_once' shafile if Overlap == 'Multiple')
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param ENV_Zones_File string Tiff file containing EcoZones (No longer used...)
#' @param Out_File numeric BaseName of the output RData and CSV files
#' @param erode flag if 1, then perfom analysis on eroded ROIS (default to 1)
#' @param erode_file string Filename of the ENVI mask containing the ERODED ROIS (generated automatically in FRG_Compute_SVI.R)
#' @param Intermed_Dir string name of the Folder containing the intermediate processing results
#' @param Overlap string If == 'Single', extract time series for areas burned once.
#'        if =='Multiple, extract time series for areas burned multiple times
#' @param Shape_File_Orig string Filename of the original input BAs shapefile
#' @param LUT_File_Multiple string Filenlame of the LUT table created by FRG_Process_Shapefile.py
#'
#' @return 'DONE' if all went OK, otherwise error message
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom sf st_read
#' @importFrom lubridate ymd
#' @importFrom raster raster stack setZ extent
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom stringr str_split
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2012
#' @export
#'
frg_extract_svi <- function(SVI_File, Shape_File, CLC_File_00, ENV_Zones_File, 
                            Out_File, erode, FireMask_File_Er, Intermed_Dir, Overlap, Shape_File_Orig, 
                            LUT_File_Multiple) {
  
  message("----------------------------------------------------------")
  message("---- Extracting sVI time series for burnt areas - ", Overlap, " fires ----")
  message("----------------------------------------------------------")
  message(paste("---- -> In File for TS extraction:  ", SVI_File))
  message(paste("---- -> Out File for TS extraction: ", Out_File))
  
  # Define output file names (csv and RData) ----
  
  if (Overlap == "Single") {
    Out_RData   <- paste(Out_File, "RData.RData", sep = "_")  # Filenames used for output saving RData Matrix
    Out_IDL_CSV <- paste(Out_File, "IDL_Matrix.csv", sep = "_")
  } else {
    Out_RData   <- paste(Out_File, "RData.RData", sep = "_")  # Filenames used for output saving RData Matrix
    Out_IDL_CSV <- paste(Out_File, "IDL_Matrix.csv", sep = "_")
  }
  
  selection <- "no"
  
  if (!(file.exists(Out_RData)) | selection == "yes") {
    svi_folder  <- file.path(dirname(SVI_File), "Yearly_Images")
    
    # Open required raster files ----
    svi_files   <- file_path_sans_ext(list.files(svi_folder, pattern = ".hdr$",
                                                 recursive = TRUE, include.dirs = TRUE, 
                                                 full.names = TRUE)) 
    svi_stack   <- raster::stack(svi_files) 
    clc_rast    <- raster::raster(CLC_File_00) 
    erode_rast  <- raster::raster(FireMask_File_Er)
    # envzon_rast <- raster::raster(ENV_Zones_File) 
    
    raster::NAvalue(svi_stack) <- 32767 # set nodatavalue for input SVI file  
    
    # set Dates as attribute to input svi time series ----
    dates     <- ymd(paste0(str_split(names(svi_stack),'_', simplify = TRUE)[,3],'-01-01')) 
    svi_stack <- setZ(svi_stack, dates,'time') 
    
    # Open required shape file ----
    # (Single otr multiple burnt areas)
    
    totburn_shp <- as(sf::st_read(Shape_File, stringsAsFactors = FALSE, quiet = TRUE),'Spatial') 
    nfires      <- dim(totburn_shp)[1]
    tempraster  <- tempfile(tmpdir = tempdir(), fileext = ".tiff") 
    id_field    <- ifelse(Overlap == 'Single', 'OBJECTID','OVERLAP_ID') 
    
    #Rasterize the shapefile to allow a fast extraction of SVI data ----
    
    gdal_rasterize(Shape_File, tempraster, tr = raster::res(svi_stack), 
                   te = extent(svi_stack)[c(1, 3, 2, 4)], a = id_field, 
                   ot = 'Int32') 
    
    ts_data <- frg_fastzonal(in_rts = svi_stack, zone_object = tempraster, 
                             mask_object = erode_rast, clc_object = clc_rast, 
                             # envzone_object = envzon_rast, 
                             id_field = "ID", 
                             small = FALSE,
                             verbose = TRUE, 
                             end_band = 3) 
    # write.csv(ts_data, file = Out_IDL_CSV)
    # save(ts_data, file = Out_RData) 
    
    # Data <- ts_data  # "Data" now contains time series for all burnt pixels 
    
    
    # Load the burned Areas shapefile and save the attribute table as a ----
    # data frame
    message("-----------------------------------------")
    message("---- Joining MODIS and Shapefile info ----")
    message("-----------------------------------------")
    
    # BAreas_Name <- strsplit(basename(Shape_File_Orig), ".shp")[[1]]
    # BAreas_Dir  <- dirname(Shape_File_Orig)
    BAreas_shp  <- as(st_read(Shape_File_Orig, stringsAsFactors = FALSE, quiet = TRUE),"Spatial")
    Data_Shape  <- BAreas_shp@data  # Get attributes data from the shp
    
    
    if (Overlap == "Single") {
      
      # Analyze areas burned only once ----
      
      #
      # Load the statistics file computed by IDL (Derived from Application of
      # FRG_ROI_STAT_ERODE.pro)
      
      # Data = read.csv(Out_IDL_CSV,header = TRUE, na.strings =
      # FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE) Data[Data ==
      # FRG_Options$No_Data_Out_Rast] = NA # Put the NODATA to 'R' NA
      
      # Data <- read.csv(Out_IDL_CSV)
      # Data <- select(Data, -X)
      n_Years           <- length(unique(ts_data$Year))
      names(ts_data)[1] <- "OBJECTID"
      ts_data$OBJECTID  <- as.factor(ts_data$OBJECTID)
      # names(Data)[5:(5+n_Years-1)] =
      # as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))) #
      # Correct columns names names(Data)[(5+n_Years):(5+2*n_Years-1)] =
      # paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))),
      # sep = '_') # Correct columns names
      
      # Remove from Data_Shape records not found in the MODIS ROI data
      
      Data_Shape <- Data_Shape[which(Data_Shape$OBJECTID %in% unique(ts_data$OBJECTID)), 
                               ]
      Data_Shape$Area_HA <- as.numeric(as.character(Data_Shape$Area_HA))
      Data_Shape <- arrange(Data_Shape, desc(Area_HA))
      
      # Compute the FireYear and Area variable for each fire (From the
      # sahpefile data) and add it to the DataFrame
      
      FireYear    <- numeric(length(ts_data$OBJECTID))
      Area_All    <- numeric(length(ts_data$OBJECTID))
      Area_Forest <- numeric(length(ts_data$OBJECTID))
      IDS         <- unique(Data_Shape$OBJECTID)
      
      
      ts_data    <- data.table(ts_data,    key = c("OBJECTID", "Year"))
      Data_Shape <- data.table(Data_Shape, key = "OBJECTID")
      
      ts_data_sub <- ts_data[Year == 2005]
      # Accessory function used to compute Burnt area for each CLC class from
      # the number of pixels in the ROI (Used only for BAs for which Info is not already
      # present in the shapefile
      
      estimate_Area <- function(Data_tmp) {
        
        full_Area  <- 250 * 250 * (length(Data_tmp$N_PIX))/10000
        tot_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(2, 3, 4, 5, 6))]))/10000
        bro_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(2))]))/10000
        con_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(3))]))/10000
        mix_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(4))]))/10000
        scler_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(5))]))/10000
        trans_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(6))]))/10000
        est_Area   <- data.frame(tot_Area = tot_Area, bro_Area = bro_Area, 
                                 con_Area = con_Area, mix_Area = mix_Area, trans_Area = trans_Area,  
                                 scler_Area = scler_Area, full_Area = full_Area)
      }
      
      # setkey(ts_data, "OBJECTID")
      # setkey(Data_Shape, "OBJECTID")
      # Retrieve or compute burned area for each CLC class ----
      # pb <- txtProgressBar(min = 1, max = length(IDS), style = 3)
      for (FireID in 1:length(IDS)) {
        
        # Sys.sleep(0.005)
        # setTxtProgressBar(pb, FireID)
        if (FireID %in% seq(1, 20000, 400)) {
          message("---- Processing Burnt area: ", FireID, "  of: ", length(IDS)," ----")
        }
        
        ID_to_find  <- as.numeric(as.character(IDS[FireID]))
        
        # subdata_shp <- Data_Shape[OBJECTID == ID_to_find]
        subdata_shp <- Data_Shape[J(ID_to_find), nomatch = 0L]
        # FireRows    <- which(ts_data$OBJECTID == ID_to_find)  # Rows of the dataframe corresponding to the selected fire
        
        FireRows    <- ts_data[OBJECTID == ID_to_find, which = TRUE]
        YY          <- subdata_shp$YearSeason[1]  # Retrieve Fire Year
        subdata     <- ts_data_sub[J(ID_to_find), nomatch = 0L] 
        
        # subdata     <- ts_data[J(ID_to_find,YY), nomatch = 0L] 
        
        # Retrieve Total Area burned in considered CLC_Classes
        Area_For <- as.numeric(subdata_shp[, "BroadLeave"] + 
                                 subdata_shp[, "Coniferous"] + 
                                 subdata_shp[, "MixedFores"] + 
                                 subdata_shp[, "Sclerophyl"] + 
                                 subdata_shp[, "Transition"])
        
        # If Area_Forest = 0 then, noinfo in shape and area is estimated from
        # number of pixels.
        # 
        # 
        if (Area_For == 0) {
          est_Area <- estimate_Area(subdata)
          Area_For <- est_Area$tot_Area
          Data_Shape[OBJECTID == ID_to_find, 
                     c("BroadLeave", "Coniferous", "MixedFores", "Sclerophyl","Transition")] <- 
            est_Area[,2:6]
          
          # Data_Shape[OBJECTID == ID_to_find]$BroadLeave <- est_Area$bro_Area
          # Data_Shape[OBJECTID == ID_to_find]$Coniferous <- est_Area$con_Area
          # Data_Shape[OBJECTID == ID_to_find]$MixedFores <- est_Area$mix_Area
          # Data_Shape[OBJECTID == ID_to_find]$Sclerophyl <- est_Area$scler_Area
          # Data_Shape[OBJECTID == ID_to_find]$Transition <- est_Area$trans_Area
        }
        
        Area_Tot <- subdata_shp$Area_HA[1]  # Retrieve Total Area burned
        
        # If Area_Tot = 0 then, noinfo in shape and area estimated from number of pixels.
        if (Area_Tot == 0) {
          Area_All <- est_Area$full_Area
        }  
        
        FireYear[FireRows]    <- YY        # Assign FireYear 
        Area_Forest[FireRows] <- Area_For  # Assign total area burnt in forest land cover types
        Area_All[FireRows]    <- Area_Tot  # Assign total area burnt in forest land cover types
        
      }
      
      # Add retrieveQd area and FireYears to "Data
      ts_data$FireYear    <- FireYear    # Assign FireYear
      ts_data$Area_All    <- Area_All    # Assign total area of the intersectr
      ts_data$Area_Forest <- Area_Forest # Assign total area burnt in forest land cover types
      
      # Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1,
      # ncol(Data),5:(ncol(Data)-3))] #Reorder Columns
      
      # Reclass the values of CORINE using the same scheme used in the EFFIS ----
      # data base
      
      ts_data$CLC_Class <- factor(ts_data$CLC_Class, levels = c(0:10), 
                                  labels = c("Artificial Surfaces", "Agricultural Areas", 
                                             "Broadleaved Forests", "Coniferous Forests", "Mixed Forests", 
                                             "Schlerophyllus Vegetation", "Transitional Vegetation", 
                                             "Other Natural Land", "Wetlands", "Water Bodies", "Other"))
      
      # ts_data$ENV_ZONE <- factor(ts_data$ENV_ZONE, levels = c(1:13), labels = c("ALN", 
      #                                                                           "BOR", "NEM", "ATN", "ALS", "CON", "ATC", "PAN", "LUS", 
      #                                                                           "ANA", "MDM", "MDN", "MDS"))
      # 
      # recode(Data$ENV_ZONE , # Recode ENV_ZONE according to legend '1 =
      # 'ALN'; 2 = 'BOR'; 3 = 'NEM'; 4 = 'ATN'; 5 = 'ALS'; 6 = 'CON'; 7 =
      # 'ATC'; 8 = 'PAN'; 9 = 'LUS'; 10 = 'ANA'; 11 = 'MDM'; 12 = 'MDN'; 13 =
      # 'MDS'' , as.factor.result = TRUE)
      
      # Add attributes to the Data Frame (Useful to keep track of processing
      # options!)
      
      # attr(Data, 'SVI_File') = SVI_File ;
      
      # Add some attributyes to "Data" to allow a-posteriori knowledge on ----
      # the conductyed processing 
      
      # attr(ts_data, "Shape_File") <- Shape_File  #; \tattr(Data, 'CSV_File') = Out_IDL_CSV
      # attr(ts_data, "CLC_File")   <- CLC_File_00
      # attr(ts_data, "Processing Date") <- Sys.Date()
      # attr(ts_data, "Start_Year") <- Start_Year
      # attr(ts_data, "End_Year")   <- End_Year
      # attr(ts_data, "Index")      <- "Med_SNDVI"
      
      # Save the computed Data Frames in "Out_RData" ----
      
      print(paste("-> Saving TS info RData file"))
      save(ts_data, Data_Shape, file = Out_RData)
      
      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(ts_data, BAreas_shp)
      gc()
    } else {
      
      # Process areas burned multiple times ----
      #- -
      
      # BAreas_Name     <- strsplit(basename(Shape_File), ".shp")[[1]]  # Open shapefile of areas burned multiple times
      # BAreas_Dir      <- dirname(Shape_File)  # (Used to determine burnt surface in the overlaps )
      # BAreas_shp_mult <- readOGR(BAreas_Dir, BAreas_Name)
      # 
      BAreas_shp_mult <-as(st_read(Shape_File, stringsAsFactors = FALSE, quiet = TRUE),"Spatial")
      Data_Shape_mult <- BAreas_shp_mult@data  # Get attributes data from the shp
      
      # Load the statistics file computed by IDL (Derived from Application of
      # FRG_ROI_STAT_ERODE.pro) Data = read.csv(Out_IDL_CSV,header = TRUE,
      # na.strings = FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE)
      # Data[Data == FRG_Options$No_Data_Out_Rast] = NA # Put the NODATA to
      # 'R' NA n_Years = (length(names(Data)) - 4)/2
      # Data <- read.csv(Out_IDL_CSV)
      # Data <- select(Data, -X)
      n_Years            <- length(unique(ts_data$Year))
      names(ts_data)[1]  <- "OVERLAP_ID"
      ts_data$OVERLAP_ID <- as.factor(ts_data$OVERLAP_ID)
      
      
      # names(Data)[5:(5+n_Years-1)] =
      # as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))) #
      # Correct columns names names(Data)[(5+n_Years):(5+2*n_Years-1)] =
      # paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))),
      # sep = '_') # Correct columns names
      
      # Load the LUT data relating each overlap with the corresponding
      # original BAs
      Data_LUT <- read.csv2(file = LUT_File_Multiple, stringsAsFactors = FALSE, 
                            header = TRUE, sep = ";")  # Restore the LUT
      # Create a new empty Data_Shape object. This will be filled using info
      # from the ROIS
      Data_Shape    <- droplevels(Data_Shape[0, ])
      names(Data_Shape)[1] <- "OVERLAP_ID"
      names         <- names(Data_Shape)
      n_recs        <- length(unique(ts_data$OVERLAP_ID))
      tmp_df        <- as.data.frame(array(NA, dim = c(n_recs, length(names))))
      names(tmp_df) <- names
      Data_Shape    <- tmp_df
      
      # Set the Data_Shape OVERLAPID to the values derived from the ROIS
      Data_Shape$OVERLAP_ID <- unique(ts_data$OVERLAP_ID)
      Data_Shape$FireYear   <- "Multiple"
      Data_Shape$YearSeason <- "Multiple"
      Data_Shape$FireDate   <- "Multiple"
      
      # Compute the FireYear and Area variable for each fire (From the
      # sahpefile data) and add it to the DataFrame FireYear corresponds to
      # the FIreYear of the earlier fire that originated the overlap
      estimate_Area <- function(Data_tmp) { 
        # Accessory function used to compute Burnt area for each CLC class from 
        # the number of pixels in the ROI Used ALWAYS for areas burned multiple 
        # times, since no Info is already available regarding the area burned 
        # in different classes within an overlap 
        
        full_Area  <- 250 * 250 * (length(Data_tmp$N_PIX))/10000 
        Area_For   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(2, 3, 4, 5, 6))]))/10000 
        bro_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(2))]))/10000 
        con_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(3))]))/10000 
        mix_Area   <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(4))]))/10000 
        scler_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(5))]))/10000 
        trans_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in%  
                                                                 c(6))]))/10000 
        est_Area  <- data.frame(Area_For = Area_For, bro_Area = bro_Area,  
                                con_Area = con_Area, mix_Area = mix_Area, trans_Area = trans_Area,  
                                scler_Area = scler_Area, full_Area = full_Area) 
      } 
      
      
      FireYear    <- numeric(length(ts_data$OVERLAP_ID))
      Area_All    <- numeric(length(ts_data$OVERLAP_ID))
      Area_Forest <- numeric(length(ts_data$OVERLAP_ID))
      IDS         <- unique(ts_data$OVERLAP_ID)
      ts_data     <- data.table(ts_data, key = "OVERLAP_ID", "Year")
      Data_Shape  <- data.table(Data_Shape, key = "OVERLAP_ID")
      ts_data_sub <- ts_data[Year == 2005]
      
      
      # pb <- txtProgressBar(min = 1, max = length(IDS), style = 3)
      for (FireID in 1:length(IDS)) {
        
        # Sys.sleep(0.005)
        # setTxtProgressBar(pb, FireID)
        if (FireID %in% seq(1, 20000, 400)) {
          message("---- Processing Burnt area: ", FireID, "  of: ", length(IDS)," ----")
        }
        
        ID_to_find  <- as.numeric(as.character(IDS[FireID]))
        
        subdata_shp <- Data_Shape[OVERLAP_ID == ID_to_find]
        # FireRows    <- which(ts_data$OVERLAP_ID == ID_to_find)  # Rows of the dataframe corresponding to the selected fire
        FireRows    <- ts_data[OVERLAP_ID == ID_to_find, which = TRUE]
        # Retrieve Total Area burned in considered CLC_Classes
        Area_For <- as.numeric(subdata_shp[, "BroadLeave"] + 
                                 subdata_shp[, "Coniferous"] + 
                                 subdata_shp[, "MixedFores"] + 
                                 subdata_shp[, "Sclerophyl"] + 
                                 subdata_shp[, "Transition"])
        
        
        
        # Rows of the dataframe corresponding to the selected fire
        # FireRows <- which(ts_data$OVERLAP_ID == IDS[FireID])
        # Get fire year of the first fire originating the overlap
        min_FireYear <- min(Data_LUT[which(Data_LUT$OVERLAP_ID == 
                                             ID_to_find), ]$YearSeason)  
        YY      <- min_FireYear  # Set Fire Year to the year of the first fire
        # subdata <- ts_data_sub[OVERLAP_ID == ID_to_find & Year == YY] 
        subdata     <- ts_data_sub[J(ID_to_find), nomatch = 0L] 
        
        # Estimate burned area in the different LC classes, as the product of the
        # number of pixel*250*250
        est_Area <- estimate_Area(subdata)
        # where_id <- which(Data_Shape$OVERLAP_ID == IDS[FireID])
        Data_Shape[OVERLAP_ID == ID_to_find]$Area_HA  <- 
          Data_Shape_mult$Area_Int[BAreas_shp_mult$OVERLAP_ID ==  ID_to_find][1]  # Area burned in all land cover types
        # Data_Shape[OVERLAP_ID == ID_to_find]$BroadLeave <- est_Area$bro_Area
        # Data_Shape[OVERLAP_ID == ID_to_find]$Coniferous <- est_Area$con_Area
        # Data_Shape[OVERLAP_ID == ID_to_find]$MixedFores <- est_Area$mix_Area
        # Data_Shape[OVERLAP_ID == ID_to_find]$Sclerophyl <- est_Area$scler_Area
        # Data_Shape[OVERLAP_ID == ID_to_find]$Transition <- est_Area$trans_Area
        
        Data_Shape[OVERLAP_ID == ID_to_find, 
                   c("BroadLeave", "Coniferous", "MixedFores", "Sclerophyl","Transition")] <- 
          est_Area[,2:6]
        
        # Assign FireYear
        FireYear[FireRows]  <- YY
        
        # Assign total area burnt in forest land cover types
        Area_Forest[FireRows] <- est_Area$Area_For
        # Assign total area burnt in forest land cover types - it is the area of the intersect considered !
        Area_All[FireRows]    <- Data_Shape_mult$Area_Int[
          BAreas_shp_mult$OVERLAP_ID == ID_to_find][1]  
      }
      
      ts_data$FireYear    <- FireYear  # Assign FireYear
      ts_data$Area_All    <- Area_All  # Assign total area of the intersect
      ts_data$Area_Forest <- Area_Forest  # Assign total area burnt in forest land cover types
      
      # Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1,
      # ncol(Data),5:(ncol(Data)-3))] # Reorder Columns
      
      # Reclass the values of CORINE using the same scheme used in the EFFIS
      # data base
      
      ts_data$CLC_Class <- factor(ts_data$CLC_Class, levels = c(0:10), 
                                  labels = c("Artificial Surfaces", "Agricultural Areas", 
                                             "Broadleaved Forests", "Coniferous Forests", "Mixed Forests", 
                                             "Schlerophyllus Vegetation", "Transitional Vegetation", 
                                             "Other Natural Land", "Wetlands", "Water Bodies", "Other"))
      
      ts_data$ENV_ZONE <- factor(ts_data$ENV_ZONE, levels = c(1:13), labels = c("ALN", 
                                                                                "BOR", "NEM", "ATN", "ALS", "CON", "ATC", "PAN", "LUS", 
                                                                                "ANA", "MDM", "MDN", "MDS"))
      
      # Add attributes to the Data Frame (Useful to keep track of processing
      # options!)
      
      # attr(ts_data, "SVI_File") <- SVI_File
      # attr(ts_data, "Shape_File") <- Shape_File
      # attr(ts_data, "CSV_File") <- Out_IDL_CSV
      # attr(ts_data, "CLC_File") <- CLC_File_00
      # attr(ts_data, "Processing Date") <- Sys.Date()
      # attr(ts_data, "Start_Year") <- Start_Year
      # attr(ts_data, "End_Year") <- End_Year
      # if (length(grep("RDVI", SVI_File)) > 0) 
      # {
      #   attr(ts_data, "Index") <- "RDVI"
      # }  # to be removed !
      # if (length(grep("NDVI", SVI_File)) > 0) 
      # {
      #   attr(ts_data, "Index") <- "NDVI"
      # }  # to be removed !
      # if (length(grep("SNDVI", SVI_File) > 0)) {
      #   attr(ts_data, "Index") <- "SNDVI"
      # }
      # if (length(grep("SRDVI", SVI_File) > 0)) {
      #   attr(ts_data, "Index") <- "SRDVI"
      # }
      # if (length(grep("Med_SNDVI", SVI_File) > 0)) {
      #   attr(ts_data, "Index") <- "Med_SNDVI"
      # }
      # if (length(grep("Med_SRDVI", SVI_File)) > 0) {
      #   attr(ts_data, "Index") <- "Med_SRDVI"
      # }
      
      # Save the computed Data Frames
      
      print(paste("-> Saving TS info RData file"))
      save(ts_data, Data_Shape, file = Out_RData)
      
      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(ts_data, BAreas_shp)
      gc()
      
    }
  } else {
    message("---- -> RData file joining MODIS and Shapefile info already existing: ", 
            basename(Out_RData), "----")
  }
  
  # Completed
  return("DONE")
}
