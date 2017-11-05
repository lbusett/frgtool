#' @title frg_plotstat_multiple 
#' @description Function used extract the info necessary to plot the Med_SVI 
#'  time series (i.e., boxplots, overlapid of BA, fireyears, ecc) for areas
#'  burnt multiple times
#' @param in_file string File of scaled VI Time Series extracted for multiple-fire
#'  areas
#' @param out_file string Basename for output files
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @importFrom dplyr select
#' @return Saves a .RData file containing data needed to plot Med_SVI time series
#'  for the different multiple-fire BAs
#' @rdname frg_plotstat_multiple
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom dplyr select group_by summarise filter
#' @importFrom data.table data.table rbindlist ":="
#' @importFrom graphics boxplot
#' @importFrom stats complete.cases sd
#' @importFrom magrittr "%>%"
#' 
frg_plotstat_multiple <- function(in_file,
                                  out_file, 
                                  opts) {
  
  CLC_Class <- OVERLAP_ID <- ENV_ZONE <- Year <- J <- N_PIX <- 
    Index <- lgtyy <- CASE_ID <- Time_Signif <- low_ci <-std_dev <- 
    low_box <- X25th <- median <- X75th <- up_box <- YearDiff <- 
    hi_ci <- NULL
  # Load data and define processing options.
  
  Out_File_R <- out_file
  
  #- ---------------------------------------------------- - 
  # Get time series data from the input file ----
  #- ---------------------------------------------------- - 
  
  # Status Message
  message("") 
  message("- ---------------------------------------------------- -")
  message("- Extract plotting data on areas burned multiple times -")
  message("- ---------------------------------------------------- -")
  message("")
  message(paste("---- -> In File for Analysis: ", in_file), " ----")
  message(paste("---- -> Out File for Analysis: ", out_file), " ----")
  message("- ---------------------------------------------------- -")
  load(in_file)
  
  # Retrieve time series data if(erode == 0) { # Get data in the case
  # that erode = 0 Data = Data [,1:(8+N_Years)] } else { # Get data in
  # the case that erode = 1 names = names(Data)[1:(8+N_Years)] Data =
  # Data [,c(1:7,(8+N_Years+1):(8+2*N_Years+1))] names(Data)= names }
  
  # Initialization and Preliminary pre elaborations ----
  counter    <- 1
  
  # Select 'Interesting' CLC Classes
  sel_levels <- c("Schlerophyllus Vegetation", "Broadleaved Forests", 
                  "Coniferous Forests", "Mixed Forests", "Transitional Vegetation")
  # Remove unneeded CLC classes
  ts_data       <- droplevels(subset(ts_data, CLC_Class %in% sel_levels))
  # Convert N_PIX to factor and add a 'p' before the number
  ts_data$N_PIX <- as.factor(paste("p", ts_data$N_PIX, sep = "_"))  
  # Number of records to be analyzed (total number of FIRES in both
  # burned areas shapefile and MODIS ROI )
  n_fires    <- length(unique(ts_data$OVERLAP_ID))
  
  # Start and end indexes of 'years' columns
  # st_ind     <- 8
  # end_ind    <- length(names(ts_data))
  
  # Get ancillary data regarding fires from the EFFIS shape file ----
  
  # Remove fires not 'present' in the MODIS dataset
  Data_Shape  <- droplevels(subset(Data_Shape, OVERLAP_ID %in% unique(ts_data$OVERLAP_ID)))
  recs        <- unique(Data_Shape$OVERLAP_ID)  # Count the remaining fires
  
  # n_recs      <- length(recs)  # Number of fires to process
  # Reshape data structure to facilitate the analyis (melting)
  # and get info about the data (e.g., number of years, ecc)
  
  ts_data  <- dplyr::select(ts_data, -ENV_ZONE)
  ts_data  <- ts_data[stats::complete.cases(ts_data), ]
  # Data_Melted = melt(Data, id.vars = c(1:(st_ind-1)))
  # names(Data_Melted)[st_ind:(st_ind+1)] = c('Year', 'Index')
  Data_Melted <- ts_data
  
  # If present, remove the data regarding year 2000 (Strong uncertainties in
  # MODIS data of this year !!!!
  Data_Melted <- droplevels(subset(Data_Melted, Year != '2000')) 
  
  Start_Year  <- min(Data_Melted$Year)  # Starting year of the time serie
  End_Year    <- max(Data_Melted$Year)  # Ending year of the time serie
  N_Years     <- 1 + as.numeric(End_Year) - as.numeric(Start_Year)
  Avail_Years <- seq(Start_Year, End_Year, 1)
  
  #- --------------------------------------------------------------------------- -#
  #- #Initialize output matrixes
  #- --------------------------------------------------------------------------- -#
  
  plot_stat_names <- c("CASE_ID", "OVERLAP_ID", "FireYear", "YearFromFire", 
                       "Area_All", "Area_Forest", "Area_CLC", "CLC_Class", 
                       "N_PIX", "Time_Signif", "Year", "YearDiff", "low_ci", "mean", "hi_ci", 
                       "std_dev", "low_box", "X25th", "median", "X75th", "up_box")
  plot_stat       <- NULL  # Initialize output variables
  #- ---------------------------------------------------- - #
  # Start Analysis on single fires. FirePix is a counter on fires. ?
  # Fires are analyzed ordered by total area according to the EFFIS
  # shapefile ?
  #- ---------------------------------------------------- - #
  
  case_ID              <- 0
  # Compute the 'Years from fire' variable.
  Data_Melted$YearDiff <- (as.numeric(as.character(Data_Melted$Year)) - 
                             as.numeric(Data_Melted$FireYear)) 
  # cONVERT TO A DATA.TABLE to improve speed !!!
  Data_Melted          <- data.table::data.table(Data_Melted)  
  # Set the 'keys' for the table. 
  setkey(Data_Melted, OVERLAP_ID, CLC_Class)  
  
  for (FirePix in 1:n_fires) {
    # Getrr OBJECTID of the fire to be analyzed
    FireCode         <- recs[FirePix]  
    # Get Data of the selected fire
    Data_Fire        <- droplevels(Data_Melted[J(FireCode)])  
    Data_Fire_Shape  <- subset(Data_Shape, OVERLAP_ID == FireCode)
    # get levels of 'Difference' with fire year - full and before fire
    YearDiffs        <- (unique(Data_Fire$YearDiff))
    YearDiffs_Before <- YearDiffs[which(YearDiffs < 0)]
    
    if (length(Data_Fire$FireYear) != 0 & is.finite(Data_Fire$FireYear[1])) {
      # control on number of records for selected fire
      # Get year of occurrence of the selected fire
      FireYear     <- Data_Fire$FireYear[1]
      # Get total burnt Area
      Area_All     <- Data_Fire$Area_All[1]
      # Get total in forest CLC types
      Area_Forest  <- Data_Fire$Area_Forest[1]
      # Compute years from fire at the beginning of the time serie for the selected fire 
      YearFromFire <- Start_Year - FireYear
      
      # Limit the analysis to fires occurred at least three years later than
      # the start of the time series AND at least one year before its end
      if ((FireYear - Start_Year >= 3) & (FireYear < End_Year)) {
        
        CLC_classes <- c(levels(Data_Fire$CLC_Class), "All")  # Get array of CLC classes present in the selected fire Fire
        
        # Start cyclying on CLC classes 'available' in the selected fire
        
        for (Class in CLC_classes) {
          
          if (Class != "All") {
            Data_Class <- droplevels(Data_Fire[CLC_Class == Class])
          } else {
            Data_Class <- Data_Fire
          }
          
          Data_Class_check <- Data_Class %>% 
            dplyr::group_by(N_PIX) %>% 
            dplyr::summarise(lgtyy = length(Index)) %>% 
            dplyr::filter(lgtyy == N_Years) %>% 
            dplyr::select(N_PIX)
          
          Data_Class <- subset(Data_Class, Data_Class$N_PIX %in% Data_Class_check$N_PIX)
          # Check to see if at least min_pix pixels are 'available' for the selected 
          # CLC class and Zone in the selected fire
          
          n_pix <- length(unique(Data_Class$N_PIX))  
          case_ID <- case_ID + 1
          
          if (n_pix >= opts$min_pix) {
            # if number of pixels grater than minimum , perform analysis
            
            # Compute the 'Area_CLC' class, i.e., the area burnt in each CLC class
            # for the analyzed fire
            if (Class == "All") {
              Area_CLC <- Area_Forest
            }
            if (Class == "Broadleaved Forests") {
              Area_CLC <- Data_Fire_Shape[["BroadLeave"]]
            }
            if (Class == "Coniferous Forests") {
              Area_CLC <- Data_Fire_Shape[["Coniferous"]]
            }
            if (Class == "Mixed Forests") {
              Area_CLC <- Data_Fire_Shape[["MixedFores"]]
            }
            if (Class == "Transitional Vegetation") {
              Area_CLC <- Data_Fire_Shape[["Transition"]]
            }
            if (Class == "Schlerophyllus Vegetation") {
              Area_CLC <- Data_Fire_Shape[["Sclerophyl"]]
            }
            
            #- --------------------------------------------------------------------------- -#
            #- Compute data to be saved to allow plotting ----
            #- --------------------------------------------------------------------------- -#
            
            int_data  <- array(999, dim = c(length(Avail_Years), 3))
            # Compute quantiles of distribution for different years
            
            box_data  <- t(
              graphics::boxplot(Index ~ YearDiff, data = Data_Class, plot = FALSE)$stats
            )
            sdev_data <- Data_Class[, list(sd = stats::sd(Index, na.rm = TRUE)),
                                    by = list(YearDiff)]
            
            # Fill-in the output matrix for the analyzed fire.
            
            plot_stat_tmp <- as.data.frame(
              cbind(case_ID, FireCode, 
                    FireYear, YearFromFire, Area_All, Area_Forest, 
                    Area_CLC, as.character(Class), 
                    n_pix, Time_Signif = "Yes", Avail_Years, YearDiffs, 
                    int_data, sdev_data$sd, box_data), stringsAsFactors = FALSE)
            names(plot_stat_tmp) <- plot_stat_names
            # Add the results for the selected fire to the full output matrix
            plot_stat[[counter]] <- plot_stat_tmp  
            counter              <- counter + 1
          } else {
            # If n_pix too low, the fire is skipped. Neither 'plot_stat' nor the
            # p-values matrixes are filled
            
          }
          
        }  # End of cycle on CLC classes for a single fire
        
      } else case_ID <- case_ID + 1  # End of check for sufficient number of years before and after fire
    }
    
    if ((FirePix/250) - floor(FirePix/250) == 0) {
      message("-> Analysing Burnt Area: ", FirePix, " of: ", n_fires)
    }
    
  }  # End of Cycle on Fires

  plot_stat <- data.table::rbindlist(plot_stat)
  
  plot_stat <- plot_stat[, CASE_ID      := as.ordered(as.numeric(CASE_ID))]
  plot_stat <- plot_stat[, OVERLAP_ID   := as.factor(OVERLAP_ID)]
  plot_stat <- plot_stat[, FireYear     := as.factor(FireYear)]
  plot_stat <- plot_stat[, YearFromFire := as.ordered(as.numeric(YearFromFire))]
  plot_stat <- plot_stat[, Area_All     := as.numeric(Area_All)]
  plot_stat <- plot_stat[, Area_Forest  := as.numeric(Area_Forest)]
  plot_stat <- plot_stat[, Area_CLC     := as.numeric(Area_CLC)]
  plot_stat <- plot_stat[, CLC_Class    := as.factor(CLC_Class)]
  plot_stat <- plot_stat[, N_PIX        := as.numeric(N_PIX)]
  plot_stat <- plot_stat[, Time_Signif  := as.factor(Time_Signif)]
  plot_stat <- plot_stat[, Year         := as.ordered(as.numeric(Year))]
  plot_stat <- plot_stat[, YearDiff     := as.ordered(as.numeric(YearDiff))]
  plot_stat <- plot_stat[, low_ci       := as.numeric(low_ci)]
  plot_stat <- plot_stat[, mean         := as.numeric(mean)]
  plot_stat <- plot_stat[, hi_ci        := as.numeric(hi_ci)]
  plot_stat <- plot_stat[, std_dev      := as.numeric(std_dev)]
  plot_stat <- plot_stat[, low_box      := as.numeric(low_box)]
  plot_stat <- plot_stat[, X25th        := as.numeric(X25th)]
  plot_stat <- plot_stat[, median       := as.numeric(median)]
  plot_stat <- plot_stat[, X75th        := as.numeric(X75th)]
  plot_stat <- plot_stat[, up_box       := as.numeric(up_box)]
  
  # save the output in RData format
  save(plot_stat, file = Out_File_R)
  gc(verbose = FALSE)  #Garbage collection
  
  message("- ------------------------------------------------------ -")
  message("- Plotting data extraction and statistical analysis Completed")
  message("- ------------------------------------------------------ -")
  
  
  return("DONE")
  
}
