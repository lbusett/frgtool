#' @title FRG_Comp_Sig_Matrix
#' 
#' @description Function used to perform the Wilcoxon test of significant differences, using differnt number of 'before fire years' as reference \cr
#' The function creates and saves matrixes of p-values obtained using differnt number of before fire years. Matrix is saved within the 'p-matrixes' subfolder \cr
#' of the folder containing the final output file. Additional info necessary to plot the time series (i.e., boxplots, objectids of fires, fireyears, ecc) \ cr
#' are also saved in the same file (plot_stat data frame). 
#' 
#' @details
#'
#' @param In_File string File of scaled VI Time Series extracted for single-fire areas
#' @param out_file_pmat string Output file name where the p-values matrix is saved
#' @param min_pix numeric Minimum number of CORE pixels for a bunt area to be considered
#' @param perc_diff numeric Minimum percentage difference checked for significant reduction 
#' @param MedWdt numeric Number of pre-fire years considered as reference
#' @param sub_zones flag If 1 , consider ENV_ZONES differences when analyzing
#' @param sig_level numeric Significance level used for Wilcoxon test 
#' @param erode flag If 1, perform analysis only on NON CORE burnt pixels 
#'
#' @return Saves a p_matrix.RData file containing the p-values matrix and the 'plot_stats' data frame in the 'p_matrixes' subfolder
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' @importFrom data.table rbindlist setkey 
#' @importFrom abind abind
#' @importFrom dplyr summarise filter select group_by
#' @importFrom gdata drop.levels
#' 
frg_wilcox <- function(In_File = In_File, out_file_pmat = out_file_pmat, 
                       min_pix = min_pix, perc_diff = perc_diff, MedWdt = MedWdt, sub_zones = sub_zones, 
                       erode = erode) {
  
  median_analysis <- TRUE
  Out_File_R <- out_file_pmat
  
  #- ---------------------------------------------------- - 
  # Get time series data from the input file ----
  #- ---------------------------------------------------- - 
  
  get(load(In_File))
  
  # ts_data <- select(ts_data, -ENV_ZONE)
  
  # Initialization and Preliminary pre elaborations ----
  counter <- 1
  
  # Select 'Interesting' CLC Classes
  sel_levels    <- c("Schlerophyllus Vegetation", "Broadleaved Forests", 
                     "Coniferous Forests", "Mixed Forests", "Transitional Vegetation")
  ts_data       <- gdata::drop.levels(subset(ts_data, CLC_Class %in% sel_levels))  # Remove unneeded CLC classes
  ts_data$N_PIX <- as.factor(paste("p", ts_data$N_PIX, sep = "_"))  # Convert N_PIX to factor and add a 'p' before the number
  n_fires       <- length(unique(ts_data$OBJECTID))  # Number of records to be analyzed (total number of FIRES in both burned areas shapefile and MODIS ROI )
  
  # Get ancillary data regarding fires from the EFFIS shape file ----
  
  Data_Shape <- droplevels(subset(Data_Shape, OBJECTID %in% unique(ts_data$OBJECTID)))  # Remove fires not 'present' in the MODIS dataset\t\t\t\t\t
  recs       <- unique(Data_Shape$OBJECTID)  # Count the remaining fires
  n_recs     <- length(recs)  # Number of fires to process
  
  ts_data <- ts_data[complete.cases(ts_data), ]
  ts_data_Melted <- ts_data
  
  # If present, remove
  # the data regarding year 2000 (Strong uncertainties in MODIS data of
  # this year !!!!
  
  # drop.levels(subset(Data_Melted,Year != '2000')) 
  
  # Get list of available years Number of years in the time series
  Start_Year  <- min(ts_data_Melted$Year)  # Starting year of the time serie
  End_Year    <- max(ts_data_Melted$Year)  # Ending year of the time serie
  N_Years     <- 1 + as.numeric(End_Year) - as.numeric(Start_Year)
  Avail_Years <- seq(Start_Year, End_Year, 1)
  
  #- ---------------------------------------------------- - 
  #- #Initialize output matrixes ----
  #- ---------------------------------------------------- - 
  
  plot_stat_names <- c("CASE_ID", "OBJECTID", "FireYear", "YearFromFire", 
                       "Area_All", "Area_Forest", "Area_CLC",  "CLC_Class", 
                       "N_PIX", "Time_Signif", "Year", "YearDiff", "low_ci", "mean", "hi_ci", 
                       "std_dev", "low_box", "X25th", "median", "X75th", "up_box")
  
  # Define the output matrixes that will contain information on p-values
  # of wilcoxon rank sum tests.
  
  max_plus_years  <- N_Years - 3  #To compute the limits of the p-values full matrixes: 
  max_minus_years <- -(N_Years - 1)  #To compute the limits of the p-values full matrixes
  
  p_matrix_median <- NULL
  plot_stat       <- NULL  # Initialize output variables
  
  # Define an empty matrix with correct dimensions to store the results of 
  # the wilcoxon comparisons - using medians 
  p_matrix_median_empty <- array(data = NA, 
                                 dim = c(max_plus_years - max_minus_years, 
                                         max_plus_years - max_minus_years))
  colnames(p_matrix_median_empty) <- seq(max_minus_years, max_plus_years - 
                                           1, 1)
  rownames(p_matrix_median_empty) <- seq(max_minus_years + 1, max_plus_years, 1)
  
  #- ---------------------------------------------------- - 
  # Start Analysis on single fires ----
  # FirePix is a counter on fires. Fires are analyzed ordered
  # by total area according to the EFFIS shapefile 
  #- ---------------------------------------------------- - 
  
  case_ID <- 0
  
  ts_data_Melted$YearDiff <- (as.numeric(as.character(ts_data_Melted$Year)) - 
                                as.numeric(ts_data_Melted$FireYear))  # Compute the 'Years from fire' variable.
  ts_data_Melted <- data.table(ts_data_Melted)  # cONVERT TO A DATA.TABLE to improve speed !!!
  # setkey(ts_data_Melted, OBJECTID, CLC_Class, ENV_ZONE)  # Set the 'keys' for the table. 
  setkey(ts_data_Melted, OBJECTID, CLC_Class)  # Set the 'keys' for the table. 
  
   for (FirePix in 1:n_fires) {
    # for (FirePix in 1:200){
    if ((FirePix/100) - floor(FirePix/300) == 0) {
      message("---- -> Analysing Burnt Area: ", FirePix, " of: ", n_fires, " ----")
    }
    
  FireCode         <- as.character(recs[FirePix])  # Getrr OBJECTID of the fire to be analyzed
  Data_Fire        <- droplevels(ts_data_Melted[J(FireCode)])  # Get Data of the selected fire
  Data_Fire_Shape  <- subset(Data_Shape, OBJECTID == FireCode)
    
  YearDiffs        <- (unique(Data_Fire$YearDiff))  # get levels of 'Difference' with fire year - full and before fire
  YearDiffs_Before <- YearDiffs[which(YearDiffs < 0)]
    
    if (length(Data_Fire$FireYear) != 0 & is.finite(Data_Fire$FireYear[1])) {
      
      # control on number of records for selected fire
      FireYear     <- Data_Fire$FireYear[1]  # Get year of occurrence of the selected fire\t\t
      Area_All     <- Data_Fire$Area_All[1]  # Get total burnt Area
      Area_Forest  <- Data_Fire$Area_Forest[1]  # Get total in forest CLC types
      YearFromFire <- Start_Year - FireYear  # Compute years from fire at the beginning of the time serie for the selected fire \t
      
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
          
          Data_Class_check <- Data_Class %>% group_by(N_PIX) %>% 
            summarise(lgtyy = length(Index)) %>% filter(lgtyy == 
                                                          N_Years) %>% select(N_PIX)
          Data_Class <- subset(Data_Class, Data_Class$N_PIX %in% 
                                 Data_Class_check$N_PIX)
          n_pix <- length(unique(Data_Class$N_PIX))  # Check to see if at least min_pix pixels are 'available' for the selected CLC class and Zone in the selected fire
          case_ID <- case_ID + 1
          
          # if number of pixels grater than minimum , perform analysis ----
          if (n_pix >= min_pix) {
            
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
            
            if (median_analysis == T) {
              
              #- ---------------------------------------------------- - 
              # Creation of the 'means' matrix. ----
              # Replace data of the 'before' years with medians computed 
              # considering 1 year, 2 years, etcetera.
              #- ---------------------------------------------------- - 
              
              Data_Class_med <- Data_Class
              
              # Start of cycle on: Before_Year
              for (Before_Year in YearDiffs_Before) {
                
                # Get before fire data up to 'Before_Year' 
                Data_Before <- droplevels(Data_Class[YearDiff >= Before_Year & YearDiff < 0])  
                # Compute means per pixels of the before years
                mean_before <- as.integer(Data_Before[, 
                                                      list(mean = mean(Index, na.rm = T)), 
                                                      by = list(N_PIX)]$mean)  
                Data_Class_med[YearDiff == Before_Year]$Index <- mean_before  # Replace the original values with the means of before years
              }  # End of cycle on: Before_Year
              
              #- ---------------------------------------------------- - 
              # Statistical analysis ----- 
              # Apply wilcoxon test to compute the p-valuesmatrixes - check 
              # differences with respect to means of before years
              #- ---------------------------------------------------- - 
              
              #Define an empty matrix with correct dimensions to store the results
              p_matrix_median_tmp <- p_matrix_median_empty  
              
              paired_wtest_median <- try(pairwise.wilcox.test(Data_Class_med$Index, 
                                                              Data_Class_med$YearDiff, paired = TRUE, alternative = "less", 
                                                              p.adjust.method = "none", mu = -perc_diff))  # Compute p-values
              
              if (class(paired_wtest_median) == "try-error") {
                browser()
              }
              
              #- ---------------------------------------------------- - 
              # Put results in the cumulated matrix ----
              # Assign the 'CASE_ID' as dimname of the 3rd dimension for easy referencing
              #- ---------------------------------------------------- - 
              #
              p_matrix_median_tmp[attributes(paired_wtest_median$p.value)[2]$dimnames[[1]], 
                                  attributes(paired_wtest_median$p.value)[2]$dimnames[[2]]] <- paired_wtest_median$p.value
              
              p_matrix_median <- try(abind(p_matrix_median, p_matrix_median_tmp, 
                                           along = 3))
              if (class(p_matrix_median) == "try-error") {
                # browser()
              }
              dimnames(p_matrix_median)[[3]][dim(p_matrix_median)[3]] <- case_ID
              
            }
            
            #- ---------------------------------------------------- - 
            #- Compute data to be saved to allow plotting (CIs + boxplot thresholds -----
            #- ---------------------------------------------------- - 
            #
            int_data <- array(999, dim = c(length(Avail_Years), 3))
            
            # Compute quantiles of distribution for different years
            
            box_data <- t(boxplot(Index ~ YearDiff, data = Data_Class, plot = FALSE)$stats)
            sdev_data <- Data_Class[, list(sd = sd(Index, na.rm = TRUE)), 
                                    by = list(YearDiff)]
            
            # Fill-in the output matrix for the analyzed fire.
            
            plot_stat_tmp <- as.data.frame(cbind(case_ID, FireCode, 
                                                 FireYear, YearFromFire, Area_All, Area_Forest, Area_CLC, 
                                                 as.character(Class), n_pix, Time_Signif = "Yes", 
                                                 Avail_Years, YearDiffs, int_data, sdev_data$sd, box_data), 
                                           stringsAsFactors = FALSE)
            
            names(plot_stat_tmp) <- plot_stat_names
            plot_stat[[counter]] <- plot_stat_tmp  # Add the results for the selected fire to the full output matrix
            
            counter <- counter + 1
          } else {
            # If n_pix too low, the fire is skipped. Neither 'plot_stat' nor the
            # p-values matrixes are filled
            
          }
          
        }  # End of cycle on ENV_Zones  for a single fire
        
        # } # End of cycle on CLC classes for a single fire
        
      } else { # End of check for sufficient number of years before and after fire
        case_ID <- case_ID + 1  
      } 
    }  # End of IF on sufficient data on the analyzed fire
    

    
  }  # End of Cycle on Fires
  browser()
  plot_stat <- rbindlist(plot_stat)
  
  # browser() Convert the columns to the correct formats for (cc_df in
  # c(5,6,7,10, 14:22)) plot_stat[, cc_df, with = FALSE] =
  # as.numeric(plot_stat[,cc_df, with = FALSE]) for (cc_df in
  # c(2,3,8,9,11)) plot_stat [,cc_df, with = FALSE] = as.factor(plot_stat
  # [,cc_df, with = FALSE]) for (cc_df in c(1,4,12,13)) plot_stat
  # [,cc_df, with = FALSE] = as.ordered(as.numeric(plot_stat [,cc_df,
  # with = FALSE]))
  
  # plot_stat <- rbindlist(plot_stat)
  
  plot_stat <- plot_stat[, CASE_ID      := as.ordered(as.numeric(CASE_ID))]
  plot_stat <- plot_stat[, OBJECTID     := as.factor(OBJECTID)]
  plot_stat <- plot_stat[, FireYear     := as.factor(FireYear)]
  plot_stat <- plot_stat[, YearFromFire := as.ordered(as.numeric(YearFromFire))]
  plot_stat <- plot_stat[, Area_All     := as.numeric(Area_All)]
  plot_stat <- plot_stat[, Area_Forest  := as.numeric(Area_Forest)]
  plot_stat <- plot_stat[, Area_CLC     := as.numeric(Area_CLC)]
  # plot_stat <- plot_stat[, ENV_ZONE     := as.factor(ENV_ZONE)]
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
  
  
  save(plot_stat, p_matrix_median, file = Out_File_R)
  gc(verbose = FALSE)  #Garbage collection
  
  return("DONE")
  
}
