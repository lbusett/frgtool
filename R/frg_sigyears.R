#' @title FRG_Comp_Sig_Matrix
#' 
#' @description Function used to compute the number of consecutive years with significant VIR reductions for the different BAS, considering the selected number  \cr
#'              of before fire years (opts$MedWdt). Also, joins 'plotting' information computed in FRG_Comp_Sig_Matrix with 'Significance' information \cr
#'              in the 'recov_stat' data frame and computes the Recovery Times. 
#'              All informations derived (plot_stat + recov_stat) from the analysis are saved in the final Statistical Analysis output RData file
#'
#' @details
#'
#' @param file_in string RData file created by FRG_Comp_Sig_Matrix
#' @param file_out string Name of the final Statistical Analysis output RData file 
#' @param opts$MedWdt numeric Number of pre-fire years considered as reference
#' @param opts$sig_level numeric Significance level used for Wilcoxon test 
#'
#' @return Saves a file containing the p-values matrix and the 'plot_stats' data frame in the 'p_matrixes' subfolder
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' 

frg_sigyears <- function(file_in,
                         file_out,
                         opts,
                         median_analysis = TRUE) {
  
  
  load(file.path(file_in))  # Load the statistical analysis results (p-values matrix and plot_stat)
  
  
  
  # Define empty arrays that will contain data related to number of
  # significantly different years and pre-fire stability
  
  Sig_Years_median_tot <- NULL
  
  #- --------------------------------------------------------------------------- -#
  #- Compute number of significant years, using one or more 'before' years as a reference ----
  #- --------------------------------------------------------------------------- -#
  
  # Helper function used to compute the number of consecutive years after
  # the fire showing significant reduction
  
  find_N_Seq <- function(x) {
    
    x[which(is.finite(x) == F)] <- -999  # Convert NODATA to -999
    x <- 1 * x  # From logical to numeric
    if (N == 1) {
      # If considering only one year, build the significance array
      N_GoodCols   <- 1
      GoodRows     <- which(x >= 0)
      N_GoodRows   <- length(GoodRows)
      x            <- x[GoodRows]
      Signif_Array <- x
      
    } else {
      
      # If considering more than one year, build the significance array
      GoodCols   <- which(apply(x, 2, max) >= 0)  # 'Valid' columns are the ones not at NODATA, ie, the columns containing actual data (Not outside the time serie on the left)
      N_GoodCols <- length(GoodCols)
      x          <- x[, GoodCols]  # Get data for valid columns
      GoodRows   <- which(apply(x, 1, max) >= 0)  # 'Valid' rows are the ones not at NODATA, ie, the columns containing actual data (Not outside the time serie on the right)
      N_GoodRows <- length(GoodRows)
      x          <- x[GoodRows, ]  # Get valid data
      
      if (N_GoodRows == 1) {
        Signif_Array <- sum(x, na.rm = T)/N_GoodCols  # Exception for fires occured only since one year
      } else {
        Signif_Array <- rowSums(x, na.rm = T)/N_GoodCols
      }
    }
    
    if (max(Signif_Array, na.rm = T) == 1) {
      # Check if at least one year was significantly lower If first year
      # significantly different
      if (Signif_Array[1] == 1) {
        Signif_FireYear <- 1
        # Number of significant years
        N_Signif        <- min(which(Signif_Array < 1)) - 1 
        if (N_Signif == N_GoodRows | is.finite(N_Signif) == FALSE) {
          # If N_Signif is equal to maximum possible number of years, or is not
          # finite , then Recov = 0
          Recov        <- 0  # and N_Signif equal to maximum possible years (i.e., number of years occurred since the fire)
          N_Signif     <- N_GoodRows
          N_Signif_Rec <- "NR"
          
        } else {
          # If N_Signif is finite and lower than possible maximum , then Recov = 1
          Recov        <- 1  
          N_Signif_Rec <- N_Signif
        }
      } else {
        # Condition to account for possibility that Year 0 is not significant,
        # but the successive years are.
        Signif_FireYear  <- 0
        Signif_array_tmp <- Signif_Array[2:length(Signif_Array)]
        N_Signif         <- min(which(Signif_array_tmp < 1)) - 1
        if (N_Signif == (N_GoodRows - 1) | is.finite(N_Signif) == 
            F) {
          Recov        <- 0
          N_Signif     <- N_GoodRows - 1
          N_Signif_Rec <- "NR"
        } else {
          Recov        <- 1
          N_Signif_Rec <- N_Signif
        }
      }
    } else {
      # If no significantly different years, set N_Signif at 0 and Recov at 1
      N_Signif     <- 0  
      Recov        <- 1
      N_Signif_Rec <- N_Signif
      Signif_FireYear <- 0
    }
    
    return(c(N_Signif, N_Signif_Rec, Recov, Signif_FireYear))
  }
  
  if (median_analysis == TRUE) {
    
    # Assign Row and column names of the p_values matrixe
    Rownames_Full <- as.numeric(row.names(p_matrix_median))
    Colnames_Full <- as.numeric(colnames(p_matrix_median))
    # Rows to be considered for the post-fire comparison (only after fire)
    selRows_Full  <- which(Rownames_Full >= 0)  
    
    for (N in opts$MedWdt) {
      # Start of cycle on: Number of before-years to be considered for
      # assessing significance
      
      message("--- Analysis considering ", N, " before fire years as reference", 
              sep = "", " ----")
      
      # Columns to be considered for the post-fire comparison (only before fire
      #  and lower than the number of years considered for comparing)
      SelCols_Full       <- which(Colnames_Full < 0 & Colnames_Full >= -N) 
      # Convert p_values to significance.
      Data_Matrix_Signif <- p_matrix_median[selRows_Full, SelCols_Full, ] < (opts$sig_level) 
      
      if (N >= 2) {
        Sig_Years_median <- apply(Data_Matrix_Signif, 3, find_N_Seq)
      } else {
        Sig_Years_median <- apply(Data_Matrix_Signif, 2, find_N_Seq)
      }  # compute number of years AND FLAG recovered/Unrecovered
      
      dimnames(Sig_Years_median)[[1]] <- c("N_Signif", "N_Signif_Rec", 
                                           "Recov", "Signif_FireYear")
      
      # Convert to a data frame and add accessory info (opts$MedWdt)
      Sig_Years_median        <- as.data.frame(t(Sig_Years_median))  
      Sig_Years_median$Comp_N <- N
      # Add to the 'FULL' matrix
      Sig_Years_median_tot    <- rbind(Sig_Years_median_tot, Sig_Years_median)  
    }
    
    # Add the case_id as a dimension (to allow joining with other statistics of 
    # the fire contained in plot_stat)
    Sig_Years_median_tot$CASE_ID       <- as.factor(rep(dimnames(Sig_Years_median)[[1]], 
                                                        length(opts$MedWdt)))  
    # Convert Recov field to string factor
    levels(Sig_Years_median_tot$Recov) <- c("UnRecovered", "Recovered")  
    
  }  # End of cycle on: Number of years considered for significance analysis
  
  
  message("---- -> Computing cumulated stats ----")
  
  # Compute a 'recov_stat' data frame, which joins info in plot_stat for
  # what concerns FireYear, eccetera, with info related to the number of
  # significantly different years
  
  recov_stat <- FRG_RecovStat_Matrix(file_in = file_in,
                                     plot_stat = plot_stat, 
                                     Data_Median = Sig_Years_median_tot,
                                     MedWdt = opts$MedWdt)
  
  # save output
  save(Sig_Years_median_tot, plot_stat, recov_stat, file = file_out)
  
}

#- ----------------------------------------------- -
#- Helper function used to add additional info to the signiicance matrix (More complex than really necessary.... should be stripped....)
#- ------------------------------------------------ -

#' FRG_RecovStat_Matrix
#'
#' @param file_in 
#' @param plot_stat 
#' @param Data_Median 
#' @param opts$MedWdt 
#'
#' @return
#' @export
#' @importFrom plyr ddply summarize
#' @examples
FRG_RecovStat_Matrix <- function(file_in = file_in,
                                 plot_stat = plot_stat, 
                                 Data_Median = Data_Median,
                                 MedWdt) {
  # Join results obtained on multiple runs with varying parameters
  
  # Data_Median = Sig_Years_median_tot
  Data_Median$Comp_N   <- as.factor(Data_Median$Comp_N)
  Data_Median$N_Signif <- as.numeric(as.character(Data_Median$N_Signif))  #, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9','10')))
  recov_stat_full <- NULL
  # plot_stat$Index <- attr(plot_stat, "Index")
  plot_stat_first <- plyr::ddply(plot_stat, .(CASE_ID, CLC_Class), 
                                 plyr::summarize, OBJECTID = OBJECTID[1], FireYear = FireYear[1], Area_All = Area_All[1], 
                                 Area_Forest = Area_Forest[1], Area_CLC = Area_CLC[1], YearFromFire = YearFromFire[1], 
                                 N_PIX = N_PIX[1], Index = "Med_SNDVI", .parallel = F, .progress = "text")
  # Compute the variable 'N_Years_After. Contains the number of
  # 'available' years after the fire in the MODIS time serie.
  plot_stat_first$N_Years_After <- max(as.numeric(as.character(levels(plot_stat$FireYear)))) - 
    as.numeric(as.character(plot_stat_first$FireYear)) + 1
  for (N in opts$MedWdt) {
    
    Data_Median_sub <- droplevels(subset(Data_Median, Comp_N == N))  # Get results obtained using a median of width N
    recov_stat_tmp <- plot_stat_first
    recov_stat_tmp <- join(recov_stat_tmp, Data_Median_sub, by = "CASE_ID")  # Join plot_stat with information related to significance. 
    recov_stat_tmp$Min_Percentage <- attr(Data_Median, "Min_Percentage")
    recov_stat_full <- rbind(recov_stat_full, recov_stat_tmp)  # Add to the output matrix information on results obtained considering a median width of N
    print(N)
  }
  
  # Compute the Recovery Time. For UnRecovered BAs, set to -999 For
  # Recovered BAs with significant reduction in the fire year, set to
  # N_Signif For Recovered BAs with significant reduction in the fire
  # year, set to N_Signif+1 In this way, the sum of FireYear+RT returns
  # the first year for which the Wilcoxon test was not significant
  
  recov_stat_full$RT <- -999
  recov_stat_full$RT[which(recov_stat_full$Recov == "UnRecovered")] <- -999
  recov_stat_full$RT[which(recov_stat_full$Recov == "Recovered" & recov_stat_full$Signif_FireYear == 
                             "0")] <- recov_stat_full$N_Signif[which(recov_stat_full$Recov == 
                                                                       "Recovered" & recov_stat_full$Signif_FireYear == "0")] + 1
  recov_stat_full$RT[which(recov_stat_full$Recov == "Recovered" & recov_stat_full$Signif_FireYear == 
                             "1")] <- recov_stat_full$N_Signif[which(recov_stat_full$Recov == 
                                                                       "Recovered" & recov_stat_full$Signif_FireYear == "1")]
  
  return(recov_stat_full)  # Data frame containing info on Fire 'characteristics (ID, Year, CLC_Class, ecc + Info on results of significance analysis)
}

