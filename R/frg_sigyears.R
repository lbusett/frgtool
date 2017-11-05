#' @title frg_sigyears
#' @description Function used to compute the number of consecutive years with
#'  significant VIR reductions for the different BAS, considering the selected number
#'  of before fire years (opts$MedWdt). Also, joins 'plotting' information computed
#'  in FRG_Comp_Sig_Matrix with 'Significance' information in the 'recov_stat' 
#'  data frame and computes the Recovery Times. All informations derived
#'  (plot_stat + recov_stat) from the analysis are saved in the final Statistical
#'  Analysis output RData file
#' @param file_in `character` RData file created by FRG_Comp_Sig_Matrix
#' @param file_out `character` Name of the final Statistical Analysis output RData file 
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param median_analysis `logical` (obsolete - always TRUE)
#' @return Saves a file containing the p-values matrix and the 'plot_stats' data
#'  frame in the 'p_matrixes' subfolder
#' @rdname frg_sigyears
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

frg_sigyears <- function(file_in,
                         file_out,
                         opts,
                         median_analysis = TRUE) {
  plot_stat <- p_matrix_median <- NULL
  # Load the statistical analysis results (p-values matrix and plot_stat)
  load(file.path(file_in))  
  
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
      #
      # 'Valid' columns are the ones not at NODATA, ie, the columns containing
      # actual data (Not outside the time serie on the left)
      GoodCols   <- which(apply(x, 2, max) >= 0) 
      N_GoodCols <- length(GoodCols)
      x          <- x[, GoodCols]  # Get data for valid columns
      
      # 'Valid' rows are the ones not at NODATA, ie, the columns containing actual
      #  data (Not outside the time serie on the right)
      GoodRows   <- which(apply(x, 1, max) >= 0)
      N_GoodRows <- length(GoodRows)
      x          <- x[GoodRows, ]  # Get valid data
      
      if (N_GoodRows == 1) {
        # Exception for fires occured only since one year
        Signif_Array <- sum(x, na.rm = T)/N_GoodCols  
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
          # finite , then Recov = 0 and N_Signif equal to maximum possible years
          # (i.e., number of years occurred since the fire)
          Recov        <- 0
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
      
      message("- Analysis considers ", N, " before-fire years as reference")
      
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
  
  
  message("- -> Computing Recovery Statistics ----")
  
  # Compute a 'recov_stat' data frame, which joins info in plot_stat for
  # what concerns FireYear, eccetera, with info related to the number of
  # significantly different years
  
  recov_stat <- frg_recov_statmatrix(plot_stat = plot_stat, 
                                     Data_Median = Sig_Years_median_tot,
                                     MedWdt = opts$MedWdt)
  
  # save output
  save(Sig_Years_median_tot, plot_stat, recov_stat, file = file_out)
  
}