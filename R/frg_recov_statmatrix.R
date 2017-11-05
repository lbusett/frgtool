#' @title frg_recov_statmatrix
#' @description Helper function used to add additional info to the signiicance
#'  matrix computed in `frg_sigmatrix`
#' @param plot_stat data passed from frg_sigmatrix
#' @param Data_Median data passed from frg_sigmatrix
#' @param MedWdt data passed from frg_sigmatrix
#'
#' @return Data frame containing info on Fire characteristics (ID, Year, 
#'  CLC_Class, ecc + Info on results of significance analysis)
#' @rdname frg_recov_statmatrix
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom plyr ddply join
#' 
frg_recov_statmatrix <- function(plot_stat,
                                 Data_Median,
                                 MedWdt) {
  
  OBJECTID <- FireYear <- Area_All <- Area_CLC <- YearFromFire <- 
    N_PIX <- Comp_N <- Area_Forest <- NULL
  # Join results obtained on multiple runs with varying parameters
  
  Data_Median$Comp_N   <- as.factor(Data_Median$Comp_N)
  Data_Median$N_Signif <- as.numeric(as.character(Data_Median$N_Signif))
  recov_stat_full <- NULL
  plot_stat$Index <- "Med_SNDVI"
  plot_stat_first <- plyr::ddply(
    plot_stat, c("CASE_ID", "CLC_Class"),
    plyr::summarize, OBJECTID = OBJECTID[1], FireYear = FireYear[1],
    Area_All = Area_All[1], Area_Forest = Area_Forest[1],
    Area_CLC = Area_CLC[1], YearFromFire = YearFromFire[1],
    N_PIX = N_PIX[1], Index = "Med_SNDVI", .parallel = F, .progress = "text"
  )
  
  # Compute the variable 'N_Years_After. Contains the number of
  # 'available' years after the fire in the MODIS time serie.
  plot_stat_first$N_Years_After <- max(
    as.numeric(as.character(levels(plot_stat$FireYear)))) - 
    as.numeric(as.character(plot_stat_first$FireYear)) + 1
  
  for (N in MedWdt) {
    
    # Get results obtained using a median of width N
    Data_Median_sub <- droplevels(subset(Data_Median, Comp_N == N)) 
    recov_stat_tmp <- plot_stat_first
    # Join plot_stat with information related to significance. 
    recov_stat_tmp <- plyr::join(recov_stat_tmp, Data_Median_sub, by = "CASE_ID")  
    recov_stat_tmp$Min_Percentage <- attr(Data_Median, "Min_Percentage")
    # Add to the output matrix information on results obtained considering a median
    # width of N
    recov_stat_full <- rbind(recov_stat_full, recov_stat_tmp) 
    
  }
  
  # Compute the Recovery Time. For UnRecovered BAs, set to -999 For
  # Recovered BAs with significant reduction in the fire year, set to
  # N_Signif For Recovered BAs with significant reduction in the fire
  # year, set to N_Signif+1 In this way, the sum of FireYear+RT returns
  # the first year for which the Wilcoxon test was not significant
  
  recov_stat_full$RT <- -999
  recov_stat_full$RT[which(recov_stat_full$Recov == "UnRecovered")] <- -999
  recov_stat_full$RT[which(recov_stat_full$Recov == "Recovered" &
                             recov_stat_full$Signif_FireYear == "0")] <-
    recov_stat_full$N_Signif[which(recov_stat_full$Recov == "Recovered"
                                   & recov_stat_full$Signif_FireYear == "0")] + 1
  
  recov_stat_full$RT[which(recov_stat_full$Recov == "Recovered" & 
                             recov_stat_full$Signif_FireYear == "1")] <-
    recov_stat_full$N_Signif[which(recov_stat_full$Recov == "Recovered"
                                   & recov_stat_full$Signif_FireYear == "1")]
  
  # Data frame containing info on Fire 'characteristics (ID, Year, CLC_Class,
  #  ecc + Info on results of significance analysis)
  return(recov_stat_full)  
}

