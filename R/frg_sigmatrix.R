#' frg_sigmatrix
#' @description Function used to perform statistical analysis in order to determine significance of scaled VI reductions in years following \cr
#' the fire. Starting from extracted time series, compute matrixes of p-values using FRG_Comp_Regr_Matrix. p-values matrixes contain result \cr
#' of the Wilcoxon test, computed using the selected number of reference pre-fire years and the selected \cr
#' minimum difference percentage. Matrixes of significances are then created using FRG_Significance_Analysis and saved. \cr
#' ( Note: this is only a wrapper used to call the FRG_Comp_Regr_Matrix. and FRG_Significance_Analysis functions)
#' 
#' @details
#'
#' @param in_file string File of scaled VI Time Series extracted for single-fire areas
#' @param out_file string Basename for output files
#' @param opts$min_pix numeric MInimum number of CORE pixels for a bunt area to be considered
#' @param opts$perc_diff numeric Minimum percentage difference checked for significant reduction 
#' @param opts$MedWdt numeric Number of pre-fire years considered as reference
#' @param opts$sub_zones flag If 1 , consider ENV_ZONES differences when analyzing
#' @param opts$sig_level numeric Significance level used for Wilcoxon test 
#' @param opts$erode flag If 1, perform analysis only on NON CORE burnt pixels 
#' 
#' @return Saves a .RData file containing data needed to plot NDVIR time series for the different single-fire BAs
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' 


frg_sigmatrix <- function(in_file, out_file, opts){
  
  # Initialize and define output file names and Dirs
  
  message("----------------------------------------------------------")
  message("------ Extract plotting data and perform Statistical Analysis of Med_SNDVI reductions ----")
  message("------ on areas burned once                                                        ----")
  message("----------------------------------------------------------")
  message("")
  message(paste("---- -> In File for Analysis: ", in_file), " ----")
  message(paste("---- -> Out File for Analysis: ", out_file), " ----")
  message("----------------------------------------------------------")
  
  
  #- -------------------------------------------
  # Compute the matrix of the p-values
  #- -------------------------------------------
  
  out_dir          <- dirname(out_file)
  out_dir_p_matrix <- file.path(out_dir, "p_matrix")  # Filename for output p-values matrix
  out_file_pmat    <- file.path(out_dir_p_matrix, "p_matrix.RData")
  dir.create(out_dir_p_matrix, recursive = T)
  message("--- Creating p-values matrix for significance analysis : ", out_file_pmat, " ----")
  
  out <- frg_wilcox(in_file, out_file_pmat, opts)  # Compute p-values matrix
  gc()  #garbage collection 
  message("---- Creating Significance analysis output files: ", out_file, " ----")
  
  # Create significance matrix, according to significance level
  out <- frg_sigyears(file_in = out_file_pmat, file_out = out_file, opts)
  
}