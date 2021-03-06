#' frg_sigmatrix
#' @description Function used to perform statistical analysis in order to 
#'  determine significance of scaled VI reductions in years following the fire.
#'  - Starting from extracted time series, compute matrixes of p-values using
#'   FRG_Comp_Regr_Matrix. p-values matrixes contain result of the Wilcoxon
#'   test, computed using the selected number of reference pre-fire years and
#'   the selected minimum difference percentage.
#'  - Matrixes of significances are then created using FRG_Significance_Analysis and saved.
#'   
#'  Note: this is only a wrapper used to call the FRG_Comp_Regr_Matrix
#'   and FRG_Significance_Analysis functions)
#' @param in_file string File of scaled VI Time Series extracted for single-fire areas
#' @param out_file string Basename for output files
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @return Saves a .RData file containing data needed to plot NDVIR time series for the different single-fire BAs
#' @rdname frg_sigmatrix
#' @importFrom magrittr "%>%"
#' @export 
#' @author Lorenzo Busetto, phD (2013-2017) <lbusett@gmail.com>


frg_sigmatrix <- function(in_file, out_file, opts){
  
  # Initialize and define output file names and Dirs
  message("")
  message("- ------------------------------------------------------ -")
  message("- Extract plotting data and perform Statistical Analysis -") 
  message("- of Med_SVI reductions on areas burned once             -")
  message("- ------------------------------------------------------ -")
  message("")
  message("- -> In File for Analysis: ", in_file)
  message("- -> Out File for Analysis: ", out_file)
  message("- ------------------------------------------------------ -")
  message("")
  
  #- -------------------------------------------
  # Compute the matrix of the p-values
  #- -------------------------------------------
  
  out_dir          <- dirname(out_file)
  
  # Filename for output p-values matrix
  out_dir_p_matrix <- file.path(out_dir, "p_matrix")  
  out_file_pmat    <- file.path(out_dir_p_matrix, "p_matrix.RData")
  dir.create(out_dir_p_matrix, recursive = T)
  message("- -> Creating p-values matrix for significance analysis : ", 
          out_file_pmat, " ----")
  
  out <- frg_wilcox(in_file, out_file_pmat, opts)  # Compute p-values matrix
  gc()  #garbage collection 
  message("")
  message("- -> Creating Significance analysis output files: ",
          out_file, " ----")
  
  # Create significance matrix, according to significance level
  out <- frg_sigyears(file_in = out_file_pmat, file_out = out_file, opts)
  
}