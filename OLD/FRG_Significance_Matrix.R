#' @title FRG_Significance_Matrix
#' @description Function used to perform statistical analysis in order to determine significance of scaled VI reductions in years following \cr
#' the fire. Starting from extracted time series, compute matrixes of p-values using FRG_Comp_Regr_Matrix. p-values matrixes contain result \cr
#' of the Wilcoxon test, computed using the selected number of reference pre-fire years and the selected \cr
#' minimum difference percentage. Matrixes of significances are then created using FRG_Significance_Analysis and saved. \cr
#' ( Note: this is only a wrapper used to call the FRG_Comp_Regr_Matrix. and FRG_Significance_Analysis functions)
#' 
#' @details
#'
#' @param In_File string File of scaled VI Time Series extracted for single-fire areas
#' @param Out_File string Basename for output files
#' @param min_pix numeric MInimum number of CORE pixels for a bunt area to be considered
#' @param perc_diff numeric Minimum percentage difference checked for significant reduction 
#' @param MedWdt numeric Number of pre-fire years considered as reference
#' @param sub_zones flag If 1 , consider ENV_ZONES differences when analyzing
#' @param sig_level numeric Significance level used for Wilcoxon test 
#' @param erode flag If 1, perform analysis only on NON CORE burnt pixels 
#' 
#' @return Saves a .RData file containing data needed to plot NDVIR time series for the different single-fire BAs
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' 


FRG_Significance_Matrix = function(In_File = In_File, Out_File = Out_File ,min_pix = min_pix, 
                                   perc_diff = perc_diff , MedWdt = MedWdt ,sub_zones = sub_zones,
                                   sig_level = sig_level ,erode =erode){
  
  #- -------------------------------------------
  # Compute the matrix of the p-values
  #- -------------------------------------------
  
  out_dir = dirname(Out_File)
  out_dir_p_matrix = file.path(out_dir,'p_matrix')                                    # Filename for output p-values matrix
  dir.create(out_dir_p_matrix, recursive = T)
  out_file_pmat = file.path(out_dir_p_matrix,'p_matrix')
  print(paste('--- Creating p-values matrix for significance analysis : ',out_file_pmat))
  
  out = FRG_Comp_Sig_Matrix(In_File = In_File , out_file_pmat = out_file_pmat ,min_pix = min_pix, 
                            perc_diff = perc_diff , MedWdt = MedWdt ,sub_zones = sub_zones, erode = erode)    # Compute p-values matrix
  gc()   #garbage collection 
  
  out_file_pmat = file.path(out_dir_p_matrix,'p_matrix.RData')      #Name of the p-value matrix file createdin FRG_Comp_Sig_Matrix
  print(paste('--- Creating Significance analysis output files: ',Out_File))
  
  out = FRG_Significance_Analysis(file_in = out_file_pmat , file_out = Out_File, # Create significance matrix, according to significance level
                                  sig_level = sig_level, MedWdt = MedWdt)                
  
}