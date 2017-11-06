FRG_Plot_Stat_Analysis = function(){
  
  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Statistical_Analysis'
  dir.create(plot_folder, recursive =T) 
  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 11, vjust =2, hjust = 0.5),
                                  axis.title.x = element_text( face = "bold", vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text( hjust = 0.5, vjust = 0.3,size = 10, angle = 90),
                                  axis.text.x = element_text( face = "bold", vjust = 0, size = 9, angle = 0 ), 
                                  axis.text.y = element_text( hjust = 0.5, vjust = 0.3,size = 9, angle = 90))
  
  out_file_NDVI= file.path('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Statistical_Analysis','Stats_NDVI.RData')
  load(out_NDVI)
  out_file_RDVI= file.path('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Statistical_Analysis','Stats_RDVI.RData')
  load(out_RDVI)
  
  
  # Create Data frame of statistics of model
  out_stats_NDVI = with(out_NDVI, data.frame(Formula = Reduce(paste, deparse(Final_Form)), 
                                             R2 =(out_NDVI$rms_model$stats['R2']),
                                             Dxy = (out_NDVI$rms_model$stats['Dxy']),
                                             Fit_Acc = Accuracy_Fitting[[1]],
                                             Fit_k = Accuracy_Fitting[[2]],
                                             fit_rho = rho_fitting, 
                                             Acc_10= Accuracy_Prediction_1[[1]],
                                             k_10= Accuracy_Prediction_1[[2]],
                                             rho_10= rho_Prediction_1,
                                             Acc_rep= mean(Accuracy_Prediction_rep[,1]),
                                             k_rep= mean(Accuracy_Prediction_rep[,2]),
                                             rho_rep= mean(rho_prediction_rep[,1])))
  
  out_stats_NDVI_small = with(out_NDVI, data.frame(R2 =(out_NDVI$rms_model$stats['R2']),
                                             Dxy = (out_NDVI$rms_model$stats['Dxy']),
                                             Accuracy_cv= mean(Accuracy_Prediction_rep[,1]),
                                             Kappa_cv= mean(Accuracy_Prediction_rep[,2]),
                                             Rho_cv= mean(rho_prediction_rep[,1])))
  
  xtable(out_stats_NDVI_small, digits = 3, caption ='NDVI Discrimination Diagnostics')    # Table of reg diagnostics
  
  an_table = out_NDVI$rms_anova
  an_table =  matrix(sprintf("%.4f",an_table),nrow=12)
  colnames(an_table) = c('Chi-Square','d.f.','P')
  xtable(cbind(rownames(out_NDVI$rms_anova),an_table),include.rownames = FALSE, caption = "ANOVA Table for NDVI")
 
  out_file = file.path(plot_folder,'Prediction_Fit_NDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(out_NDVI$p_pred_fit) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - fitting                      
  out_file = file.path(plot_folder, 'Error_Fit_NDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  out_NDVI$pred_err_stats_fit$Error = factor(out_NDVI$pred_err_stats_fit$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  p = ggplot(out_NDVI$pred_err_stats_fit, aes(x = Error, y = Percentage, label = sprintf("%.1f%%", Percentage)))
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - 10Fold
  
  out_file = file.path(plot_folder, 'Error_10_NDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  out_NDVI$pred_err_stats_1$Error = factor(out_NDVI$pred_err_stats_1$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  p = ggplot(out_NDVI$pred_err_stats_1, aes(x = Error, y = Percentage, label = sprintf("%.1f%%", Percentage)), drop = F)
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2, drop = F)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - 10x50Fold
  
  stats_1050 = ddply(out_NDVI$pred_err_stats_rep, .(Error), summarise, mean_P = mean(Percentage))    
  stats_1050$Error = factor(stats_1050$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  out_file = file.path(plot_folder, 'Error_10x50_NDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  p = ggplot(stats_1050, aes(x = Error, y = mean_P, label = sprintf("%.1f%%", mean_P)))
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  
  # Analysis for RDVI             
  out_stats_RDVI = with(out_RDVI, data.frame(Formula = Reduce(paste, deparse(Final_Form)), 
                                             R2 =(out_RDVI$rms_model$stats['R2']),
                                             Dxy = (out_RDVI$rms_model$stats['Dxy']),
                                             Fit_Acc = Accuracy_Fitting[[1]],
                                             Fit_k = Accuracy_Fitting[[2]],
                                             fit_rho = rho_fitting, 
                                             Acc_10= Accuracy_Prediction_1[[1]],
                                             k_10= Accuracy_Prediction_1[[2]],
                                             rho_10= rho_Prediction_1,
                                             Acc_rep= mean(Accuracy_Prediction_rep[,1]),
                                             k_rep= mean(Accuracy_Prediction_rep[,2]),
                                             rho_rep= mean(rho_prediction_rep[,1])))
  
  out_stats_RDVI_small = with(out_RDVI, data.frame(R2 =(out_RDVI$rms_model$stats['R2']),
                                             Dxy = (out_RDVI$rms_model$stats['Dxy']),
                                             Accuracy_cv= mean(Accuracy_Prediction_rep[,1]),
                                             Kappa_cv= mean(Accuracy_Prediction_rep[,2]),
                                             Rho_cv= mean(rho_prediction_rep[,1])))
  
  xtable(out_stats_RDVI_small, digits = 3, caption ='RDVI Discrimination Diagnostics')    # Table of reg diagnostics
  
  an_table = out_RDVI$rms_anova
  an_table =  matrix(sprintf("%.3f",an_table),nrow=8)
  colnames(an_table) = c('Chi-Square','d.f.','P')
  xtable(cbind(rownames(out_RDVI$rms_anova),an_table),include.rownames = FALSE, caption = "ANOVA Table for RDVI")
 
  
  out_file = file.path(plot_folder, 'Prediction_Fit_RDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(out_RDVI$p_pred_fit) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - fitting                      
  out_file = file.path(plot_folder, 'Error_Fit_RDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  out_RDVI$pred_err_stats_fit$Error = factor(out_RDVI$pred_err_stats_fit$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  p = ggplot(out_RDVI$pred_err_stats_fit, aes(x = Error, y = Percentage, label = sprintf("%.1f%%", Percentage)))
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - 10Fold
  
  out_file = file.path(plot_folder, 'Error_10_RDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  out_RDVI$pred_err_stats_1$Error = factor(out_RDVI$pred_err_stats_1$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  p = ggplot(out_RDVI$pred_err_stats_1, aes(x = Error, y = Percentage, label = sprintf("%.1f%%", Percentage)), drop = F)
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2, drop = F)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Pred - Obs difference - 10x50Fold
  
  stats_1050 = ddply(out_RDVI$pred_err_stats_rep, .(Error), summarise, mean_P = mean(Percentage))    
  stats_1050$Error = factor(stats_1050$Error, levels = c("-6", "-5","-4", "-3", "-2", "-1", "0" , "1",  "2",  "3" , "4" ))
  out_file = file.path(plot_folder, 'Error_10x50_RDVI.tif')
  tiff(out_file, width = 5, height = 3, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  p = ggplot(stats_1050, aes(x = Error, y = mean_P, label = sprintf("%.1f%%", mean_P)))
  p = p +geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)+ xlab(expression(RT[Pred]~-~RT[Obs] ))+ ylab ('%') +my_theme_bw
  p = p + geom_text(vjust = -0.5, size =2)+ scale_x_discrete(drop=FALSE)
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
}
