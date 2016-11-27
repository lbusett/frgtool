#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#
FRG_Compare_Indexes = function() {
  
  
  library(ggplot2)
  library(plyr)
  library(gridExtra)
  library(reshape2)
  library(lattice)
  library(taRifx)
  
  #- --------------------------------------------------------------------------- -#
  #- Functions to plot the percentages of fires recovered after k years
  #- --------------------------------------------------------------------------- -#
  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 15, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 9), axis.text.y = element_text(size =9), 
                                  axis.title.x = element_text( vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
  max_N_Years_after = 9
  
  # In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/Stat_Analysis/9.5%/Percentages/Percentages_9.5%.RData'
  
  # Define files to be analyzed
  SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/'
  SRDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SRDVI/Stat_Analysis/'
  
  files_SNDVI = c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData')
  files_SRDVI = c('10/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData')
  
  In_File_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
  In_File_SRDVI = file.path(SRDVI_Folder,files_SRDVI)
  
  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Index_Comparison'
  dir.create (plot_folder)
  
  # Load Files and Recategorize areas
  
  load(In_File_SNDVI)             # Load SNDVI file
  recov_stat_full = recov_stat
  
  load(In_File_SRDVI)           # Load SRDVI file
  recov_stat_full$N_Signif_Rec_SRDVI = recov_stat$N_Signif_Rec  #Add column of SRDVI results
  recov_stat_full_tmp = recov_stat_full
  # Recode Area_Cat and add a "All" level
  recov_stat_full_tmp$Area_Cat = 'All'
  recov_stat_full = rbind(recov_stat_full_tmp, recov_stat_full)
  recov_stat_full$Area_Cat = factor(recov_stat_full$Area_Cat, levels = c("(-1,250]", "(250,500]","(500,1e+03]","(1e+03,5e+03]",
                                                                         "(5e+03,1e+04]", "(1e+04,1e+05]","All"))
  levels(recov_stat_full$Area_Cat) = c('0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000','All')
  
  # Recode the Areas to have a lower number of levels
  
  recov_stat_full <- within(recov_stat_full, Area_Cat_New <- recode(Area_Cat, 'c("0-250", "250-500")="< 500 ha";
                                                                               c("500-1000", "1000-5000")="500 - 5000 ha";
                                                                               c("5000-10000")="5000 - 10000 ha";
                                                                               c("10000-50000")=">10000 ha"'))
  
  recov_stat_full$Area_Cat_New = factor(recov_stat_full$Area_Cat_New, levels = c("< 500 ha", "500 - 5000 ha","5000 - 10000 ha",">10000 ha","All"))
  
  # Use only MedWdt = 3
  recov_stat_full = (subset(recov_stat_full, Comp_N == 3 & CLC_Class == 'All'))
  
  # Convert "NR" to numeric (useful for change analysis)
  levels(recov_stat_full$N_Signif_Rec)[length(levels(recov_stat_full$N_Signif_Rec))]='999'
  levels(recov_stat_full$N_Signif_Rec_SRDVI)[length(levels(recov_stat_full$N_Signif_Rec_SRDVI))]='999'
  
  # Compute the RT deltas between NDVI  and RDVI (Negative = shorter times on RDVI)
  recov_stat_full$N_Rec_Num = as.numeric(as.character(recov_stat_full$N_Signif_Rec))
  recov_stat_full$N_Rec_Num_SRDVI = as.numeric(as.character(recov_stat_full$N_Signif_Rec_SRDVI))
  recov_stat_full$Delta =recov_stat_full$N_Rec_Num_SRDVI- recov_stat_full$N_Rec_Num
  recov_stat_full$Delta[which(recov_stat_full$Delta<= -900)] =-999
  recov_stat_full$Delta[which(recov_stat_full$Delta>= 900)] =999
  
  levels = levels(recov_stat_full$N_Signif_Rec)
  recov_stat_full$N_Signif_Rec_SRDVI = factor(recov_stat_full$N_Signif_Rec_SRDVI, levels = levels,
                                              labels = levels)
  
  #---------------------------------------------------
  # This plots results as a "Classification matrix". Informative but a bit confusing    
  #---------------------------------------------------
  
  # Create a "summary" data frame containing counts and percentges of the diffrent RT Full vs RT Eroded combinations
  summary = ddply(recov_stat_full, .(Area_Cat_New,N_Signif_Rec, N_Signif_Rec_SRDVI), summarize, count = length(N_Signif_Rec),.drop = FALSE)
  summary_totals = ddply(summary, .(Area_Cat_New  ), summarize, total = sum(count), .drop = FALSE)
  summary = join(summary, summary_totals, by = c( 'Area_Cat_New'))
  # Compute percentages with respect to total in Area Class
  summary$Percentage =100* summary$count/summary$total
  summary_equals = summary[which(as.numeric(as.character(summary$N_Signif_Rec)) == as.numeric(as.character(summary$N_Signif_Rec_SRDVI))),] 
  summary_changes = ddply(summary_equals, .(Area_Cat_New), summarize, change = 100-sum(Percentage, na.rm = T), .drop = FALSE)
  summary = join(summary, summary_changes)
  dove_NaN = which(summary$Percentage == 'NaN')
  levels(summary$N_Signif_Rec)[length(levels(summary$N_Signif_Rec))]='NR'
  levels(summary$N_Signif_Rec_SRDVI)[length(levels(summary$N_Signif_Rec_SRDVI))]='NR'
  summary_changes$x = 1
  summary_changes$y = 0
  summary_changes$Percentage_cut = 0
  summary_changes$Text = paste('Total Variation = ',format(summary_changes$change,digits = 4),'%')

  # Plot results, Wrapped by Area_Cat_New
  
  summary$Percentage_cut = cut(summary$Percentage, breaks = c(0,1,5,15,30,50))
  p = ggplot(summary, drop = FALSE)+ my_theme_bw
  p = p + ylab(expression(Recovery~Time~-~RDVI^R))+  xlab(expression(Recovery~Time~-~NDVI^R))
  p = p + facet_wrap(~Area_Cat_New,drop = FALSE)
  p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec),
                           breaks=levels(summary$N_Signif_Rec), drop = FALSE,
                           labels =c(seq(0,(length(levels(summary$N_Signif_Rec))-2),1),'NR' ))
  
  p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec),
                            breaks=levels(summary$N_Signif_Rec), drop = FALSE, 
                            labels =c(seq(0,(length(levels(summary$N_Signif_Rec))-2),1),'NR' ))
  
  p = p + geom_point(aes(x = N_Signif_Rec, y = N_Signif_Rec_SRDVI, 
                         size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) +geom_abline(intercept = 0, slope = 1)
  p = p + scale_size_discrete('Percentage',range = c(1.5,6),labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
  p = p + scale_fill_hue('Percentage', labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
  
  p = p + geom_text(aes(x=7, y=1, label=Text),size = 3,  data = summary_changes, size = 4,hjust=0.5)
  p = p+theme(legend.position = c(0.73,0.34))
  out_file = file.path(plot_folder,paste('Index_Comparison_Full_Matrix.tif'))
  tiff(out_file, width = 8, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) 
  dev.off()
  
  # Plot results, for Area_Cat_New == 'All
  df = droplevels(subset(summary, Area_Cat_New =='All' ))
  df_text = droplevels(subset(summary_changes, Area_Cat_New =='All' ))
  p = ggplot(df)+ my_theme_bw
  p = p + ylab(expression(Recovery~Time~-~RDVI^R))+  xlab(expression(Recovery~Time~-~NDVI^R))
  p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec),
                           breaks=levels(summary$N_Signif_Rec), drop = FALSE,
                           labels =c(seq(0,(length(levels(summary$N_Signif_Rec))-2),1),'NR' ))
  
  p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec),
                            breaks=levels(summary$N_Signif_Rec), drop = FALSE, 
                            labels =c(seq(0,(length(levels(summary$N_Signif_Rec))-2),1),'NR' ))
  
  p = p + geom_point(aes(x = N_Signif_Rec, y = N_Signif_Rec_SRDVI, 
                         size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) +geom_abline(intercept = 0, slope = 1)
  p = p + scale_size_discrete('Percentage',range = c(1.5,6),labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
  p = p + scale_fill_hue('Percentage', labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
  p = p + geom_text(aes(x=7, y=1, label=Text),size = 3,  data = df_text, size = 4,hjust=0.5)
  
  out_file = file.path(plot_folder,paste('Index_Comparison_Matrix_all.tif'))
  tiff(out_file, width = 6, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  
  #---------------------------------------------------
  # This plots results as Variation Histogram. Clearer but less informative
  #---------------------------------------------------
  
  # Create a "summary_delta" data frame containing counts and percentges of the diffrent RT Full-RT Eroded values (-999 and 999 
  # indicate transitions from/to Not Recovered)
  
  summary_delta = ddply(recov_stat_full, .(Area_Cat_New,Delta), summarize, count = length(N_Signif_Rec),.drop = FALSE)
  summary_delta_totals = ddply(summary_delta, .(Area_Cat_New), summarize, total = sum(count), .drop = FALSE)
  summary_delta = join(summary_delta, summary_delta_totals, by = c( 'Area_Cat_New'))
  summary_delta$Percentage =100* summary_delta$count/summary_delta$total
  summary_delta_equals = summary_delta[which(summary_delta$Delta == 0),] 
  summary_delta_changes = ddply(summary_delta_equals, .(Area_Cat_New), summarize, change = 100-sum(Percentage, na.rm = T))
  summary_delta = join(summary_delta, summary_delta_changes)
  
  # Restructure levels of difference correctly
  summary_delta$Delta = factor(summary_delta$Delta, levels = c('-999','-6','-5','-4','-3','-2','-1','0','1','2','3','999'),
                               labels = c('Rec->NR','-6','-5','-4','-3','-2','-1','0','1','2','3','NR->Rec'))
  
  
  # Plot results, Wrapped by Area_Cat_New
  p = ggplot(summary_delta, aes (x = Delta, y = count, label =sprintf("%.1f%%", Percentage)))+ my_theme_bw
  p = p+scale_x_discrete(drop = FALSE)
  p = p + xlab(expression(RT[RDVI^R]~-~RT[NDVI^R] ))+ ylab ('Number of Cases')
  p = p + facet_wrap(~Area_Cat_New,drop = FALSE, scales = 'free_y')
  p = p + geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)
  p = p + geom_text(vjust = -0.5, size =2)
  p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  p = p + theme(axis.text.y = element_text(size = 8))
  
  out_file = file.path(plot_folder, paste('Index_Comparison_Full_Histo.tif'))
  tiff(out_file, width = 8, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
  # Plot results, for Area_Cat_New == 'All'
  
  df = (subset(summary_delta, Area_Cat_New =='All' ))
  df_text = droplevels(subset(summary_changes, Area_Cat_New =='All' ))
  p = ggplot(df, aes (x = Delta, y = count, label =sprintf("%.1f%%", Percentage)))+ my_theme_bw
  p = p+scale_x_discrete(drop = FALSE)
  p = p + xlab(expression(RT[RDVI^R]~-~RT[NDVI^R] ))+ ylab ('Number of Cases')
  p = p + geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)
  p = p + geom_text(vjust = -0.5, size =2)
  p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  p = p + theme(axis.text.y = element_text(size = 8))
  out_file = file.path(plot_folder, paste('Index_Comparison_Histo_all.tif'))
  tiff(out_file, width = 6, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()
  
}