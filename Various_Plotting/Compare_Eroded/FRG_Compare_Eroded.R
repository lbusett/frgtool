#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#
FRG_Compare_Eroded = function() {
  
  library(ggplot2)
  library(plyr)
  library(gridExtra)
  library(reshape2)
  
  #- --------------------------------------------------------------------------- -#
  #- Functions to plot the percentages of fires recovered after k years
  #- --------------------------------------------------------------------------- -#
  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 12, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 9), axis.text.y = element_text(size =9), 
                                  axis.title.x = element_text( vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
  
  # For Analysis on minpixs = 10 and percentages 9.5,11.5 -------------------------------
  
  # Define analysis File Names
  SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis'
  SRDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SRDVI/Stat_Analysis'
  
  files_SNDVI_Normal = file.path(SNDVI_Folder, c('10/9.5%/Normal/Percentages/Percentages_Area_Common9.5%.RData'))  # Contains results obtained on "uneroded" fires present also in the eroded case
  files_SNDVI_Eroded = file.path(SNDVI_Folder, c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData'))
  
  files_SRDVI_Normal = file.path(SRDVI_Folder, c('10/11.5%/Normal/Percentages/Percentages_Area_Common11.5%.RData'))  # Contains results obtained on "uneroded" fires present also in the eroded case
  files_SRDVI_Eroded = file.path(SRDVI_Folder, c('10/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData'))
  
  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Compare_Erosion'   # out folder for plots
  dir.create(plot_folder, recursive = T)
  
  cy = 1
  
  for (Index in c('SNDVI','SRDVI')) {
    #     for (Index in c('SRDVI')) {
    
    # Eventually, cycle on index
    if (Index == 'SNDVI') { files_Normal = files_SNDVI_Normal ; files_Eroded = files_SNDVI_Eroded}
    if (Index == 'SRDVI') { files_Normal = files_SRDVI_Normal ; files_Eroded = files_SRDVI_Eroded}
   
    # Load Files and create a d.f. containing results from both the "normal" and "Eroded" analysis
    load(files_Normal[cy])
    recov_stat_full = recov_stat
    load(files_Eroded[cy])
    recov_stat_full$N_Signif_Rec_Er = recov_stat$N_Signif_Rec  #RT Eroded with Recover indication
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
    levels(recov_stat_full$N_Signif_Rec_Er)[length(levels(recov_stat_full$N_Signif_Rec_Er))]='999'
    
    # Compute the RT deltas between Full and Eroded (Negative = shorter times on eroded )
    recov_stat_full$N_Rec_Num = as.numeric(as.character(recov_stat_full$N_Signif_Rec))
    recov_stat_full$N_Rec_Num_Er = as.numeric(as.character(recov_stat_full$N_Signif_Rec_Er))
    recov_stat_full$Delta =recov_stat_full$N_Rec_Num_Er- recov_stat_full$N_Rec_Num
    recov_stat_full$Delta[which(recov_stat_full$Delta<= -900)] =-999
    recov_stat_full$Delta[which(recov_stat_full$Delta>= 900)] =999
    
    
    #---------------------------------------------------
    # This plots results as a "Classification matrix". Informative but a bit confusing    
    #---------------------------------------------------
   
    # Create a "summary" data frame containing counts and percentges of the diffrent RT Full vs RT Eroded combinations
    summary = ddply(recov_stat_full, .(Area_Cat_New,N_Signif_Rec, N_Signif_Rec_Er), summarize, count = length(N_Signif_Rec),.drop = FALSE)
    summary_totals = ddply(summary, .(Area_Cat_New  ), summarize, total = sum(count), .drop = FALSE)
    summary = join(summary, summary_totals, by = c( 'Area_Cat_New'))
    summary$Percentage =100* summary$count/summary$total
    summary_equals = summary[which(as.numeric(as.character(summary$N_Signif_Rec)) == as.numeric(as.character(summary$N_Signif_Rec_Er))),] 
    summary_changes = ddply(summary_equals, .(Area_Cat_New), summarize, change = 100-sum(Percentage, na.rm = T))
    summary = join(summary, summary_changes)
    dove_NaN = which(summary$Percentage == 'NaN')
    
    # Compute percentages with respect to total in Area Class
    summary$Percentage [dove_NaN] = NA  ;    dove_zero = which(summary$Percentage == 0)  ;    summary$Percentage [dove_zero] = NA
    summary$N_Signif_Rec_Er [dove_zero] = NA  ;     summary$N_Signif_Rec_Er [dove_NaN] = NA
    summary= droplevels(subset(summary,N_Signif_Rec_Er != 'NA' ))
    levels(summary$N_Signif_Rec)[length(levels(summary$N_Signif_Rec))]='NR'
    levels(summary$N_Signif_Rec_Er)[length(levels(summary$N_Signif_Rec_Er))]='NR'
    summary_changes$x = 1
    summary_changes$y = 0
    summary_changes$Percentage_cut = 0
    summary_changes$Text = paste('Total Variation = ',format(summary_changes$change,digits = 4),'%')
    
    # Plot results, Wrapped by Area_Cat_New
     
    summary$Percentage_cut = cut(summary$Percentage, breaks = c(0,1,5,15,30,50))
    p = ggplot(summary)+ my_theme_bw
    
    p = p + ylab(expression(RT[Core]))+ xlab (expression(RT[All]))
    p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec_Er),
                             breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE,
                             labels =c(seq(0,(length(levels(summary$N_Signif_Rec_Er))-2),1),'NR' ))
    p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec_Er),
                              breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE, 
                              labels =c(seq(0,length(levels(summary$N_Signif_Rec_Er))-2,1),'NR' ))
    
    p = p + geom_point(aes(x = N_Signif_Rec, y = N_Signif_Rec_Er, 
                           size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) +geom_abline(intercept = 0, slope = 1)
    p = p + scale_size_discrete('Percentage',range = c(1.5,6),labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
    p = p + scale_fill_hue('Percentage', labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
    p = p + facet_wrap(~Area_Cat_New,drop = FALSE)
    p = p + geom_text(aes(x=7, y=1, label=Text),size = 3,  data = summary_changes, size = 4,hjust=0.5)
    p = p+theme(legend.position = c(0.73,0.34))
    out_file = file.path(plot_folder,paste(Index, '_Eroded_vs_Full_Matrix.tif'))
    tiff(out_file, width = 8, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
    print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
    dev.off()
    
    # Plot results, for Area_Cat_New == 'All
    df = droplevels(subset(summary, Area_Cat_New =='All' ))
    df_text = droplevels(subset(summary_changes, Area_Cat_New =='All' ))
    p = ggplot(df)+ my_theme_bw
    p = p + ylab(expression(RT[Core]))+ xlab (expression(RT[All]))
    p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec_Er),
                             breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE,
                             labels =c(seq(0,(length(levels(summary$N_Signif_Rec_Er))-2),1),'NR' ))
    p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec_Er),
                              breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE, 
                              labels =c(seq(0,length(levels(summary$N_Signif_Rec_Er))-2,1),'NR' ))
    
    p = p + geom_point(aes(x = N_Signif_Rec, y = N_Signif_Rec_Er, 
                           size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) +geom_abline(intercept = 0, slope = 1)
    p = p + scale_size_discrete('Percentage',range = c(1.5,6),labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
    p = p + scale_fill_hue('Percentage', labels = c("<1 %","1-5 % ","5-15 %","15-30%",">30%"))
    p = p + geom_text(aes(x=9, y=1, label=Text), data = df_text, size = 4,hjust=0.5)
    
    out_file = file.path(plot_folder,paste(Index, '_Eroded_vs_Full_Matrix_All.tif'))
    tiff(out_file, width = 6, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
    print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
    dev.off()
    
    #---------------------------------------------------
    # This plots results as Variation Histogram. Clearer but less informative
    #---------------------------------------------------
    
    # Create a "summary_delta" data frame containing counts and percentges of the diffrent RT Full-RT Eroded values (-999 and 999 
    # indicate transitions from/to Not Recovered)
    
    summary_delta = ddply(recov_stat_full, .(Area_Cat_New,Delta), summarize, count = length(N_Signif_Rec),.drop = FALSE)
    summary_delta_totals = ddply(summary_delta, .(Area_Cat_New  ), summarize, total = sum(count), .drop = FALSE)
    summary_delta = join(summary_delta, summary_delta_totals, by = c( 'Area_Cat_New'))
    summary_delta$Percentage =100* summary_delta$count/summary_delta$total
    summary_delta_equals = summary_delta[which(summary_delta$Delta == 0),] 
    summary_delta_changes = ddply(summary_delta_equals, .(Area_Cat_New), summarize, change = 100-sum(Percentage, na.rm = T))
    summary_delta = join(summary_delta, summary_delta_changes)
    
    if (Index == 'SNDVI') {
      summary_delta$Delta = factor(summary_delta$Delta, levels = c('-999','-4','-3','-2','-1','0','1','2','3','4','5','999'),
                                   labels = c('Rec->NR','-4','-3','-2','-1','0','1','2','3','4','5','NR->Rec'))
    } else{
      summary_delta$Delta = factor(summary_delta$Delta, levels = c('-999','-4','-3','-2','-1','0','1','2','3','4','5','6','7','999'),
                                   labels = c('Rec->NR','-4','-3','-2','-1','0','1','2','3','4','5','6','7','NR->Rec'))
    }
    
    
     # Plot results, Wrapped by Area_Cat_New
    p = ggplot(summary_delta, aes (x = Delta, y = count, label =sprintf("%.1f%%", Percentage)))+ my_theme_bw
    p = p+scale_x_discrete(drop = FALSE)
    p = p + xlab(expression(RT[Core]~-~RT[All]))+ ylab ('Number of Cases')
    p = p + facet_wrap(~Area_Cat_New,drop = FALSE, scales = 'free_y')
    p = p + geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)
    p = p + geom_text(vjust = -0.5, size =2.3)
    p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    p = p + theme(axis.text.y = element_text(size = 8))
    
    out_file = file.path(plot_folder, paste(Index, '_Eroded_vs_Full_Histo.tif'))
    tiff(out_file, width = 8, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
    print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
    dev.off()
    
     # Plot results, for Area_Cat_New == 'All'
    
    df = droplevels(subset(summary_delta, Area_Cat_New =='All' ))
    df_text = droplevels(subset(summary_changes, Area_Cat_New =='All' ))
    p = ggplot(df, aes (x = Delta, y = count, label =sprintf("%.1f%%", Percentage)))+ my_theme_bw
    p = p+scale_x_discrete(drop = FALSE)
    p = p + xlab(expression(RT[Core]~-~RT[All]))+ ylab ('Number of Cases')
    p = p + geom_bar(stat= 'identity',  fill = 'grey75', color = 'black', size = 0.2)
    p = p + geom_text(vjust = -0.5, size =2.3)
    p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    p = p + theme(axis.text.y = element_text(size = 8))
    p = p + theme(axis.title.x = element_text(size = 9))
    p = p + theme(axis.title.y = element_text(size = 9))
    out_file = file.path(plot_folder, paste(Index, '_Eroded_vs_Full_Histo_all.tif'))
    tiff(out_file, width = 6, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
    print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
    dev.off()
  }
}


# For Full analysis and comparisons between results obtained using variable percentages and minimum N of pixels(deprecated) -------------------------------

# SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SNDVI/Stat_Analysis/'
# SRDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SRDVI/Stat_Analysis/'
# 
# files_SNDVI_Normal = file.path(SNDVI_Folder,
#                                c('10/6%/Normal/Percentages/Percentages_Area_Common6%.RData',
#                                  '20/6%/Normal/Percentages/Percentages_Area_Common6%.RData',
#                                  '10/9.5%/Normal/Percentages/Percentages_Area_Common9.5%.RData',
#                                  '20/9.5%/Normal/Percentages/Percentages_Area_Common9.5%.RData'))
# files_SNDVI_Eroded = file.path(SNDVI_Folder,
#                                c('10/6%/Eroded/Percentages/Percentages_Area6%.RData',
#                                  '20/6%/Eroded/Percentages/Percentages_Area6%.RData',
#                                  '10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData',
#                                  '20/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData'))
# 
# 
# files_SRDVI_Normal = file.path(SRDVI_Folder,c('10/7.5%/Normal/Percentages/Percentages_Area_Common7.5%.RData',
#                                               '20/7.5%/Normal/Percentages/Percentages_Area_Common7.5%.RData',
#                                               '10/11.5%/Normal/Percentages/Percentages_Area_Common11.5%.RData',
#                                               '20/11.5%/Normal/Percentages/Percentages_Area_Common11.5%.RData'))
# files_SRDVI_Eroded = file.path(SRDVI_Folder,c('10/7.5%/Eroded/Percentages/Percentages_Area7.5%.RData',
#                                               '20/7.5%/Eroded/Percentages/Percentages_Area7.5%.RData',
#                                               '10/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData',
#                                               '20/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData'))
# 
# out_plot_folder_main = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/plots/CompareCommon'
# out_pdf_names = file.path(out_plot_folder_main, c('Compare_10_6_7.5.pdf',
#                                                   'Compare_20_6_7.5.pdf',
#                                                   'Compare_10_9_11.5.pdf',
#                                                   'Compare_20_9_11.5.pdf'))
# 
# str_options = c('N_PIX = 10 - Percs = 6-7.5',
#                 'N_PIX = 20 - Percs = 6-7.5',
#                 'N_PIX = 10 - Percs = 9.5-11.5',
#                 'N_PIX = 20 - Percs = 9.5-11.5')
# dir.create(out_plot_folder_main, recursive = T)

#- Computation and plottin ---------------------------------------------
# for (cy in 1:length(str_options)) {
#   
#   str_opt = str_options[cy]
#   pdf_out = out_pdf_names[cy]
#   pdf(pdf_out,width = 11.69, height = 8.27, pointsize = 9)
#   
#   for (Index in c('SNDVI','SRDVI')) {
# #     for (Index in c('SRDVI')) {
#     
#     # Eventually, cycle on index
#     if (Index == 'SNDVI') { files_Normal = files_SNDVI_Normal ; files_Eroded = files_SNDVI_Eroded}
#     if (Index == 'SRDVI') { files_Normal = files_SRDVI_Normal ; files_Eroded = files_SRDVI_Eroded}
#     
#     # Eventually, cycle on percs and N_PIX
#     
#     load(files_Normal[cy])
#     recov_stat_full = recov_stat
#     recov_stat_full$recov_str = NULL
#     load(files_Eroded[cy])
#     recov_stat_full$N_Signif_Er = recov_stat$N_Signif
#     recov_stat_full$N_Signif_Rec_Er = recov_stat$N_Signif_Rec
#     recov_stat_full_tmp = recov_stat_full
#     recov_stat_full_tmp$Area_Cat = 'All'
#     recov_stat_full = rbind(recov_stat_full_tmp, recov_stat_full)
#     recov_stat_full$Area_Cat = factor(recov_stat_full$Area_Cat, levels = c("All","(-1,250]", "(250,500]","(500,1e+03]","(1e+03,5e+03]",
#                                                                            "(5e+03,1e+04]", "(1e+04,1e+05]"))
#     
#     levels(recov_stat_full$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
#     
#     recov_stat_full = (subset(recov_stat_full, Comp_N == 3 & CLC_Class == 'All'))
#     
#     levels(recov_stat_full$N_Signif_Rec)[length(levels(recov_stat_full$N_Signif_Rec))]='11'
#     levels(recov_stat_full$N_Signif_Rec_Er)[length(levels(recov_stat_full$N_Signif_Rec_Er))]='11'
    
    
 
#     p1 = ggplot(recov_stat_full,aes(x= N_Signif_Rec, y = N_Signif_Rec_Er))+my_theme_bw
#     p1 = p1 + scale_x_discrete(limits = levels(recov_stat_full$N_Signif_Rec),
#                                breaks=levels(recov_stat_full$N_Signif_Rec), drop = FALSE,
#                                labels =c(seq(0,(length(levels(recov_stat_full$N_Signif_Rec))-2),1),'NR' ))
#     p1 = p1 + scale_y_discrete( limits = levels(recov_stat_full$N_Signif_Rec),
#                                 breaks=levels(recov_stat_full$N_Signif_Rec), drop = FALSE, 
#                                 labels =c(seq(0,length(levels(recov_stat_full$N_Signif_Rec))-2,1),'NR' ))
#     p1 = p1 + facet_wrap(~Area_Cat,drop = FALSE)
#     p1 = p1 + geom_point(aes(fill=..count..),size = 4, pch = 21, stat="bin", binwidth = 1)+ geom_abline(intercept = 0, slope = 1)
#     p1 = p1 + xlab('Recovery Time - On Original Burnt Areas')+ ylab ('Recovery Time - On Eroded Burnt Areas')
#     title = paste("Recovery Time - Original vs Eroded Burnt Areas\n\n",Index,' - ',str_opt, sep = '')
#     p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#     p1 = p1 + scale_fill_gradientn('Number of Cases\n',colours = topo.colors(10), trans = 'log',  breaks = c(1,5,20,50,100,300),guide = "colorbar")
#  
# #     scale_x_continuous(breaks = seq(0,11,1), labels=c(seq(0,9,1),' ','NR'))+
#         scale_y_continuous(breaks = seq(0,11,1), labels=c(seq(0,9,1),' ','NR'))+
#         
#     print(p1)
# #    levels(recov_stat_full$N_Signif_Rec)[length(levels(recov_stat_full$N_Signif_Rec))]='11'
#  #   levels(recov_stat_full$N_Signif_Rec_Er)[length(levels(recov_stat_full$N_Signif_Rec_Er))]='11'
#     
#     summary = ddply(recov_stat_full, .(Area_Cat,N_Signif_Rec, N_Signif_Rec_Er), summarize, count = length(N_Signif_Rec),.drop = FALSE)
#     summary_totals = ddply(summary, .(Area_Cat,N_Signif_Rec  ), summarize, total = sum(count), .drop = FALSE)
#     summary2 = join(summary, summary_totals, by = c('N_Signif_Rec', 'Area_Cat'))
#     summary2$Percentage =100* summary2$count/summary2$total
#     dove_NaN = which(summary2$Percentage == 'NaN')
#     summary2$Percentage [dove_NaN] = NA
#     dove_zero = which(summary2$Percentage == 0)
#     summary2$Percentage [dove_zero] = NA
#     summary2$N_Signif_Rec_Er [dove_zero] = NA
#     summary2$N_Signif_Rec_Er [dove_NaN] = NA
#     summary2= droplevels(subset(summary2,N_Signif_Rec_Er != 'NA' ))
    
#      browser()
    
#     summary = ddply(recov_stat_full, .(Area_Cat,N_Signif_Rec, N_Signif_Rec_Er), summarize, count = length(N_Signif_Rec),.drop = FALSE)
#     summary_totals = ddply(summary, .(Area_Cat  ), summarize, total = sum(count), .drop = FALSE)
#     summary = join(summary, summary_totals, by = c( 'Area_Cat'))
#     summary$Percentage =100* summary$count/summary$total
#     summary_equals = summary[which(as.numeric(as.character(summary$N_Signif_Rec)) == as.numeric(as.character(summary$N_Signif_Rec_Er))),] 
#     summary_changes = ddply(summary_equals, .(Area_Cat), summarize, change = 100-sum(Percentage, na.rm = T))
#     summary = join(summary, summary_changes)
# #     summary_changes = ddply(summary, .(Area_Cat  ), summarize, change = sum(Percentage[which(N_Signif_Rec_Er != N_Signif_Rec)]), .drop = FALSE)
#     dove_NaN = which(summary$Percentage == 'NaN')
#     summary$Percentage [dove_NaN] = NA
#     dove_zero = which(summary$Percentage == 0)
#     summary$Percentage [dove_zero] = NA
#     summary$N_Signif_Rec_Er [dove_zero] = NA
#     summary$N_Signif_Rec_Er [dove_NaN] = NA
#     summary= droplevels(subset(summary,N_Signif_Rec_Er != 'NA' ))
#     
#     levels(summary$N_Signif_Rec)[length(levels(summary$N_Signif_Rec))]='NR'
#     levels(summary$N_Signif_Rec_Er)[length(levels(summary$N_Signif_Rec_Er))]='NR'
#     
#     summary_changes$x = 1
#     summary_changes$y = 0
#     summary_changes$Percentage_cut = 0
#     summary_changes$Text = paste('Total Variation = ',format(summary_changes$change,digits = 4),'%')
#     
# #     browser()
#     
#     title = paste("Recovery Time - Original vs Eroded Burnt Areas\n\n",Index,' - ',str_opt, sep = '')  
#     summary$Percentage_cut = cut(summary$Percentage, breaks = c(0,1,5,15,30,50))
#     p = ggplot(summary)+ my_theme_bw
#     p = p + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#     p = p + xlab('Recovery Time - On Original Burnt Areas')+ ylab ('Recovery Time - On Eroded Burnt Areas')
#     p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec_Er),
#                              breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE,
#                              labels =c(seq(0,(length(levels(summary$N_Signif_Rec_Er))-2),1),'NR' ))
#     p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec_Er),
#                               breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE, 
#                               labels =c(seq(0,length(levels(summary$N_Signif_Rec_Er))-2,1),'NR' ))
#     
#     p = p + geom_point(aes(x = N_Signif_Rec, y = N_Signif_Rec_Er, 
#                            size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) +geom_abline(intercept = 0, slope = 1)
#     
#     p = p +  scale_fill_hue()
#     p = p + scale_size_discrete(range = c(2, 6))
#     p = p + facet_wrap(~Area_Cat,drop = FALSE)
#     p = p + geom_text(aes(x=6, y=1, label=Text), data = summary_changes, size = 4,hjust=0.5)
#    
#     print(p)
#     p = p + scale_fill_discrete('Percentage of Cases\n(On total number per Area Class)\n',colours = heat.colors(7),breaks = c(0,1,5,10,15,30,50),guide = "legend", na.value = NA)
#     p = p + scale_size_area('Percentage of Cases\n(On total number per Area Class)\n', trans ='log', breaks = c(0,1,5,10,25,50,75, 100), na.value = NA)
#     p = p + scale_size_area('Percentage of Cases\n(On total number per Area Class)\n', trans ='log', na.value = NA)
#     p = p + scale_size_area('Percentage of Cases\n(On total number per Area Class)\n',breaks = c(0,1,5,10,15,30,50), na.value = NA, max_size = 8)
#     p = p + scale_size('Percentage of Cases\n(On total number per Area Class)\n',range = c(2,3,4,5,6), na.value = NA)
    
    
#     load(files_Normal[cy])
#     recov_stat_full = recov_stat
#     recov_stat_full$recov_str = NULL
#     recov_stat_full$Type = 'Normal'
#     load(files_Eroded[cy])
#     recov_stat$Type = 'Eroded'
#     recov_stat_full = rbind(recov_stat, recov_stat_full)
#     recov_stat_full_tmp = recov_stat_full
#     recov_stat_full_tmp$Area_Cat = 'All'
#     recov_stat_full = rbind(recov_stat_full_tmp, recov_stat_full)
#     recov_stat_full = droplevels(subset(recov_stat_full, Comp_N == 3 & CLC_Class == 'All'))
#     recov_stat_full$Area_Cat = factor(recov_stat_full$Area_Cat, levels = c("All","(-1,250]", "(250,500]","(500,1e+03]","(1e+03,5e+03]",
#                                                                            "(5e+03,1e+04]", "(1e+04,1e+05]"))
#     
#     levels(recov_stat_full$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
#     
#     p1 = ggplot(recov_stat_full, aes(x = (N_Signif_Rec), fill = Type))
#     p1 = p1 + geom_bar(position = position_dodge())
#     p1 = p1 + theme_bw()+xlab('Recovery Time')+ylab('Number of cases')
#     p1 = p1+ facet_wrap(~Area_Cat, scales = 'free_y', drop = FALSE)
#     title = paste("Recovery Time - Original vs Eroded Burnt Areas\n\n",Index,' - ',str_opt, sep = '')
#     p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))+theme_bw()
# #     print(p1)
#   }
#   dev.off()
# }


