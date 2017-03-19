# TODO: Add comment
# 
# Compoute percentages of fires recovered after YY years; 
# Create the pdf plots showing varitions with variable MedWdt
# Author: busetlo
###############################################################################

# library(ggplot2)
# library(plyr)
# library(reshape)
# memory.limit(7000)

# # Used to perform analysis on multiple results from multiple indexes and minimum percentages - to be removed on final !!!
# 
# files_in = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              #
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData'
# )
# 
# kernels = c(10,25,50,100,150,200,10,25,50,100,150,200)
# 
# files_in = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData', 
#              'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData')
# 
# kernels = c(50,50)
# 
# 
# 
# sig_level = 0.05  					# MODIFY  ! Must be an input parameter to the stat analysis !
# 
# perc_diff = 10
# use_subset = 0
# max_N_Years_after = 6
# 
# End_Year = 2012
# 
# 
# res = FRG_Compute_Percentages_Matrix(files_in, kernels, sig_level, use_subset, max_N_Years_after,End_Year)

FRG_Compute_Percentages_Matrix_CatArea = function(file_in_Normal, file_in_Eroded, kernels, sig_level, use_subset, 
                                                  max_N_Years_after, perc_diff,End_Year, sub_zones, erode,min_pix) {
  #   recov_stat_Full = NULL
  Perc_Recovered_Full = NULL
  Perc_UnRecovered_Full = NULL
  Perc_Recovered_YY_Full = NULL
  
  kernel = kernels
  max_Rec_yy = End_Year - 2003
  
  
  if (erode == 0 ) {
    load(file_in_Eroded)
    recov_Eroded = recov_stat
    load(file_in_Normal)
    if (use_subset == 1 ) {
        recov_stat = droplevels(subset(recov_stat, N_Years_After<=max_N_Years_after))
        recov_Eroded= droplevels(subset(recov_Eroded, N_Years_After<=max_N_Years_after))
        max_Rec_yy = max(as.numeric(as.character(levels(recov_stat$N_Signif))))
     }
    print(paste('Computing Recovery Percentages from:',file_in_Normal))
    for(common in c("All","Common")) {
      
      if(common =='Common') {
        if (use_subset == 0) {file_out = file.path(dirname(file_in_Normal),'Percentages',paste('Percentages_Area_Common',perc_diff,'%.RData',sep = ''))}
        if (use_subset == 1) {file_out = file.path(dirname(file_in_Normal),'Percentages_Subset',paste('Percentages_Subset_Area_Common',perc_diff,'%.RData',sep = ''))}
        #         browser()
        dir.create(dirname(file_out), recursive = T)
        recov_eroded_str = paste(recov_Eroded$OBJECTID, recov_Eroded$CLC_Class)
        recov_stat$recov_str = paste(recov_stat$OBJECTID, recov_stat$CLC_Class)
        recov_stat = droplevels(subset(recov_stat, recov_str %in% recov_eroded_str))
        er = Comp_Percentages(recov_stat,  max_Rec_yy,  use_subset, max_N_Years_after, file_out)
        load(file_out)
        erPlot =   Plot_Pdf_percentages (Perc_Recovered,  Perc_Recovered_YY, Perc_UnRecovered, file_in_Normal, erode, use_subset, kernel, min_pix, common) 
        
      } else {
        if (use_subset == 0) {file_out = file.path(dirname(file_in_Normal),'Percentages',paste('Percentages_Area',perc_diff,'%.RData',sep = ''))}
        if (use_subset == 1) {file_out = file.path(dirname(file_in_Normal),'Percentages_Subset',paste('Percentages_Subset_Area',perc_diff,'%.RData',sep = ''))}
        dir.create(dirname(file_out), recursive = T)
        er = Comp_Percentages(recov_stat,  max_Rec_yy,  use_subset, max_N_Years_after, file_out)
        load(file_out)
        erPlot =   Plot_Pdf_percentages (Perc_Recovered,  Perc_Recovered_YY, Perc_UnRecovered, file_in_Normal, erode, use_subset, kernel, min_pix, common) 
        
      }
      
      
    }
    #     if (sub_zones == 0) { 
    #       In_Folder = file.path(dirname(dirname(file_in)),'Stat_Analysis',paste(perc_diff,'%',sep = ''))
    #     } else {
    #       In_Folder = file.path(dirname(dirname(file_in)),'Stat_Analysis','ENV',paste(perc_diff,'%',sep = ''))  
    #     }
    
    
    
    #     print(file_in)
    
    
    
    #     recov_stat = FRG_CumStat_Matrix(plot_stat,Sig_Years_median_tot, Data_Median )
    #      browser()
    
    rm(recov_stat, Perc_Recovered, Perc_Recovered_YY, Perc_UnRecovered)
    
  } else {
    common = 'All'
    load(file_in_Eroded)
    if (use_subset == 1 ) {
        recov_stat = droplevels(subset(recov_stat, N_Years_After<=max_N_Years_after))
        max_Rec_yy = max(as.numeric(as.character(levels(recov_stat$N_Signif))))
     }
    print(paste('Computing Recovery Percentages from:',file_in_Eroded))
    if (use_subset == 0) {file_out = file.path(dirname(file_in_Eroded),'Percentages',paste('Percentages_Area',perc_diff,'%.RData',sep = ''))}
    if (use_subset == 1) {file_out = file.path(dirname(file_in_Eroded),'Percentages_Subset',paste('Percentages_Subset_Area',perc_diff,'%.RData',sep = ''))}
    dir.create(dirname(file_out), recursive = T)
    er = Comp_Percentages(recov_stat, max_Rec_yy,  use_subset, max_N_Years_after, file_out)
    load(file_out)
    erPlot =   Plot_Pdf_percentages (Perc_Recovered,  Perc_Recovered_YY, Perc_UnRecovered, file_in_Eroded, erode, use_subset, kernel, min_pix, common) 
    rm(recov_stat, Perc_Recovered, Perc_Recovered_YY, Perc_UnRecovered)
  }
  #   }
  
}


Comp_Percentages <- function (recov_stat,  max_Rec_yy,  use_subset, max_N_Years_after, file_out) {
  print(paste('Saving Recovery Percentages to:',file_out))
  recov_stat$Index = as.factor(recov_stat$Index)
  recov_stat$Min_Percentage = as.factor(recov_stat$Min_Percentage)
  
  #     browser()
  #     recov_stat$Area = 250*250*recov_stat$N_PIX/10000 
  recov_stat$Area_Cat = cut(recov_stat$Area_CLC, breaks = c(-1,250,500,1000,5000,10000,100000))
  
  #     recov_stat$Area_Cat_CLC = cut(recov_stat$Area_CLC, breaks = c(0,250,500,1000,5000,10000,100000))
  
  
  #     browser()
  #     recov_stat_Full = rbind(recov_stat, recov_stat_Full)
  
  #     recov_stat$YearFromFire = as.numeric(as.character(recov_stat$YearFromFire))
  #     recov_stat$YearFromFire_fc =as.factor((11+1*recov_stat$YearFromFire))
  #     file_out = paste(file,'_',perc,'_','Joined_Stats.RData', sep = '')
  #     save(recov_stat, file = file_out)     # Save the "cumulate"stats, i.e., the matrixes obtained by joining results obtained using different widths for the median 
  #     recov_stat_full = rbind(recov_stat_full, recov_stat)
  # Compute asnd save the computed "Percentages" statistics. These collects the number of fires recovered after YY years, 
  # The number of fires not recovered, the percentages (computed adjusting for the time serie length), ecc (IMPROVE THIS DESCRIPTION !!!)
  
  #    max_Rec_yy = max(as.numeric(as.character(levels(recov_stat$N_Signif))))    
  
  
  
  # Compute percentages - Old methods computing ration between Total N(RT = k)/Total N(Yf = >=k)
  
  Perc_Recovered_AreaFull = ddply(recov_stat, .(Index,Comp_N,CLC_Class,ENV_ZONE), function(df) Compute_Perc_Recov (df, max_Rec_yy), .drop = F,.progress = "text")
  Perc_Recovered_AreaFull$Area_Cat = 'All'
  Perc_Recovered = ddply(recov_stat, .(Index,Comp_N,CLC_Class,ENV_ZONE,Area_Cat), function(df) Compute_Perc_Recov (df, max_Rec_yy), .drop = F,.progress = "text")
  Perc_Recovered = rbind(Perc_Recovered_AreaFull, Perc_Recovered)
  Perc_Recovered$CLC_Class = factor(Perc_Recovered$CLC_Class)
  Perc_Recovered$ENV_ZONE = factor(Perc_Recovered$ENV_ZONE)
  Perc_Recovered$Index = ordered(Perc_Recovered$Index)    
  Perc_Recovered$Comp_N = ordered(Perc_Recovered$Comp_N)
  Perc_Recovered$Min_Percentage = recov_stat$Min_Percentage[1]
  
  # Compute percentages - Different percentages for each year
  Perc_Recovered_YY_AreaFull = ddply(recov_stat, .(Index,Comp_N,CLC_Class,ENV_ZONE,FireYear), function(df) Compute_Perc_Recov (df, max_Rec_yy), .drop = F,.progress = "text")
  Perc_Recovered_YY_AreaFull$Area_Cat = 'All'
  Perc_Recovered_YY = ddply(recov_stat, .(Index, Comp_N,CLC_Class,ENV_ZONE,Area_Cat,FireYear), function(df) Compute_Perc_Recov_YY (df, max_Rec_yy), .drop = F, .progress = 'text')
  Perc_Recovered_YY = rbind(Perc_Recovered_YY_AreaFull, Perc_Recovered_YY)
  Perc_Recovered_YY$CLC_Class = factor(Perc_Recovered_YY$CLC_Class)
  Perc_Recovered_YY$ENV_ZONE = factor(Perc_Recovered_YY$ENV_ZONE)
  Perc_Recovered_YY$Index = ordered(Perc_Recovered_YY$Index)    
  Perc_Recovered_YY$Comp_N = ordered(Perc_Recovered_YY$Comp_N)
  Perc_Recovered_YY$Min_Percentage =  recov_stat$Min_Percentage[1]
  
  # Compute percentages - Unrecovered for each year - useless and to be removed
  Perc_UnRecovered = ddply(recov_stat, .(Index,Comp_N,CLC_Class,ENV_ZONE,Area_Cat), function(df) Compute_Perc_Unrec_YY (df, max_Rec_yy),.drop = F,.progress = "text")
  
  Perc_UnRecovered$CLC_Class = factor(Perc_UnRecovered$CLC_Class)
  Perc_UnRecovered$ENV_ZONE = factor(Perc_UnRecovered$ENV_ZONE)
  Perc_UnRecovered$Index = ordered(Perc_UnRecovered$Index)    
  Perc_UnRecovered$Comp_N = ordered(Perc_UnRecovered$Comp_N)
  Perc_UnRecovered$Min_Percentage =  recov_stat$Min_Percentage[1]
  
  #     browser()
  if (use_subset == 1 ) {
    Perc_Recovered = droplevels(subset(Perc_Recovered, (as.numeric(as.character(Comp_N)) <= 11-max_N_Years_after)))
    Perc_UnRecovered = droplevels(subset(Perc_UnRecovered, (as.numeric(as.character(Comp_N)) <= 11-max_N_Years_after)))
    Perc_Recovered_YY = droplevels(subset(Perc_Recovered_YY, (as.numeric(as.character(Comp_N)) <= 11-max_N_Years_after)))
  }
  
  #     file_out_perc = paste(file,'_',perc,'%_','Percentages.RData', sep = '')
  save(recov_stat, Perc_Recovered, Perc_Recovered_YY, Perc_UnRecovered,  file = file_out)
  
}

#- -----------------------------------------------------
# Start Creating the pdf plot reports - skippable on final ????
#- ------------------------------------------------------------

Plot_Pdf_percentages <- function (Perc_Recovered,  Perc_Recovered_YY, Perc_UnRecovered, file_in, erode, use_subset, kernel, min_pix, common) {
  Index = Perc_Recovered$Index[1]
  Min_Percentage = Perc_Recovered$Min_Percentage[1]
  Comp_N = Perc_Recovered$Comp_N[1]
  Perc_Recovered$Percentage = 100*Perc_Recovered$Percentage 
  
  Perc_Recovered$N_Years = ordered(Perc_Recovered$N_Years)
  Perc_Recovered$Comp_N = ordered(as.numeric(as.character(Perc_Recovered$Comp_N)))
  #     browser()
  plot_folder = file.path(dirname(dirname(dirname(dirname(file_in)))),'Plots')
  if (erode == 0 ) {plot_folder = file.path(plot_folder,'Normal')} else  {plot_folder = file.path(plot_folder,'Eroded')}
  dir.create(plot_folder, recursive = T )  
  
  if (use_subset == 0) {pdf_out = file.path(plot_folder,paste('Result_Graphs_',Index,'_', Min_Percentage,'_',kernel,'_',min_pix,common,'.pdf',sep = ''))}
  if (use_subset == 1) {pdf_out = file.path(plot_folder,paste('Result_Graphs_',Index,'_', Min_Percentage,'_',kernel,'_',min_pix,common,'_subset.pdf',sep = ''))}
  print(paste('Output File of graphical reaults: ',pdf_out))
  
  
  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 13, vjust =2, hjust = 0.5),
                                  axis.title.x = element_text( face = "bold", vjust = 0, size = 12, angle = 0 ),  axis.title.y = element_text( face = "bold", hjust = 0.5, vjust = 0.3,size = 12, angle = 90))
  levels(Perc_Recovered$N_Years) = c('0', '1','2','3','4','5','6','7','8','9',' ','NR')
  levels(Perc_Recovered$CLC_Class) = c('All','Broadleaved','Coniferous','Mixed','Schleropyllus','Transitional')
  #     browser()
  ENV_ZONE_Sub = 'All'    # Take only data for "All" ENV_ZONES - maybe later analysis on variations with the selected Index and median widyth
  Area_Sub = 'All'
  
  #     browser()
  
  Perc_Rec_sub = droplevels(subset(Perc_Recovered, ENV_ZONE %in% ENV_ZONE_Sub & Area_Cat %in% Area_Sub))          
  Perc_Rec_sub$N_Years = ordered(Perc_Rec_sub$N_Years)
  Perc_Rec_sub$Comp_N = ordered(as.numeric(as.character(Perc_Rec_sub$Comp_N)))
  levels(Perc_Recovered$N_Years) = c('0', '1','2','3','4','5','6','7','8','9',' ','NR')
  
  diff_func = function(Data) {
    
    Data_diffs_perc =  Data$Percentage - Data$Percentage[1]
    Data_diffs_Number = Data$N_Rec_YY - Data$N_Rec_YY[1]
    #       browser()
    if (use_subset == 1) {Comp_N = seq(1,5,1)} else {Comp_N = seq(1,11,1)}
    
    out = data.frame(Comp_N = Comp_N, Data_diffs_perc = Data_diffs_perc,Data_diffs_Number=Data_diffs_Number)
    out
  }
  # Compute the differences in percentages of recovered fires and number of recovered fires when extending the width of the median
  Perc_Rec_Diff = ddply(Perc_Rec_sub, .(CLC_Class,N_Years), function(df) diff_func (df),.progress = "text")
  Perc_Rec_sub = join(Perc_Rec_sub,Perc_Rec_Diff)
  
  pdf(file = pdf_out,8.5,10.5, pointsize = 4)
  
  #- ----------------------------------------- - 
  # Plot number of fires vs recovery time, variable with median width and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
  p = p + geom_bar(aes(y = N_Rec_YY),  stat=  'identity', position = position_dodge(width = 0.9)) 
  p = p + facet_grid(CLC_Class~Comp_N, scales = "free_y") + theme(legend.position="none")
  p = p + xlab('Recovery Time (Years)') + ylab ('Number of Fires')
  p = p + ggtitle(paste(Index, ' - Number of Fires vs Recovery Time for different median Widths',sep = ''))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  #- ----------------------------------------- - 
  # Plot  %  of fires vs recovery time, variable with median width and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
  p = p + geom_bar(aes(y = Percentage),  stat=  'identity', position = position_dodge(width = 0.9)) 
  p = p + facet_grid(CLC_Class~Comp_N, scales = "free_y") + theme(legend.position="none")
  p = p + xlab('Recovery Time (Years)') + ylab ('% of cases')
  p = p + ggtitle(paste(Index, 'Percentage of Cases vs Recovery Time for different median Widths',sep = ' '))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  #- ----------------------------------------- - 
  # Plot number of fires vs Median Width, variable with recovery time and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = N_Rec_YY),  stat=  'identity',size = 0.8) 
  p = p + facet_wrap(~N_Years, scales = "free_y") #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('Number of Fires')
  p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
  p = p + ggtitle(paste(Index, ' - Number of Fires vs Median Width for different Recovery Times',sep = ''))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  #- ----------------------------------------- - 
  # Plot % of fires vs Median Width, variable with recovery time and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = Percentage),  stat=  'identity',size = 0.8) 
  p = p + facet_wrap(~N_Years) #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('% of Cases')
  p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
  p = p + ggtitle(paste(Index, ' - % of Cases vs Median Width for different Recovery Times',sep = ''))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  #- ----------------------------------------- - 
  # Plot variation of number of fires vs Median Width, variable with recovery time and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = Data_diffs_Number),  stat=  'identity',size = 0.8) 
  p = p + facet_wrap(~N_Years, scales = 'free_y') #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('Variation of Number of Cases')
  p = p + ggtitle(paste(Index, 'Variation of Number of cases vs Median Width, for different Recovery Times',sep = ' '))
  p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+scale_colour_brewer(palette = "Set2")
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  #- ----------------------------------------- - 
  # Plot variation of % of cases vs Median Width, variable with recovery time and CLC_Class
  #- ----------------------------------------- - 
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = Data_diffs_perc),  stat=  'identity',size = 0.8) 
  p = p + facet_wrap(~N_Years)
  p = p + xlab('Median Width (Years)') + ylab ('Variation of % of cases')
  p = p + ggtitle(paste(Index, 'Variation of % of cases vs Median Width, for different Recovery Times',sep = ' '))
  p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+scale_colour_brewer(palette = "Set2")
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  
  dev.off()
  
  rm(recov_stat, Perc_Recovered, Perc_Recovered_YY, Perc_UnRecovered)
  gc()
}

#- ----------------------------------------------- -
#- Helper function used to compute, for each "category" the percentage of fires happened at least N-Years years 
#- before the end the time serie, which recovered exactly N-Years after fire
#- ------------------------------------------------ -

Compute_Perc_Recov <- function(Data, max_Rec_yy) {
  N_Tot = length(Data$Recov)     								# Total number of fires in category
  N_Tot_Unrec = length(which(Data$Recov == 'UnRecovered')) 					# Total number of unRecovered fires
  N_Tot_Rec = length(which(Data$Recov == 'Recovered'))						# Total number of Recovered fires
  N_Tot_YY = NULL																# Total number of Recovered fires	happened at least N-Years before
  N_Rec_YY = NULL																	# Total number of fires Recovered/unRecovered exactly N-Years after fire
  Data_Rec = droplevels(subset(Data, Recov == 'Recovered'))				# Keep only Recovered fires
  for (N_years in seq(0,max_Rec_yy)) {
    
    N_Tot_YY_tmp = length(which(Data$N_Years_After >= N_years ))		# Total number of fires Recovered/unRecovered happened at least N-Years before
    N_Tot_YY = c(N_Tot_YY,N_Tot_YY_tmp )
    N_Rec_YY_tmp = length(which(Data_Rec$N_Signif == N_years))				# Total number of fires recovered/unrecovered exactly N-Years after fire
    N_Rec_YY = c(N_Rec_YY,N_Rec_YY_tmp )
  }
  Percs_Rec = N_Rec_YY/N_Tot_YY                     # Compute percentages
  Percs_Rec [which (is.finite(Percs_Rec) == F)] = 0      # Put to zero the Nan originated if 0 fires recovered after YY
  #   if (max(Percs_Rec) == 1) {browser()}
  
  N_Rec_YY = c(N_Rec_YY,N_Tot_Unrec)                    # Create array composed by number of fires recovered after YY and number of unrecovered
  N_Tot_YY = c(N_Tot_YY,999)                     # Create array of total number of fires considered for computing the percentages
  N_Years = c(seq(0,max_Rec_yy),999)                    # Create array of number of years + 999 for unrecovered
  Percs_Rec = c(Percs_Rec,N_Tot_Unrec/N_Tot)                          # Create array of percentages + percentage of unrecovered vs total
  out = data.frame(N_Years = N_Years, Percentage = Percs_Rec, N_Rec_YY = N_Rec_YY, N_Tot_YY = N_Tot_YY,  N_Tot_Rec = N_Tot_Rec, N_Tot_Unrec = N_Tot_Unrec, N_Tot = N_Tot )
}



#Old Method

# Compute_Perc_Recov <- function(Data, max_Rec_yy) {
#   N_Tot = length(Data$Recov)   									# Total number of fires in category
#   N_Tot_Unrec = length(which(Data$Recov == 'UnRecovered')) 					# Total number of unRecovered fires
#   N_Tot_Rec = length(which(Data$Recov == 'Recovered'))						# Total number of Recovered fires
#   N_Tot_Rec_YY = NULL																# Total number of Recovered fires	happened at least N-Years before
#   N_Rec_YY = NULL																	# Total number of fires Recovered/unRecovered exactly N-Years after fire
#   Data_Rec = droplevels(subset(Data, Recov == 'Recovered'))				# Keep only Recovered fires
#   for (N_years in seq(0,max_Rec_yy)) {
#     
#     N_Tot_Rec_YY_tmp = length(which(Data_Rec$N_Years_After >= N_years ))		# Total number of fires Recovered/unRecovered happened at least N-Years before
#     N_Tot_Rec_YY = c(N_Tot_Rec_YY,N_Tot_Rec_YY_tmp )
#     N_Rec_YY_tmp = length(which(Data_Rec$N_Signif == N_years))				# Total number of fires recovered/unrecovered exactly N-Years after fire
#     N_Rec_YY = c(N_Rec_YY,N_Rec_YY_tmp )
#   }
#   Percs_Rec = N_Rec_YY/N_Tot_Rec_YY                     # Compute percentages
#   Percs_Rec [which (is.finite(Percs_Rec) == F)] = 0      # Put to zero the Nan originated if 0 fires recovered after YY
#   
#   
#   N_Rec_YY = c(N_Rec_YY,N_Tot_Unrec)                    # Create array composed by number of fires recovered after YY and number of unrecovered
#   N_Tot_Rec_YY = c(N_Tot_Rec_YY,999)                     # Create array of total number of fires considered for computing the percentages
#   N_Years = c(seq(0,max_Rec_yy),999)                    # Create array of number of years + 999 for unrecovered
#   Percs_Rec = c(Percs_Rec,N_Tot_Unrec/N_Tot)                          # Create array of percentages + percentage of unrecovered vs total
#   out = data.frame(N_Years = N_Years, Percentage = Percs_Rec, N_Rec_YY = N_Rec_YY, N_Tot_Rec_YY = N_Tot_Rec_YY,  N_Tot_Rec = N_Tot_Rec, N_Tot_Unrec = N_Tot_Unrec, N_Tot = N_Tot )
# }

#- --------------------------------------------------------------------------- -#
#- Helper function to Compute statistics relative to percentage of fires unrecovered after YY years, separated by FireYear 
#- --------------------------------------------------------------------------- -#
Compute_Perc_Recov_YY <- function(Data, max_Rec_yy) {
  N_Tot = length(Data$Recov)   									# Total number of fires in category
  N_Tot_Unrec = length(which(Data$Recov == 'UnRecovered')) 					# Total number of unrecovered fires
  N_Tot_Rec = length(which(Data$Recov == 'Recovered'))						# Total number of Recovered fires
  N_Tot_YY = NULL																# Total number of Recovered fires	happened at least N-Years before
  N_Rec_YY = NULL																	# Total number of fires recovered/unrecovered exactly N-Years after fire
  Data_Rec = droplevels(subset(Data, Recov == 'Recovered'))				# Keep only recovered fires
  #      browser()
  for (N_years in seq(0,max_Rec_yy)) {
    
    N_Tot_YY_tmp = length(which(Data$N_Years_After >= N_years ))		# Total number of fires recovered/unrecovered happened at least N-Years before
    N_Tot_YY = c(N_Tot_YY,N_Tot_YY_tmp )
    N_Rec_YY_tmp = length(which(Data_Rec$N_Signif == N_years ))				# Total number of fires recovered/unrecovered exactly N-Years after fire
    N_Rec_YY = c(N_Rec_YY,N_Rec_YY_tmp )
  }
  Percs_Rec = N_Rec_YY/N_Tot_YY                     # Compute percentages
  Percs_Rec [which (is.finite(Percs_Rec) == F)] = 0      # Put to zero the Nan originated if 0 fires recovered after YY
  N_Rec_YY = c(N_Rec_YY,N_Tot_Unrec)
  N_Years = c(seq(0,max_Rec_yy),999)
  Percs_Rec = c(Percs_Rec,N_Tot_Unrec/N_Tot)                          # Create array of percentages + percentage of unrecovered vs total
  N_Tot_YY = c(N_Tot_YY,999)
  
  out = data.frame(N_Years = N_Years, Percentage = Percs_Rec, N_Rec_YY = N_Rec_YY, N_Tot_YY = N_Tot_YY,  N_Tot_Rec = N_Tot_Rec, N_Tot_Unrec = N_Tot_Unrec, N_Tot = N_Tot )
}

# #Old Method
# Compute_Perc_Recov_YY <- function(Data, max_Rec_yy) {
#   N_Tot = length(Data$Recov)     								# Total number of fires in category
#   N_Tot_Unrec = length(which(Data$Recov == 'UnRecovered')) 					# Total number of unrecovered fires
#   N_Tot_Rec = length(which(Data$Recov == 'Recovered'))						# Total number of Recovered fires
#   N_Tot_Rec_YY = NULL																# Total number of Recovered fires	happened at least N-Years before
#   N_Rec_YY = NULL																	# Total number of fires recovered/unrecovered exactly N-Years after fire
#   Data_Rec = droplevels(subset(Data, Recov == 'Recovered'))				# Keep only recovered fires
#   #      browser()
#   for (N_years in seq(0,max_Rec_yy)) {
#     
#     N_Tot_Rec_YY_tmp = length(which(Data_Rec$N_Years_After >= N_years ))		# Total number of fires recovered/unrecovered happened at least N-Years before
#     N_Tot_Rec_YY = c(N_Tot_Rec_YY,N_Tot_Rec_YY_tmp )
#     N_Rec_YY_tmp = length(which(Data_Rec$N_Signif == N_years ))				# Total number of fires recovered/unrecovered exactly N-Years after fire
#     N_Rec_YY = c(N_Rec_YY,N_Rec_YY_tmp )
#   }
#   Percs_Rec = N_Rec_YY/N_Tot_Rec_YY                     # Compute percentages
#   Percs_Rec [which (is.finite(Percs_Rec) == F)] = 0      # Put to zero the Nan originated if 0 fires recovered after YY
#   N_Rec_YY = c(N_Rec_YY,N_Tot_Unrec)
#   N_Years = c(seq(0,max_Rec_yy),999)
#   Percs_Rec = c(Percs_Rec,N_Tot_Unrec/N_Tot)                          # Create array of percentages + percentage of unrecovered vs total
#   N_Tot_Rec_YY = c(N_Tot_Rec_YY,999)
#   
#   out = data.frame(N_Years = N_Years, Percentage = Percs_Rec, N_Rec_YY = N_Rec_YY, N_Tot_Rec_YY = N_Tot_Rec_YY,  N_Tot_Rec = N_Tot_Rec, N_Tot_Unrec = N_Tot_Unrec, N_Tot = N_Tot )
# }


#- --------------------------------------------------------------------------- -#
#- Compute statistics relative to number and percentage of fires unrecovered after YY years 
#- --------------------------------------------------------------------------- -#

Compute_Perc_Unrec_YY <- function(Data, max_Rec_yy) {
  N_Tot = NULL  				; N_UnRec = NULL
  for (N_years in seq(1,max_Rec_yy)) {
    N_Tot_YY = length(which(Data$N_Years_After == N_years ))		# Total number of fires happened N-Years before
    N_Tot = c(N_Tot,N_Tot_YY )
    N_UnRec_YY = length(which(Data$Recov == 'UnRecovered'  & (as.numeric(as.character(Data$N_Signif)) >= N_years-1 )))				# Total number of fires recovered/unrecovered exactly N-Years after fire
    N_UnRec = c(N_UnRec,N_UnRec_YY )
    #     browser()
  }
  #  browser()
  N_Tot_Unrec = sum(N_UnRec)
  Percs_UnRec = N_UnRec/N_Tot
  out = data.frame(N_Years = seq(1,max_Rec_yy), Percentage = Percs_UnRec, N_UnRec = N_UnRec, N_Tot = N_Tot,  N_Tot_Unrec = N_Tot_Unrec )
}


# Perc_Recovered_Full$CLC_Class = factor(Perc_Recovered_Full$CLC_Class)
# Perc_Recovered_Full$ENV_ZONE = factor(Perc_Recovered_Full$ENV_ZONE)
# Perc_Recovered_Full$Index = ordered(Perc_Recovered_Full$Index)    
# Perc_Recovered_Full$Comp_N = ordered(Perc_Recovered_Full$Comp_N)
# Perc_Recovered_Full$Min_Percentage = perc
# 
# Perc_Recovered_YY_Full$CLC_Class = factor(Perc_Recovered_YY_Full$CLC_Class)
# Perc_Recovered_YY_Full$ENV_ZONE = factor(Perc_Recovered_YY_Full$ENV_ZONE)
# Perc_Recovered_YY_Full$Index = ordered(Perc_Recovered_YY_Full$Index)    
# Perc_Recovered_YY_Full$Comp_N = ordered(Perc_Recovered_YY_Full$Comp_N)
# Perc_Recovered_YY_Full$Min_Percentage = perc
# 
# Perc_UnRecovered_Full$CLC_Class = factor(Perc_UnRecovered_Full$CLC_Class)
# Perc_UnRecovered_Full$ENV_ZONE = factor(Perc_UnRecovered_Full$ENV_ZONE)
# Perc_UnRecovered_Full$Index = ordered(Perc_UnRecovered_Full$Index)    
# Perc_UnRecovered_Full$Comp_N = ordered(Perc_UnRecovered_Full$Comp_N)
# Perc_UnRecovered_Full$Min_Percentage = perc
# 
# 
# # file_out_perc_full = file.path(dirname(file), "Percentages_Full.RData")
# save(recov_stat_Full, Perc_Recovered_Full, Perc_Recovered_YY_Full, Perc_UnRecovered_Full,  file = file_out_perc_full)

# recov_stat_rec = droplevels(subset(recov_stat, N_Signif_Rec != 'NR' ))								# Remove fires not recovered

#- ----------------------------------------------- -
#- Helper function used to create a cumulative stats matrix from results obtained using different median widths
#- ------------------------------------------------ -
# 
# FRG_CumStat_Matrix = function(plot_stat, Sig_Years_median_tot, Data_Median) {  # Join results obtained on multiple runs with varying parameters
#   
# #   a = load(file_in)					# Load the statistical analysis results -   # File Derived from analysis of p-values matrixes
#   Data_Median = Sig_Years_median_tot
#   Data_Median$Comp_N = ordered(Data_Median$Comp_N , levels = c('1','2','3','4','5','6','7','8','9','10','11'))
#   Data_Median$N_Signif= factor(Data_Median$N_Signif, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9","10"))
#   plot_stat_full = NULL
#   plot_stat_first = ddply(plot_stat, .(CASE_ID, CLC_Class, ENV_ZONE), summarize, OBJECTID = OBJECTID[1],FireYear = FireYear [1],YearFromFire = YearFromFire [1],N_PIX = N_PIX[1] ,
#                          Index= attr(plot_stat,"Index"), .parallel = F, .progress = "text")
#   plot_stat_first$N_Years_After = max(as.numeric(as.character(levels(plot_stat$FireYear))))-   # Compute the variable "N_Years_After. Contains the number of "available" years after the fire in the MODIS time serie. 
#     as.numeric(as.character(plot_stat_first$FireYear))+1
#   for (N in (1:max(Data_Median$Comp_N))) {
#     
#     Data_Median_sub = droplevels(subset(Data_Median, Comp_N == N))    # Get results obtained using a median of width N
#     plot_stat_tmp = plot_stat_first
#     plot_stat_tmp = join(plot_stat_tmp, Data_Median_sub,by = 'CASE_ID') # Join plot_stat with information related to significance. 
#     plot_stat_tmp$Min_Percentage = attr(Sig_Years_median_tot,'Min_Percentage')
#     #      browser()
#     plot_stat_full = rbind(plot_stat_full, plot_stat_tmp)                # Add to the output matrix information on results obtained considering a median width of N
#     print(N)
#   }
#   
#   plot_stat_full
# }

#- --------------------------------------------------------------------------- -#
#- Plotting of recovery statistics - 
#- --------------------------------------------------------------------------- -#
# files_in =  c('E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/Med_SNDVI',
#               'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/Med_SDVI',
#               'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/Med_SRDVI',
#               'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/SNDVI',
#               'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/SDVI',
#               'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/SRDVI')

# files_in =  c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SNDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                #
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SRDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SRDVI_2000_2012_META_RData.RData', 
#                'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SRDVI/Stat_Analysis/Percentages/Percentages_Med_SRDVI_2000_2012_META_RData.RData'
# )
# 
# # files_in =  c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SNDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   #
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/25/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SNDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/100/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SRDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/150/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SRDVI_2000_2012_META_RData.RData', 
# #   'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/200/Med_SRDVI/Stat_Analysis/Percentages_subset/Percentages_Med_SRDVI_2000_2012_META_RData.RData'
# # )
# 
# kernels = c(10,25,50,100,150,200,10,25,50,100,150,200)
# 
# for (file in 1:length(files_in)) {
#   
# #   for (perc in c('5','10','15')) {
#     
# #     print(paste(file_in))
# #     
# #     file_out_perc = paste(file,'_',perc,'%_','Percentages.RData', sep = '')
# #     print(paste(file_out_perc, '  ', perc))
#     file_in = files_in[file]
#     load(file_in)
#     kernel = kernels[file]
#     
#     Index = Perc_Recovered$Index[1]
#     Min_Percentage = Perc_Recovered$Min_Percentage[1]
#     Comp_N = Perc_Recovered$Comp_N[1]
#     Perc_Recovered$Percentage = 100*Perc_Recovered$Percentage 
#     
#     Perc_Recovered$N_Years = ordered(Perc_Recovered$N_Years)
#     Perc_Recovered$Comp_N = ordered(as.numeric(as.character(Perc_Recovered$Comp_N)))
#     
#     plot_folder = file.path(dirname(file_in),'Plots')
#     dir.create(plot_folder)
#     pdf_out = file.path(plot_folder,paste('Result_Graphs_',Index,'_', Min_Percentage,'_',kernel,'.pdf',sep = ''))
#     
#     
#     
#     my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 13, vjust =2, hjust = 0.5),
#                                     axis.title.x = element_text( face = "bold", vjust = 0, size = 12, angle = 0 ),	axis.title.y = element_text( face = "bold", hjust = 0.5, vjust = 0.3,size = 12, angle = 90))
#     levels(Perc_Recovered$N_Years) = c('0', '1','2','3','4','5','6','7','8','9',' ','NR')
#     levels(Perc_Recovered$CLC_Class) = c('All','Broadleaved','Coniferous','Mixed','Other Natural','Scleropyllus','Transitional')
#     
#     ENV_ZONE_Sub = 'All'    # Take only data for "All" ENV_ZONES - maybe later analysis on variations with the selected Index and median widyth
#     
#     Perc_Rec_sub = droplevels(subset(Perc_Recovered, (ENV_ZONE == ENV_ZONE_Sub)))          
#     Perc_Rec_sub$N_Years = ordered(Perc_Rec_sub$N_Years)
#     Perc_Rec_sub$Comp_N = ordered(as.numeric(as.character(Perc_Rec_sub$Comp_N)))
#     levels(Perc_Recovered$N_Years) = c('0', '1','2','3','4','5','6','7','8','9',' ','NR')
#     
#     diff_func = function(Data) {
#       
#       Data_diffs_perc =  Data$Percentage - Data$Percentage[1]
#       Data_diffs_Number = Data$N_Rec_YY - Data$N_Rec_YY[1]
# #       browser()
#       Comp_N = seq(1,5,1)
#       out = data.frame(Comp_N = Comp_N, Data_diffs_perc = Data_diffs_perc,Data_diffs_Number=Data_diffs_Number)
#       
#     }
#     # Compute the differences in percentages of recovered fires and number of recovered fires when extending the width of the median
#     Perc_Rec_Diff = ddply(Perc_Rec_sub, .(CLC_Class,N_Years), function(df) diff_func (df),.progress = "text")
#     Perc_Rec_sub = join(Perc_Rec_sub,Perc_Rec_Diff)
#     
#     pdf(pdf_out,8.5,10.5, pointsize = 4)
#     
#     #- ----------------------------------------- - 
#     # Plot number of fires vs recovery time, variable with median width and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_bar(aes(y = N_Rec_YY),  stat=  'identity', position = position_dodge(width = 0.9)) 
#     p = p + facet_grid(CLC_Class~Comp_N, scales = "free_y") + theme(legend.position="none")
#     p = p + xlab('Recovery Time (Years)') + ylab ('Number of Fires')
#     p = p + ggtitle(paste(Index, ' - Number of Fires vs Recovery Time for different median Widths',sep = ''))
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     #- ----------------------------------------- - 
#     # Plot  %  of fires vs recovery time, variable with median width and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_bar(aes(y = Percentage),  stat=  'identity', position = position_dodge(width = 0.9)) 
#     p = p + facet_grid(CLC_Class~Comp_N, scales = "free_y") + theme(legend.position="none")
#     p = p + xlab('Recovery Time (Years)') + ylab ('% of cases')
#     p = p + ggtitle(paste(Index, 'Percentage of Cases vs Recovery Time for different median Widths',sep = ' '))
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     #- ----------------------------------------- - 
#     # Plot number of fires vs Median Width, variable with recovery time and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_line(aes(y = N_Rec_YY),  stat=  'identity',size = 0.8) 
#     p = p + facet_wrap(~N_Years, scales = "free_y") #+ theme(legend.position="none")
#     p = p + xlab('Median Width (Years)') + ylab ('Number of Fires')
#     p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
#     p = p + ggtitle(paste(Index, ' - Number of Fires vs Median Width for different Recovery Times',sep = ''))
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     #- ----------------------------------------- - 
#     # Plot % of fires vs Median Width, variable with recovery time and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_line(aes(y = Percentage),  stat=  'identity',size = 0.8) 
#     p = p + facet_wrap(~N_Years) #+ theme(legend.position="none")
#     p = p + xlab('Median Width (Years)') + ylab ('% of Cases')
#     p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
#     p = p + ggtitle(paste(Index, ' - % of Cases vs Median Width for different Recovery Times',sep = ''))
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     #- ----------------------------------------- - 
#     # Plot variation of number of fires vs Median Width, variable with recovery time and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_line(aes(y = Data_diffs_Number),  stat=  'identity',size = 0.8) 
#     p = p + facet_wrap(~N_Years, scales = 'free_y') #+ theme(legend.position="none")
#     p = p + xlab('Median Width (Years)') + ylab ('Variation of Number of Cases')
#     p = p + ggtitle(paste(Index, 'Variation of Number of cases vs Median Width, for different Recovery Times',sep = ' '))
#     p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+scale_colour_brewer(palette = "Set2")
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     #- ----------------------------------------- - 
#     # Plot variation of % of cases vs Median Width, variable with recovery time and CLC_Class
#     #- ----------------------------------------- - 
#     p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
#     p = p + geom_line(aes(y = Data_diffs_perc),  stat=  'identity',size = 0.8) 
#     p = p + facet_wrap(~N_Years)
#     p = p + xlab('Median Width (Years)') + ylab ('Variation of % of cases')
#     p = p + ggtitle(paste(Index, 'Variation of % of cases vs Median Width, for different Recovery Times',sep = ' '))
#     p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+scale_colour_brewer(palette = "Set2")
#     print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
#     
#     
#     dev.off()
#     
# #   }
# }

# # Plot % of fires recovered after YY years - use the percentages by year to compute mean and min-max bars
# 
# Perc_Rec_sub_YY = droplevels(subset(Perc_Recovered_YY,  ENV_ZONE == ENV_ZONE_Sub & Percentage != 0 ))
# Perc_Rec_sub_YY$N_Years = as.factor(Perc_Rec_sub_YY$N_Years)
# levels(Perc_Rec_sub_YY$N_Years) = c('0', '1','2','3','4','5','6','7','8','9',' ','NR')
# Perc_Rec_sub_YY$Comp_N = ordered(as.numeric(as.character(Perc_Rec_sub_YY$Comp_N)))
# 
# p = ggplot(data = Perc_Rec_sub_YY , aes(x = N_Years, color = CLC_Class))  + my_theme_bw 
# p = p + geom_boxplot(aes(y = Percentage), position = position_dodge(width = 0.9), outlier.colour='transparent') 
# # p = p + geom_jitter(aes(y = Percentage))
# p = p + facet_grid(CLC_Class~Comp_N, scales = "free_y") + theme(legend.position="none")
# p = p + xlab('Recovery Time (Years)') + ylab ('% of cases')
# p = p + ggtitle(paste(Index, 'Percentage of Fires vs Recovery Time - Variable Median Width',sep = ' '))
# p

# Plot cumulated % of fires recovered after YY years vs number fires recovered happened at least YY years before

# Cum_Recovered = ddply(Perc_Rec_sub, .(Comp_N,CLC_Class), function(df) cumsum (df$Percentage), .drop = F)
# names(Cum_Recovered)[3:14] = c('0', '1','2','3','4','5','6','7','8','9','10','NR')
# Cum_Recovered_mm = melt(Cum_Recovered)
# Cum_Recovered_mm = subset(Cum_Recovered_mm, variable != 'NR')
# Cum_Recovered_mm$Comp_N = ordered(as.numeric(as.character(Cum_Recovered_mm$Comp_N)))
# 
# 
# p = ggplot(data = Cum_Recovered_mm , aes(x = as.numeric(variable), y = as.numeric(value), color = CLC_Class))  + my_theme_bw 
# p = p + geom_line() +geom_point(aes(shape =CLC_Class )) + my_theme_bw 
# p = p + facet_grid(~Comp_N, scales = "free_y") 
# p = p + xlab('Recovery Time (Years)') + ylab ('% of cases')
# p = p + ggtitle(paste(Index, 'Cumulative Percentage of Fires vs Recovery Time - Variable Median Width',sep = ' '))
# p

#- --------------------------------------------------------------------------- -#
#- Plotting of recovery statistics by ENV_ZONE
#- --------------------------------------------------------------------------- -#

# Plot number of fires recovered after YY years + number of fires unrecovered
# NK = '100'
# Meths = c('Med_SDVI')
# ENV_ZONE = 'All'
# Data_Sub = droplevels(subset(Perc_Recovered, NKer == NK))
# Data_Sub = droplevels(subset(Perc_Recovered, NKer == NK & Method %in% Meths & ENV_ZONE != 'All' ))
# 
# Data_Sub$N_Years = as.factor(Data_Sub$N_Years)
# levels(Data_Sub$N_Years) = c('<1', '1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','10-11','11-12','NR')
# p = ggplot(data = Data_Sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
# p = p + geom_bar(aes(y = N_Rec_YY),  stat=  'identity', position = position_dodge(width = 0.9)) + theme_bw()
# p = p + facet_wrap(~ENV_ZONE+CLC_Class, ncol = 6) + theme(legend.position="none")
# p = p + xlab('Recovery Time (Years)') + ylab ('Number of Fires')
# p
# 
# # Plot % of fires recovered after YY years vs number fires recovered happened at least YY years before
# 
# NK = '100'
# Meths = c('Med_SNDVI')
# Data_Sub = droplevels(subset(Perc_Recovered, NKer == NK &  N_Years != 999))
# Data_Sub = droplevels(subset(Perc_Recovered, NKer == NK & Method %in% Meths &  N_Years != 999))
# Data_Sub$N_Years = as.factor(Data_Sub$N_Years)
# levels(Data_Sub$N_Years) = c('<1', '1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','10-11','11-12')
# 
# p = ggplot(data = Data_Sub , aes(x = as.factor(N_Years), color = CLC_Class))  + my_theme_bw 
# p = p + geom_bar(aes(y = Percentage),  stat=  'identity', position = position_dodge(width = 0.9)) + theme_bw()
# p = p + facet_wrap(~ENV_ZONE+CLC_Class, ncol = 6) + theme(legend.position="none")
# p = p + xlab('Recovery Time (Years)') + ylab ('% of cases')
# p

# Boxplots of  number of significant years in recovered fires. (Useless !!!! 
#
#Data_Sub = droplevels(subset(Data_Sum, NKer == NK &  recov == 'Yes'))
#Data_Sub = droplevels(subset(Perc_Recovered, NKer == NK & Method %in% Meths &  N_Years != 999))
#p = ggplot(data = Data_Sub , aes(x = CLC_Class))    #+ my_theme_bw()
#p = p + geom_boxplot(aes(y =as.numeric(N_Signif )))+
#		geom_jitter(aes(y = as.numeric(N_Signif)), position = position_jitter(height=0.2), alpha = 0.3) + theme_bw() 
#p = p + facet_wrap(~Method+NKer, ncol = 2) + theme(legend.position="none")









  
  
