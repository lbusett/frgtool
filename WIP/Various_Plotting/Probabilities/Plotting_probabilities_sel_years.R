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


files_SNDVI = c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData')
in_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
 In_File_SNDVI = In_Files_SNDVI[cy]   ; In_File_SRDVI = In_Files_SRDVI[cy]   ; out_plot_folder = out_plot_folders[cy]
  type_code = type_codes[cy]  ; out_pdf = out_pdf_names[cy]; str_opt = str_options[cy]

 load(In_File_SNDVI)             # Load SNDVI file

recov_stat$numFireYear = as.numeric(as.character(recov_stat$FireYear))
sel_fire = droplevels(subset(recov_stat, numFireYear<2008 & Comp_N ==3 ))                             # Remove fires after 2007
sel_fire$N_Signif_Rec[which(sel_fire$N_Signif_Rec %in% c("6","7",'8','9','NR'))] = 'NR'     # Assign all recovery times above 5 to "NR"
sel_fire = droplevels(sel_fire)                                                         # drop unused fires
levels(sel_fire$N_Signif_Rec)[length(levels(sel_fire$N_Signif_Rec))] = "6"
sel_fire$Recov[which(sel_fire$N_Signif_Rec== 6)] = 'UnRecovered'

levels(sel_fire$CLC_Class)=c('All Classes','Broadleaved For.','Coniferous For.','Mixed For.','Schleropyllus','Transitional W/S')
max_rec_yy = 5
percs = ddply(sel_fire)
Newprobs = ddply(sel_fire, .(CLC_Class) , function(df) Compute_Perc_Recov(df,max_rec_yy),.progress = "text" )
Newprobs$N_Years = as.factor(Newprobs$N_Years)
levels(Newprobs$N_Years)[7] = '>5'

p = ggplot(Newprobs, aes(x = (N_Years), y = 100*Percentage,  shape = CLC_Class, group = CLC_Class))
p = p +geom_bar(stat= 'identity', fill = 'grey75', colour = 'black')+my_theme_bw
p = p + facet_wrap(~CLC_Class)+ylab('%')+xlab('Recovery Time [Years]')
p


sel_fire2= droplevels(subset(sel_fire, CLC_Class!= 'All Classes'))
Newprobs2 =Compute_Perc_Recov(sel_fire2,max_rec_yy)
Newprobs2$N_Years = as.factor(Newprobs2$N_Years)
levels(Newprobs2$N_Years)[7] = '>5'

p = ggplot(Newprobs2, aes(x = (N_Years), y = 100*Percentage))
p = p +geom_bar(stat= 'identity', fill = 'grey75', colour = 'black')+my_theme_bw
p = p + facet_wrap(~CLC_Class)+ylab('%')+xlab('Recovery Time [Years]')
p


cumsum = ddply(pro_sub_all, .(CLC_Class), summarise, cumsum = cumsum(Percentage), N_Years = levels(N_Years))
p = ggplot(cumsum, aes(x = (N_Years), y = cumsum, color = CLC_Class, shape = CLC_Class, group = CLC_Class))
p = p +geom_point()+geom_line()+my_theme_bw
p = p + facet_wrap(~CLC_Class, ncol = 2)
p


Compute_Perc_Recov <- function(Data, max_Rec_yy) {
  N_Tot = length(Data$Recov)       							# Total number of fires in category
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