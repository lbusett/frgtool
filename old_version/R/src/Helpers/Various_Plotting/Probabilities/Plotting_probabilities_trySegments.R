

In_f = "E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/src/MarkDown/Regression_Analysis_rms_only.Rmd"
Out_f_md = "E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/src/MarkDown/Regression_Analysis_rms_only.md"
knit(In_f,Out_f_md,encoding = "utf-8")

library(xtable)


p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index, fill = Index, shape = Index))  + my_theme_bw
p1 = p1+geom_line()
p1 = p1 + geom_segment(data = NR_Data, aes(x = N_Years,xend = N_Years, y = 0 ,  yend = 100*Percentage), color = 'white')
p1 = p1+geom_point(stat = 'identity',size = 1.5, col = 'black',lwd=0.1)
p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="bottom")
p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
p1 = p1+ scale_fill_manual(name = '',values = c('black','grey75'), labels =c(expression(NDVI^R),expression(RDVI^R)))
p1 = p1+ scale_color_manual(name = '',values = c('black','grey75'), labels =c(expression(NDVI^R),expression(RDVI^R)))
p1 = p1+ scale_shape_manual(name = '',values = c(1,2), labels =c(expression(NDVI^R),expression(RDVI^R)))
#   p1 = p1+ scale_color_manual(name = '',values = c('grey75','black'), labels =c(expression(NDVI^R),expression(RDVI^R)))
p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
panel.border = element_rect(colour = "black"))
p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
p1 = p1 + coord_cartesian(ylim =c(0,110))
out_file = file.path(out_plot_folder,paste('Pointplot_Perc_CLC_Class_YY_bothIndexes',type_code,'.tif', sep = ''))


 df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & as.numeric(as.character(FireYear)) <2009 & N_Tot_YY != 0 & Area_Cat =='All' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage, group = CLC_Class ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2, color = 'black', fill = 'grey50', width = 0.80)
    p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 7, angle = 90))
    p1 = p1 + coord_cartesian(ylim =c(0,55))
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
    p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
    out_file = file.path(out_plot_folder,paste('Barplot_Perc_CLC_Class_YY_',Ind,'_', type_code,'.tif', sep = ''))
    tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
    dev.off()
