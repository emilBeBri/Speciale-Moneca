############# boxplot roede whatever##########



# alle med flere end 4 noder 
plot.df <- filter(discodata, !grepl("^1.*", membership), Nodes>=4)
nrow(plot.df)

# dem under 2 % roede 
plot.df <- filter(discodata, roede.mean.gns.beregn<.0195) %>% filter(!grepl("^1.*", membership))

is_outlier

plot.df <- plot.df %>%  group_by(membership) %>%  mutate(is.outlier = is_outlier(roede.mean.gns,0.1,4))
plot.df <-  plot.df %>%   mutate(outlier = ifelse(is.outlier==TRUE, as.character(disco),NA)) 
getPalette= colorRampPalette(xmen)
p <- ggplot(plot.df, aes(x=fct_reorder(membership,roede.mean.gns), y=roede.mean.gns,fill=membership,label=plot.df$disco))
p1 <-  p + scale_fill_manual(values=getPalette(length(unique(plot.df$membership))))
p2 <-  p1 +  stat_summary(fun.data=bp.vals, geom="boxplot",alpha=0.7,color="black") + geom_point(position= position_jitter(width=0.2),aes(size=beskaeft.andel.gns),alpha=.75)
p3 <-  p2 + stat_summary(fun.y=whisk.emil, geom="point",shape=95,size=8,color="black") + theme_bw() + theme(legend.position="none") + scale_y_continuous(labels=percent)

p3 +  geom_label_repel(aes(fill=membership,label=outlier),    box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"),size=3.5) # kan evt manuelt ændre de segmenter hvor der ikke er noget label, bare så man kan se hvad det indeholder. Nok meget god ide. 


p_out <-  p3 + geom_text(aes(label = outlier), na.rm = TRUE, vjust = -0.6,size=2.75) 

p_out





p2 + geom_label(hjust=-0.1,aes(size=beskaeft.andel.gns)) + scale_size_continuous(range=c(1,3))
p2 + geom_text_repel(size=2,force=0.3,nudge_x=1)
p2 + geom_text(nudge_x=1,hjust=-0.05,aes(size=beskaeft.andel.gns),check_overlap=TRUE) + scale_size_continuous(range=c(1,3))


 geom_label_repel(
    aes(wt, mpg, fill = factor(cyl), label = rownames(mtcars)),
    fontface = 'bold', color = 'white',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50'
  ) +


cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/boxplots/roede_seg3-5.pdf", onefile = TRUE, height = 20, width = 20)
p_out 
dev.off()




