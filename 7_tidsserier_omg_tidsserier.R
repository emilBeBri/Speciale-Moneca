# colnames(mat.e) <- paste(as.character(discodata$`2: Segment`),as.character(discodata$disco),sep=":")


# roed - seriøse problemer med dataen over tid. Den kan man ikke stole på... smoothi måske?

library(ggthemes)




# antal beskæftigede 

#enkelte segment
plot.df  <-  filter(beskaeft.tid.df,membership=="3.34") %>% gather(key, value, -disco, -membership) %>%  select(-membership)

#flere segmenter 
beskaeft.tid.seg.df <-  beskaeft.andel.tid.df %>% group_by(membership) %>% summarise_each(funs(sum), `1996`=`1996`, `1997`=`1997`, `1998`=`1998`, `1999`=`1999`, `2000`=`2000`, `2001`=`2001`, `2002`=`2002`, `2003`=`2003`, `2004`=`2004`, `2005`=`2005`, `2006`=`2006`, `2007`=`2007`, `2008`=`2008`, `2009`=`2009`)  %>%    filter(!grepl("^1.*", membership))

#udvælger frekvens ud fra relativ frekvens
# plot.df <-  left_join(beskaeft.tid.seg.df,select(seg.df,membership,beskaeft.andel.gns.beregn)) %>%  filter(beskaeft.andel.gns.beregn>=0.05) %>%   select(-beskaeft.andel.gns.beregn) %>%   gather(key, value, -membership) %>%   rename(segmenter=membership)     

# plot.df <-  beskaeft.tid.seg.df %>%     filter(membership=="4.8"|membership=="5.1" | membership=="5.2") %>% gather(key, value, -membership) %>%   rename(segmenter=membership)

plot.df <- beskaeft.tid.seg.df %>%     filter(grepl("^4.*", membership) | grepl("^5.*", membership)) 
%>%  filter(`2009`>=10000)     
%>%   gather(key, value, -membership) %>%   rename(segmenter=membership)   

ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) + geom_line(size=1.3) + xlab("") + ylab("") + theme_tufte() + theme(panel.grid.major= element_line(colour="black", size=0.05)) + scale_colour_manual(values=colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))) + geom_rangeframe(color="black") + scale_y_continuous(breaks = extended_range_breaks()(plot.df$value))




#klasserne 

beskaeft.tid.klasse.df <-  beskaeft.andel.tid.df %>% group_by(klasse_oesch16) %>% summarise_each(funs(sum), `1996`=`1996`, `1997`=`1997`, `1998`=`1998`, `1999`=`1999`, `2000`=`2000`, `2001`=`2001`, `2002`=`2002`, `2003`=`2003`, `2004`=`2004`, `2005`=`2005`, `2006`=`2006`, `2007`=`2007`, `2008`=`2008`, `2009`=`2009`)  

#laver et level til dem der ikke har nogen klasse fremfor bare at være "forsvundet"
# levels(beskaeft.tid.klasse.df$klasse_oesch8) <- c(levels(beskaeft.tid.klasse.df$klasse_oesch8),"Uklassicificeret")
# beskaeft.tid.klasse.df$klasse_oesch8[is.na(beskaeft.tid.klasse.df$klasse_oesch8)] <- "Uklassicificeret"

# forsøg på afrunding, virker ikke. #todoiR
# beskaeft.tid.klasse.df <- append(beskaeft.tid.klasse.df[,1], round_any(as.matrix(beskaeft.tid.klasse.df[,-1]), 0.01, ceiling))

plot.df <- beskaeft.tid.klasse.df %>%   gather(key, value, -klasse_oesch16) %>%   rename(segmenter=klasse_oesch16)   
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/tidsserier/tid_Oesch8.pdf", onefile = TRUE, height = 15, width = 20)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) + geom_line(size=1.3) + xlab("") + ylab("") + theme_tufte() + theme(panel.grid.major= element_line(colour="black", size=0.05),text=element_text(size=20)) + scale_colour_manual(values=skala_oesch8) + geom_rangeframe(color="black") + scale_y_continuous(label=percent,breaks = extended_range_breaks()(plot.df$value))
dev.off()


# segmenterne 
library(ggthemes)


tmp.df <-  discodata %>%    filter(discodata$klasse_oesch8=="4 Manuelle arbejdere" & beskaeft.andel.gns.beregn>=0.015)  %>%   select(membership,disco)
manuel.liste <-  levels(factor(tmp.df$membership))
plot.df <-   filter(beskaeft.tid.seg.df,beskaeft.tid.seg.df$membership %in% manuel.liste) %>% gather(key, value, -membership) %>%   rename(segmenter=membership) 



cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/tidsserier/tid_seg_manuelt.pdf", onefile = TRUE, height = 15, width = 20)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) + geom_line(size=1.3) + xlab("") + ylab("") + theme_tufte() + theme(panel.grid.major= element_line(colour="grey", size=0.05)) + scale_colour_manual(name="",values=colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))) + geom_rangeframe(color="black") + scale_y_continuous(label=percent, breaks = extended_range_breaks()(plot.df$value))
dev.off()
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(stringr)
# library(scales)
# library(gridExtra)
# library(grid)
# gg <- ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) + geom_line(size=1.3) + xlab("") + ylab("") + theme_tufte() + theme(panel.grid.major= element_line(colour="black", size=0.05)) + scale_colour_manual(name="",values=colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))) + geom_rangeframe(color="black") + scale_y_continuous(label=percent, breaks = extended_range_breaks()(plot.df$value))
# last_vals <- plot.df$value[(length(plot.df$value)- nlevels(factor(plot.df$segmenter))+1):length(plot.df$value)] 
# last_names <- as.character(plot.df$segmenter[(length(plot.df$segmenter)- nlevels(factor(plot.df$segmenter))+1):length(plot.df$segmenter)] )
# tmp_col <-  colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))
# for (i in 1:length(last_vals)) {
#   gg <- gg + annotation_custom(grob=textGrob(last_names[i], hjust=0,
#                                              gp=gpar(fontsize=8, 
#                                                      col=tmp_col,
#                                xmin=2009, xmax=2009,
#                                ymin=last_vals[i], ymax=last_vals[i])))
# }
# gb <- ggplot_build(gg)
# gt <- ggplot_gtable(gb)
# gt$layout$clip[gt$layout$name=="panel"] <- "off"
# grid.draw(gt)
#  gg = NULL
# gt = NULL 
# gb = NULL 
# gg = NULL 
# gg <- gg + annotation_custom(grob=textGrob(last_names[1], hjust=0,
#                                              gp=gpar(fontsize=8, 
#                                                      col=tmp_col,
#                                xmin=2010, xmax=2010,
#                                ymin=last_vals[1], ymax=last_vals[1])
# ))

# røde fagforeninger
roede.tid.seg.df <-  roede.tid.df %>% group_by(membership) %>% summarise_each(funs(mean), `1996`=`1996`, `1997`=`1997`, `1998`=`1998`, `1999`=`1999`, `2000`=`2000`, `2001`=`2001`, `2002`=`2002`, `2003`=`2003`, `2004`=`2004`, `2005`=`2005`, `2006`=`2006`, `2007`=`2007`, `2008`=`2008`, `2009`=`2009`)  %>%    filter(!grepl("^1.*", membership))
view(roede.tid.seg.df)


#flere segmenter *DUER IKKE 
plot.df  <-  filter(roede.tid.seg.df,grepl("^4.*", membership)) %>% gather(key, value, -membership) %>%   rename(segmenter=membership)

view(seg.df)
view(plot.df)
#enkelte segment
plot.df  <-  filter(roede.tid.df,membership=="3.34") %>% gather(key, value, -disco, -membership) %>%  select(-membership)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) +
                      geom_line(size=1.3) +
                      # geom_point(size=2) + 
                     xlab("") + ylab("") +
                     theme_tufte() +
                     theme(panel.grid.major= element_line(colour="black", size=0.05)) +
                      scale_colour_manual(values=colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))) +
                      geom_rangeframe(color="black",size=0.80) +
                      scale_y_continuous(labels=percent, breaks = extended_range_breaks()(plot.df$value))













############# metode disco (IKKE FÆRDIGT, virker ikke ordentligt) ############



plot.df  <- df %>%   select(beskaeft.andel.gns) %>% gather(key, value)

DescTools::Desc(df$beskaeft.andel.gns*100)


ggplot(data=mtcars, aes(x=hp)) + geom_line(aes(y=..count..), stat="bin", binwidth=10)
ggplot(data=mtcars, aes(x=hp)) + geom_line(aes(y=..count..), stat="bin", binwidth=10)

s

ggplot(plot.df, aes(x=value,colour="indianred4")) +
                      geom_line(size=1.3,aes(y=..count..),stat="bin") +
                     xlab("") + ylab("") +
                     theme_tufte() +
                     theme(panel.grid.major= element_line(colour="black", size=0.05),text = element_text(size=25)) +
                      scale_x_continuous(labels=percent, breaks = extended_range_breaks()(c(0,0.01,0.02,0.03,0.04,0.05))) +
                      scale_y_continuous(breaks = extended_range_breaks()(c(0,20,40,60,80,94))) 
  
)






