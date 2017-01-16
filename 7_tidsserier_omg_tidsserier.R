# colnames(mat.e) <- paste(as.character(discodata$`2: Segment`),as.character(discodata$disco),sep=":")


# roed - seriøse problemer med dataen over tid. Den kan man ikke stole på... smoothi måske?


roede.tid.seg.df <-  roede.tid.df %>% group_by(membership) %>% summarise_each(funs(mean), `1996`=`1996`, `1997`=`1997`, `1998`=`1998`, `1999`=`1999`, `2000`=`2000`, `2001`=`2001`, `2002`=`2002`, `2003`=`2003`, `2004`=`2004`, `2005`=`2005`, `2006`=`2006`, `2007`=`2007`, `2008`=`2008`, `2009`=`2009`)  %>%    filter(!grepl("^1.*", membership))

plot.df  <-  filter(roede.tid.seg.df,grepl("^4.*", membership)) %>% gather(key, value, -membership) %>%   rename(segmenter=membership)
plot.df  <-  filter(roede.tid.df,membership=="3.34") %>% gather(key, value, -disco, -membership) %>%  select(-membership)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) +
                      geom_line(size=1.3) +
                      # geom_point(size=2) + 
                     xlab("") + ylab("") +
                     theme_tufte() +
                     theme(panel.grid.major= element_line(colour="black", size=0.05)) +
                      scale_colour_manual(values=colorRampPalette(xmen)(
                      length(unique(as.matrix(plot.df[,1]))))) +  
                      geom_rangeframe(color="black",size=0.80) +
                      scale_y_continuous(labels=percent, breaks = extended_range_breaks()(plot.df$value))



p + geom_rangeframe() +
  theme_tufte() + 
  scale_x_continuous(breaks = extended_range_breaks()(mtcars$wt)) +
  scale_y_continuous(breaks = extended_range_breaks()(mtcars$mpg))








