
#############3

view(df)
view(seg.df)

view(discodata)



tmp <- df %>% 	select(disco_s,membership)
tmp$membership2 <-   as.numeric(tmp$membership)


view(tmp)




quants <- seq(0,1,0.05)

round(quantile(discodata$fagf.seg.gns.ja.andel,quants),2)



