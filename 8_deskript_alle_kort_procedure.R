##


# discodatasæt
view(discodata)


#udvikling i segmenter
seg.qual.df        <- segment.quality(seg)
seg.qual.df$raekkefoelge <- seq(1:273)
seg.qual.df <- tbl_df(seg.qual.df)
seg.qual.df <-   arrange(seg.qual.df, `1: Segment`)
seg.qual.df$disco <- label[-l]
seg.qual.df <-   arrange(seg.qual.df, raekkefoelge)
seg.qual.df <-   select(seg.qual.df,disco, everything())
view(seg.qual.df)


#membership df 
### deskriptivt om segmenter i socio/socstil moneca
seg.df <- seg.qual.final
seg.df <-  seg.df %>%
  select(membership, everything())
# gns og den slags - husk at det afhænger af om de skal summeres eller ej!!
# til.df.seg <- ddply(discodata,~membership,summarise,mean=mean(alder.gns),sd=sd(alder.gns))
til.seg.df <- ddply(discodata,~membership,summarise,ledbeskaeft.gns=sum(ledbeskaeft.gns))
seg.df <- left_join(seg.df, til.seg.df)
til.seg.df.2 <-  discodata %>% select(intern.mobilitet.seg,membership)
til.seg.df.2 <-  unique(til.seg.df.2)
seg.df <- left_join(seg.df, til.seg.df.2)
view(seg.df)
