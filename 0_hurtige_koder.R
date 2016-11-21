
#############3

view(df)
view(seg.df)

tmp <- df %>% 	select(disco_s,membership)
tmp$membership2 <-   as.numeric(tmp$membership)

view(tmp)






