# df med de nyttige variable 

df  <- discodata %>% 	select(-contains("200"),-contains("199"),-timelon.helepop.gns.inf.dst) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()

df <- tbl_df(df)
tmp <-  select(df,disco_s,membership)
tmp$label <- as.numeric(factor(tmp$membership))
tmp$label2 <- as.numeric(as.character(tmp$membership))*1000

