# df med de nyttige variable 

# df  <- discodata %>% 	select(-contains("200"),-contains("199"),-timelon.helepop.gns.inf.dst) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()


discodata$membership <- as.factor(discodata$membership)



df  <- discodata %>% 	select(-contains("200"),-contains("199")) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()


df <- tbl_df(df)
# view(df)




# # tmp <-  select(df,disco_s,membership)
# # tmp$label <- as.numeric(factor(tmp$membership))
# # tmp$label2 <- as.numeric(as.character(tmp$membership))*1000




# mean(df$alder.sd.gns)
# sd(df$alder.sd.gns)


#### segmentbeskrivelser - kunne godt laves til funktion #todoinr

seg.qual <- segment.quality(seg)
seg.qual <-  tbl_df(seg.qual)
view(seg.qual)
view(seg.qual.final)


#1. level 
level1 <-  list()
level.names <- c("antal noder","gennemsnitlig intern mobilitet") 
level1[level.names] <- list(NULL)
level1[1] <-  nrow(seg.qual)
level1[2] <-  mean(seg.qual$`1: within.mobility`)

#2. level 
au2.1 <- seg.qual %>% 	filter(is.na(`2: Segment`)) 
level2.1 <-  mean(au2.1$`1: within.mobility`)

au2.2 <- seg.qual %>% 	filter(!is.na(`2: Segment`)) 
au2.2 <-  au2.2 %>% distinct(`2: Segment`,.keep_all=TRUE) 
level2.2 <-  sum(au2.2$`2: within.mobility`) /nrow(au2.2)

level2 <-  list()
level2[level.names] <- list(NULL)
level2[1] <-  nrow(au2.1) + nrow(au2.2)
level2[2] <-  (level2.1 + level2.2) / 2

#3. level 
au3.1 <- seg.qual %>% 	filter(is.na(`3: Segment`) & is.na(`2: Segment`)) # & is.na(`4: Segment`) & is.na(`5: Segment`))  
level3.1 <-  mean(au3.1$`1: within.mobility`)

au3.2 <- seg.qual %>% 	filter(is.na(`3: Segment`) & !is.na(`2: Segment`))
au3.2 <-  au3.2 %>% distinct(`2: Segment`,.keep_all=TRUE) 
level3.2 <-  sum(au3.2$`2: within.mobility`) /nrow(au3.2)

au3.3 <- seg.qual %>% 	filter(!is.na(`3: Segment`)) 
au3.3 <-  au3.3 %>% distinct(`3: Segment`,.keep_all=TRUE) 
level3.3 <-  sum(au3.3$`3: within.mobility`) /nrow(au3.3)


level3 <-  list()
level3[level.names] <- list(NULL)
level3[1] <- nrow(au3.1) + nrow(au3.2) + nrow(au3.3)
level3[2] <- (level3.1 + level3.2 +level3.3) / 3

#4. level 
au4.1 <- seg.qual %>% 	filter(is.na(`3: Segment`) & is.na(`2: Segment`)  & is.na(`4: Segment`)) 
level4.1 <-  mean(au4.1$`1: within.mobility`)

au4.2 <- seg.qual %>% 	filter(is.na(`4: Segment`) & is.na(`3: Segment`) & !is.na(`2: Segment`))
au4.2 <-  au4.2 %>% distinct(`2: Segment`,.keep_all=TRUE) 
level4.2 <-  sum(au4.2$`2: within.mobility`) /nrow(au4.2)

au4.3 <- seg.qual %>% 	filter(!is.na(`3: Segment`) & is.na(`4: Segment`)) 
au4.3 <-  au4.3 %>% distinct(`3: Segment`,.keep_all=TRUE) 
level4.3 <-  sum(au4.3$`3: within.mobility`) /nrow(au4.3)


au4.4 <- seg.qual %>% 	filter(!is.na(`4: Segment`)) 
au4.4 <-  au4.4 %>% distinct(`4: Segment`,.keep_all=TRUE) 
level4.4 <-  sum(au4.4$`4: within.mobility`) /nrow(au4.4)


level4 <-  list()
level4[level.names] <- list(NULL)
level4[1] <- nrow(au4.1) + nrow(au4.2) + nrow(au4.3) + nrow(au4.4)
level4[2] <- (level4.1 + level4.2 +level4.3 + level4.4) / 4

#5. level 
au5.1 <- seg.qual %>% 	filter(is.na(`3: Segment`) & is.na(`2: Segment`)  & is.na(`4: Segment`)  & is.na(`5: Segment`))  
level5.1 <-  mean(au5.1$`1: within.mobility`)

au5.2 <- seg.qual %>% 	filter(is.na(`5: Segment`) & is.na(`4: Segment`) & is.na(`3: Segment`) & !is.na(`2: Segment`))

au5.2 <-  au5.2 %>% distinct(`2: Segment`,.keep_all=TRUE) 
level5.2 <-  sum(au5.2$`2: within.mobility`) /nrow(au5.2)

au5.3 <- seg.qual %>% 	filter(!is.na(`3: Segment`) & is.na(`4: Segment`) & is.na(`5: Segment`)) 
au5.3 <-  au5.3 %>% distinct(`3: Segment`,.keep_all=TRUE) 
level5.3 <-  sum(au5.3$`3: within.mobility`) /nrow(au5.3)

au5.4 <- seg.qual %>% 	filter(!is.na(`4: Segment`) & is.na(`5: Segment`)) 
au5.4 <-  au5.4 %>% distinct(`4: Segment`,.keep_all=TRUE) 
level5.4 <-  sum(au5.4$`4: within.mobility`) /nrow(au5.4)

au5.5 <- seg.qual %>% 	filter(!is.na(`5: Segment`)) 
au5.5 <-  au5.5 %>% distinct(`5: Segment`,.keep_all=TRUE) 
level5.5 <-  sum(au5.4$`2: within.mobility`) /nrow(au5.4)

level5 <-  list()
level5[level.names] <- list(NULL)
level5[1] <- nrow(au5.1) + nrow(au5.2) + nrow(au5.3) + nrow(au5.4) + nrow(au5.5)
level5[2] <- (level5.1 + level5.2 +level5.3 + level5.4 + level5.5) / 5
# (level5.2 +level5.3 + level5.4 + level5.5) / 4 den blir faktisk lidt højere af at at fjerne niveau 1-segmenterne. 



## bringin' it all back home 

seg.opsummering <- list(level1,level2,level3,level4,level5)
# kan eventuelt laves til funktion. Bør også inkludere tre elementer til per niveau: forøgelse i intern mobilitet fra det foregående niveau, samt faldet i antallet af noder i procent. samt ændring i procentpoint. Dette kan naturligvis kun være for niveau 2+.

# forøgelse i intern mobilitet i %
(seg.opsummering[[c(2,2)]] - seg.opsummering[[c(1,2)]])*100
(seg.opsummering[[c(3,2)]] - seg.opsummering[[c(2,2)]])*100
(seg.opsummering[[c(4,2)]] - seg.opsummering[[c(3,2)]])*100
(seg.opsummering[[c(5,2)]] - seg.opsummering[[c(4,2)]])*100


############### relativ risiko vektor 


cut.off.default <-  1 #skal måske ikke være 1 her jo
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE)
# wm1[is.na(wm1)] <- 0
relativrisiko.vector  <-  as.vector(t(wm1))












