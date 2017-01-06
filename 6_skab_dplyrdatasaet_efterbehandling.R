# df med de nyttige variable 

# df  <- discodata %>% 	select(-contains("200"),-contains("199"),-timelon.helepop.gns.inf.dst) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()


discodata$membership <- as.factor(discodata$membership)


til.df.1 <-  discodata %>% group_by(membership) %>% summarise_each(funs(sd), timelon.sd.gns.beregn=timelon.mean.gns, koen.gns.kvinder.sd.beregn=koen.gns.kvinder.andel,ledighed.sd.gns.beregn=ledighed.mean.gns)
til.df.2 <-  discodata %>% group_by(membership) %>% summarise_each(funs(mean), timelon.mean.gns.beregn=timelon.mean.gns, koen.gns.kvinder.mean.beregn=koen.gns.kvinder.andel,ledighed.mean.gns.beregn=ledighed.mean.gns)


seg.df <-  left_join(seg.df,til.df.1)
seg.df <-  left_join(seg.df,til.df.2)
discodata <-  inner_join(discodata,til.df.1)
discodata <-  inner_join(discodata,til.df.2)



seg.df$membership <- as.factor(seg.df$membership)
discodata$membership <- as.factor(discodata$membership)


df  <- discodata %>% 	select(-contains("200"),-contains("199")) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()


df <- tbl_df(df)
# view(df)

# # tmp <-  select(df,disco_s,membership)
# # tmp$label <- as.numeric(factor(tmp$membership))
# # tmp$label2 <- as.numeric(as.character(tmp$membership))*1000




# mean(df$alder.sd.gns)
# sd(df$alder.sd.gns)

## forsøg med "naturlige breaks"

discodata <- mutate(discodata,ledighed.mean.gns.cutoff=replace(ledighed.mean.gns, ledighed.mean.gns>=0.0750001, 0.075))


natur.interval.ledighed.jenks = classInt::classIntervals(discodata$ledighed.mean.gns.cutoff, n = 8, style = 'jenks')$brks
natur.interval.ledighed.fisher = classInt::classIntervals(discodata$ledighed.mean.gns.cutoff, n = 8, style = 'fisher')$brks
natur.interval.ledighed.quantile.cut = classInt::classIntervals(discodata$ledighed.mean.gns.cutoff, n = 8, style = 'quantile')$brks



#Hmisc::describe(discodata$ledighed.mean.gns.cutoff)
natur.interval.ledighed.kmeans = classInt::classIntervals(discodata$ledighed.mean.gns.cutoff, n = 8, style = 'kmeans')$brks





#### segmentbeskrivelser - kunne godt laves til funktion #todoinr

seg.qual <- segment.quality(seg)
seg.qual <-  tbl_df(seg.qual)
# view(seg.qual)
# view(seg.qual.final)


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





########## DST fagbetegnelser register ###########

DST_fagbet   <- read.csv2("./statistik/R/moneca/vores/voresdata/DST_fagbetegnelser_DISCO88.csv", sep = ";")
discogmem  <- select(discodata,disco,disco_4cifret, membership,skillvl) 
discogmem$disco_4cifret <-  as.numeric(as.character(discogmem$disco_4cifret))
DST_fagbet$disco_4cifret <-  as.numeric(DST_fagbet$disco_4cifret)
DST_fagbet  <-  	full_join(DST_fagbet, discogmem )


#
# esm_omkodningsliste   <- read.csv2("./statistik/R/moneca/vores/voresdata/Disco_esm_omkodningsliste.csv", sep = ";")

# test  <-  	anti_join(DST_fagbet, esm_omkodningsliste)




### delanalyse 2 om within.mob 

discodata <- discodata %>%  mutate(within.mob.over.median = within.mob > median(discodata$within.mob))
discodata <- discodata %>%  mutate(within.mob.under.median = within.mob < median(discodata$within.mob))
#find de grupper der har mix af over/under median i within.mob 
seg.df.mix.within.mob <-  discodata %>% group_by(membership) %>% summarise(seg.antal.hoj.within.mob=sum(within.mob.over.median)) %>%   filter(!grepl("^1.*", membership))
tmp1 <- seg.df %>%   select(Nodes,membership) %>%   filter(!grepl("^1.*", membership))
seg.df.mix.within.mob <- left_join(seg.df.mix.within.mob,tmp1)
seg.df.mix.within.mob <- mutate(seg.df.mix.within.mob, seg.within.mob.fordeling = mosaic::derivedFactor("Kun hoej within.mob" = (Nodes == seg.antal.hoj.within.mob), "Kun lav within.mob" = (seg.antal.hoj.within.mob == 0), .method = "first", .default = "Mikset"))
seg.df <- left_join(seg.df,seg.df.mix.within.mob)
discodata <- left_join(discodata,seg.df.mix.within.mob)
#view(discodata)
# view(seg.df.mix.within.mob)
# view(seg.df)

### delanalyse 2 om alder 

discodata <- discodata %>%  mutate(alder.over.median = alder.mean.gns > median(discodata$alder.mean.gns))
discodata <- discodata %>%  mutate(alder.under.median = alder.mean.gns < median(discodata$alder.mean.gns))
#find de grupper der har mix af over/under median i alder 
seg.df.mix.alder <-  discodata %>% group_by(membership) %>% summarise(seg.antal.hoj.alder=sum(alder.over.median)) %>%   filter(!grepl("^1.*", membership))
tmp1 <- seg.df %>%   select(Nodes,membership) %>%   filter(!grepl("^1.*", membership))
seg.df.mix.alder <- left_join(seg.df.mix.alder,tmp1)
seg.df.mix.alder <- mutate(seg.df.mix.alder, seg.alder.fordeling = mosaic::derivedFactor("Kun hoej alder" = (Nodes == seg.antal.hoj.alder), "Kun lav alder" = (seg.antal.hoj.alder == 0), .method = "first", .default = "Mikset"))
seg.df <- left_join(seg.df,seg.df.mix.alder)
discodata <- left_join(discodata,seg.df.mix.alder)
#view(discodata)
# view(seg.df.mix.alder)
# view(seg.df)



### delanalyse 2 om timelon 

discodata <- discodata %>%  mutate(timelon.over.median = timelon.mean.gns > median(discodata$timelon.mean.gns))
discodata <- discodata %>%  mutate(timelon.under.median = timelon.mean.gns < median(discodata$timelon.mean.gns))
#find de grupper der har mix af over/under median i timelon 
seg.df.mix.timelon <-  discodata %>% group_by(membership) %>% summarise(seg.antal.hoj.timelon=sum(timelon.over.median)) %>%   filter(!grepl("^1.*", membership))
tmp1 <- seg.df %>%   select(Nodes,membership) %>%   filter(!grepl("^1.*", membership))
seg.df.mix.timelon <- left_join(seg.df.mix.timelon,tmp1)
seg.df.mix.timelon <- mutate(seg.df.mix.timelon, seg.timelon.fordeling = mosaic::derivedFactor("Kun hoej timelon" = (Nodes == seg.antal.hoj.timelon), "Kun lav timelon" = (seg.antal.hoj.timelon == 0), .method = "first", .default = "Mikset"))
seg.df <- left_join(seg.df,seg.df.mix.timelon)
discodata <- left_join(discodata,seg.df.mix.timelon)
#view(discodata)
# view(seg.df.mix.timelon)
# view(seg.df)


### delanalyse 2 om koen 

#find de grupper der har mix af de to koen 
discodata <- discodata %>%  mutate(seg.flest.kvinder = koen.gns.kvinder.andel > .5)
discodata <- discodata %>%  mutate(seg.flest.maend = koen.gns.kvinder.andel < .5)


seg.df.mix.koen <-  discodata %>% group_by(membership) %>% summarise(seg.antal.kvindgrp=sum(seg.flest.kvinder)) %>%   filter(!grepl("^1.*", membership))
tmp1 <- seg.df %>%   select(Nodes,membership) %>%   filter(!grepl("^1.*", membership))
seg.df.mix.koen <- left_join(seg.df.mix.koen,tmp1)
seg.df.mix.koen <- mutate(seg.df.mix.koen, seg.koen.fordeling = mosaic::derivedFactor("Kun kvinder" = (Nodes == seg.antal.kvindgrp), "Kun Maend" = (seg.antal.kvindgrp == 0), .method = "first", .default = "Mikset"))
seg.df <- left_join(seg.df,seg.df.mix.koen)
discodata <- left_join(discodata,seg.df.mix.koen)
#view(discodata)


### delanalyse 2 om ledighed 

discodata <- discodata %>%  mutate(ledighed.over.median = ledighed.mean.gns > median(discodata$ledighed.mean.gns))
discodata <- discodata %>%  mutate(ledighed.under.median = ledighed.mean.gns < median(discodata$ledighed.mean.gns))
#find de grupper der har mix af over/under median i ledighed 
seg.df.mix.ledig <-  discodata %>% group_by(membership) %>% summarise(seg.antal.lang.ledighed=sum(ledighed.over.median)) %>%   filter(!grepl("^1.*", membership))
tmp1 <- seg.df %>%   select(Nodes,membership) %>%   filter(!grepl("^1.*", membership))
seg.df.mix.ledig <- left_join(seg.df.mix.ledig,tmp1)
seg.df.mix.ledig <- mutate(seg.df.mix.ledig, seg.ledighed.fordeling = mosaic::derivedFactor("Kun hoej ledighed" = (Nodes == seg.antal.lang.ledighed), "Kun lav ledighed" = (seg.antal.lang.ledighed == 0), .method = "first", .default = "Mikset"))
seg.df <- left_join(seg.df,seg.df.mix.ledig)
discodata <- left_join(discodata,seg.df.mix.ledig)
#view(discodata)
# view(seg.df.mix.ledig)
# view(seg.df)


############# mob.mat manipulation ##########################

mob.2cifret <- NULL 
mob <- mob.mat#[-274,-274]
mob.c = NULL
mob.c <- cbind(mob[,1],mob[,2])
mob.c <- cbind(mob.c,rowSums(mob[,3:19]))
mob.c <- cbind(mob.c,rowSums(mob[,20:28]))
mob.c <- cbind(mob.c,rowSums(mob[,29:41]))
mob.c <- cbind(mob.c,rowSums(mob[,42:50]))
mob.c <- cbind(mob.c,rowSums(mob[,51:57]))
mob.c <- cbind(mob.c,rowSums(mob[,58:76]))
mob.c <- cbind(mob.c,rowSums(mob[,77:94]))
mob.c <- cbind(mob.c,rowSums(mob[,95:104]))
mob.c <- cbind(mob.c,rowSums(mob[,105:108]))
mob.c <- cbind(mob.c,rowSums(mob[,109:132]))
mob.c <- cbind(mob.c,rowSums(mob[,133:144]))
mob.c <- cbind(mob.c,rowSums(mob[,145:149]))
mob.c <- cbind(mob.c,rowSums(mob[,150:164]))
mob.c <- cbind(mob.c,mob[,165])
mob.c <- cbind(mob.c,rowSums(mob[,166:172]))
mob.c <- cbind(mob.c,rowSums(mob[,173:186]))
mob.c <- cbind(mob.c,rowSums(mob[,173:186]))
mob.c <- cbind(mob.c,rowSums(mob[,187:202]))
mob.c <- cbind(mob.c,rowSums(mob[,203:207]))
mob.c <- cbind(mob.c,rowSums(mob[,208:213]))
mob.c <- cbind(mob.c,rowSums(mob[,214:219]))
mob.c <- cbind(mob.c,rowSums(mob[,220:247]))
mob.c <- cbind(mob.c,rowSums(mob[,248:256]))
mob.c <- cbind(mob.c,rowSums(mob[,257:266]))
mob.c <- cbind(mob.c,rowSums(mob[,267:269]))
mob.c <- cbind(mob.c,rowSums(mob[,270:273]))
mob.c <- cbind(mob.c,mob[,274])
mob.r = NULL 
mob.r <- rbind(mob.c[1,],mob.c[2,])
mob.r <- rbind(mob.r,colSums(mob.c[3:19,]))
mob.r <- rbind(mob.r,colSums(mob.c[20:28,]))
mob.r <- rbind(mob.r,colSums(mob.c[29:41,]))
mob.r <- rbind(mob.r,colSums(mob.c[42:50,]))
mob.r <- rbind(mob.r,colSums(mob.c[51:57,]))
mob.r <- rbind(mob.r,colSums(mob.c[58:76,]))
mob.r <- rbind(mob.r,colSums(mob.c[77:94,]))
mob.r <- rbind(mob.r,colSums(mob.c[95:104,]))
mob.r <- rbind(mob.r,colSums(mob.c[105:108,]))
mob.r <- rbind(mob.r,colSums(mob.c[109:132,]))
mob.r <- rbind(mob.r,colSums(mob.c[133:144,]))
mob.r <- rbind(mob.r,colSums(mob.c[145:149,]))
mob.r <- rbind(mob.r,colSums(mob.c[150:164,]))
mob.r <- rbind(mob.r,mob.c[165,])
mob.r <- rbind(mob.r,colSums(mob.c[166:172,]))
mob.r <- rbind(mob.r,colSums(mob.c[173:186,]))
mob.r <- rbind(mob.r,colSums(mob.c[173:186,]))
mob.r <- rbind(mob.r,colSums(mob.c[187:202,]))
mob.r <- rbind(mob.r,colSums(mob.c[203:207,]))
mob.r <- rbind(mob.r,colSums(mob.c[208:213,]))
mob.r <- rbind(mob.r,colSums(mob.c[214:219,]))
mob.r <- rbind(mob.r,colSums(mob.c[220:247,]))
mob.r <- rbind(mob.r,colSums(mob.c[248:256,]))
mob.r <- rbind(mob.r,colSums(mob.c[257:266,]))
mob.r <- rbind(mob.r,colSums(mob.c[267:269,]))
mob.r <- rbind(mob.r,colSums(mob.c[270:273,]))
mob.r <- rbind(mob.r,mob.c[274,])
mob.2cifret <- mob.r 
mob.r = NULL
mob.c = NULL
mob = NULL 



##############

mob.3cifret <- NULL 
mob <- mob.mat#[-274,-274]
mob.c = NULL
mob.c <- cbind(mob[,1],mob[,2])
mob.c <- cbind(mob.c,mob[,3])
mob.c <- cbind(mob.c,mob[,4])
mob.c <- cbind(mob.c,rowSums(mob[,5:11]))
mob.c <- cbind(mob.c,rowSums(mob[,12:19]))
mob.c <- cbind(mob.c,rowSums(mob[,20:28]))
mob.c <- cbind(mob.c,mob[,29])
mob.c <- cbind(mob.c,mob[,30])
mob.c <- cbind(mob.c,rowSums(mob[,31:33]))
mob.c <- cbind(mob.c,rowSums(mob[,33:41]))
mob.c <- cbind(mob.c,rowSums(mob[,42:49]))
mob.c <- cbind(mob.c,mob[,50])
mob.c <- cbind(mob.c,mob[,51])
mob.c <- cbind(mob.c,mob[,52])
mob.c <- cbind(mob.c,mob[,53])
mob.c <- cbind(mob.c,mob[,54])
mob.c <- cbind(mob.c,rowSums(mob[,55:57]))
mob.c <- cbind(mob.c,rowSums(mob[,58:60]))
mob.c <- cbind(mob.c,rowSums(mob[,61:63]))
mob.c <- cbind(mob.c,mob[,64])
mob.c <- cbind(mob.c,rowSums(mob[,65:70]))
mob.c <- cbind(mob.c,rowSums(mob[,71:74]))
mob.c <- cbind(mob.c,mob[,75])
mob.c <- cbind(mob.c,mob[,76])
mob.c <- cbind(mob.c,rowSums(mob[,77:84]))
mob.c <- cbind(mob.c,rowSums(mob[,85:86]))
mob.c <- cbind(mob.c,rowSums(mob[,87:89]))
mob.c <- cbind(mob.c,rowSums(mob[,90:93]))
mob.c <- cbind(mob.c,mob[,94])
mob.c <- cbind(mob.c,rowSums(mob[,95:97]))
mob.c <- cbind(mob.c,rowSums(mob[,98:103]))
mob.c <- cbind(mob.c,mob[,104])
mob.c <- cbind(mob.c,mob[,105])
mob.c <- cbind(mob.c,mob[,106])
mob.c <- cbind(mob.c,mob[,107])
mob.c <- cbind(mob.c,mob[,108])
mob.c <- cbind(mob.c,rowSums(mob[,109:116]))
mob.c <- cbind(mob.c,rowSums(mob[,117:120]))
mob.c <- cbind(mob.c,rowSums(mob[,121:125]))
mob.c <- cbind(mob.c,rowSums(mob[,126:127]))
mob.c <- cbind(mob.c,mob[,128])
mob.c <- cbind(mob.c,mob[,129])
mob.c <- cbind(mob.c,rowSums(mob[,130:131]))
mob.c <- cbind(mob.c,mob[,132])
mob.c <- cbind(mob.c,rowSums(mob[,133:136]))
mob.c <- cbind(mob.c,rowSums(mob[,137:138]))
mob.c <- cbind(mob.c,rowSums(mob[,139:141]))
mob.c <- cbind(mob.c,rowSums(mob[,142:143]))
mob.c <- cbind(mob.c,mob[,144])
mob.c <- cbind(mob.c,rowSums(mob[,145:146]))
mob.c <- cbind(mob.c,rowSums(mob[,147:149]))
mob.c <- cbind(mob.c,rowSums(mob[,150:152]))
mob.c <- cbind(mob.c,rowSums(mob[,153:155]))
mob.c <- cbind(mob.c,rowSums(mob[,156:158]))
mob.c <- cbind(mob.c,mob[,159])
mob.c <- cbind(mob.c,mob[,160])
mob.c <- cbind(mob.c,rowSums(mob[,161:164]))
mob.c <- cbind(mob.c,mob[,165])
mob.c <- cbind(mob.c,rowSums(mob[,166:167]))
mob.c <- cbind(mob.c,rowSums(mob[,168:169]))
mob.c <- cbind(mob.c,mob[,170])
mob.c <- cbind(mob.c,mob[,171])
mob.c <- cbind(mob.c,mob[,172])
mob.c <- cbind(mob.c,rowSums(mob[,173:176]))
mob.c <- cbind(mob.c,rowSums(mob[,177:183]))
mob.c <- cbind(mob.c,rowSums(mob[,184:186]))
mob.c <- cbind(mob.c,rowSums(mob[,187:190]))
mob.c <- cbind(mob.c,rowSums(mob[,191:194]))
mob.c <- cbind(mob.c,rowSums(mob[,195:197]))
mob.c <- cbind(mob.c,rowSums(mob[,198:202]))
mob.c <- cbind(mob.c,mob[,203])
mob.c <- cbind(mob.c,mob[,204])
mob.c <- cbind(mob.c,rowSums(mob[,205:207]))
mob.c <- cbind(mob.c,rowSums(mob[,208:210]))
mob.c <- cbind(mob.c,rowSums(mob[,211:212]))
mob.c <- cbind(mob.c,mob[,213])
mob.c <- cbind(mob.c,rowSums(mob[,214:215]))
mob.c <- cbind(mob.c,mob[,216])
mob.c <- cbind(mob.c,mob[,217])
mob.c <- cbind(mob.c,mob[,218])
mob.c <- cbind(mob.c,mob[,219])
mob.c <- cbind(mob.c,rowSums(mob[,220:221]))
mob.c <- cbind(mob.c,rowSums(mob[,222:224]))
mob.c <- cbind(mob.c,rowSums(mob[,225:226]))
mob.c <- cbind(mob.c,mob[,227])
mob.c <- cbind(mob.c,rowSums(mob[,228:230]))
mob.c <- cbind(mob.c,rowSums(mob[,231:235]))
mob.c <- cbind(mob.c,rowSums(mob[,236:240]))
mob.c <- cbind(mob.c,rowSums(mob[,241:246]))
mob.c <- cbind(mob.c,mob[,247])
mob.c <- cbind(mob.c,rowSums(mob[,248:249]))
mob.c <- cbind(mob.c,rowSums(mob[,250:252]))
mob.c <- cbind(mob.c,rowSums(mob[,253:255]))
mob.c <- cbind(mob.c,mob[,256])
mob.c <- cbind(mob.c,mob[,257])
mob.c <- cbind(mob.c,rowSums(mob[,258:260]))
mob.c <- cbind(mob.c,rowSums(mob[,261:262]))
mob.c <- cbind(mob.c,rowSums(mob[,263:264]))
mob.c <- cbind(mob.c,rowSums(mob[,265:266]))
mob.c <- cbind(mob.c,rowSums(mob[,267:269]))
mob.c <- cbind(mob.c,rowSums(mob[,270:271]))
mob.c <- cbind(mob.c,mob[,272])
mob.c <- cbind(mob.c,mob[,273])
mob.c <- cbind(mob.c,mob[,274])
mob.r = NULL 
mob.r <- rbind(mob.c[1,],mob.c[2,])
mob.r <- rbind(mob.r,mob.c[3,])
mob.r <- rbind(mob.r,mob.c[4,])
mob.r <- rbind(mob.r,colSums(mob.c[5:11,]))
mob.r <- rbind(mob.r,colSums(mob.c[12:19,]))
mob.r <- rbind(mob.r,colSums(mob.c[20:28,]))
mob.r <- rbind(mob.r,mob.c[29,])
mob.r <- rbind(mob.r,mob.c[30,])
mob.r <- rbind(mob.r,colSums(mob.c[31:33,]))
mob.r <- rbind(mob.r,colSums(mob.c[34:41,]))
mob.r <- rbind(mob.r,colSums(mob.c[42:49,]))
mob.r <- rbind(mob.r,mob.c[50,])
mob.r <- rbind(mob.r,mob.c[51,])
mob.r <- rbind(mob.r,mob.c[52,])
mob.r <- rbind(mob.r,mob.c[53,])
mob.r <- rbind(mob.r,mob.c[54,])
mob.r <- rbind(mob.r,colSums(mob.c[55:57,]))
mob.r <- rbind(mob.r,colSums(mob.c[58:60,]))
mob.r <- rbind(mob.r,colSums(mob.c[61:63,]))
mob.r <- rbind(mob.r,mob.c[64,])
mob.r <- rbind(mob.r,colSums(mob.c[65:70,]))
mob.r <- rbind(mob.r,colSums(mob.c[71:74,]))
mob.r <- rbind(mob.r,mob.c[75,])
mob.r <- rbind(mob.r,mob.c[76,])
mob.r <- rbind(mob.r,colSums(mob.c[77:84,]))
mob.r <- rbind(mob.r,colSums(mob.c[85:86,]))
mob.r <- rbind(mob.r,colSums(mob.c[87:89,]))
mob.r <- rbind(mob.r,colSums(mob.c[90:93,]))
mob.r <- rbind(mob.r,mob.c[94,])
mob.r <- rbind(mob.r,colSums(mob.c[95:97,]))
mob.r <- rbind(mob.r,colSums(mob.c[98:103,]))
mob.r <- rbind(mob.r,mob.c[104,])
mob.r <- rbind(mob.r,mob.c[105,])
mob.r <- rbind(mob.r,mob.c[106,])
mob.r <- rbind(mob.r,mob.c[107,])
mob.r <- rbind(mob.r,mob.c[108,])
mob.r <- rbind(mob.r,colSums(mob.c[109:116,]))
mob.r <- rbind(mob.r,colSums(mob.c[117:120,]))
mob.r <- rbind(mob.r,colSums(mob.c[121:125,]))
mob.r <- rbind(mob.r,colSums(mob.c[126:127,]))
mob.r <- rbind(mob.r,mob.c[128,])
mob.r <- rbind(mob.r,mob.c[129,])
mob.r <- rbind(mob.r,colSums(mob.c[130:131,]))
mob.r <- rbind(mob.r,mob.c[132,])
mob.r <- rbind(mob.r,colSums(mob.c[133:136,]))
mob.r <- rbind(mob.r,colSums(mob.c[137:138,]))
mob.r <- rbind(mob.r,colSums(mob.c[139:141,]))
mob.r <- rbind(mob.r,colSums(mob.c[142:143,]))
mob.r <- rbind(mob.r,mob.c[144,])
mob.r <- rbind(mob.r,colSums(mob.c[145:146,]))
mob.r <- rbind(mob.r,colSums(mob.c[147:149,]))
mob.r <- rbind(mob.r,colSums(mob.c[150:152,]))
mob.r <- rbind(mob.r,colSums(mob.c[153:155,]))
mob.r <- rbind(mob.r,colSums(mob.c[156:158,]))
mob.r <- rbind(mob.r,mob.c[159,])
mob.r <- rbind(mob.r,mob.c[160,])
mob.r <- rbind(mob.r,colSums(mob.c[161:164,]))
mob.r <- rbind(mob.r,mob.c[165,])
mob.r <- rbind(mob.r,colSums(mob.c[166:167,]))
mob.r <- rbind(mob.r,colSums(mob.c[168:169,]))
mob.r <- rbind(mob.r,mob.c[170,])
mob.r <- rbind(mob.r,mob.c[171,])
mob.r <- rbind(mob.r,mob.c[172,])
mob.r <- rbind(mob.r,colSums(mob.c[173:176,]))
mob.r <- rbind(mob.r,colSums(mob.c[177:183,]))
mob.r <- rbind(mob.r,colSums(mob.c[184:186,]))
mob.r <- rbind(mob.r,colSums(mob.c[187:190,]))
mob.r <- rbind(mob.r,colSums(mob.c[191:194,]))
mob.r <- rbind(mob.r,colSums(mob.c[195:197,]))
mob.r <- rbind(mob.r,colSums(mob.c[198:202,]))
mob.r <- rbind(mob.r,mob.c[203,])
mob.r <- rbind(mob.r,mob.c[204,])
mob.r <- rbind(mob.r,colSums(mob.c[205:207,]))
mob.r <- rbind(mob.r,colSums(mob.c[208:210,]))
mob.r <- rbind(mob.r,colSums(mob.c[211:212,]))
mob.r <- rbind(mob.r,mob.c[213,])
mob.r <- rbind(mob.r,colSums(mob.c[214:215,]))
mob.r <- rbind(mob.r,mob.c[216,])
mob.r <- rbind(mob.r,mob.c[217,])
mob.r <- rbind(mob.r,mob.c[218,])
mob.r <- rbind(mob.r,mob.c[219,])
mob.r <- rbind(mob.r,colSums(mob.c[220:221,]))
mob.r <- rbind(mob.r,colSums(mob.c[222:224,]))
mob.r <- rbind(mob.r,colSums(mob.c[225:226,]))
mob.r <- rbind(mob.r,mob.c[227,])
mob.r <- rbind(mob.r,colSums(mob.c[228:230,]))
mob.r <- rbind(mob.r,colSums(mob.c[231:235,]))
mob.r <- rbind(mob.r,colSums(mob.c[236:240,]))
mob.r <- rbind(mob.r,colSums(mob.c[241:246,]))
mob.r <- rbind(mob.r,mob.c[247,])
mob.r <- rbind(mob.r,colSums(mob.c[248:249,]))
mob.r <- rbind(mob.r,colSums(mob.c[250:252,]))
mob.r <- rbind(mob.r,colSums(mob.c[253:255,]))
mob.r <- rbind(mob.r,mob.c[256,])
mob.r <- rbind(mob.r,mob.c[257,])
mob.r <- rbind(mob.r,colSums(mob.c[258:260,]))
mob.r <- rbind(mob.r,colSums(mob.c[261:262,]))
mob.r <- rbind(mob.r,colSums(mob.c[263:264,]))
mob.r <- rbind(mob.r,colSums(mob.c[265:266,]))
mob.r <- rbind(mob.r,colSums(mob.c[267:269,]))
mob.r <- rbind(mob.r,colSums(mob.c[270:271,]))
mob.r <- rbind(mob.r,mob.c[272,])
mob.r <- rbind(mob.r,mob.c[273,])
mob.r <- rbind(mob.r,mob.c[274,])
mob.3cifret <- mob.r
mob.r = NULL
mob.c = NULL 
mob = NULL 








