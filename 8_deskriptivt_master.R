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



view(seg.opsummering)
view(seg.qual.final)
view(seg.qual)

disco.inseg <- filter(discodata, !grepl("^1.*", membership))
disco.not.inseg <- filter(discodata, grepl("^1.*", membership))

sd(discodata$within.mob.seg)
sd(seg.qual.final$within.mob.seg)

median(discodata$within.mob.seg)
median(seg.qual.final$within.mob.seg)


sd(discodata$within.mob)
(sd(discodata$within.mob)- sd(discodata$within.mob.seg)) * 100
# standardafvigelse er faldet


summary(discodata$within.mob)

view(discodata)

# df med de nyttige variable 

df  <- discodata %>% 	select(-contains("200"),-contains("199"),-timelon.helepop.gns.inf.dst) %>% 	select(disco,membership,disco_s,within.mob,within.mob.seg,within.mob.dif,Density,Nodes,max.path,share.of.mob,contains("gns"),ends_with("cifret"),everything()) %>% 	tbl_df()

df <- tbl_df(df)
tmp <-  select(df,disco_s,membership)
tmp$label <- as.numeric(factor(tmp$membership))
tmp$label2 <- as.numeric(as.character(tmp$membership))*1000



view(arrange(tmp,desc(label)))
options(dplyr.width = Inf)

S <- rep(letters[1:12],each=6)
R = sort(replicate(9, sample(5000:6000,4)))
df <- data.frame(R,S)

result <- df %>% mutate(label = as.numeric(S)) 

S <- rep(letters[1:12],each=6)
R = sort(replicate(9, sample(5000:6000,4)))
df <- data.frame(R,S)







### 


# find navn der matcher mønster
filter(df,grepl(".*mili*.", as.character(disco),ignore.case=TRUE)) %>% 	select(disco)

#check enkelt disco indhold
desk.tmp1 <- c(5131:5133,5113)
desk.tmp2 <- c("disco_s","membership","within.mob","within.mob.seg","alder.helepop.gns","koen.gns.kvinder.andel")
df.t <-  df %>% select_(.dots=desk.tmp2) %>% filter(grepl(paste(desk.tmp1,collapse="|"),disco_s)) %>% 	 mutate(disco_s2 = disco_s) 
view(df.t)





### density #########

dens1 <- discodata %>% 	filter(Density==1) %>% 	select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns)

deskall <- discodata %>% select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns)
	

view(discodata)
view(seg.qual.final)






mean(discodata$alder.helepop.gns)
summary(discodata$alder.helepop.gns)


view(df)
view(discodata)

view(dens1)






print(dens1, n=50)

help(print)

options(dplyr.width = Inf)


### antal beskæftigede 

deskall <- discodata %>% select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns) %>% arrange(desc(beskaeft.andel.gns))
view(deskall)




## intern mobilitet seg 
# seg.df <- seg.qual.final
seg.df$membership  <-  as.factor(seg.df$membership)
til.seg.df.1 <- ddply(discodata,~membership,summarise,beskaeft.gns=sum(beskaeft.gns))
til.seg.df.2 <- ddply(discodata,~membership,summarise,beskaeft.andel.gns=sum(beskaeft.andel.gns))
seg.df <- left_join(seg.df, til.seg.df.1)
seg.df <- left_join(seg.df, til.seg.df.2)
# lapply(seg.df,is.numeric)
seg.df <-   select(seg.df,membership, everything())

view(seg.df)





til.seg.qual.detailed <- select(discodata,disco,membership,within.mob.seg,Density,max.path,within.mob,within.mob.dif) 
seg.qual.detailed.df  <-  left_join(seg.qual.detailed,til.seg.qual.detailed)
view(seg.qual.detailed.df)








## segment labels 


# egp11_lab  =  c("I"="I: Oevre Serviceklasse","II"="II: Nedre Serviceklasse","IIIa"="IIIa: Rutinepraeget, ikke-manuelt arbejde hoejeste niveau","IIIb"="IIIb: Rutinepraeget, ikke-manuelt arbejde laveste niveau","Iva"="IVa: små selvstaendige med ansatte","Ivb"="IVb: små selvstaendige uden ansatte","Ivc"="IVc: Landmaend og andre selvstaendige i primaer produktion ","V"="V: Teknikere af laveste grad, supervisorer af manuelt arbejde","VI"="VI: Faglaerte, manulle arbejdere","VIIa"="VIIa: Ikke-faglaerte, manuelle arbejdere","VIIb"="VIIb: landbrugsarbejdere")

order(as.character(seg.df$membership))

is.factor(seg.df$membership)


seg.lab.tmp <- seg.df$membership
view(seg.lab.tmp)

view(seg.df)

levels()

help(order)


seg.df$seg.lab <- recode_factor(seg.df$membership, 
"5.1"	= "5.1",
"5.2"	= "Bygningsarbejdere I",
"4.8"	= "4.8",
"4.10"	= "4.10",
"4.6"	= "4.6",
"4.1"	= "4.1",
"4.9"	= "Bygningsarbejdere II",
"4.12"	= "4.12",
"4.11"	= "4.11",
"4.13"	= "4.13",
"4.4"	= "4.4",
"4.2"	= "4.2",
"3.35"	= "3.35",
"3.8"	= "3.8",
"3.36"	= "3.36",
"3.26"	= "3.26",
"3.24"	= "3.24",
"3.21"	= "3.21",
"3.14"	= "3.14",
"3.20"	= "3.20",
"3.30"	= "3.30",
"3.18"	= "3.18",
"3.37"	= "3.37",
"3.25"	= "3.25",
"3.15"	= "3.15",
"3.12"	= "3.12",
"3.2"	= "3.2",
"3.9"	= "3.9",
"2.66"	= "2.66",
"2.78"	= "2.78",
"2.61"	= "2.61",
"2.56"	= "2.56",
"2.31"	= "2.31",
"2.64"	= "2.64",
"2.24"	= "2.24",
"2.74"	= "2.74",
"2.80"	= "2.80",
"2.58"	= "2.58",
"2.40"	= "2.40",
"1.45"	= "1.45",
"1.159"	= "1.159",
"1.75"	= "1.75",
"1.93"	= "1.93",
"1.162"	= "1.162",
"1.260"	= "1.260",
"1.27"	= "1.27",
"1.194"	= "1.194",
"1.204"	= "1.204",
"1.214"	= "1.214",
"1.217"	= "1.217",
"1.223"	= "1.223",
.default=NA_character_)
# view(seg.df)

seg.lab.df <- select(seg.df,membership,seg.lab)

discodata <- inner_join(discodata,seg.lab.df)
view(discodata)


# old school metode 

view(seg.df)

segment.labels <- read.csv("./statistik/R/moneca/vores/voresdata/segment_labels.csv", sep = ";", encoding = "UTF-8")$Langt
segment.labels <-  as.factor(segment.labels)
is.factor(segment.labels) #ja
nlevels(segment.labels) # 51 levels (de 51 segmenter)
length(segment.labels) # 51 elementer, dvs antal levels og antal elementer er det samme 
levels(segment.labels) #factor levels er rangeret alfabetisk
segment.labels #men det er elementerne i vektoren ikke


seg.lab        <- seg.mem$membership
is.character(seg.lab) #charactervector med numeriske værdier for alle 273 noder
length(seg.lab) # 273 elementer svarende til de 273 noder
length(unique(seg.lab)) # 51 unikke værdier, svarende til de 51 segmenter is my guess
levels(seg.lab) # har ingen levels


seg.qual.final  <- segment.quality(seg, final.solution = TRUE)

levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(discodata$seg.lab))])
is.character(seg.lab) #stadig charactor vector
length(seg.lab) # stadig 273 elementer 
length(unique(seg.lab)) # stadig  51 unikke værdier
levels(seg.lab) # har nu levels, og de er rangeret efter hvordan de ligger i seg.qual.final 
seg.lab

seg.lab2 <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])



seg.lab3        <- format(as.character(seg.lab2))
levels(seg.lab3) #levels fjernes igen? wtf? og det er vist ikke engang nødvendigt.



