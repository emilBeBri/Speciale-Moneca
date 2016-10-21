################################# tema  data ##### 

coltab2xl <- c(2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54)



#########################
### Alle beskÃ¦ftigede#########
#########################

#is.matrix(beskaeftigede)
#View(beskaeftigede)
#writeWorksheetToFile("./statistik/R/moneca/vores/output/baggrund/beskaeftigede.xlsx", data = list(beskaeftigede, beskaeftigede.smooth, beskaeftigede.andel,beskaeftigede.andel.smooth),
#                     sheet = c("count", "count.smooth", "andel", "andel.smooth"),
#                     startRow = c(1,1,1,1), startCol = c(1,1,1,1), header=TRUE)
beskaeftigede               <-   beskaeftigede[-nrowtab2xl,]
beskaeftigede.smooth        <-   beskaeftigede.smooth[-nrowtab2xl,]
beskaeftigede.andel         <-   beskaeftigede.andel[-nrowtab2xl,]
beskaeftigede.andel.smooth  <-   beskaeftigede.andel.smooth[-nrowtab2xl,]
label_moneca_beskaeftigede   <- list("beskaeftigede1996" ,"beskaeftigede1997" , "beskaeftigede1998" , "beskaeftigede1999" , "beskaeftigede2000" , "beskaeftigede2001", "beskaeftigede2002" , "beskaeftigede2003" , "beskaeftigede2004" , "beskaeftigede2005", "beskaeftigede2006",  "beskaeftigede2007",  "beskaeftigede2008",  "beskaeftigede2009")
label_moneca_beskaeftigede.smooth   <- list("beskaeftigede.smooth1996" ,"beskaeftigede.smooth1997" , "beskaeftigede.smooth1998" , "beskaeftigede.smooth1999" , "beskaeftigede.smooth2000" , "beskaeftigede.smooth2001", "beskaeftigede.smooth2002" , "beskaeftigede.smooth2003" , "beskaeftigede.smooth2004" , "beskaeftigede.smooth2005", "beskaeftigede.smooth2006",  "beskaeftigede.smooth2007",  "beskaeftigede.smooth2008",  "beskaeftigede.smooth2009")
label_moneca_beskaeftigede.andel   <- list("beskaeftigede.andel1996" ,"beskaeftigede.andel1997" , "beskaeftigede.andel1998" , "beskaeftigede.andel1999" , "beskaeftigede.andel2000" , "beskaeftigede.andel2001", "beskaeftigede.andel2002" , "beskaeftigede.andel2003" , "beskaeftigede.andel2004" , "beskaeftigede.andel2005", "beskaeftigede.andel2006",  "beskaeftigede.andel2007",  "beskaeftigede.andel2008",  "beskaeftigede.andel2009")
label_moneca_beskaeftigede.andel.smooth   <- list("beskaeftigede.andel.smooth1996" ,"beskaeftigede.andel.smooth1997" , "beskaeftigede.andel.smooth1998" , "beskaeftigede.andel.smooth1999" , "beskaeftigede.andel.smooth2000" , "beskaeftigede.andel.smooth2001", "beskaeftigede.andel.smooth2002" , "beskaeftigede.andel.smooth2003" , "beskaeftigede.andel.smooth2004" , "beskaeftigede.andel.smooth2005", "beskaeftigede.andel.smooth2006",  "beskaeftigede.andel.smooth2007",  "beskaeftigede.andel.smooth2008",  "beskaeftigede.andel.smooth2009")
beskaeftigede <- disco.df(beskaeftigede, label_moneca_beskaeftigede)
beskaeftigede.smooth <- disco.df(beskaeftigede.smooth, label_moneca_beskaeftigede.smooth)
beskaeftigede.andel <- disco.df(beskaeftigede.andel, label_moneca_beskaeftigede.andel)
beskaeftigede.andel.smooth <- disco.df(beskaeftigede.andel.smooth, label_moneca_beskaeftigede.andel.smooth)
beskaeft.samlet.tmp1 <- left_join(beskaeftigede, beskaeftigede.smooth)
beskaeft.samlet.tmp2 <- left_join(beskaeftigede.andel, beskaeftigede.andel.smooth)
beskaeft.samlet <- left_join(beskaeft.samlet.tmp1, beskaeft.samlet.tmp2)
#View(beskaeft.samlet)
# is.character(beskaeft.samlet$disco)




#seg.mem proces datasaet
seg.mem.df <- tbl_df(seg.mem) 
seg.mem.df <- rename(seg.mem.df, disco = name)
# seg.mem.df$membership  <-  as.factor(as.numeric(seg.mem.df$membership)) #ødelægger levels måske pga punktummet?!?
# seg.mem.df$membership  <-  as.factor(seg.mem.df$membership)
# test <- distinct(seg.mem.df,membership,.keep_all=TRUE)
# nrow(test)
# view(seg.mem.df)
# der er kun 49 levels hvorfor ikke 51?!?

#view(seg.mem.df)



#kunne evt laves til et slags loop-der-ikke-er-et-loop med alle variablene
seg.qual.detailed <- seg.qual
seg.qual <- NULL 
seg.qual.detailed   <-  tbl_df(seg.qual.detailed) 
seg.qual.detailed$raekkefoelge <- seq(1:nrow(seg.qual.detailed))
seg.qual.detailed <-   arrange(seg.qual.detailed, `1: Segment`)
seg.qual.detailed$disco <- label[-l]
seg.qual.detailed <- rename(seg.qual.detailed, lvl1.segment = `1: Segment`)
seg.qual.detailed <- rename(seg.qual.detailed, lvl1.within.mob = `1: within.mobility`)
seg.qual.detailed <- rename(seg.qual.detailed, membership=Membership)
seg.qual.detailed <-   select(seg.qual.detailed,disco, everything())
seg.qual.detailed_tmp <- seg.qual.detailed
seg.qual.detailed_tmp <- seg.qual.detailed_tmp %>% select(disco,lvl1.within.mob)
seg.qual.detailed_tmp <- rename(seg.qual.detailed_tmp, within.mob = lvl1.within.mob)

# view(seg.qual.detailed)
# nrow(seg.qual.detailed)

#### segment.membership for hver gruppe


seg.qual.final  <- segment.quality(seg, final.solution = TRUE)
seg.qual.final <- tbl_df(seg.qual.final)
# view(seg.qual.final)
# nrow(seg.qual.final)

# omdÃ¸b og numerisk
seg.qual.final <- rename(seg.qual.final, membership=Membership)
seg.qual.final <- rename(seg.qual.final, within.mob.seg = `Within mobility`)
seg.qual.final <- rename(seg.qual.final, share.of.mob = `Share of mobility`)
seg.qual.final <- rename(seg.qual.final, max.path = `Max.path`)
seg.qual.final <- rename(seg.qual.final, share.total.size = `Share of total size`)

seg.qual.final$within.mob.seg <- as.numeric(as.character(seg.qual.final$within.mob.seg))
seg.qual.final$share.of.mob <- as.numeric(as.character(seg.qual.final$share.of.mob))
seg.qual.final$max.path <- as.numeric(as.character(seg.qual.final$max.path))
seg.qual.final$share.total.size <- as.numeric(as.character(seg.qual.final$share.total.size))
seg.qual.final$Density <- as.numeric(as.character(seg.qual.final$Density))
seg.qual.final$Nodes <- as.numeric(as.character(seg.qual.final$Nodes))
#sÃ¦tter de uendelige vÃ¦rdier i densitet for enkelte noder lig NA (burde mÃ¥ske ogsÃ¥ gÃ¸res for deres max.path der er lig 0)
is.na(seg.qual.final) <- do.call(cbind,lapply(seg.qual.final, is.infinite))
# nrow(seg.qual.final)
# seg.qual.final$membership  <-  as.factor(seg.qual.final$membership)
# levels(seg.qual.final$membership)

#omdoeber så vi har en final segmentdata 
seg.df <-  seg.qual.final 


#### MASTERJOIN sammensmeltning ###


# tmp data 
# view(seg.mem.df)
# discodata <- left_join(ledbeskaeft.samlet, beskaeft.samlet)
# discodata <- left_join(beskaeft.samlet, seg.mem.df)


# # tmp data 
# save.image("./tmp1")

# rm(list=ls())
# load("./tmp1")



membership <-  seg.mem.df$membership
discodata <- cbind(beskaeft.samlet, membership)





# hot fix til 250-kat version, gud ved hvorfor men den fjerner membership til 1. række, dvs. 110: militær 
# discodata$membership[1] <- 4.9




discodata <- inner_join(discodata, seg.qual.final)



# nrow(discodata)
# view(discodata)
# view(seg.qual.final)

# test <-  discodata %>% distinct(membership,.keep_all=TRUE) 
# nrow(test)
# view(test)

# is.factor(discodata$membership) 
# is.factor(seg.qual.final$membership) 
# levels(seg.qual.final$membership)


# levels(discodata$membership)

# nrow(discodata)
# nrow(seg.qual.final)
# # nrow(seg.mem.df)



discodata <- left_join(discodata,seg.qual.detailed)


# is.numeric(seg.qual.detailed_tmp$within.mob)

# view(discodata)

discodata <- left_join(discodata,seg.qual.detailed_tmp)

seg.qual.detailed_tmp <- NULL 





#### udregning af nye variable i dplyr #########

#view(seg.qual)
#view(seg.qual.final)


# discodata <-  count(v1, v2) %>% mutate(prop = n/sum(n))



# # gns for alle Ã¥r for ledige
# discodata <- mutate(discodata,
#                     ledbeskaeft.gns =  (ledbeskaeft1996 + ledbeskaeft1997 + ledbeskaeft1998 + ledbeskaeft1999 +  ledbeskaeft2000 +  ledbeskaeft2001 + ledbeskaeft2002 + ledbeskaeft2003 + ledbeskaeft2004 + ledbeskaeft2005 + ledbeskaeft2006 + ledbeskaeft2007 + ledbeskaeft2008 + ledbeskaeft2009)/14 )
# discodata$ledbeskaeft.gns <- round(discodata$ledbeskaeft.gns)
# discodata <- mutate(discodata,
#                     ledbeskaeft.andel.gns =  (ledbeskaeft.andel1996 + ledbeskaeft.andel1997 + ledbeskaeft.andel1998 + ledbeskaeft.andel1999 +  ledbeskaeft.andel2000 +  ledbeskaeft.andel2001 + ledbeskaeft.andel2002 + ledbeskaeft.andel2003 + ledbeskaeft.andel2004 + ledbeskaeft.andel2005 + ledbeskaeft.andel2006 + ledbeskaeft.andel2007 + ledbeskaeft.andel2008 + ledbeskaeft.andel2009)/14 )
# #View(discodata$ledbeskaeft.andel.gns)


# gns for alle Ã¥r for alle beskaeftigede
discodata <- mutate(discodata,
                    beskaeft.gns =  (beskaeftigede1996 + beskaeftigede1997 + beskaeftigede1998 + beskaeftigede1999 +  beskaeftigede2000 +  beskaeftigede2001 + beskaeftigede2002 + beskaeftigede2003 + beskaeftigede2004 + beskaeftigede2005 + beskaeftigede2006 + beskaeftigede2007 + beskaeftigede2008 + beskaeftigede2009)/14 )
discodata$beskaeft.gns <- round(discodata$beskaeft.gns)
discodata <- mutate(discodata,
                    beskaeft.andel.gns =  (beskaeftigede.andel1996 + beskaeftigede.andel1997 + beskaeftigede.andel1998 + beskaeftigede.andel1999 +  beskaeftigede.andel2000 +  beskaeftigede.andel2001 + beskaeftigede.andel2002 + beskaeftigede.andel2003 + beskaeftigede.andel2004 + beskaeftigede.andel2005 + beskaeftigede.andel2006 + beskaeftigede.andel2007 + beskaeftigede.andel2008 + beskaeftigede.andel2009)/14 )

#forskel mellem intern mob i noder og segmenter
discodata <-  mutate(discodata, 
                    within.mob.dif = within.mob.seg - within.mob)  








# view(discodata$beskaeft.andel.gns)

# antal ledige på segment niveau
# discodata <-  discodata %>%
#     group_by(membership) %>%
#     mutate(ledbeskaeft.gns.seg = sum(ledbeskaeft.gns))

# discodata <-  discodata %>%
#     group_by(membership) %>%
#     mutate(ledbeskaeft.andel.seg = sum(ledbeskaeft.andel.gns))

# view(discodata)

# segmentnumre fra alle beskaeftigede (til brug i socio/socstil, fx)
# seg.mem.alle.beskaeft <-  tbl_df(seg.mem)
# seg.mem.alle.beskaeft <- rename(seg.mem.alle.beskaeft, disco = name)
# seg.mem.alle.beskaeft <- rename(seg.mem.alle.beskaeft, alle.beskaeft.membership = membership)
# seg.mem.alle.beskaeft$alle.beskaeft.membership <- as.factor(seg.mem.alle.beskaeft$alle.beskaeft.membership)
# gemmer objektet til brug i socio/socstil 
# save(seg.mem.alle.beskaeft, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg.mem.alle.beskaeft.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg.mem.alle.beskaeft.Rdata")
# discodata <- left_join(discodata, seg.mem.alle.beskaeft)
# is.factor(discodata$alle.beskaeft.membership)
# #Erstatter alle levels der starter med 1. med 1 rent ud 
# nlevels(discodata$alle.beskaeft.membership)
# levels(discodata$alle.beskaeft.membership) <- sub("^1.*$", "1", levels(discodata$alle.beskaeft.membership))

# segmentnumre fra sociosocstil
# seg.mem.sociosocstil <-  tbl_df(seg.mem)
# seg.mem.sociosocstil <- rename(seg.mem.sociosocstil, disco = name)
# seg.mem.sociosocstil <- rename(seg.mem.sociosocstil, seg.mem.sociosocstil = membership)
# seg.mem.sociosocstil$seg.mem.sociosocstil <- as.factor(seg.mem.sociosocstil$seg.mem.sociosocstil)
# gemmer objektet til brug i socio/socstil 
# save(seg.mem.sociosocstil, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg.mem.sociosocstil.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg.mem.sociosocstil.Rdata")
# discodata <- left_join(discodata, seg.mem.sociosocstil)
# is.factor(discodata$seg.mem.sociosocstil)
# ##Erstatter alle levels der starter med 1. med 1 rent ud 
# levels(discodata$seg.mem.sociosocstil) <- sub("^1.*$", "1", levels(discodata$seg.mem.sociosocstil))
# nlevels(discodata$seg.mem.sociosocstil)








