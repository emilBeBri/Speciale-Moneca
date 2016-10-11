#########################
### ledsoc_tael #####
#########################

# socio/socstil population "moneca"
desk.aupop.ledsoc_tael <- NULL 
desk.aupop.ledsoc_tael <- loadWorkbook("./statistik/DST/DST_output/08 august/ledsoc_tael_desk_socstil_socio_pop.xlsx")
lst = readWorksheet(desk.aupop.ledsoc_tael, sheet = getSheets(desk.aupop.ledsoc_tael))
# view(lst[[1]])
desk.aupop.ledsoc_tael <- data.frame(matrix(unlist(lst), nrow=2),stringsAsFactors=FALSE)
columns <- c(2,6,10,14,18,22,26,30,34,38,42,46)
desk.aupop.ledsoc_tael <- desk.aupop.ledsoc_tael[1,c(columns)]
desk.aupop.ledsoc_tael <-  t(desk.aupop.ledsoc_tael)
desk.aupop.ledsoc_tael <- tbl_df(data.frame(desk.aupop.ledsoc_tael))
desk.aupop.ledsoc_tael$X1 <- as.numeric(as.character(desk.aupop.ledsoc_tael$X1))
desk.aupop.ledsoc_tael$seq <- seq_len(nrow(desk.aupop.ledsoc_tael))
desk.aupop.ledsoc_tael <- rbind(desk.aupop.ledsoc_tael,data.frame(X1=0, seq=0.1))
desk.aupop.ledsoc_tael <- rbind(desk.aupop.ledsoc_tael,data.frame(X1=0, seq=12.1))
desk.aupop.ledsoc_tael <- desk.aupop.ledsoc_tael[order(desk.aupop.ledsoc_tael$seq),]
# grp <-  rep("ledsoc_tael_aupop_socstil_socio", 14)
grp <-  rep("1", 14)
desk.aupop.ledsoc_tael <-  cbind(grp,desk.aupop.ledsoc_tael)
desk.aupop.ledsoc_tael$grp <-  as.numeric(as.character(desk.aupop.ledsoc_tael$grp))
desk.aupop.ledsoc_tael$Aar  <-  seq(1996,2009,by=1)
desk.aupop.ledsoc_tael$seq <- NULL 
desk.aupop.ledsoc_tael$N <- desk.aupop.ledsoc_tael$X1
desk.aupop.ledsoc_tael$X1 <- NULL 
# view(desk.aupop.ledsoc_tael) 

 # socstil "moneca"

desk.aupop.ledsoc_tael.socstil <- NULL 
desk.aupop.ledsoc_tael.socstil <- loadWorkbook("./statistik/DST/DST_output/08 august/ledsoc_tael_desk_socstil_pop.xlsx")
lst = readWorksheet(desk.aupop.ledsoc_tael.socstil, sheet = getSheets(desk.aupop.ledsoc_tael.socstil))
# view(lst[[1]])
desk.aupop.ledsoc_tael.socstil <- data.frame(matrix(unlist(lst), nrow=2),stringsAsFactors=FALSE)
columns <- c(2,6,10,14,18,22,26,30,34,38,42,46)
desk.aupop.ledsoc_tael.socstil <- desk.aupop.ledsoc_tael.socstil[1,c(columns)]
desk.aupop.ledsoc_tael.socstil <-  t(desk.aupop.ledsoc_tael.socstil)
desk.aupop.ledsoc_tael.socstil <- tbl_df(data.frame(desk.aupop.ledsoc_tael.socstil))
desk.aupop.ledsoc_tael.socstil$X1 <- as.numeric(as.character(desk.aupop.ledsoc_tael.socstil$X1))
desk.aupop.ledsoc_tael.socstil$seq <- seq_len(nrow(desk.aupop.ledsoc_tael.socstil))
desk.aupop.ledsoc_tael.socstil <- rbind(desk.aupop.ledsoc_tael.socstil,data.frame(X1=0, seq=0.1))
desk.aupop.ledsoc_tael.socstil <- rbind(desk.aupop.ledsoc_tael.socstil,data.frame(X1=0, seq=12.1))
desk.aupop.ledsoc_tael.socstil <- desk.aupop.ledsoc_tael.socstil[order(desk.aupop.ledsoc_tael.socstil$seq),]
# grp <-  rep("ledsoc_tael_aupop_socstil", 14)
grp <-  rep("2", 14)
desk.aupop.ledsoc_tael.socstil <-  cbind(grp,desk.aupop.ledsoc_tael.socstil)
desk.aupop.ledsoc_tael.socstil$grp <-  as.numeric(as.character(desk.aupop.ledsoc_tael.socstil$grp))
desk.aupop.ledsoc_tael.socstil$Aar  <-  seq(1996,2009,by=1)
desk.aupop.ledsoc_tael.socstil$seq <- NULL 
desk.aupop.ledsoc_tael.socstil$N <- desk.aupop.ledsoc_tael.socstil$X1 
desk.aupop.ledsoc_tael.socstil$X1 <- NULL 
# view(desk.aupop.ledsoc_tael.socstil)
desk.aupop.ledsoc_tael.socstil$grp 


 # socio/socio02 "moneca"

desk.aupop.ledsoc_tael.socio <- NULL 
desk.aupop.ledsoc_tael.socio <- loadWorkbook("./statistik/DST/DST_output/08 august/ledsoc_tael_desk_socio_pop.xlsx")
lst = readWorksheet(desk.aupop.ledsoc_tael.socio, sheet = getSheets(desk.aupop.ledsoc_tael.socio))
# view(lst[[1]])
desk.aupop.ledsoc_tael.socio <- data.frame(matrix(unlist(lst), nrow=2),stringsAsFactors=FALSE)
columns <- c(2,6,10,14,18,22,26,30,34,38,42,46)
desk.aupop.ledsoc_tael.socio <- desk.aupop.ledsoc_tael.socio[1,c(columns)]
desk.aupop.ledsoc_tael.socio <-  t(desk.aupop.ledsoc_tael.socio)
desk.aupop.ledsoc_tael.socio <- tbl_df(data.frame(desk.aupop.ledsoc_tael.socio))
desk.aupop.ledsoc_tael.socio$X1 <- as.numeric(as.character(desk.aupop.ledsoc_tael.socio$X1))
desk.aupop.ledsoc_tael.socio$seq <- seq_len(nrow(desk.aupop.ledsoc_tael.socio))
desk.aupop.ledsoc_tael.socio <- rbind(desk.aupop.ledsoc_tael.socio,data.frame(X1=0, seq=0.1))
desk.aupop.ledsoc_tael.socio <- rbind(desk.aupop.ledsoc_tael.socio,data.frame(X1=0, seq=12.1))
desk.aupop.ledsoc_tael.socio <- desk.aupop.ledsoc_tael.socio[order(desk.aupop.ledsoc_tael.socio$seq),]
# grp <-  rep("ledsoc_tael_aupop_socio", 14)
grp <-  rep("3", 14)
desk.aupop.ledsoc_tael.socio <-  cbind(grp,desk.aupop.ledsoc_tael.socio)
desk.aupop.ledsoc_tael.socio$grp <-  as.numeric(as.character(desk.aupop.ledsoc_tael.socio$grp))
desk.aupop.ledsoc_tael.socio$Aar  <-  seq(1996,2009,by=1)
desk.aupop.ledsoc_tael.socio$seq <- NULL 
desk.aupop.ledsoc_tael.socio$N <- desk.aupop.ledsoc_tael.socio$X1 
desk.aupop.ledsoc_tael.socio$X1 <- NULL 

 # socio/socstil VORES MONECA REAL AU

desk.aupop.ledsoc_tael.voresmoneca <- NULL 
desk.aupop.ledsoc_tael.voresmoneca <- loadWorkbook("./statistik/DST/DST_output/08 august/ledsoc_tael_desk_socstil_socio_AU.xlsx")
lst = readWorksheet(desk.aupop.ledsoc_tael.voresmoneca, sheet = getSheets(desk.aupop.ledsoc_tael.voresmoneca))
# view(lst[[1]])
desk.aupop.ledsoc_tael.voresmoneca <- data.frame(matrix(unlist(lst), nrow=2),stringsAsFactors=FALSE)
columns <- c(2,6,10,14,18,22,26,30,34,38,42,46)
desk.aupop.ledsoc_tael.voresmoneca <- desk.aupop.ledsoc_tael.voresmoneca[1,c(columns)]
desk.aupop.ledsoc_tael.voresmoneca <-  t(desk.aupop.ledsoc_tael.voresmoneca)
desk.aupop.ledsoc_tael.voresmoneca <- tbl_df(data.frame(desk.aupop.ledsoc_tael.voresmoneca))
desk.aupop.ledsoc_tael.voresmoneca$X1 <- as.numeric(as.character(desk.aupop.ledsoc_tael.voresmoneca$X1))
desk.aupop.ledsoc_tael.voresmoneca$seq <- seq_len(nrow(desk.aupop.ledsoc_tael.voresmoneca))
desk.aupop.ledsoc_tael.voresmoneca <- rbind(desk.aupop.ledsoc_tael.voresmoneca,data.frame(X1=0, seq=0.1))
desk.aupop.ledsoc_tael.voresmoneca <- rbind(desk.aupop.ledsoc_tael.voresmoneca,data.frame(X1=0, seq=12.1))
desk.aupop.ledsoc_tael.voresmoneca <- desk.aupop.ledsoc_tael.voresmoneca[order(desk.aupop.ledsoc_tael.voresmoneca$seq),]
# grp <-  rep("ledsoc_tael_voresmoneca", 14)
grp <-  rep("4", 14)
desk.aupop.ledsoc_tael.voresmoneca <-  cbind(grp,desk.aupop.ledsoc_tael.voresmoneca)
desk.aupop.ledsoc_tael.voresmoneca$grp <-  as.numeric(as.character(desk.aupop.ledsoc_tael.voresmoneca$grp))
desk.aupop.ledsoc_tael.voresmoneca$Aar  <-  seq(1996,2009,by=1)
desk.aupop.ledsoc_tael.voresmoneca$seq <- NULL 
desk.aupop.ledsoc_tael.voresmoneca$N <- desk.aupop.ledsoc_tael.voresmoneca$X1 
desk.aupop.ledsoc_tael.voresmoneca$X1 <- NULL 
# view(desk.aupop.ledsoc_tael.voresmoneca)


# socio socstil internt (lavet i hånden)
desk.socio_socstil_netto.ledsoc_tael <- NULL 
desk.socio_socstil_netto.ledsoc_tael <- loadWorkbook("./statistik/DST/DST_output/08 august/socio_socstil_netto_lavetihaanden.xlsx")
desk.socio_socstil_netto.ledsoc_tael = readWorksheet(desk.socio_socstil_netto.ledsoc_tael, sheet = getSheets(desk.socio_socstil_netto.ledsoc_tael))
desk.socio_socstil_netto.ledsoc_tael$N <- as.numeric(as.character(desk.socio_socstil_netto.ledsoc_tael$N))
desk.socio_socstil_netto.ledsoc_tael$Aar <- as.numeric(as.character(desk.socio_socstil_netto.ledsoc_tael$Aar))
desk.socio_socstil_netto.ledsoc_tael <- tbl_df(data.frame(desk.socio_socstil_netto.ledsoc_tael))

# is.numeric(desk.socio_socstil_netto.ledsoc_tael$grp)
# as.factor(desk.socio_socstil_netto.ledsoc_tael$grp)


# kombiner datasæt
desk.ledsoc_tael <- NULL  
desk.ledsoc_tael <- bind_rows(desk.aupop.ledsoc_tael,desk.aupop.ledsoc_tael.socstil)
desk.ledsoc_tael <-  bind_rows(desk.ledsoc_tael, desk.aupop.ledsoc_tael.socio)
desk.ledsoc_tael <-  bind_rows(desk.ledsoc_tael, desk.aupop.ledsoc_tael.voresmoneca)
desk.ledsoc_tael <-  bind_rows(desk.ledsoc_tael, desk.socio_socstil_netto.ledsoc_tael)
desk.ledsoc_tael[desk.ledsoc_tael == 0] <- NA
desk.ledsoc_tael$grp <- as.factor(desk.ledsoc_tael$grp)


################### alder #################################


#########################
### gns alder #####
#########################

# socio/socstil population "moneca"
desk.aupop.alder <- NULL 
desk.aupop.alder <- loadWorkbook("./statistik/DST/DST_output/08 august/aldernov2_desk_socstil_socio_pop.xlsx")
lst = readWorksheet(desk.aupop.alder, sheet = getSheets(desk.aupop.alder))
# view(lst)
desk.aupop.alder <- data.frame(lst) 
# view(desk.aupop.alder)
desk.aupop.alder <- desk.aupop.alder[,c(1,2,4,6)]
grp <-  rep("sociosocstil.alder", 14)
desk.aupop.alder <-  tbl_df(desk.aupop.alder)
desk.aupop.alder <- rename(desk.aupop.alder, mean = sum)
desk.aupop.alder <- cbind(grp,desk.aupop.alder) 
# view(desk.aupop.alder)


# socio/socstil VORES MONECA REAL AU
desk.au.alder <- NULL 
desk.au.alder <- loadWorkbook("./statistik/DST/DST_output/08 august/aldernov2_desk_socstil_socio_AU.xlsx")
lst = readWorksheet(desk.au.alder, sheet = getSheets(desk.au.alder))
# view(lst)
desk.au.alder <- data.frame(lst) 
# view(desk.au.alder)
desk.au.alder <- desk.au.alder[,c(1,2,4,6)]
grp <-  rep("sociosocstildisco.alder", 14)
desk.au.alder <-  tbl_df(desk.au.alder)
desk.au.alder <- rename(desk.au.alder, mean = sum)
desk.au.alder <- cbind(grp,desk.au.alder) 
# view(desk.au.alder)

 # hele danske befolkning for reference
desk.pop.alder <- NULL 
desk.pop.alder <- loadWorkbook("./statistik/DST/DST_output/08 august/aldernov2_desk_helepop.xlsx")
lst = readWorksheet(desk.pop.alder, sheet = getSheets(desk.pop.alder))
# view(lst)
desk.pop.alder <- data.frame(lst) 
# view(desk.pop.alder)
desk.pop.alder <- desk.pop.alder[,c(1,2,4,6)]
grp <-  rep("pop.alder", 14)
desk.pop.alder <-  tbl_df(desk.pop.alder)
desk.pop.alder <- cbind(grp,desk.pop.alder) 
# view(desk.pop.alder)


# kombiner de tre
desk.alder <- bind_rows(desk.au.alder,desk.pop.alder)
desk.alder <- bind_rows(desk.alder,desk.aupop.alder)
desk.alder$Aar <- as.numeric(desk.alder$Aar)
desk.alder$grp <- as.factor(desk.alder$grp)
desk.alder$sd <- as.numeric(desk.alder$sd)
desk.alder$mean <- as.numeric(desk.alder$mean)
desk.alder$N <- as.numeric(desk.alder$N)
desk.alder[desk.alder == 0] <- NA
# view(desk.alder)


##########################################################
                      koen  
##########################################################

# socio/socstil population "moneca"

desk.aupop.koen <- loadWorkbook("./statistik/DST/DST_output/08 august/koen_desk_socstil_socio_pop.xlsx")
lst = readWorksheet(desk.aupop.koen, sheet = getSheets(desk.aupop.koen))
# view(lst[[1]])


## rod 

# desk.ledsoc_tael$grp



# desk.ledsoc_tael$X1 <- NULL 
# view(desk.ledsoc_tael)


# is.numeric(desk.ledsoc_tael$grp)



# test <-  bind_rows(desk.aupop.ledsoc_tael.socio, desk.aupop.ledsoc_tael.socstil)


# desk.aupop.ledsoc_tael.socio$grp 
# desk.aupop.ledsoc_tael.socstil$grp 

# test$grp 


