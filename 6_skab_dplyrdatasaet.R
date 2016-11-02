

#########lønvariable###


## timelon 
timelon.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/timelon_udenti_kat250__helepop.xlsx")
timelon.helepop <- data.frame(matrix(unlist(timelon.helepop), nrow=nrowputexcel),stringsAsFactors=FALSE)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121)
timelon.helepop <- timelon.helepop[,c(columns)]
fix_disco_labels <- timelon.helepop[,1]
colnames(timelon.helepop) <- label_moneca_[]
#l1_r            <- nrow(timelon.helepop)
#l1_r
timelon.helepop <- sapply(timelon.helepop, as.numeric)
# View(timelon.helepop)
moneca.labels.num <- as.vector(timelon.helepop[, 1])
#View(moneca.labels.num)
timelon.helepop           <- as.matrix(timelon.helepop[, -1]) 

timelon.helepop           <- rbind(timelon.helepop, (colSums(timelon.helepop)/nrowputexcel))

rownames(timelon.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_timelon.helepop   <- list("timelon.helepop1996" ,"timelon.helepop1997" , "timelon.helepop1998" , "timelon.helepop1999" , "timelon.helepop2000" , "timelon.helepop2001", "timelon.helepop2002" , "timelon.helepop2003" , "timelon.helepop2004" , "timelon.helepop2005", "timelon.helepop2006",  "timelon.helepop2007",  "timelon.helepop2008",  "timelon.helepop2009")

timelon.helepop            <- disco.df(timelon.helepop, label_moneca_timelon.helepop)
timelon.helepop     <- timelon.helepop[-nrowtab2xl,]
# view(timelon.helepop)



# source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_loenvariable.R")
# 


## alder 

alder.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/aldernov2_kat250_helepop.xlsx")
alder.helepop <- data.frame(matrix(unlist(alder.helepop), nrow=nrowputexcel),stringsAsFactors=FALSE)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121)
alder.helepop <- alder.helepop[,c(columns)]
colnames(alder.helepop) <- label_moneca_[]


alder.helepop$moneca_label <- fix_disco_labels
alder.helepop <- sapply(alder.helepop, as.numeric)
# view(alder.helepop)
moneca.labels.num <- as.vector(alder.helepop[, 1])
# View(moneca.labels.num)
alder.helepop           <- as.matrix(alder.helepop[, -1]) 
alder.helepop           <- rbind(alder.helepop, (colSums(alder.helepop)/nrowputexcel))

rownames(alder.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_alder.helepop   <- list("alder.helepop1996" ,"alder.helepop1997" , "alder.helepop1998" , "alder.helepop1999" , "alder.helepop2000" , "alder.helepop2001", "alder.helepop2002" , "alder.helepop2003" , "alder.helepop2004" , "alder.helepop2005", "alder.helepop2006",  "alder.helepop2007",  "alder.helepop2008",  "alder.helepop2009")

alder.helepop            <- disco.df(alder.helepop, label_moneca_alder.helepop)
alder.helepop     <- alder.helepop[-nrowtab2xl,]
# view(alder.helepop)


## koen 
koen.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/koen_kat250__helepop.xlsx")
koen.helepop <- data.frame(matrix(unlist(koen.helepop), nrow=nrowputexcel),stringsAsFactors=FALSE)
columns <- seq(2,56,2)
columns <-  append(columns, 1, after = 0)
koen.helepop <- koen.helepop[,c(columns)]
view(koen.helepop)



label_moneca_koen <- c("moneca_label","koen1996total", "koen1996kvinder","koen1997total", "koen1997kvinder","koen1998total", "koen1998kvinder","koen1999total", "koen1999kvinder","koen2000total","koen2000kvinder","koen2001total","koen2001kvinder","koen2002total","koen2002kvinder","koen2003total","koen2003kvinder","koen2004total","koen2004kvinder","koen2005total","koen2005kvinder","koen2006total","koen2006kvinder","koen2007total","koen2007kvinder","koen2008total","koen2008kvinder","koen2009total","koen2009kvinder")
colnames(koen.helepop) <- label_moneca_koen[]
#l1_r            <- nrow(koen.helepop)
#l1_r
koen.helepop <- sapply(koen.helepop, as.numeric)
 # View(koen.helepop)
moneca.labels.num <- as.vector(koen.helepop[, 1])
# View(moneca.labels.num)
koen.helepop           <- as.matrix(koen.helepop[, -1]) 
koen.helepop           <- rbind(koen.helepop, (colSums(koen.helepop)/nrowputexcel))

rownames(koen.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_koen.helepop   <- list("koen1996total", "koen1996kvinder","koen1997total", "koen1997kvinder","koen1998total", "koen1998kvinder","koen1999total", "koen1999kvinder","koen2000total","koen2000kvinder","koen2001total","koen2001kvinder","koen2002total","koen2002kvinder","koen2003total","koen2003kvinder","koen2004total","koen2004kvinder","koen2005total","koen2005kvinder","koen2006total","koen2006kvinder","koen2007total","koen2007kvinder","koen2008total","koen2008kvinder","koen2009total","koen2009kvinder")

koen.helepop            <- disco.df(koen.helepop, label_moneca_koen.helepop)
koen.helepop     <- koen.helepop[-nrowtab2xl,]
# View(koen.helepop)


## ledighed 
ledighed.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/arledgr_kat250_helepop.xlsx")
ledighed.helepop <- data.frame(matrix(unlist(ledighed.helepop), nrow=nrowputexcel),stringsAsFactors=FALSE)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121)
ledighed.helepop <- ledighed.helepop[,c(columns)]
colnames(ledighed.helepop) <- label_moneca_[]
#l1_r            <- nrow(ledighed.helepop)
#l1_r
# view(ledighed.helepop)

ledighed.helepop$moneca_label <- fix_disco_labels
ledighed.helepop <- sapply(ledighed.helepop, as.numeric)
 # View(ledighed.helepop)
moneca.labels.num <- as.vector(ledighed.helepop[, 1])
# View(moneca.labels.num)
ledighed.helepop           <- as.matrix(ledighed.helepop[, -1]) 

ledighed.helepop           <- rbind(ledighed.helepop, (colSums(ledighed.helepop)/nrowputexcel))

rownames(ledighed.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_ledighed.helepop   <- list("ledighed.helepop1996" ,"ledighed.helepop1997" , "ledighed.helepop1998" , "ledighed.helepop1999" , "ledighed.helepop2000" , "ledighed.helepop2001", "ledighed.helepop2002" , "ledighed.helepop2003" , "ledighed.helepop2004" , "ledighed.helepop2005", "ledighed.helepop2006",  "ledighed.helepop2007",  "ledighed.helepop2008",  "ledighed.helepop2009")

ledighed.helepop            <- disco.df(ledighed.helepop, label_moneca_ledighed.helepop)
ledighed.helepop     <- ledighed.helepop[-nrowtab2xl,]
# view(ledighed.helepop)



###################### join på disco #########################


discodata     <- left_join(discodata, timelon.helepop)
discodata     <- left_join(discodata, alder.helepop)
discodata     <- left_join(discodata, koen.helepop)
discodata     <- left_join(discodata, ledighed.helepop)
# discodata     <- left_join(discodata, ledperiod.gns.lngde)
# discodata     <- left_join(discodata, ledperiod.antal)
# discodata     <- left_join(discodata, loenmv)
# discodata     <- left_join(discodata, loenmv.helepop)
# discodata     <- left_join(discodata, disponny.helepop)
# discodata     <- left_join(discodata, perindkialt.helepop)
# discodata     <- left_join(discodata, joblon.helepop)

# de korte labels 
discodata$disco_s <-  label.short[-nrowtab2xl] 
#faktor-konvertering
discodata$disco <- as.factor(discodata$disco)
discodata$disco_s <- as.factor(discodata$disco_s)
discodata$membership <- as.factor(discodata$membership)

#disco 1-cifret niveau
discodata$disco_1cifret        <- as.factor(as.numeric(strtrim(discodata$disco_s, 1)))
discodata$disco_1cifret[1] <- NA 



#EGP-11
egp11_lab  =  c("I"="I: Oevre Serviceklasse","II"="II: Nedre Serviceklasse","IIIa"="IIIa: Rutinepraeget, ikke-manuelt arbejde hoejeste niveau","IIIb"="IIIb: Rutinepraeget, ikke-manuelt arbejde laveste niveau","Iva"="IVa: små selvstaendige med ansatte","Ivb"="IVb: små selvstaendige uden ansatte","Ivc"="IVc: Landmaend og andre selvstaendige i primaer produktion ","V"="V: Teknikere af laveste grad, supervisorer af manuelt arbejde","VI"="VI: Faglaerte, manulle arbejdere","VIIa"="VIIa: Ikke-faglaerte, manuelle arbejdere","VIIb"="VIIb: landbrugsarbejdere")



discodata$disco_1cifret

discodata$skillvl <-  recode_factor(discodata$disco_1cifret,
# '110'="		",
# '1'="",
'2'="4",
'3'="3",
'4'="2",
'5'="2",
'6'="2",
'7'="2",
'8'="2",
'9'="1",
.default=NA_character_)



#disco 4-cifret (dvs uden labels) #skulle muligvis impliceres i selve disco-variablen, men har ikke lyst til at fucke noget op, sÃ¥ bliver pÃ¥ det her niveau medmindre det er nÃ¸dvendigt.




# discodata$disco_4cifret <- as.character(discodata$disco)
# discodata$disco_4cifret[1] <- "0110: Militaert arbejde"
# discodata$disco_4cifret <- as.factor(discodata$disco_4cifret)
# # discodata$disco_4cifret


discodata$disco_4cifret        <- as.factor(as.numeric(strtrim(discodata$disco_s, 4)))
# view(discodata$disco_4cifret)

#disco 3-cifret (dvs uden labels) 
# discodata$disco_3cifret <- as.character(discodata$disco)
# discodata$disco_3cifret[1] <- "0110: Militaert arbejde"
# discodata$disco_3cifret <- as.factor(discodata$disco_3cifret)
# discodata$disco_3cifret
discodata$disco_3cifret        <- as.factor(as.numeric(strtrim(discodata$disco_s, 3)))
# view(discodata$disco_3cifret)

#disco 2-cifret (dvs uden labels) 
# discodata$disco_2cifret <- as.character(discodata$disco)
# discodata$disco_2cifret[1] <- "0110: Militaert arbejde"
# discodata$disco_2cifret <- as.factor(discodata$disco_2cifret)
# discodata$disco_2cifret
discodata$disco_2cifret        <- as.factor(as.numeric(strtrim(discodata$disco_s, 2)))
# view(discodata$disco_2cifret)
discodata$disco_2cifret[1] <- NA 




# levels(discodata$disco)
# view(discodata)

###################### nye variable #######################


#gns for alder 
discodata <- mutate(discodata,
                    ledighed.helepop.gns =  (ledighed.helepop1996 + ledighed.helepop1997 + ledighed.helepop1998 + ledighed.helepop1999 +  ledighed.helepop2000 +  ledighed.helepop2001 + ledighed.helepop2002 + ledighed.helepop2003 + ledighed.helepop2004 + ledighed.helepop2005 + ledighed.helepop2006 + ledighed.helepop2007 + ledighed.helepop2008 + ledighed.helepop2009)/14 )
discodata$ledighed.helepop.gns <-  round_to(discodata$ledighed.helepop.gns,2)



## koen
discodata <- mutate(discodata,
                    koen.gns.total =  (koen1996total + koen1997total + koen1998total + koen1999total +  koen2000total +  koen2001total + koen2002total + koen2003total + koen2004total + koen2005total + koen2006total + koen2007total + koen2008total + koen2009total)/14 )
discodata$koen.gns.total <- round_to(discodata$koen.gns.total)

discodata <- mutate(discodata,
                    koen.gns.kvinder =  (koen1996kvinder + koen1997kvinder + koen1998kvinder + koen1999kvinder +  koen2000kvinder +  koen2001kvinder + koen2002kvinder + koen2003kvinder + koen2004kvinder + koen2005kvinder + koen2006kvinder + koen2007kvinder + koen2008kvinder + koen2009kvinder)/14 )
discodata$koen.gns.kvinder <- round_to(discodata$koen.gns.kvinder)
#andele af kvinder 
discodata <-    discodata %>%
    mutate(koen.gns.kvinder.andel = koen.gns.kvinder/koen.gns.total )
discodata$koen.gns.kvinder.andel <- round_to(discodata$koen.gns.kvinder.andel,3)

#gns for alder 
discodata <- mutate(discodata,
                    alder.helepop.gns =  (alder.helepop1996 + alder.helepop1997 + alder.helepop1998 + alder.helepop1999 +  alder.helepop2000 +  alder.helepop2001 + alder.helepop2002 + alder.helepop2003 + alder.helepop2004 + alder.helepop2005 + alder.helepop2006 + alder.helepop2007 + alder.helepop2008 + alder.helepop2009)/14 )
discodata$alder.helepop.gns <-  round_to(discodata$alder.helepop.gns,1)


# gns for timelon, korregeret for inflationen (til 2010 niveau, sammenligning med DST)
discodata <- mutate(discodata, timelon.helepop2009.inf.dst=timelon.helepop2009*93.9/91.6)
discodata <- mutate(discodata, timelon.helepop2008.inf.dst=timelon.helepop2008*93.9/90.4)
discodata <- mutate(discodata, timelon.helepop2007.inf.dst=timelon.helepop2007*93.9/88)
discodata <- mutate(discodata, timelon.helepop2006.inf.dst=timelon.helepop2006*93.9/85.9)
discodata <- mutate(discodata, timelon.helepop2005.inf.dst=timelon.helepop2005*93.9/84.4)
discodata <- mutate(discodata, timelon.helepop2004.inf.dst=timelon.helepop2004*93.9/82.8)
discodata <- mutate(discodata, timelon.helepop2003.inf.dst=timelon.helepop2003*93.9/81.8)
discodata <- mutate(discodata, timelon.helepop2002.inf.dst=timelon.helepop2002*93.9/80.5)
discodata <- mutate(discodata, timelon.helepop2001.inf.dst=timelon.helepop2001*93.9/78.3)
discodata <- mutate(discodata, timelon.helepop2000.inf.dst=timelon.helepop2000*93.9/76.9)
discodata <- mutate(discodata, timelon.helepop1999.inf.dst=timelon.helepop1999*93.9/74.9)
discodata <- mutate(discodata, timelon.helepop1998.inf.dst=timelon.helepop1998*93.9/72.8)
discodata <- mutate(discodata, timelon.helepop1997.inf.dst=timelon.helepop1997*93.9/71.6)
discodata <- mutate(discodata, timelon.helepop1996.inf.dst=timelon.helepop1996*93.9/70.1)

discodata <- mutate(discodata,
                    timelon.helepop.gns.inf.dst =  (timelon.helepop1996.inf.dst + timelon.helepop1997.inf.dst + timelon.helepop1998.inf.dst + timelon.helepop1999.inf.dst +  timelon.helepop2000.inf.dst +  timelon.helepop2001.inf.dst + timelon.helepop2002.inf.dst + timelon.helepop2003.inf.dst + timelon.helepop2004.inf.dst + timelon.helepop2005.inf.dst + timelon.helepop2006.inf.dst + timelon.helepop2007.inf.dst + timelon.helepop2008.inf.dst + timelon.helepop2009.inf.dst)/14 )


# gns for timelon, korregeret for inflationen til 2009 niveau 


discodata <- mutate(discodata, timelon.helepop2008.inf=timelon.helepop2008*91.6/90.4)
discodata <- mutate(discodata, timelon.helepop2007.inf=timelon.helepop2007*91.6/88)
discodata <- mutate(discodata, timelon.helepop2006.inf=timelon.helepop2006*91.6/85.9)
discodata <- mutate(discodata, timelon.helepop2005.inf=timelon.helepop2005*91.6/84.4)
discodata <- mutate(discodata, timelon.helepop2004.inf=timelon.helepop2004*91.6/82.8)
discodata <- mutate(discodata, timelon.helepop2003.inf=timelon.helepop2003*91.6/81.8)
discodata <- mutate(discodata, timelon.helepop2002.inf=timelon.helepop2002*91.6/80.5)
discodata <- mutate(discodata, timelon.helepop2001.inf=timelon.helepop2001*91.6/78.3)
discodata <- mutate(discodata, timelon.helepop2000.inf=timelon.helepop2000*91.6/76.9)
discodata <- mutate(discodata, timelon.helepop1999.inf=timelon.helepop1999*91.6/74.9)
discodata <- mutate(discodata, timelon.helepop1998.inf=timelon.helepop1998*91.6/72.8)
discodata <- mutate(discodata, timelon.helepop1997.inf=timelon.helepop1997*91.6/71.6)
discodata <- mutate(discodata, timelon.helepop1996.inf=timelon.helepop1996*91.6/70.1)

discodata <- mutate(discodata,
                    timelon.helepop.gns.inf =  (timelon.helepop1996.inf + timelon.helepop1997.inf + timelon.helepop1998.inf + timelon.helepop1999.inf +  timelon.helepop2000.inf +  timelon.helepop2001.inf + timelon.helepop2002.inf + timelon.helepop2003.inf + timelon.helepop2004.inf + timelon.helepop2005.inf + timelon.helepop2006.inf + timelon.helepop2007.inf + timelon.helepop2008.inf + timelon.helepop2009)/14 )

#månedsløn 

discodata <- mutate(discodata,timelon.helepop.gns.inf.mndr=timelon.helepop.gns.inf*160.33)




# test  <- select(discodata,disco,timelon.helepop.gns.inf,timelon.helepop.gns.test,timelon.helepop2009)
# view(test)




# # gns for joblon
# discodata <- mutate(discodata,
#                     joblon.helepop.gns =  (joblon.helepop1996 + joblon.helepop1997 + joblon.helepop1998 + joblon.helepop1999 +  joblon.helepop2000 +  joblon.helepop2001 + joblon.helepop2002 + joblon.helepop2003 + joblon.helepop2004 + joblon.helepop2005 + joblon.helepop2006 + joblon.helepop2007 + joblon.helepop2008 + joblon.helepop2009)/14 )

# # gns for perindkialt
# discodata <- mutate(discodata,
#                     perindkialt.helepop.gns =  (perindkialt.helepop1996 + perindkialt.helepop1997 + perindkialt.helepop1998 + perindkialt.helepop1999 +  perindkialt.helepop2000 +  perindkialt.helepop2001 + perindkialt.helepop2002 + perindkialt.helepop2003 + perindkialt.helepop2004 + perindkialt.helepop2005 + perindkialt.helepop2006 + perindkialt.helepop2007 + perindkialt.helepop2008 + perindkialt.helepop2009)/14 )


# # gns for loenmv
# discodata <- mutate(discodata,
#                     loenmv.helepop.gns =  (loenmv.helepop1996 + loenmv.helepop1997 + loenmv.helepop1998 + loenmv.helepop1999 +  loenmv.helepop2000 +  loenmv.helepop2001 + loenmv.helepop2002 + loenmv.helepop2003 + loenmv.helepop2004 + loenmv.helepop2005 + loenmv.helepop2006 + loenmv.helepop2007 + loenmv.helepop2008 + loenmv.helepop2009)/14 )

# # gns for dispon_ny
# discodata <- mutate(discodata,
#                     disponny.helepop.gns =  (disponny.helepop1996 + disponny.helepop1997 + disponny.helepop1998 + disponny.helepop1999 +  disponny.helepop2000 +  disponny.helepop2001 + disponny.helepop2002 + disponny.helepop2003 + disponny.helepop2004 + disponny.helepop2005 + disponny.helepop2006 + disponny.helepop2007 + disponny.helepop2008 + disponny.helepop2009)/14 )

# gns for loenmv (ledige)
# discodata <- mutate(discodata,
#                     loenmv.gns =  (loenmv1996 + loenmv1997 + loenmv1998 + loenmv1999 +  loenmv2000 +  loenmv2001 + loenmv2002 + loenmv2003 + loenmv2004 + loenmv2005 + loenmv2006 + loenmv2007 + loenmv2008 + loenmv2009)/14 )













#############################################################################
################# GAMMELT FRA ARBEJDSLØSE ###################################
#############################################################################


#########################
### gns længde på ledighedsperiode #####
#########################

# ledperiod.gns.lngde <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/sociosocstil/baggrundsvar/ledsoc_tael_gnslngd_spell_kat150__socstilsocio_version1.xlsx")
# lst = readWorksheet(ledperiod.gns.lngde, sheet = getSheets(ledperiod.gns.lngde))
# # view(lst[[1]])
# ledperiod.gns.lngde <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# # view(ledperiod.gns.lngde)
# columns <- c(1,4,13,22,31,40,49,58,67,76,85,94,103,112,121) #, 26, 30, 34, 38, 42, 46, 50, 54)
# ledperiod.gns.lngde <- ledperiod.gns.lngde[,c(columns)]
# colnames(ledperiod.gns.lngde) <- label_moneca_[]
# #l1_r            <- nrow(ledperiod.gns.lngde)
# #l1_r
# ledperiod.gns.lngde <- sapply(ledperiod.gns.lngde, as.numeric)
# #View(ledperiod.gns.lngde)
# moneca.labels.num <- as.vector(ledperiod.gns.lngde[, 1])
# #View(moneca.labels.num)
# ledperiod.gns.lngde           <- as.matrix(ledperiod.gns.lngde[, -1]) 

# ledperiod.gns.lngde           <- rbind(ledperiod.gns.lngde, (colSums(ledperiod.gns.lngde)/nrowputexcel))

# rownames(ledperiod.gns.lngde) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen
# #nrow(ledperiod.gns.lngde)

# #dplyr conversion
# label_moneca_ledperiod.gns.lngde   <- list("ledperiod.gns.lngde1996" ,"ledperiod.gns.lngde1997" , "ledperiod.gns.lngde1998" , "ledperiod.gns.lngde1999" , "ledperiod.gns.lngde2000" , "ledperiod.gns.lngde2001", "ledperiod.gns.lngde2002" , "ledperiod.gns.lngde2003" , "ledperiod.gns.lngde2004" , "ledperiod.gns.lngde2005", "ledperiod.gns.lngde2006",  "ledperiod.gns.lngde2007",  "ledperiod.gns.lngde2008",  "ledperiod.gns.lngde2009")

# ledperiod.gns.lngde.tmp1            <- disco.df(ledperiod.gns.lngde, label_moneca_ledperiod.gns.lngde)

# ledperiod.gns.lngde     <- ledperiod.gns.lngde.tmp1[-nrowtab2xl,]

# # view(ledperiod.gns.lngde)

# #########################
# ### gns antal ledighedsperioder #####
# #########################

# ledperiod.antal <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/sociosocstil/baggrundsvar/ledsoc_tael_antal_kat150__socstilsocio_version1.xlsx")
# lst = readWorksheet(ledperiod.antal, sheet = getSheets(ledperiod.antal))
# # view(lst[[1]])
# ledperiod.antal <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# columns <- c(1,4,13,22,31,40,49,58,67,76,85,94,103,112,121) #, 26, 30, 34, 38, 42, 46, 50, 54)
# ledperiod.antal <- ledperiod.antal[,c(columns)]
# colnames(ledperiod.antal) <- label_moneca_[]
# # view(ledperiod.antal)
# #l1_r            <- nrow(ledperiod.antal)
# #l1_r
# ledperiod.antal <- sapply(ledperiod.antal, as.numeric)
# #View(ledperiod.antal)
# moneca.labels.num <- as.vector(ledperiod.antal[, 1])
# #View(moneca.labels.num)
# ledperiod.antal           <- as.matrix(ledperiod.antal[, -1]) 

# ledperiod.antal           <- rbind(ledperiod.antal, (colSums(ledperiod.antal)/nrowputexcel))

# rownames(ledperiod.antal) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen
# #nrow(ledperiod.antal)

# #dplyr conversion
# label_moneca_ledperiod.antal   <- list("ledperiod.antal1996" ,"ledperiod.antal1997" , "ledperiod.antal1998" , "ledperiod.antal1999" , "ledperiod.antal2000" , "ledperiod.antal2001", "ledperiod.antal2002" , "ledperiod.antal2003" , "ledperiod.antal2004" , "ledperiod.antal2005", "ledperiod.antal2006",  "ledperiod.antal2007",  "ledperiod.antal2008",  "ledperiod.antal2009")

# ledperiod.antal.tmp1            <- disco.df(ledperiod.antal, label_moneca_ledperiod.antal)

# ledperiod.antal     <- ledperiod.antal.tmp1[-nrowtab2xl,]

# # view(ledperiod.antal)

# ################################
# ### køn ########################
# ################################

# # koen <- NULL 
# koen <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/sociosocstil/baggrundsvar/koen_kat150__socstilsocio_version1.xlsx")
# lst = readWorksheet(koen, sheet = getSheets(koen))
# koen <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# columns <- c(-3,-5,-7, -9,-11,-13,-15,-17,-19,-21,-23,-25,-27,-29,-31,-33,-35,-37,-39,-41,-43,-45,-47,-49,-51,-53,-55) 
# koen <- koen[,c(columns)]
# label_moneca_koen <- c("moneca_label","koen1996total", "koen1996kvinder","koen1997total", "koen1997kvinder","koen1998total", "koen1998kvinder","koen1999total", "koen1999kvinder","koen2000total","koen2000kvinder","koen2001total","koen2001kvinder","koen2002total","koen2002kvinder","koen2003total","koen2003kvinder","koen2004total","koen2004kvinder","koen2005total","koen2005kvinder","koen2006total","koen2006kvinder","koen2007total","koen2007kvinder","koen2008total","koen2008kvinder","koen2009total","koen2009kvinder")
# colnames(koen) <- label_moneca_koen[]
# # view(koen)
# koen <- sapply(koen, as.numeric)
# # view(koen)
# #nrow(koen)
# moneca.labels.num <- as.vector(koen[, 1])
# # view(moneca.labels.num)
# koen           <- as.matrix(koen[, -1]) 
# # view(koen)
# koen           <- rbind(koen, colSums(koen)) #her laves ny række med det gennemsnitlige antal indenfor hver kolonne. For some reason?!
# rownames(koen) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen
# # view(koen)
# koen <- koen[-144,]
# # view(koen)
# koen <- as.data.frame(koen)
# koen$disco = rownames(koen)
# rownames(koen) = NULL
# koen <- tbl_df(koen)
# # koen <- NULL 
# # koen 
# # view(koen)
# # nrow(koen)


# # koen  <-  mutate(koen, koen.andel.test.1996 =  (koen1996kvinder/koen1996total))

# ### kønstest

# ## koen 
# # koen <- koen %>%
# #       mutate(koen2007kvinder = ifelse(
# #         is.na(koen2007kvinder)
# #         ,1,koen2007kvinder))
# # koen <- koen %>%
# #       mutate(koen2008kvinder = ifelse(
# #         is.na(koen2008kvinder)
# #         ,1,koen2008kvinder))
# # koen <- koen %>%
# #       mutate(koen2009kvinder = ifelse(
# #         is.na(koen2009kvinder)
# #         ,1,koen2009kvinder))
# # koen <- koen %>%
# #       mutate(koen1996kvinder = ifelse(
# #         is.na(koen2009kvinder)
# #         ,1,koen2009kvinder))


# koen[is.na(koen)] <- 0


# koen <- mutate(koen,
#                     koen.gns.total =  (koen1996total + koen1997total + koen1998total + koen1999total +  koen2000total +  koen2001total + koen2002total + koen2003total + koen2004total + koen2005total + koen2006total + koen2007total + koen2008total + koen2009total)/14 )
# # koen$koen.gns.total <- round(koen$koen.gns.total)
# koen <- mutate(koen,
#                     koen.gns.kvinder =  (koen1996kvinder + koen1997kvinder + koen1998kvinder + koen1999kvinder +  koen2000kvinder +  koen2001kvinder + koen2002kvinder + koen2003kvinder + koen2004kvinder + koen2005kvinder + koen2006kvinder + koen2007kvinder + koen2008kvinder + koen2009kvinder)/14 )

# #andele af kvinder 
# koen <-    koen %>%
#     mutate(koen.gns.kvinder.andel = koen.gns.kvinder/koen.gns.total )



# # gns for ledighedsperiode
# discodata <- mutate(discodata,
#                     ledperiod.gns.lngde.gns =  (ledperiod.gns.lngde1996 + ledperiod.gns.lngde1997 + ledperiod.gns.lngde1998 + ledperiod.gns.lngde1999 +  ledperiod.gns.lngde2000 +  ledperiod.gns.lngde2001 + ledperiod.gns.lngde2002 + ledperiod.gns.lngde2003 + ledperiod.gns.lngde2004 + ledperiod.gns.lngde2005 + ledperiod.gns.lngde2006 + ledperiod.gns.lngde2007 + ledperiod.gns.lngde2008 + ledperiod.gns.lngde2009)/14 )

# # gns for ledighedsantal
# discodata <- mutate(discodata,
#                     ledperiod.antal.gns =  (ledperiod.antal1996 + ledperiod.antal1997 + ledperiod.antal1998 + ledperiod.antal1999 +  ledperiod.antal2000 +  ledperiod.antal2001 + ledperiod.antal2002 + ledperiod.antal2003 + ledperiod.antal2004 + ledperiod.antal2005 + ledperiod.antal2006 + ledperiod.antal2007 + ledperiod.antal2008 + ledperiod.antal2009)/14 )


# ## koen 
# discodata <- discodata %>%
#       mutate(koen2007kvinder = ifelse(
#         is.na(koen2007kvinder)
#         ,1,koen2007kvinder))
# discodata <- discodata %>%
#       mutate(koen2008kvinder = ifelse(
#         is.na(koen2008kvinder)
#         ,1,koen2008kvinder))
# discodata <- discodata %>%
#       mutate(koen2009kvinder = ifelse(
#         is.na(koen2009kvinder)
#         ,1,koen2009kvinder))
# discodata <- discodata %>%
#       mutate(koen1996kvinder = ifelse(
#         is.na(koen2009kvinder)
#         ,1,koen2009kvinder))


# discodata <- mutate(discodata,
#                     koen.gns.total =  (koen1996total + koen1997total + koen1998total + koen1999total +  koen2000total +  koen2001total + koen2002total + koen2003total + koen2004total + koen2005total + koen2006total + koen2007total + koen2008total + koen2009total)/14 )
# # discodata$koen.gns.total <- round(discodata$koen.gns.total)
# discodata <- mutate(discodata,
#                     koen.gns.kvinder =  (koen1996kvinder + koen1997kvinder + koen1998kvinder + koen1999kvinder +  koen2000kvinder +  koen2001kvinder + koen2002kvinder + koen2003kvinder + koen2004kvinder + koen2005kvinder + koen2006kvinder + koen2007kvinder + koen2008kvinder + koen2009kvinder)/14 )




# #andele af kvinder 
# discodata <-    discodata %>%
#     mutate(koen.gns.kvinder.andel = koen.gns.kvinder/koen.gns.total )


# ####### ligger de vigtigste variable først igen

# discodata$disco <- as.factor(discodata$disco)
# # is.factor(discodata$disco)

# # reorder variables sÃ¥ disco-variable ligger fÃ¸rst
# discodata <-  discodata %>%
#   select(disco,disco_1cifret,disco_4cifret, everything())

# # view(discodata)


# ## afrundninger

# discodata$loenmv.gns <- round(discodata$loenmv.gns)
# discodata$loenmv.helepop.gns <-  round(discodata$loenmv.helepop.gns)












