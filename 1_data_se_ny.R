###################################################
# data bearbejdelse

###################################################
# Data


allbeskaeft <- data.frame(matrix(unlist(allbeskaeft), nrow=274),stringsAsFactors=FALSE)
colbesk <- c(1, 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54)
allbeskaeft <- allbeskaeft[,c(colbesk)]
label_moneca_   <- list("moneca_label", "1996" ,"1997" , "1998" , "1999" , "2000" , "2001", "2002" , "2003" , "2004" , "2005", "2006",  "2007",  "2008",  "2009")
colnames(allbeskaeft) <- label_moneca_[]
l1_r            <- nrow(allbeskaeft)
allbeskaeft[l1_r,1]            <- c(9999)
allbeskaeft <- sapply(allbeskaeft, as.numeric)
# view(allbeskaeft)




############## laver mot.mat uden excel-beregninger #### 

mob.mat <- as.matrix(mob.mat)
mob.mat  <- mob.mat[,-1]


backup_allbeskaeft <- allbeskaeft
allbeskaeft <- backup_allbeskaeft


backup_allbeskaeft <- allbeskaeft
allbeskaeft <- backup_allbeskaeft



#til 1996-2002 version
# allbeskaeft <- allbeskaeft[,1:8]
#til 2003-2009 version
# allbeskaeft <- allbeskaeft[,9:15]

totalbeskaeft_tmp           <- allbeskaeft[,-1]

totalbeskaeft1997.2009           <- rowSums(totalbeskaeft_tmp[,-1])
totalbeskaeft1997.2009           <-  totalbeskaeft1997.2009[-l1_r]
totalbeskaeft1996.2008           <- rowSums(totalbeskaeft_tmp[,-14])
#for både 1996-2002 OG 2003-2009 version
# totalbeskaeft1996.2008           <- rowSums(totalbeskaeft_tmp[,-7])
totalbeskaeft1996.2008           <-  totalbeskaeft1996.2008[-l1_r]



# UDEN sidste år, 2009, med 
totalbeskaeft_medsums           <- cbind(totalbeskaeft_tmp,rowSums(totalbeskaeft_tmp[,-14]))
#for både 1996-2002 OG 2003-2009 version 
# totalbeskaeft_medsums           <- cbind(totalbeskaeft_tmp,rowSums(totalbeskaeft_tmp[,-7]))


totalbeskaeft_medsums           <- cbind(totalbeskaeft_medsums,rowSums(totalbeskaeft_tmp[,-1]))

colnames(totalbeskaeft_medsums)[15] <- c("1996-2008")
colnames(totalbeskaeft_medsums)[16] <- c("1997-2009")
#for både 1996-2002 OG 2003-2009 version 
# colnames(totalbeskaeft_medsums)[8] <- c("1996-2008")
# colnames(totalbeskaeft_medsums)[9] <- c("1997-2009")
# view(totalbeskaeft_medsums)
#View(totalbeskaeft1997.2009)
#View(totalbeskaeft1996.2008)
#write.table(totalbeskaeft_medsums, file="./statistik/R/moneca/vores/output/totalbeskaeft_medsums_jonasanton.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)

#mob.mat.row           <- rbind(mob.mat, totalbeskaeft1997.2009)
#write.table(mob.mat.row, file="./statistik/R/moneca/vores/output/mob.mat.row.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
#View(mob.mat)
#mob.mat.col           <- cbind(mob.mat, totalbeskaeft1996.2008)
#write.table(mob.mat.col, file="./statistik/R/moneca/vores/output/mob.mat.col.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)

# save.image("./statistik/R/moneca/vores/voresdata/tmp_nyeste")
 # load("./statistik/R/moneca/vores/voresdata/tmp_nyeste")

mob.mat         <- rbind(mob.mat, totalbeskaeft1997.2009)


############################################################
# totalbeskaeft1996.2008[144] <- 0 # VIGTIGT KUN FOR 144 kat versionen!!!###############################################


# Her januar 2016: problemer med at cbind og rbind ikke længere vil 
# Her juli 2016: problemer med at cbind her siger noget med at number of rows of rsulut is not a multiple of vector length (arg2). men når man viewer mob.mat ser det fint ud. stay vigilant!
mob.mat          <- cbind(mob.mat, totalbeskaeft1996.2008)
# view(mob.mat)

mob.mat[nrow(mob.mat),ncol(mob.mat)]         <- ((sum(totalbeskaeft1997.2009)+sum(totalbeskaeft1996.2008))/2)                                                                                   
mob.mat[nrow(mob.mat),ncol(mob.mat)] #(tjek om the grand total er korrekt, det er det vigtigste tal i moneca)
# det er 30.394.390 i kat350 versionen.

l            <- ncol(mob.mat) # displays no. of columns #gir os antal kolonner og l?gger den som en vektor med et tal
#label        <- strtrim(rownames(mob.mat), 40) #fjerner alle characters udover 40 (leftover fra antons version)
#view(label)

dimnames(mob.mat) <- list(label, label) # s?tter labels b?de p? rows og columns p? mob.mat matricen

#view(mob.mat)
# View(label)

#View(label)
#colnames(mob.mat)
#View(mob.mat[,125:130])



#mob.mat.allemobile <- mob.mat



#mob.mat <- mob.mat + mob.mat.allemobile

# moneca krydstabel
#write.table(mob.mat, file="./statistik/R/moneca/vores/output/mob.mat.allemobile.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
#write.table(mob.mat, file="./statistik/R/moneca/vores/output/csv/mob.mat.ledige.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
#write.table(mob.mat, file="./statistik/R/moneca/vores/output/mob.mat.allemobile.jonasanton.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)


####################################################
# Periode - skal eyeballes og inddeles s?dan som vi skal bruge det! #todoiR
periode                    <- 1996:2009
smooth.periode.1996.2001   <- 1996:2001
smooth.periode.2001.2009   <- 2001:2009
aar                         <- 1996:2009

########################################################
# Tema data

beskaeftigede           <- as.matrix(allbeskaeft[-l1_r, -1]) 
beskaeftigede           <- rbind(beskaeftigede, colSums(beskaeftigede)) #summer over columns, laver ny row med dem (det er hvad rbind g?r)
#View(beskaeftigede)
rownames(beskaeftigede) <- label[] #tager label-objektet og s?tter labels på fra det

beskaeftigede.andel     <- t(t(beskaeftigede) / c(beskaeftigede[l,])) #transposer matricen og bruger totalen per år til at vurdere det samlede antal. 
#View(beskaeftigede.andel)
# beskaeftigede.smooth                                        <- smoothie(beskaeftigede)


#breakdown af smoothie-funktionen med besk?ftigelsesdata
#smoothie   <- function(x, negative = FALSE, max.value = FALSE){
#  xm         <- melt(beskaeftigede) #"smelter" data så man får en liste med alle variabel-kombinationer
#  sxm        <- split(xm, f = xm$Var1) #laver liste med hver sin (matrice? dataframe?) af udfaldene i xm
#help(split)
#print(sxm$'9999: Total') 
#  lx         <- lapply(sxm, FUN = lm, formula = value ~ Var2)
#help(lapply) #apply a function over a list or vector - her, liste. Den afh?ngige variabel er antallet af besk?ftigede indenfor hver kategori. Dvs. skaber en simpel to-variabels-regressionsmodel for hver kategori
#print(lx$'9999: Total')
#print(lx) #en lm-formular for hver besk?ftigelseskategori.
#  lxemil <- sapply(lx, predict)
#  lxemil 
#  px         <- t(lxemil)
#  px         <- t(sapply(lx, predict)) #Her g?res det i ét skridt
#  px
#help(predict) #predict forudsiger v?rdier for en model, dvs 
#help(sapply) #"sapply is a user-friendly version of lapply", her applicerer den bare predict på hver kategori på listen, dvs. besk?ftigelseskategorierne.
#  colnames(px) <- colnames(beskaeftigede) #applicerer columnnames fra oprindeligt objekt på den nye predictede version
#  if (negative == FALSE)  px[px < 0] <- 0.00000001 # option, s?rger for at s?tte predictede v?rdier under 0 til 0
#  if (max.value != FALSE)  px[px > max.value] <- max.value #et argumen sat til en v?rdi - t?nkt til at v?re 1 i forbindelse med andele, der naturligvis ikke skal overstige 1 
#  px
#}

# #erstatter de forudsagte v?rdier med regressioner for hver periode - antal
# beskaeftigede.smooth[, aar %in% smooth.periode.1996.2001]    <- smoothie(beskaeftigede[, aar %in% smooth.periode.1996.2001])
# beskaeftigede.smooth[, aar %in% smooth.periode.2001.2009]    <- smoothie(beskaeftigede[, aar %in% smooth.periode.2001.2009])
# #write.table(beskaeftigede.smooth, file="./statistik/R/moneca/vores/output_emil/beskaeftigede.smooth1.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)


# #erstatter de forudsagte v?rdier med regressioner for hver periode - andele - men hvorfor s?tter han kun max.value på den f?rste og ikke resten? #sp?rganton 
# beskaeftigede.andel.smooth                                        <- smoothie(beskaeftigede.andel, max.value = 1)
# beskaeftigede.andel.smooth[, aar %in% smooth.periode.1996.2001]    <- smoothie(beskaeftigede.andel[, aar %in% smooth.periode.1996.2001], max.value = 1) 
# beskaeftigede.andel.smooth[, aar %in% smooth.periode.2001.2009]    <- smoothie(beskaeftigede.andel[, aar %in% smooth.periode.2001.2009], max.value = 1)


#uklart hvad der sker her #sp?rganton. han ender med en smoothede kategorier uden decimaler, der ligger semit?t p? den tidligere smoothede version
# beskaeftigede.smooth             <- beskaeftigede.smooth[, aar %in% periode]
# beskaeftigede.smooth             <- round(beskaeftigede.smooth)
#View(beskaeftigede.smooth)
#write.table(beskaeftigede, file="./statistik/R/moneca/vores/output_emil/beskaeftigede.smooth2.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
beskaeftigede                    <- beskaeftigede[, aar %in% periode]
beskaeftigede.andel              <- beskaeftigede.andel[, aar %in% periode]
# beskaeftigede.andel.smooth       <- beskaeftigede.andel.smooth[, aar %in% periode]


###################### mobilitetsberegninger ################################      
#############################################################################

# St?rrelse af mobilitet
row.stor          <- mob.mat[l,-l] #tager den 156 row (row total), men fjerner den sidste column i denne, som er column total. 
# view(row.stor)
col.stor          <- mob.mat[-l, l] #Det g?res også for row bare omvendt
stor              <- (row.stor + col.stor) / 2 # der tages et gennemsnit af de to
diag.stor         <- diag(mob.mat)[-l] #diagonalen tages og ligges som selvst?ndig "named integer" undtagen totalen, der findes i den sidste row som ikke tages med (pga -l i indekseringen
mob.mat.l         <- mob.mat[-l, -l] #total column og row barberes af
stor.mob          <- ((rowSums(mob.mat.l) + colSums(mob.mat.l)) / 2) #giver den gennmemsnitlige sum af antal mobile, hvor stor-objektet giver den gennemsnitlige sum af alle besk?ftigede

# view(mob.mat.l)
# view(rowSums(mob.mat.l))

# view(stor.mob)


# intern.mobilitet  <- diag.stor/stor # Det her er ikke den interne mobilitet. Det må v?re hvor mange procent internt mobile udg?r af hele klassen - altså inklusive de immobile. (Antons note - E)
intern.mobilitet  <- diag.stor/stor.mob #andel af den interne mobilitet blandt de mobile (overskrider den ovenstående som er forkert ifht formålet)

andel.mobile      <- stor.mob/stor #mål for forskellen i mobilitet blandt de mobile og blandt alle besk?ftigede per år for den kategori
#View(intern.mobilitet)
# view(andel.mobile)

stor.mobilitet    <- stor #hov den her skal manipuleres f?rst 
#View(stor.mobilitet)

# St?rrelse besk?ftigede
#View(beskaeftigede)


stor.beskaeftigede <- beskaeftigede[-l, ncol(beskaeftigede)] 
#View(stor.beskaeftigede)
#View(beskaeftigede[-l, ncol(beskaeftigede)]) # tager tallet fra 2009? kan det passe?
#View(beskaeftigede)
stor.beskaeftigede.andel <- beskaeftigede.andel[-l, ncol(beskaeftigede.andel)]
# view(stor.beskaeftigede.andel)

stor.beskaeftigede.andel <- stor.beskaeftigede.andel*100



