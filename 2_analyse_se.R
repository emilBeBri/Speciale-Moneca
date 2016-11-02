# Analyser

######################################################################
# Analyse defaults

# save.image("./statistik/R/moneca/vores/voresdata/tmp_nyeste")
# load("./statistik/R/moneca/vores/voresdata/tmp_nyeste")
cut.off.default          <- 1
mode.default             <- 1
small.cell.default       <- 5
# help(anton)

set.controls.for.the.heart.of.the.sun <- 4 #her betyder 6 *OVER* det første niveau, dvs 7 i alt. 
seg           <- anton(mob.mat, cut.off = cut.off.default, small.cell.reduction = small.cell.default, segment.levels = set.controls.for.the.heart.of.the.sun)
seg.bak  <- seg

# alle beskaeftigede 250 kat
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_250kat.Rdata")
load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_250kat.Rdata")


########## segment ændringer 



# split segment 4.8 op, der har en max path på 4 som det eneste, og sammenlægningen giver desuden ikke mening.
seg$segment.list[[4]][[8]] <- NULL

#segment 4.2, 4.10 og  består af uens elementer 



bab <- seg$segment.list[[4]][[8]]
 view(discodata[bab,])

############



# alle beskaeftigede 144 kat cutoff=0!
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_cutoff0.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_cutoff0.Rdata")



# alle beskaeftigede 144 kat d. 22/08/2016
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede.Rdata")

##socio/socstil
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version2.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version2.Rdata")


# alle beskaeftigede OG alle ledige 
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_og_ledige.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftigede_og_ledige.Rdata")


############## Fra 2015 ############
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1.Rdata")

##socio/socstil - kvinder
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1_kvinder.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1_kvinder.Rdata")

##socio/socstil - maend
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1_maend_5.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1_maend_5.Rdata") #4
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocio_version1_maend.Rdata") #5



##socstil netto
#save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstil_netto.Rdata")
#load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstil_netto.Rdata")

##socstil/socio/ledfuld/leddel/6uger
#save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocioledfuldleddel6uger.Rdata")
#load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_socstilsocioledfuldleddel6uger.Rdata")

##socstil/socio/ledfuld/leddel/6uger
#save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftige.Rdata")
#load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_allebeskaeftige.Rdata")

##minium 2 perioders ledighed
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_minimum2perioder.Rdata")
#load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_minimum2perioder.Rdata")

##minium 2 aars ledighed
# save(seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_minimum2perioder.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_seg_minimum2perioder.Rdata")



#all.equal(colnames(mob.mat), rownames(mob.mat)) #trick til at teste om ting er ens, smart

#seg.b         <- anton(mob.mat.b, cut.off = cut.off.default, small.cell.reduction = small.cell.default, segment.levels = 3)
#g? ind i anton-funktionen og jonas funktion p? et tidspunkt.
#help(anton)

#mat1 <- seg$mat.list[1]
#write.table(mat1, file="./statistik/R/moneca/vores/output/test/mat1.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
#mat2 <- seg$mat.list[2]
#write.table(mat2, file="./statistik/R/moneca/vores/output/test/mat2.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)

#seg-objektet er en liste med tre lister i sig:
# seg$segment.list  #liste med liste over segmenterne
# seg$segment.list[[]] #her det 3. niveau (dvs det 4. fordi der ogs? er det oprindelige)
# seg$segment.list[[5]][[1]]  # s?dan her refererer man til den 4. liste i den 4. liste i segment.list
# view(seg$mat.list[1]) #en liste med en matrice per niveau af moneca. Forstår ikke helt hvorfor. 
# #seg$small.cell.reduction #vektor med et tal, nemlig hvor meget cellerne er reduceret.


# #snydeløsning på socio/socstil segment
# seg$segment.list[6] <- seg$segment.list[7]
# seg$segment.list[7] <- NULL
# seg$mat.list[6] <- seg$mat.list[7]
# seg$mat.list[7] <- NULL



# Ændringer - har forstået *hvordan* han gør det, men *hvorfor* g?r han det? #sp?rganton
#seg$segment.list[[4]][[3]]   <- setdiff(seg$segment.list[[4]][[3]], c(1, 86, 149))
#seg$segment.list[[3]][[16]]  <- setdiff(seg$segment.list[[3]][[16]], c(1, 86, 149))
#seg$segment.list[[4]][[4]]   <- NULL
#forst?r stadig ikke hvorfor han g?r det - men jeg forst?r hvordan. eksempel, fra https://www.safaribooksonline.com/library/view/the-r-book/9780470510247/ch002-sec073.html 
#setA<-c("a", "b", "c", "d", "e")
#setB<-c("d", "e", "f", "g")
#setA
#setB
# dvs det man f?r tilbage er de s?rlige elementer der kun findes i x og ikke i y, her det der findes i setA og ikke i setB
#setdiff(setA,setB)
#her er x og y vendt om ,dvs vi f?r det s?rlige, der kun findes i y - setB - med
#setdiff(setB,setA)
#dvs det anton g?r er at fjerne disse 3 grupper fra niveau 4
#setdiff(seg$segment.list[[4]][[3]], c(1, 86, 149))
#seg$segment.list[[4]][[3]]
# og bagefter fjerne dem fra niveau 3
#setdiff(seg$segment.list[[3]][[16]], c(1, 86, 149))
#seg$segment.list[[3]][[16]]
#og eliminere segment 4 fra niveau 4 af gud ved hvilken grund
#seg$segment.list[[4]][[4]]


seg.b         <- seg

seg.mem       <- segment.membership(seg) 
#moneca-funktion. iflg beskrivelsen: "A dataframe with the segment membership for each category". Men der er da flere niveauer, hvilket niveau er der tale om? Det sidste niveau, dvs. det "det laveste niveau" de kunne danne klike p?? #sp?rganton 
#View(seg.mem)
#write.table(seg.mem, file="./statistik/R/moneca/vores/output/seg.mem.csv", sep = ";", fileEncoding = "UTF-8") #output
#segment.membership
#help(segment.membership)


###############
# Bryd dem op:


######################################################################
# Beskrivende
#qual.list       <- lapply(seg.list, segment.quality)
#qual.final.list <- lapply(seg.list, segment.quality, final.solution = TRUE)
seg.qual        <- segment.quality(seg)
seg.qual.final  <- segment.quality(seg, final.solution = TRUE)
#View(seg.qual.final)
#write.table(seg.qual.final, file="./statistik/R/moneca/vores/output/seg.qual.final.csv", sep = ";", fileEncoding = "UTF-8") #output


#kan f?rst laves rigtigt n?r vi har analyseret segments #todoiR
#segment.labels <- read.csv("./statistik/R/moneca/vores/Data/Segment_labels_DK.csv", sep = ";", encoding = "latin1")$X #kan ikke laves endnu, mangler analyse
seg.lab        <- seg.mem$membership
#seg.lab
#levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])
#seg.lab        <- format(as.character(seg.lab))
#cat.lab        <- paste(as.character(seg.mem$membership), " . ", rownames(mob.mat), sep = "")


######################################################################
# New variables

######################################################################
# 
nabo.out          <- neighborhood.share.of(seg, stor/sum(stor), small.cell.reduction = 5, mode = "out")
nabo.in           <- neighborhood.share.of(seg, stor/sum(stor), small.cell.reduction = 5, mode = "in")
nabo.sym          <- neighborhood.share.of(seg, stor/sum(stor), small.cell.reduction = 5, mode = "all", symmetric = TRUE)
nabo.dif          <- nabo.in - nabo.out

#### Tema data paa segment niveau

# juli 2016 warning message på 351 kat version
diag.stor.seg                    <- aggregate.membership(diag.stor, seg.mem[, 2]) 
#seg.mem[, 2]
#diag.stor.seg
#breakdown af aggregate.membership - jeg forst?r ikke rigtig dens funktion. hvis man sammenligner seg.mem med diag.stor.seg, s? g?r den fra en dataframe til en named number liste, og det er selvf?lgelig noget. M?ske er det bare det.
#str(diag.stor.seg)
#str(seg.mem)

#aggregate.membership <- function(x, y){
#  yy                    <- y
#  levels(yy)            <- aggregate(x = x, by = list(y), sum)[,2]
#  yy                    <- as.numeric(as.character(yy))
#  names(yy)             <- y
#  yy
#}
#write.table(seg.mem, file="./statistik/R/moneca/vores/output_emil/seg.mem.csv", sep = ";", fileEncoding = "UTF-8")
#write.table(diag.stor.seg, file="./statistik/R/moneca/vores/output_emil/diag.stor.seg.csv", sep = ";", fileEncoding = "UTF-8")

#aggregate.membership <- function(x, y){
#  yy                    <- seg.mem[, 2] 
#  levels(yy)            <- aggregate(x = diag.stor, by = list(seg.mem[, 2]), sum)[,2]  
  #help(levels)
  #breakdown af ovenst?ende: Den summerer antallet af personer i hver segment og g?r det til levels af seg.mem$membership
  #str(seg.mem$membership)
  #head(seg.mem$membership)
  #ag.mem.skridt1            <- aggregate(x = diag.stor, by = list(seg.mem[, 2]), sum)[,2]  
  #write.table(ag.mem.skridt1, file="./statistik/R/moneca/vores/output_emil/ag.mem.skridt1.csv", sep = ";", fileEncoding = "UTF-8")  
#  yy                    <- as.numeric(as.character(yy))
  #breakdown af ovenst?ende
  #yy1 = as.character(yy)
  #yy1
#  yy
#}

# juli 2016 warning message på 351 kat version
stor.mob.seg                     <- aggregate.membership(stor.mob, seg.mem[, 2]) #giver antallet af alle mobile indenfor de forskellige kategoriers samlede segment - dvs summerer deres segment-grupper (ingen dokumentation, fra 1_data-filen)
#stor.mob.seg

# intern mobilitet på segment-niveau. lavet af Anton, men den er forkert - han regner det rigtigt i within.mobility-funktionen i segment.quality-funktionen, men problemet er her at den kun lægger den interne mobilitet sammen for de interne mobiliter indenfor hver kategori - men hvis de to kategorier lægges sammen, så skal mobiliteten mellem de to jo også tælle med i den interne mobilitet. Det er forklaringen på at desto større segmenter, desto større er forskellen på dette mål og within.mobility målet, fordi denne her jo kun tæller den diagonalernes mobilitet. HA! Jeg klarede det. Goddamn. 
# intern.mobilitet.seg             <- diag.stor.seg / stor.mob.seg 


# juli 2016 warning message på 351 kat version
beskaeftigede.seg                 <- apply(beskaeftigede.smooth[-l, ], 2, aggregate.membership, y = seg.mem[, 2])  #fjerner column totals fra beskaeftigede.smooth, applier aggregate.membership-funktionen p? columns i beskaeftigede.smooth, og s?tter y til at v?re column 2 (ud af to) i seg.mem, dvs den column der indeholder segment.membership, og som bruges af aggregate.membership.
#View(beskaeftigede.seg)
#beskaeftigede.seg

beskaeftigede.andel.seg           <- t(t(beskaeftigede.seg) / c(beskaeftigede.smooth[l,])) #Det giver nogle absurd lave v?rdier, fordi beskaeftigede.seg ikke indeholder v?rdier, men bare segment-niveauer, nu som v?rdier i matricen. Det tror jeg virkelig ikke er meningen. #sp?rganton #l?st p? s?rens pc, tjek op p? om det virker p? din egen
#View(beskaeftigede.andel.seg)

#samme historie som ovenst?ende.
beskaeftigede.seg.unsmooth        <- apply(beskaeftigede[-l, ], 2, aggregate.membership, y = seg.mem[, 2])
#beskaeftigede.seg.unsmooth
#beskaeftigede.andel.seg.unsmooh   <- t(t(beskaeftigede.seg.unsmooth) / c(beskaeftigede[l,]))
beskaeftigede.andel.seg.unsmooth   <- t(t(beskaeftigede.seg.unsmooth) / c(beskaeftigede[l,])) #stavefejl i koden som den stod ovenfor




#det her har jeg ikke styr p? endnu #todoiR
ind                               <- duplicated(seg.lab) == FALSE
beskaeftigede.seg.u                <- beskaeftigede.seg[ind,]
beskaeftigede.seg.unsmooth.u       <- beskaeftigede.seg.unsmooth[ind,]


label.u                           <- seg.lab[ind]

