





################################################################
######## aggreger til niveau 5 ########
################################################################


view(seg.df)

tmp <- seg.df %>% select(membership,klasse_begtrupbright2) 

tmp <-  unique(tmp$klasse_begtrupbright2)





#
mat.e <- mob.mat[-274,-274]
s.klasse_begtrupbright2 <- as.character(seg.df$klasse_begtrupbright2)
seg.within.mob.REAL  <- list()
for (tmem in s.klasse_begtrupbright2) {
tmp.klasse_begtrupbright2 <- tmem
work.list <-  sort(as.vector(unlist(discodata %>%    filter(klasse_begtrupbright2 == tmp.klasse_begtrupbright2 )  %>% select(indeks))))
tmp  <-   mat.e[work.list,work.list]
tmp <- sum(tmp)
seg.within.mob.REAL[tmp.klasse_begtrupbright2] <- tmp
}

seg.within.mob.REAL.1 <-  matrix(seg.within.mob.REAL)
seg.within.mob.REAL.1 <-  cbind(names(seg.within.mob.REAL),seg.within.mob.REAL.1)
# view(seg.within.mob.REAL.1)


mat.e <- mob.mat[-274,-274]


klynge.liste.samlet.indeks <- list()
for (name in sort(liste.segmenter)) {
   # print(myList[[name]]) 
klynge.liste.samlet.indeks[name]  <-  klynge.liste.niveau.5[name]
}




view(klynge.liste.samlet.indeks)


e.seg.niveau.5 <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(seq_len(273)), unname(klynge.liste.samlet.indeks)))
# map elements of second list item to values of first list item - NB bruges anderledes end i moneca. men måske brugbar.
# e.seg.niveau.5[match(group.list[["grp2"]], e.seg.niveau.5)] <- group.list[["grp1"]] 



















#  annotation 
annotation_col.df <- data.frame(somdetskalvaere$label)
colnames(annotation_col.df) <- c("membership")
annotation_col.df$membership <- strtrim(annotation_col.df$membership,5)



annotation_col.df$membership <- gsub(":", "", annotation_col.df$membership)
annotation_col.df$membership <- gsub(" ", "", annotation_col.df$membership)


annotation_col.df$klasse_begtrupbright1<-  rep(NA,nrow(annotation_col.df))
annotation_col.df$membership_lab<-  rep(NA,nrow(annotation_col.df))



annotation_col.df$klasse_begtrupbright1[which(annotation_col.df$membership %in% seg.df$membership)] <- seg.df$klasse_begtrupbright1[which(annotation_col.df$membership %in% seg.df$membership)]
annotation_col.df$klasse_begtrupbright1[1] <- c("Andre")

annotation_col.df$membership_lab[which(annotation_col.df$membership %in% seg.df$membership)] <- seg.df$membership_lab[which(annotation_col.df$membership %in% seg.df$membership)]
annotation_col.df$membership_lab[1] <- c("Andre")


annotation_col.df <- annotation_col.df %>% select(membership)

rownames(annotation_col.df) <- names(mat.e.result)



view(annotation_col.df)

view(mat.e.result)









######################################


tet <-  ma.motor.multi(klynger)



ma.motor.uni.df.test <- function(x,klasse_label=c("ikke_angivet"),discogrp=TRUE) {



klynger <- x


klynger <- manuelle.klynger.lav

klynger <- klynger[grep("^1.*", klynger, invert = TRUE)]


#
mat.e.result <- mob.mat[-274,-274]
mat.e.result <- mat.e
# etest(mat.e)

klynge.liste.samlet.indeks <- list()
for (name in sort(klynger)) {
klynge.liste.samlet.indeks[name]  <-  klynge.liste.niveau.5[name]
}
# klynge.liste.samlet.indeks
work.list.foer.sub <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks)))) #det samme, det ene som liste og det andet som vector 
sort(as.vector(unlist(klynge.liste.samlet.indeks)))==work.list.foer.sub # yes de skal være ens det er de 

# find  de disco der går 
# 1. fra segmenter af interesse
aug.work.list.foer.sub <- sort(unique(unlist(lapply(work.list.foer.sub, function(x) which(mat.e[x,] != 0)))))
# 2. til segmenter af interesse 
# aug.work.list.foer.sub <- sort(unique(unlist(lapply(work.list.foer.sub, function(x) which(mat.e[,x] != 0)))))


# liste til aggregering af disco 
klynge.liste.e.seg.manuel.til.fra <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(seq_len(273)), unname(klynge.liste.samlet.indeks)))


# det store aggregeringsnummer 
mat.e.result <- t(rowsum(t(rowsum(mat.e.result, klynge.liste.e.seg.manuel.til.fra)), klynge.liste.e.seg.manuel.til.fra))


# ncol(mat.e.result) 
# # view(mat.e.result)# her lagt sammen med den anden 
# etest(mat.e.result)
# view(mat.e.result)
# sum(mat.e.result)


# de her to skal være ens, ellers er noget galt: 
length(unique(which(diag(mat.e.result) == 0)))==length(which(diag(mat.e.result) == 0))
# view(mat.e.result)
# max(as.vector(mat.e.result))
# min(colSums(mat.e.result))
# etest(mat.e.result)
#view(mat.e.result)
# sum(mat.e.result)


# diagonaler til at identificere elementer 

diag.submatrix <- diag(mat.e.result)

diag.seg.df <- diag(e.mobmat.seg.niveau.5)

diag.discodata <- diag(mob.mat[-274,-274])

diag.seg.df.udisco <-diag.seg.df[1:41] #hvis denne her bruges i which-statemenetet i 2. linje i nedenstående, istedet for diag.seg.df så kommer enkelt-grupperne fra 5-niveau med. men der er er noget der skal ordnes først, ikke aktuelt nu. \#todo


seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
which(names(diag.seg.df)[which(diag.seg.df %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) 
# skal være sand for alle elementer 

# find  de disco der går fra nye seg-index 
work.list.efter.sub <- seg.index

# 1. fra segmenter af interesse
aug.work.list.efter.sub <- sort(unique(unlist(lapply(work.list.efter.sub, function(x) which(mat.e.result[x,] != 0)))))

# test
tmp1  <-  setdiff(aug.work.list.foer.sub,work.list.foer.sub) 
test.list.1 <-  as.character(discodata$disco[tmp1])
tmp1  <-  setdiff(aug.work.list.efter.sub,work.list.efter.sub) 
test.list.2 <-  as.character(discodata$disco[tmp1])
length(test.list.1)==length(test.list.2) # de her to skal være ens 


mat.e.result <- mat.e.result[aug.work.list.efter.sub, aug.work.list.efter.sub]

# diagonaler til at identificere elementer 

diag.submatrix <- diag(mat.e.result)
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) # skal være sand for alle elementer 


# find ud af hvilket indeks der passer med diverse segmenter etc 
ikke.seg.index.i.disco <-  as.numeric(names(diag.submatrix[which((diag.submatrix%in%diag.discodata))]))
ikke.seg.index.i.submatrix <-  which((diag.submatrix%in%diag.discodata))


colnames(mat.e.result)[ikke.seg.index.i.submatrix] <- as.character(discodata$disco[ikke.seg.index.i.disco])
rownames(mat.e.result)[ikke.seg.index.i.submatrix] <- as.character(discodata$disco[ikke.seg.index.i.disco])
colnames(mat.e.result)[ 
	which(diag.submatrix %in% diag.seg.df.udisco)] <-
				 names(diag.seg.df.udisco)[
                   which(diag.seg.df.udisco %in% diag.submatrix.seg)]
rownames(mat.e.result)[ 
	which(diag.submatrix %in% diag.seg.df.udisco)] <-
				 names(diag.seg.df.udisco)[
                   which(diag.seg.df.udisco %in% diag.submatrix.seg)]
ncol(mat.e.result)				
# view(mat.e.result)
# max(as.vector(mat.e.result))

tmp.order  <-  append(colnames(mat.e.result) [seg.index] , colnames(mat.e.result)[ikke.seg.index.i.submatrix ] )
korrekt <-  match(tmp.order,colnames(mat.e.result))
mat.e.result <-  mat.e.result[korrekt, korrekt]
# head(colnames(mat.e.result)) # yes (gør ikke noget der også er nogle discogrupper med, det vigtige er at der er er de segmenter der skal være)



# ncol(mat.e.result)
# view(mat.e.result) #og her, det er desuden korrekte dimnames
# max(as.vector(mat.e.result))
# min(colSums(mat.e.result))
# etest(mat.e.result)



# diagonaler til at identificere elementer 

diag.submatrix <- diag(mat.e.result)
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
ikke.seg.index.i.submatrix <-  which((diag.submatrix%in%diag.discodata))
ikke.seg.index.i.disco <-  which((diag.discodata%in%diag.submatrix))

 which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) 





mat.e.result[ikke.seg.index.i.submatrix,ikke.seg.index.i.submatrix] <- 0 


#fjerner interne ties i segmenter af interesse (hvis vi bare vil se hvor de går hen)
# etest(mat.e.result) # yep 

diag.mat.e.result <- diag(mat.e.result)  # for later use 
mat.e.result[ikke.seg.index.i.submatrix,seg.index] <- 0

########## for aggregerede submatricer shit ############





# de tre matricer af interesse 

mat.e.result.seg.fra <-  mat.e.result[seg.index,seg.index]


mat.e.result  <-  mat.e.result[seg.index,ikke.seg.index.i.submatrix]

# slå segmenter af interesse sammen til forsimpling 

mat.e.result <-  rbind(mat.e.result,colSums(mat.e.result))
mat.e.result <-  mat.e.result[nrow(mat.e.result),]


# test.af.restgruppe <-   sum(mat.e.result)



if(discogrp==TRUE){

### restgrupper 

test.af.mat.e.result <-   sum(mat.e.result)

colnames.tmp <-  discodata$disco[which(discodata$disco %in% names(mat.e.result))]


colnames.tmp <-  as.character(discodata$membership[which(discodata$disco %in% names(mat.e.result))])

names(mat.e.result) <- colnames.tmp

mat.e.result  <-  mat.e.result[order(names(mat.e.result))]

colnames.tmp <- names(mat.e.result)

mat.e.result <- tbl_df(mat.e.result) 

mat.e.result$disco <- colnames.tmp

mat.e.result <- rename(mat.e.result,without.mob=value) %>% mutate(without.mob.andel=without.mob/sum(without.mob)) %>%  mutate(without.mob.andel.seg.tot=without.mob/(sum(without.mob)+sum(mat.e.result.seg.fra)))
}


if(discogrp==FALSE){

mat.e.result <-  cbind(mat.e.result, sort(colnames.tmp))
colnames(mat.e.result)[2] <- c("membership")
mat.e.result <-  aggregate(mat.e.result[, seq_len(ncol(mat.e.result)-1)], list(mat.e.result$membership), sum)
colnames.tmp <-  mat.e.result$Group.1
rownames(mat.e.result) <-  mat.e.result$Group.1

mat.e.result <- rename(mat.e.result,membership=Group.1,without.mob=x) %>% mutate(without.mob.andel=without.mob/sum(without.mob)) %>%  mutate(without.mob.andel.seg.tot=without.mob/(sum(without.mob)+sum(mat.e.result.seg.fra)))


}


sum(mat.e.result$without.mob.andel.seg.tot)== sum(mat.e.result$without.mob)  / (sum(mat.e.result$without.mob) + sum(mat.e.result.seg.fra)) #yestest


if(discogrp==TRUE){
		mat.e.result <-  discodata %>%  left_join(mat.e.result,.)
}


if(discogrp==FALSE){
mat.e.result <-  seg.df %>% left_join(mat.e.result,.)
}



mat.e.result <-  mat.e.result %>% add_row(.,membership=c("fokussegmenter"),without.mob=sum(mat.e.result.seg.fra), without.mob.andel.seg.tot= sum(mat.e.result.seg.fra)  / (sum(mat.e.result$without.mob) + sum(mat.e.result.seg.fra)),klasse_begtrupbright1=klasse_label)

return(mat.e.result)

}


