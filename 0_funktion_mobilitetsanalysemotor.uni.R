
ma.motor.uni <- function(x,procent=TRUE,min_disco=750,CH=50,ceiling_vaerdi=7.5) {
require(pheatmap)


klynger <- x

klynger <- klynger[grep("^1.*", klynger, invert = TRUE)]


# #til test
# klynger <- manuelle.klynger.lav
# procent=TRUE
# min_disco=750
# CH=50
# ceiling_vaerdi=7.5
# CW=CH*0.75

# CW=CH*0.75



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


#beskær matrice til ønskede format og med det ønskede minium antal i cellerne 
mat.e.result <- mat.e.result[seg.index,ikke.seg.index.i.submatrix]  


# slå segmenter af interesse sammen til forsimpling 
mat.e.result <-  rbind(mat.e.result,colSums(mat.e.result))
mat.e.result <-  mat.e.result[nrow(mat.e.result),]

test.af.restgruppe <-   sum(mat.e.result)


#antal discogrp tilbage efter udlusning (simpel: vektor)





#antal discogrp tilbage efter udlusning
summeringsindeks  <-  sort(unique(unlist(which(mat.e.result < min_disco))))
beskaeringsindeks.tmp  <-  sort(unique(unlist(which(mat.e.result >= min_disco))))
sum(mat.e.result) == sum(mat.e.result[beskaeringsindeks.tmp]) +sum(mat.e.result[summeringsindeks]) #skal være sand 


### restgrupper 
test.af.restgruppe <-   sum(mat.e.result)
restgrupper   <-  mat.e.result[summeringsindeks]
rowSums.restgrupper <- sum(restgrupper)
colnames.tmp <-  discodata$indeks[which(discodata$disco %in% names(restgrupper))]
colnames.tmp <-  as.character(discodata$membership_lab[which(discodata$disco %in% names(restgrupper))])
names(restgrupper) <- colnames.tmp
restgrupper  <-  restgrupper[order(names(restgrupper))]


# r <- rle(colnames(restgrupper))
# changes <- rep(seq_along(r$lengths), r$lengths)

# restgrupper <-  t(restgrupper)

restgrupper <- tbl_df(restgrupper) 
restgrupper <-  cbind(restgrupper, sort(colnames.tmp))
colnames(restgrupper)[2] <- c("membership")
restgrupper <-  aggregate(restgrupper[, seq_len(ncol(restgrupper)-1)], list(restgrupper$membership), sum)
colnames.tmp <-  restgrupper$Group.1
rownames(restgrupper) <-  restgrupper$Group.1

restgrupper$Group.1 <- NULL


restgrupper <-  t(restgrupper)

# fjern segmenter der er for små 
summeringsindeks.tmp.1  <-  sort(unique(unlist(which(restgrupper < 750))))
summeringsindeks.tmp.2  <-  sort(unique(unlist(which(restgrupper >=750))))



rest.rest.grupper   <-  restgrupper[summeringsindeks.tmp.1]

sum(restgrupper[summeringsindeks.tmp.2]) + sum(rest.rest.grupper) == sum(restgrupper)



colnames.tmp  <-  colnames(restgrupper)[summeringsindeks.tmp.1]


restgrupper.samlet <-  append(sum(rest.rest.grupper),restgrupper[,summeringsindeks.tmp.2] )

names(restgrupper.samlet)[1] <- paste(c("Andre:"), length(rest.rest.grupper), "erhvervsgrupper", c("fra"), length(unique(colnames.tmp)), "segmenter",sep=" ")




#fjerner de små discogrp der er slået sammen nu 
mat.e.result <-  mat.e.result[beskaeringsindeks.tmp]


################# labels 

col_labels.mat.e.result <- paste(discodata$membership[which(discodata$disco%in%names(mat.e.result))], ":", " ", strtrim(names(mat.e.result) , 50), sep="")

names(mat.e.result) <- col_labels.mat.e.result
# colnames(pheatmap.display) <- col_labels.mat.e.result
# rownames(pheatmap.display) <- row_labels.mat.e.result


#raekkefolege ifht til segment af discogrupper - cols
mat.e.result <-  mat.e.result[order(names(mat.e.result) , mat.e.result)]

mat.e.result <-  append(restgrupper.samlet,mat.e.result)


#ændringer rækkefølge final 
somdeter <-  data.frame(names(mat.e.result))
colnames(somdeter) <- c("label")
somdeter$label_kort <-  strtrim(somdeter$label,5)


somdeter$colsums <- mat.e.result


somdeter <- somdeter %>% group_by(label_kort) %>% mutate(colsums.grp= sum(colsums))
somdeter$colsums.grp[1] <- -9999999
somdetskalvaere <- somdeter %>% arrange(desc(colsums.grp), label_kort,label)
somdetskalvaere <-  somdetskalvaere[nrow(somdetskalvaere):1,] #vend om på rækkefølgen - til pheatmap
korrekt <-  match(names(mat.e.result),somdetskalvaere$label)
mat.e.result <- mat.e.result[order(korrekt)]


if(procent==TRUE){
mat.e.result.andel <- mat.e.result
mat.e.result <- 100*(mat.e.result/sum(mat.e.result.andel))
sum(mat.e.result)==100 #procent, skal være 100
}


#display 
 pheatmap.display <- mat.e.result


# ceiling på høje værdier
#andel 
mat.e.result[mat.e.result>= ceiling_vaerdi] <- ceiling_vaerdi
pheatmap.display <- round(pheatmap.display,1)
# pheatmap.display[pheatmap.display==2] <- c("2+ %")
pheatmap.display[pheatmap.display==0] <- c("")
# diag(pheatmap.display) <- c("im")



#  annotation 
annotation_col.df <- data.frame(somdetskalvaere$label)
colnames(annotation_col.df) <- c("membership")


annotation_col.df$membership <- strtrim(annotation_col.df$membership,5)
# rownames(annotation_col.df) <- names(mat.e.result)

annotation_col.df$membership <- gsub(":", "", annotation_col.df$membership)
annotation_col.df$membership <- gsub(" ", "", annotation_col.df$membership)
annotation_col.df$klasse_begtrupbright1[2:26] <- seg.df$klasse_begtrupbright1[match(annotation_col.df$membership[2:26],seg.df$membership)]
annotation_col.df$membership_lab[2:26] <- seg.df$membership_lab[match(annotation_col.df$membership[2:26],seg.df$membership)]
annotation_col.df$membership_lab[1] <- c("Andre")
annotation_col.df$klasse_begtrupbright1[1] <- c("Andre")
rownames(annotation_col.df) <- names(mat.e.result)



annotation_col.df.final <- annotation_col.df %>% select(membership_lab,klasse_begtrupbright1) %>%   	 rename(Segment=membership_lab, Klasse=klasse_begtrupbright1)


library(grid)
library(pheatmap)
## For pheatmap_1.0.8 and later:
draw_colnames_45 <- function (coln, gaps, ...) {
    coord = pheatmap:::find_coordinates(length(coln), gaps)
    x = coord$coord - 0.5 * coord$size
    res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot = 65, gp = gpar(...))
    return(res)}
## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_45", ns=asNamespace("pheatmap"))
pheatmap::pheatmap(t(mat.e.result), color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE, fontsize_number=10, cellwidth = CW, cellheight = CH,gaps_col=first.changes(annotation_col.df.final$Segment),annotation_col=annotation_col.df.final,display_numbers = t(pheatmap.display),border_color="black")


}