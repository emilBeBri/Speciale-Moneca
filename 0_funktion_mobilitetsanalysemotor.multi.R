



ma.motor.multi <- function(x,min_seg=1000,min_disco=1000,CH = 60,ceiling_vaerdi=3.5) {
require(pheatmap)
CW=CH*0.75

klynger <- x

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
# # view(mat.e.result)
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
diag.discodata <- diag(mob.mat[-274,-274])
diag.submatrix <- diag(mat.e.result)
diag.seg.df <- diag(e.mobmat.seg.niveau.5)
diag.seg.df.udisco <-diag.seg.df[1:41]
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) # skal være sand for alle elementer 


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
# head(colnames(mat.e.result)) # yes 


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


mat.e.result[ikke.seg.index.i.submatrix,ikke.seg.index.i.submatrix] <- 0 #fjerner interne ties i segmenter af interesse (hvis vi bare vil se hvor de går hen)
# etest(mat.e.result) # yep 

diag.mat.e.result <- diag(mat.e.result)  # for later use 
mat.e.result[ikke.seg.index.i.submatrix,seg.index] <- 0



########## for aggregerede submatricer shit ############

#beskær matrice til ønskede format og med det ønskede minium antal i cellerne 
mat.e.result <- mat.e.result[seg.index,ikke.seg.index.i.submatrix]  


# fjern segmenter der er for små 
beskaeringsindeks.tmp  <-  sort(unique(unlist(which(rowSums(mat.e.result) >= min_seg))))



mat.e.result <- mat.e.result[beskaeringsindeks.tmp,]


###

#antal discogrp tilbage efter udlusning (matrix)
summeringsindeks  <-  sort(unique(unlist(which(colSums(mat.e.result) < min_disco))))
beskaeringsindeks.tmp  <-  sort(unique(unlist(which(colSums(mat.e.result) >= min_disco))))
sum(mat.e.result) == sum(mat.e.result[,beskaeringsindeks.tmp]) +sum(mat.e.result[,summeringsindeks]) #skal være sand 




### restgrupper 
test.af.restgruppe <-   rowSums(mat.e.result)

restgrupper   <-  mat.e.result[,summeringsindeks]
rowSums.restgrupper <- rowSums(restgrupper)
colnames.tmp <-  discodata$indeks[which(discodata$disco %in% colnames(restgrupper))]
colnames.tmp <-  as.character(discodata$membership_lab[which(discodata$disco %in% colnames(restgrupper))])
colnames(restgrupper) <- colnames.tmp
restgrupper  <-  restgrupper[,order(colnames(restgrupper))]
# r <- rle(colnames(restgrupper))
# changes <- rep(seq_along(r$lengths), r$lengths)
restgrupper <-  t(restgrupper)
restgrupper <- tbl_df(restgrupper) 
restgrupper$membership <- sort(colnames.tmp)

restgrupper <-  aggregate(restgrupper[, seq_len(ncol(restgrupper)-1)], list(restgrupper$membership), sum)
restgrupper$Group.1 <- NULL
restgrupper <-  t(restgrupper)
colnames(restgrupper) <- sort(unique(colnames.tmp))
# fjern segmenter der er for små 
summeringsindeks.tmp.1  <-  sort(unique(unlist(which(colSums(restgrupper) < 1000))))
summeringsindeks.tmp.2  <-  sort(unique(unlist(which(colSums(restgrupper) >= 1000))))
rest.rest.grupper   <-  restgrupper[,summeringsindeks.tmp.1]
rowSums( cbind(restgrupper[,summeringsindeks.tmp.2] ,rowSums(rest.rest.grupper))) == rowSums(restgrupper)


restgrupper.samlet <-  cbind(rowSums(rest.rest.grupper),restgrupper[,summeringsindeks.tmp.2] )


colnames(restgrupper.samlet)[1] <- paste(c("Andre:"),
ncol(rest.rest.grupper), "erhvervsgrupper", c("fra"), length(unique(colnames(rest.rest.grupper))), "segmenter",sep=" ")




#fjerner de små discogrp der er slået sammen nu 
mat.e.result <-  mat.e.result[,beskaeringsindeks.tmp]

################# labels 



col_labels.mat.e.result <- paste(discodata$membership[which(discodata$disco%in%colnames(mat.e.result))], ":", " ", strtrim(colnames(mat.e.result) , 50), sep="")
row_labels.mat.e.result <- paste(rev(sort(unique(discodata$membership_lab[which(discodata$membership %in% rownames(mat.e.result))]))),":", " ", sep="") #OBS! Her bliver de IKKE matchet på deres (unikke) diagonal, fordi den er skåret væk, men udelukkende på position. Mulig fejlkilde. 
colnames(mat.e.result) <- col_labels.mat.e.result
rownames(mat.e.result) <- row_labels.mat.e.result
# colnames(pheatmap.display) <- col_labels.mat.e.result
# rownames(pheatmap.display) <- row_labels.mat.e.result




restgrupper.samlet <-  cbind(restgrupper.samlet,mat.e.result[,1])

#raekkefolge ifht. antal skift i segment af interesse : rows 
mat.e.result <-  mat.e.result[rev(order(rowSums(mat.e.result))),]
restgrupper.samlet <- restgrupper.samlet[order(match(restgrupper.samlet[,ncol(restgrupper.samlet)],mat.e.result[,1]) ),]

#raekkefolege ifht til segment af discogrupper - cols
mat.e.result <-  mat.e.result[,order(colnames(mat.e.result) , colSums(mat.e.result) )]

#tilføj rest.gruppe.disco men beholder mat.e.results rownames
 rownames.tmp <-  rownames(mat.e.result)
 restgrupper.samlet <- restgrupper.samlet[,-ncol(restgrupper.samlet)]
 mat.e.result <-  cbind(restgrupper.samlet,mat.e.result)

rowSums(mat.e.result) %in%  test.af.restgruppe # skal være sande 

rownames(mat.e.result) <- rownames.tmp



#ændringer rækkefølge final 
somdeter <-  data.frame(colnames(mat.e.result))
colnames(somdeter) <- c("label")
somdeter$label_kort <-  strtrim(somdeter$label,5)
somdeter$colsums <-  colSums(mat.e.result)
somdeter <- somdeter %>% group_by(label_kort) %>% mutate(colsums.grp= sum(colsums))
somdeter$colsums.grp[1] <- -9999999
somdetskalvaere <- somdeter %>% arrange(desc(colsums.grp), label_kort,label)
somdetskalvaere <-  somdetskalvaere[nrow(somdetskalvaere):1,]
korrekt <-  match(colnames(mat.e.result),somdetskalvaere$label)
mat.e.result <- mat.e.result[,order(korrekt)]




# procent 
mat.e.result.andel <- mat.e.result
mat.e.result.andel <- 100*(mat.e.result/sum(mat.e.result.andel))
sum(mat.e.result.andel)==100 #procent, skal være 100



## sæt marginaler på (ikke nødvendigvis en god ide, forsøg)

#col 
marginaler_col <-  colSums(mat.e.result)
mat.e.result  <-  rbind(mat.e.result,marginaler_col)
rownames(mat.e.result)[nrow(mat.e.result)] <- c("Række total")

marginaler_col <-  colSums(mat.e.result.andel)
mat.e.result.andel  <-  rbind(mat.e.result.andel,marginaler_col)
rownames(mat.e.result.andel)[nrow(mat.e.result.andel)] <- c("Række total")


#row
marginaler_row <-  rowSums(mat.e.result)
mat.e.result  <-  cbind(marginaler_row,mat.e.result)
colnames(mat.e.result)[1] <- c("Kolonne total")

marginaler_row <-  rowSums(mat.e.result.andel)
mat.e.result.andel  <-  cbind(marginaler_row,mat.e.result.andel)
colnames(mat.e.result.andel)[1] <- c("Kolonne total")


#display 
 pheatmap.display <- mat.e.result
 pheatmap.display.andel <- mat.e.result.andel




# ceiling på høje værdier
#andel 
mat.e.result.andel[mat.e.result.andel>=ceiling_vaerdi] <- ceiling_vaerdi
pheatmap.display.andel <- round(pheatmap.display.andel,1)
# pheatmap.display.andel[pheatmap.display.andel==2] <- c("2+ %")
pheatmap.display.andel[pheatmap.display.andel==0] <- c("")
# diag(pheatmap.display.andel) <- c("im")

#fjern værdier kolonne og række total i selve data-matricen
mat.e.result.andel[,1] <- 0
mat.e.result.andel[nrow(mat.e.result.andel),] <- 0



#  annotation 
annotation_col.df <- data.frame(somdetskalvaere$label)
colnames(annotation_col.df) <- c("label")
annotation_col.df$label <- strtrim(annotation_col.df$label,5)
annotation_col.df <-  rbind(c("Kolonne Total"),annotation_col.df)
rownames(annotation_col.df) <- colnames(mat.e.result.andel)

#gaps
# gaps_col.df <- colnames(mat.e.result.andel) 
# gaps_col.df[seq_len(ncol(restgrupper.samlet))] <- rep("store ørn",ncol(restgrupper.samlet))



pheatmap::pheatmap(mat.e.result.andel, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE, fontsize_number=10,gaps_row=append(first.changes(rownames(mat.e.result.andel)),length(rownames(mat.e.result.andel))), gaps_col=first.changes(annotation_col.df$label),display_numbers = pheatmap.display.andel, cellwidth = CW, cellheight = CH, annotation_col=annotation_col.df,border_color="grey36",main=titel)

}




