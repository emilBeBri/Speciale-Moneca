
cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 
cut.off.default <- 1
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0




mat.e <-  mob.mat[-274,-274]
mob.mat[mob.mat<=5] <- 0
mat.e <- wm1
# colnames(mat.e) <- paste(as.character(discodata$`2: Segment`),as.character(discodata$disco),sep=":")
klynge <- 5
undergr <- 2

klynger <- c("3.24","3.30")

work.list <-  sort(as.vector(unlist(seg.selector.df %>% filter(membership %in% klynger) %>%     select(indeks))))


# work.list <-  seg$segment.list[[klynge]][[undergr]]
# work.list.2 <- seg$segment.list[[klynge]][[undergr]]
# work.list <- append(work.list,work.list.2)
# work.list <- c(62)

# work.list <- sort(work.list) #Hvis det skal være i raekkefolge til 

########## simpel: kun segmentet
mat.e.result <- mat.e[work.list,work.list]
################ avanceret1: segment + ties 
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))
mat.e.result <- mat.e[aug.work.list, aug.work.list]
################ avanceret2: segment + ties (uden edges ml ties) 
irr.job.indices <- which(!(aug.work.list %in% work.list))
## first, keep diagonal values for irr.job.indices
dvals <- diag(mat.e.result)[irr.job.indices]
## set sub-matrix to zero (this will also set diagnal elements to zero)
mat.e.result[irr.job.indices,irr.job.indices] <- 0
## replace diagonal elements
diag(mat.e.result)[irr.job.indices] <- dvals
#view(mat.e.result)

#forhindrer diagonalen i at fucke det hele op 
diag(mat.e.result)[] <- 0
diag(mat.e.result)[] <- round_any(max(mat.e.result), 5, ceiling)

relativrisiko.vector.mat.e.result  <-  as.vector(t(mat.e.result))
relativrisiko.vector.mat.e.result[relativrisiko.vector.mat.e.result<=0] <- NA
relativrisiko.vector.mat.e.result[relativrisiko.vector.mat.e.result>130] <- 130
quants= seq(0,1,0.05)
quantile(relativrisiko.vector.mat.e.result, quants,na.rm=TRUE)
quantile(relativrisiko.vector, quants,na.rm=TRUE)





segmentcircle <- mat.e.result
view(segmentcircle)


em.circlize(segmentcircle)
segmentcircle[segmentcircle<=18] <- 0
segmentcircle[segmentcircle>=50] <- 50



# # kan bruges til at sorte hele matrixen 
# heat.krit.df <-  df %>% filter(membership=="3.4") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)   
#  heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
#  heat.krit.df$segkrit  <-  as.character(heat.krit.df$segkrit)


# all.equal(colnames(mat.e.result),heat.krit.df$disco) #test for at se om rækkefølgen er som den skal være 

# mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]



cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/chorddiagrams/seg_3_4_RR_7_5.pdf", onefile = TRUE, height = 20, width = 20)
em.circlize(segmentcircle)
dev.off()

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/chorddiagrams/seg_3_36_absoluttetal_min50.pdf", onefile = TRUE, height = 20, width = 20)
em.circlize(segmentcircle)
dev.off()

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/chorddiagrams/seg_3_35_RR5.pdf", onefile = TRUE, height = 20, width = 20)
em.circlize(segmentcircle)
dev.off()



em.heatmap(mat.e.result)


######### forside - e.mobmat.seg.niveau.5! (måske bedre med RR i stedet)

cut.off.default <-  0.00000001
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 


segmentcircle <- e.mobmat.seg.niveau.5.m.total
match(colnames(e.mobmat.seg.niveau.5),seg.df$membership)

colnames(segmentcircle) <- seg.df$membership_lab
rownames(segmentcircle) <- seg.df$membership_lab


diag(segmentcircle)[] <- 0
relativrisiko.vector.segmentcircle  <-  as.vector(t(segmentcircle))
relativrisiko.vector.segmentcircle[relativrisiko.vector.segmentcircle<=0] <- NA
quants= seq(0,1,0.05)
quantile(relativrisiko.vector.segmentcircle, quants,na.rm=TRUE)
Desc(relativrisiko.vector.segmentcircle)
relativrisiko.vector.segmentcircle[relativrisiko.vector.segmentcircle>130] <- 130


segmentcircle[segmentcircle<=500] <- 0
em.circlize(segmentcircle)

view(segmentcircle)


segmentcircle[segmentcircle>=50] <- 50





# noget er galt, forkerte labels eller sådan noget. tjek senere.



######## forsøg på kunst 



cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 
cut.off.default <-  0.00000001
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 



is.na(wm1) <- do.call(cbind,lapply(wm1, is.infinite))
wm1[is.na(wm1)] <- 0
view(wm1)

view(mob.2cifret)

library(circlize)
segmentcircle <- wm1  
segmentcircle <- mob.2cifret[-29,]
segmentcircle <- mob.2cifret[,-29]
segmentcircle <- mob.3cifret[-105,]
segmentcircle <- segmentcircle[,-105]
view(segmentcircle)
view(mob.3cifret)
iwanthue <-  source("./statistik/R/moneca/vores/vorescripts/7_iwanthue_default_200.R")
iwanthue <-  source("./statistik/R/moneca/vores/vorescripts/7_iwanthue_default_104.R")
iwanthue <-  iwanthue[[1]]

getPalette = colorRampPalette(xmen)
diag(segmentcircle) <- 0
df.c <- get.data.frame(graph.adjacency(segmentcircle,weighted=TRUE))



getPalette = colorRampPalette(brewer.pal(8,"Dark2"))


farve <-  getPalette(28)
farve <-  getPalette(273)


farve <-  iwanthue

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/forsogpaaarbejdsmarkedskunst12.pdf", onefile = TRUE, height = 13, width = 13)
 # farve <- c("#000000", "#FFDD89", "#957244", "#F26223")
chordDiagram(x = df.c, 
  grid.col = farve, 
  transparency = 0,
             directional = 1, symmetric=FALSE,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.065,
             link.arr.type = "big.arrow", 
             # self.link=1
             # link.sort = TRUE, 
             link.largest.ontop = TRUE,
             # link.border="black",
             # link.lwd = 2, 
             # link.lty = 2
             )
dev.off()



wm1            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE)
wm1[is.na(wm1)] <- 0


?chordDiagram




view(df.c)
view(segmentcircle.tot)


# getPalette = colorRampPalette(brewer.pal(12,"Paired"))
# farve <-  getPalette(length(work.list)


























