




library(circlize)


#####################


quants= seq(0,1,0.05)
quantile(relativrisiko.vector, quants,na.rm=TRUE)

cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 
cut.off.default <- 2
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0


klynge <- 2
undergr <- 21
work.list <-  seg$segment.list[[klynge]][[undergr]]

########## simpel: kun segmentet

mat.e <- wm1
# mat.e.result <- mob.mat

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


######## selve grafen 
library(circlize)
# segmentcircle <- sub.mat  
# segmentcircle <- mob.mat[-274,]
# segmentcircle <- segmentcircle[,-274]

# view(segmentcircle)

segmentcircle <- mat.e.result
getPalette = colorRampPalette(xmen)
diag(segmentcircle) <- 0
df.c <- get.data.frame(graph.adjacency(segmentcircle,weighted=TRUE))
 # farve <- c("#000000", "#FFDD89", "#957244", "#F26223")
chordDiagram(x = df.c, 
  grid.col = getPalette(ncol(segmentcircle)), 
  transparency = 0.3,
             directional = 1, symmetric=FALSE,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.065,
             link.arr.type = "big.arrow", 
             # self.link=1
             # link.sort = TRUE, 
             link.largest.ontop = TRUE,
             link.border="black",
             # link.lwd = 2, 
             # link.lty = 2
             )




?chordDiagram




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


























