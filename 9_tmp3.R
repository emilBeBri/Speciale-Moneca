









segment.quality

> segment.quality
function (segmenter, final.solution = FALSE) 
{
    mat <- segmenter$mat.list[[1]]
    l <- nrow(mat)
    mat <- mat[-l, -l]
    number.of.levels <- length(segmenter$mat.list)
    segment.qual.onelevel <- function(segmenter, niveau) {
        names <- rownames(segmenter$mat.list[[1]])
        names <- names[-length(names)]
        seg.list.niveau <- segmenter$segment.list[[niveau]]
        mat.niveau <- segmenter$mat.list[[niveau]]
        totals.niveau <- (mat.niveau[nrow(mat.niveau), ] + mat.niveau[, 
            nrow(mat.niveau)])/2
        totals.niveau <- totals.niveau[-length(totals.niveau)]
        mat.niveau <- mat.niveau[-nrow(mat.niveau), -nrow(mat.niveau)]
        edge.matrix <- segment.edges(segmenter, cut.off = 1, 
            niveau = 0, small.cell.reduction = 5, segment.reduction = 0)
        net.edge <- graph.adjacency(edge.matrix, weighted = TRUE)
        seg <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) seg[seg.list.niveau[[i]]] <- i
        niveau.qual <- round(diag(mat.niveau)/((rowSums(mat.niveau) + 
            colSums(mat.niveau))/2), 3)
        quality <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) quality[seg.list.niveau[[i]]] <- niveau.qual[i]
        niveau.size <- round(((rowSums(mat.niveau) + colSums(mat.niveau))/2)/sum(colSums(mat.niveau)), 
            3)
        size <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) size[seg.list.niveau[[i]]] <- niveau.size[i]
        niveau.density <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) niveau.density[seg.list.niveau[[i]]] <- graph.density(net.edge - 
            which(((1:vcount(net.edge) %in% seg.list.niveau[[i]]) == 
                FALSE)))
        nodes <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) nodes[seg.list.niveau[[i]]] <- length(seg.list.niveau[[i]])
        max.path <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) max.path[seg.list.niveau[[i]]] <- diameter(net.edge - 
            which(((1:vcount(net.edge) %in% seg.list.niveau[[i]]) == 
                FALSE)), weights = NA)
        share.of.total <- rep(NA, length(names))
        for (i in 1:length(seg.list.niveau)) share.of.total[seg.list.niveau[[i]]] <- (totals.niveau/sum(totals.niveau))[i]
        out.frame <- data.frame(Segment = seg, within.mobility = quality, 
            share.of.mobility = size, Density = niveau.density, 
            Nodes = nodes, Max.path = max.path, share.of.total = round(share.of.total, 
                3))
        colnames(out.frame) <- paste(niveau, ": ", colnames(out.frame), 
            sep = "")
        out.frame
    }
    qual.list <- lapply(1:number.of.levels, segment.qual.onelevel, 
        segmenter = segmenter)
    out.mat <- do.call(cbind, qual.list)
    out.mat <- cbind(Membership = segment.membership(segmenter)[, 
        2], out.mat)
    rownames(out.mat) <- rownames(mat)
    order.mat <- out.mat[, grep("share.of.total", colnames(out.mat))]
    order.mat <- order.mat[, ncol(order.mat):1]
    out.mat <- out.mat[do.call(order, -order.mat), ]
    if (final.solution == TRUE) {
        small.mat <- out.mat[duplicated(out.mat$Membership) == 
            FALSE, ]
        small.mat[sapply(small.mat, is.nan)] <- Inf
        tsm <- as.matrix(small.mat)[, -1]
        collapse.mat <- function(row, n) tail(na.omit(row), n)
        tsm <- as.data.frame(t(apply(tsm, 1, collapse.mat, n = 7)))
        colnames(tsm) <- c("Membership", "Within mobility", "Share of mobility", 
            "Density", "Nodes", "Max.path", "Share of total size")
        tsm$Membership <- small.mat$Membership
        out.mat <- tsm
    }
    out.mat
}
<environment: namespace:MONECA>
> 





























#### test af intern mobilitet


2446: 40


i kolonnen (AE)
191

i rækken (31)
172



3400: 1274

i kolonnen (BN)
3275

i rækken (66)
2290






stor.mob.seg har det her
2964


1314/2964 


(3275+2289)/2



2446
diag.stor[30] 

3400
diag.stor[65] 

40 + 1274 + 110 + 89

= 1314


diag.stor.seg har det her:
1314 


1513 / 2964




stor.mob[30]
181.5
stor.mob[65]
2782.5

181.5 + 2782.5
2964


stor.mob.seg[30]
2964

diag.stor.seg[30]
1314



1314/2964


2964+1314






intern.mobilitet


stor.mob.seg
diag.stor.seg



length(diag.stor.seg)





stor.mob          <- ((rowSums(mob.mat.l) + colSums(mob.mat.l)) / 2) 
diag.stor         <- diag(mob.mat)[-l]



intern.mobilitet  <- diag.stor/stor.mob




setwd("/home/emil")

library(ggplot2)
library(igraph)
library(soc.report)
library(soc.elite)
library(stringr)
#library(readODS) #kan ikke installeres - maaske problem, tjek senere
library(RColorBrewer)
library(scales)
library(reshape2)
# library(soc.ca) (måske ikke så vigtigt)
library(XLConnect)
library(plyr)
library(dplyr)
library(readstata13)
library(MONECA)




http://stackoverflow.com/questions/4372435/how-can-i-rollback-a-github-repository-to-a-specific-commit


library(scales)
library(devtools)

install_github("antongrau/soc.report")

remove.packages("soc.report")


install.packages("/home/emil/Downloads/soc.elite-d256df385c3cc394a4200cc5489d46c6cbe0a55c.zip", repos=NULL) 

install_github("antongrau/soc.elite/tree/15246d60f3b97f6e60f15fd14612b69089b9bb4c")


15246d6


install_github("antongrau/soc.report", Force=TRUE)

  , subdir="tree/15246d60f3b97f6e60f15fd14612b69089b9bb4c")


??install_github
q

install_github("antongrau/soc.elite")
