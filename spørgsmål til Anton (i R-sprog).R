spørgsmål til Anton (i R-sprog)


# der er to koder for at lave intern mobilitet. Der er koden i 2_analyse scriptet, der laver den sådan her. De er identiske på 1. niveau, men ligeså snart segmenter lægges sammen er der forskel. Hos segment.quality-funktionen stiger den interne mobilitet vist altid (?!), det gør den ikke i den manuelle metode. Problemet er jeg godt kan følge den manuelle metode, men jeg kan ikke følge den anden. 

1.
stor.mob.seg                     <- aggregate.membership(stor.mob, seg.mem[, 2]) #giver antallet af alle mobile indenfor de forskellige kategoriers samlede segment - dvs summerer deres segment-grupper (ingen dokumentation, fra 1_data-filen)
#stor.mob.seg

# internt mobilitet laves - fejl tidligere, men m? v?re m?den jeg k?rte koden p?
intern.mobilitet.seg             <- diag.stor.seg / stor.mob.seg

2. en underfunktion i segment.quality()-funktionen, som jeg simpelthen ikke kan læse:

segmenter <- seg

segment.quality
function (segmenter, final.solution = FALSE) 
{
    mat <- segmenter$mat.list[[1]]    
    l <- nrow(mat)
    mat <- mat[-l, -l]
    # fjerner totals 
    number.of.levels <- length(segmenter$mat.list)
    # antal levels
    segment.qual.onelevel <- function(segmenter, niveau) {


        names <- rownames(segmenter$mat.list[[1]])
        names <- names[-length(names)]
        #fjerner total rownames fra names-objektet
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




