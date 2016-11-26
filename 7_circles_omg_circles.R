

jobdat <- matrix(c(
1,   0,   1,   0,   0,   0,   0,
1,   1,   1,   0,   0,   0,   0,
1,   1,   1,   0,   0,   0,   0,
0,   0,   0,   1,   0,   0,   0,
0,   0,   0,   0,   1,   0,   0,
0,   0,   0,   0,   0,   1,   0,
0,   0,   0,   0,   0,   0,   1
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7"),
                c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")))


jobdat.result <- matrix(c(
1,     0,     1,     0,     0,
1,     1,     0,     0,     0,
1,     0,     1,     0,     0,
0,     0,     0,     1,     0,
0,     0,     0,     0,     1
           ), 
           nrow = 5, ncol = 5, byrow = TRUE,
           dimnames = list(c("job 1","job 2","job 3","job 5","job 7"),
                c("job 1","job 2","job 3","job 5","job 7")))









work.list <- c(1,5,7)

work.list <- sort(unique(unlist(lapply(work.list, function(x) which(jobdat[x,] != 0)))))

jobdat[work.list,work.list]


work.list2 <- sort(unique(unlist(lapply(work.list, function(x) which(jobdat[,x] != 0)))))




@EmilBB: I think what you want to find are the indices to the augmented work.list that are not in the original work.list; then use these to set those off-diagonal elements of the current result to zero. To find those do: 

aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(jobdat[x,] != 0)))))


set.to.zero <- which(!(aug.work.list %in% work.list)) 




view(jobdat)







library(circlize)


#####################

klynge <- 3
undergr <- 24
work.list <-  seg$segment.list[[klynge]][[undergr]]

########## simpel: kun segmentet

cut.off.default <-  1 #skal måske ikke være 1 her jo
wm1            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE)
wm1[is.na(wm1)] <- 0

# wm1  <-  round(wm1, digits=0)

segmentcircle.rr <- wm1[work.list,work.list]
segmentcircle.tot <- mob.mat[work.list,work.list]
segmentcircle.tot <- cbind(segmentcircle.tot,colnames(segmentcircle.tot))
segmentcircle <- segmentcircle.tot[,-ncol(segmentcircle.tot)]
segmentcircle <- segmentcircle.rr




################ avanceret: segment + ties 


cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 


wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 


wm1[is.na(wm1)] <- 0
wm1 <- round(wm1,1)
work.list.exp <- sort(unique(unlist(lapply(work.list, function(x) which(wm1[x,] != 0)))))
sub.mat <- wm1 [work.list.exp,work.list.exp]
sub.mat <-  cbind(colnames(sub.mat),sub.mat)



################ avanceret: segment + ties 2 


aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(wm1[x,] != 0)))))



aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(wm1[x,] != 0)))))


set.to.zero <- which(!(aug.work.list %in% work.list)) 







df.t <-  df %>% filter(grepl(paste(work.list,collapse="|"),disco_s)) %>%    mutate(disco_s2 = disco_s) 




######## selve grafen 





segmentcircle <- sub.mat  
diag(segmentcircle) <- 0
df.c <- get.data.frame(graph.adjacency(segmentcircle,weighted=TRUE))
farve <-  brewer.pal(ncol(segmentcircle),"Set1")
 # farve <- c("#000000", "#FFDD89", "#957244", "#F26223")
chordDiagram(x = df.c, 
  grid.col = farve, 
  transparency = 0.2,
             directional = 1, symmetric=FALSE,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.065,
             link.arr.type = "big.arrow", 
             # self.link=1
             link.sort = TRUE, link.largest.ontop = TRUE,
             link.border="black",
             # link.lwd = 2, 
             # link.lty = 2
             )



?chordDiagram




view(df.c)
view(segmentcircle.tot)


# getPalette = colorRampPalette(brewer.pal(12,"Paired"))
# farve <-  getPalette(length(work.list))
  




























