

heat.krit.df <-  df %>% filter(membership=="3.4") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)   
# view(heat.krit.df)
heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
# mat.e.result.bak <- mat.e.result
mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]
# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)
annotation <- as.data.frame(heat.krit.df$segkrit)
rownames(annotation) <- heat.krit.df$disco 
colnames(annotation) <- c("klynge")


pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))





mat.e.result.bak <- mat.e.result

mat.e.result <- mat.e.result.bak






mat.e.result[mat.e.result>=15] <- 15

pheatmap.disp <- round(mat.e.result,0)
diag(pheatmap.disp)[] <- c("-")
pheatmap.disp[pheatmap.disp==15] <- c("15+")
klynge <-  sample(iwanthue,size=length(levels(annotation[,1])))
names(klynge) <- levels(annotation[,1])
anno_farver <- list(klynge = klynge)
breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=10,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))







names(Var1) <- c("Exp1", "Exp2")

anno_colors <- list(Var1 = Var1)




## For pheatmap_1.0.8 and later:
library(grid)  
library(pheatmap)
draw_colnames_45 <- function (coln, gaps, ...) {
    coord = pheatmap:::find_coordinates(length(coln), gaps)
    x = coord$coord - 0.5 * coord$size
    res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = 1, rot = 45, gp = gpar(...))
    return(res)}

## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
ns=asNamespace("pheatmap"))

## Try it out
pheatmap(d 


display_numbers = round(mat.e.result,0)



color = colorRampPalette(rev(brewer.pal(n = 7, 
    name = "RdYlBu")))(100)


# fremragende måde at få en almindelig colorpalette expanderet på. 
color = colorRampPalette(skala.heatmap)(length(breaksList))



colorRampPalette(skala.heatmap)(length(breaksList))




pheatmap
a





######################################


mat = structure(c(2L, 5L, 0L, 6L, 3L, 0L, 5L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 5L, 1L, 1L, 1L, 0L, 3L, 2L, 1L, 3L, 5L, 0L, 4L, 1L, 7L, 6L, 9L, 1L, 4L, 6L, 9L), .Dim = c(6L, 6L), .Dimnames = list(c("A", "B", "C", "D", "E", "F"), c("A", "B", "C", "D", "E", "F")))
crit = structure(list(NAME = c("A", "B", "C", "D", "E", "F"), TYPE = c("Dog", "Other", "Cat", "Other", "Cat", "Dog")), .Names = c("NAME", "TYPE"), row.names = c(NA, -6L), class = "data.frame")


rownames(mat)
colnames(mat.e.result)



heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)


view(heat.krit.df)

all.equal(colnames(test),heat.krit.df$disco)


view(crit)
view(heat.krit.df)


is.data.frame(crit)
is.data.frame(heat.krit.df)

is.character(crit$NAME)
is.factor(crit$NAME)
is.character(crit$TYPE)
is.factor(crit$TYPE)


is.character(heat.krit.df$disco)
is.factor(heat.krit.df$disco)

is.character(heat.krit.df$segkrit)
is.factor(heat.krit.df$segkrit)

heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df$segkrit  <-  as.character(heat.krit.df$segkrit)

view(mat)
view(mat.e.result)
view(crit)





mat[order(crit$TYPE), order(crit$TYPE)]

view(mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)])






mat.e.result <-  mat.e.result[order(colnames(mat.e.result)), order(colnames(mat.e.result))]



rownames(test)




view(test[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)])


view(heat.krit.df)


em.heatmap(mat.e.result)




order(as.factor(heat.krit.df$segkrit))



view(mat.e.result)

em.vis.ties(3,4)




view(heat.krit.df)


)


  %>% summarise(lavklynge_gns = mean(ledighed.mean.gns*100)) %>%   left_join(.,df) )




view(df)


irr.job.indices <- which(!(aug.work.list %in% work.list))


  which(work.list %in% seg$segment.list[[3]])



test  <-  seg$segment.list[[2]]



lapply(work.list, function(x) which(mat.e[,x] != 0))



aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))







































#####################################


heatmap.rescale <- natur.breaks.heatmap


view(mat.e.result)

 skala.heatmap.2 <- c("mediumpurple4" ,"mediumpurple1", "orange", "yellow2", "yellow1") 


1
2
matz <- mat1[,c(2,3,6)]
head(matz)




Cast the matrix to have variable names as row names and column names

mat2df <- cast(matz, Row~Column) # Eureka!




mat2 <- as.matrix(mat2df)
head(mat2)
The matrix is ready!



corfac <- data.frame(gender,var2,var3,var4,var5,var6)
summary(corfac)
class(corfac)
view(corfac)


combos <- expand.grid(rep(list(1:ncol(corfac)), 2 )) # combinations with repetitions
view(combos)

combos <- as.matrix(combos)
combos <- t(combos) # transpose matrix
view(combos)



mat1 <- adply(combos, 2, function(x) {
test <- chisq.test(corfac[, x[1]], corfac[, x[2]])
 
out <- data.frame("Row" = colnames(corfac)[x[1]]
, "Column" = colnames(corfac[x[2]])
, "Chi.Square" = round(test$statistic,3)
,  "df"= test$parameter
,  "p.value" = round(test$p.value, 3)
)
return(out)
})


mat.e.result <- get.data.frame(graph.adjacency(mat.e.result,weighted=TRUE))

view(mat1)
view(mat.e.result)
#ens, tror jeg 




ggplot(mat1, aes(Row, Column, fill = p.value)) +
geom_tile(colour="gray80") +
theme_gray(8) +
scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"),
midpoint = 0.04, space = "Lab", na.value = "grey50", guide = "colourbar")

ggplot(mat.e.result, aes(from, to, fill = weight)) +
geom_tile(colour="gray80") +
theme_gray(8) +
scale_fill_gradient2(low = muted("blue"), mid = "white", high = muted("red"),
midpoint = 0.04, space = "Lab", na.value = "grey50", guide = "colourbar")



matz <- mat1[,c(2,3,6)]

#yes. ens. skal ligge som en adjecancy matrice. to variable, én vægt. 



head(matz)
Cast the matrix to have variable names as row names and column names



mat2df <- cast(matz, Row~Column) # Eureka!
view(mat2)

mat2 <- as.matrix(mat2df)
head(mat2)

view(mat2)
view(mat.e.result)

library(gplots)
 
# Defining breaks for the color scale
myCol <- c("yellow", "orange", "red", "gray20", "gray15")
myBreaks <- c(0, 0.001, 0.01, 0.05, 0.8, 1)
hm <- heatmap.2(mat2, scale="none", Rowv=T, Colv=T,
col = myCol, ## using your colors
breaks = myBreaks, ## using your breaks
#                 dendrogram = "none",  ## to suppress warnings
margins=c(5,5), cexRow=0.7, cexCol=0.7, key=FALSE, keysize=1.5,
trace="none")
 
legend("topleft", fill = myCol, cex=0.9,
legend = c("0 to 0.001", "0.001 to 0.01", "0.01 to 0.05", "0.05 to 0.8", ">0.8"))
hm <- heatmap.2(mat.e.result, scale="none", Rowv=T, Colv=T,
col = myCol, ## using your colors
breaks = myBreaks, ## using your breaks
#                 dendrogram = "none",  ## to suppress warnings
margins=c(5,5), cexRow=0.7, cexCol=0.7, key=FALSE, keysize=1.5,
trace="none")
 
legend("topleft", fill = myCol, cex=0.9,
legend = c("0 to 0.001", "0.001 to 0.01", "0.01 to 0.05", "0.05 to 0.8", ">0.8"))


set.seed(21)
A <- matrix(rnorm(9),3,3)
A[order(A[,1]),]



library(plyr)
library(ggplot2)
library(scales)
library(reshape)














gender <- c(
"1", "2", "1", "2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1", "1", "1", "1", "2", "2", "1", "2", "1", "1", "1", "1", "1", "1", "2", "1", "1", "1", "1", "1", "1",
"2", "2", "1", "2", "1", "2", "2", "1", "1", "1", "2", "1", "2", "2", "2", "2", "2", "1", "1", "1", "1", "1", "1", "1", "1", "2", "1", "1", "1", "2", "1", "1", "1", "1", "2", "1", "1", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1",
"1", "1", "1", "2", "1", "1", "1", "2", "1", "1", "2", "1", "1", "2", "2", "2", "1", "2", "1", "2", "1", "1", "1", "1", "2", "2", "2", "2", "2", "1", "1", "1", "1", "2", "2", "1", "1", "1", "1", "1", "1", "2", "1", "1", "2", "1", "2",
"1", "1", "1", "1", "1", "2", "1", "1", "1", "1", "1", "2", "1", "1", "2", "1", "2", "1", "1", "1", "1", "1", "2", "1", "2", "2", "1", "2", "2", "1", "1", "1", "1", "1", "2", "2", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1", "1",
"1", "1", "1", "1", "1", "2", "1", "1", "1", "2", "2", "1", "2", "1", "1", "1", "1", "2", "1", "1", "1", "1", "1", "1", "2", "2", "1", "1", "1", "1", "2", "1", "1", "1", "1", "2", "1", "2", "1", "1", "2", "1", "1", "1", "1", "1", "2",
"2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "1", "2", "1", "1", "1", "2", "1", "2", "2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1", "2", "1", "1", "1", "2", "1", "2", "2", "1", "1",
"1", "1", "2", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "1", "1", "1", "2", "1", "1", "1", "2", "2", "1", "1", "2", "2", "1", "1", "1", "2", "1", "1", "1", "1", "1", "2", "1", "1", "1", "1", "1", "2", "1", "2", "2",
"1", "1", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1", "1", "1", "2", "1", "2", "1", "1", "1", "1", "1", "1", "2", "2", "1", "1", "2", "1", "2", "2", "1", "2", "2", "1", "2", "2", "2", "1", "2", "1", "2", "2", "1", "1", "1", "1",
"1", "1", "1", "1", "1", "2", "2", "1", "1", "2", "2", "2", "1", "2", "2", "2", "2", "1", "2", "1", "1", "2")
 
var2 <- c(
"0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "1", "1", "0", "0", "0", "0", "1", "0", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "1", "1", "0", "0", "0", "0", "1", "1", "1", "1", "0",
"0", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "1", "1", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"1", "1", "1", "1", "1", "0", "0", "1", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "1", "0", "1", "1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0",
"0", "1", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "0", "0", "1", "0", "1", "0", "1", "0", "0", "1", "0", "0", "0", "1", "0", "0", "1", "1", "0", "0", "0", "0", "1", "0", "1", "1", "0",
"1", "1", "1", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "1", "1", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0",
"1", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0")
 
var3 <- c(
"0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "1", "0", "0", "0", "0", "1", "0", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "0", "1",
"0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0",
"1", "0", "0", "0", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0",
"0", "0", "1", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "0", "1", "0",
"1", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0",
"1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "0", "1", "0", "0", "0", "1", "0")
 
var4 <- c(
"0", "1", "0", "0", "0", "0", "1", "0", "1", "1", "0", "1", "0", "1", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1",
"1", "0", "1", "0", "1", "1", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "1", "1", "1", "0", "0",
"0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "1", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "0", "0", "0", "1", "0", "1", "1",
"1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "1", "0", "0", "1", "0", "1", "0", "1",
"0", "0", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "1", "0", "1", "1", "0", "1", "1", "0", "1", "0", "1", "0", "1", "0", "0", "0", "0", "1", "1", "1", "1", "0", "0", "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "1",
"1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "0", "0", "1", "1", "1", "1", "0", "0", "1", "1", "0", "0", "1", "1", "0", "0", "1", "0", "1", "1", "0", "0", "1",
"0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "1", "1", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "0", "0", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "0", "0", "1", "1", "1",
"1", "1", "0", "1", "1", "1", "0", "1", "1", "0", "0", "0", "0", "0", "1", "0", "1", "0", "1", "1", "1", "1", "0", "1", "1", "0", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "1",
"1", "1", "0", "0", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "1", "0", "1", "0")
 
var5 <- c(
"0", "1", "1", "0", "0", "1", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "0", "1", "0", "0", "1", "0", "0",
"1", "1", "0", "0", "0", "0", "0", "1", "0", "0", "1", "0", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0",
"1", "1", "0", "1", "1", "0", "1", "1", "1", "1", "1", "1", "0", "1", "1", "0", "0", "1", "1", "0", "1", "0", "0", "0", "1", "0", "1", "1", "1", "0", "0", "0", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "0", "1", "0", "1",
"0", "0", "1", "1", "0", "0", "1", "1", "1", "0", "0", "0", "0", "0", "0", "1", "1", "0", "1", "1", "1", "1", "0", "1", "0", "1", "1", "0", "0", "0", "0", "1", "1", "1", "1", "0", "1", "1", "0", "0", "1", "0", "1", "1", "0", "1", "1",
"0", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "0", "0", "1", "0", "1", "0", "0", "0", "1", "1", "0", "0", "1", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0",
"0", "0", "1", "0", "0", "0", "0", "0", "1", "0", "1", "0", "0", "0", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "1", "1", "1", "1", "0", "1", "1", "0", "1", "0", "0", "1", "1", "1", "1",
"1", "1", "0", "1", "1", "0", "1", "1", "1", "0", "0", "0", "0", "0", "1", "1", "1", "0", "1", "1", "0", "0", "0", "1", "0", "0", "1", "0", "0", "0", "1", "1", "1", "1", "1", "1", "1", "0", "1", "0", "1", "1", "0", "1", "1", "0", "1",
"1", "1", "0", "0", "0", "0", "1", "1", "1", "1", "0", "1", "1", "1", "0", "1", "1", "1", "1", "1", "1", "0")
 
var6 <- c(
"1", "1", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
"1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "0", "0", "1", "1", "0", "0", "0", "0", "0", "0", "1", "1", "1", "1", "1", "1",
"1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1",
"1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "1", "0", "0", "0", "1", "0", "0", "1", "1", "1", "0", "0", "0", "1", "1", "1",
"1", "1", "1", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "1", "1", "1", "1", "1", "1", "1",
"1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1", "1", "1", "1", "0", "0", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "1", "1", "0", "0",
"0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "1", "1", "0", "0", "1", "1", "1", "1", "0", "0", "0", "0", "1", "0", "0", "0",
"1", "1", "1", "1", "1", "1", "0", "1", "0", "0", "0", "1", "1", "0", "0", "1", "1", "0", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "0", "1", "1", "1", "0", "1", "1", "1", "1", "0", "1", "0", "0", "1", "1", "1", "1", "0", "1",
"1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0")
 


















##################

library(pheatmap)
# Generate some data
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
# original figure
pheatmap(test)








# Add annotation as described above, and change the name of annotation
annotation <- data.frame(Var1 = factor(1:10 %% 2 == 0, labels = c("Exp1", "Exp2")))
rownames(annotation) <- colnames(test) # check out the row names of annotation


view(annotation)
view(heat.krit.df)




pheatmap(test, annotation = annotation)



pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)


test2 <- as.data.frame(as.factor(heat.krit.df$segkrit))


rownames(test2) <- colnames(mat.e.result) 


pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = test2)




diag(wm1)[] <- 0
wm1[wm1>=13] <- 13
pheatmap(wm1)
pheatmap(wm1, cluster_cols=FALSE,cluster_rows=FALSE)









library(pheatmap)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), kmeans_k = NA, breaks = NA, border_color = "grey60", cellwidth = NA, cellheight = NA, scale = "none", cluster_rows = FALSE, cluster_cols = FALSE, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", clustering_method = "complete", cutree_rows = NA, cutree_cols = NA)



pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), kmeans_k = NA, breaks = NA, border_color = "grey60", cellwidth = NA, cellheight = NA, scale = "none", cluster_rows = FALSE, cluster_cols = FALSE, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", clustering_method = "complete", cutree_rows = NA, cutree_cols = NA)




  , treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows, 50, 0))


  , treeheight_col = ifelse((class(cluster_cols) == "hclust") || cluster_cols, 50, 0))


  , legend = TRUE, legend_breaks = NA, legend_labels = NA, annotation_row = NA, annotation_col = NA, annotation = NA, annotation_colors = NA, annotation_legend = TRUE, annotation_names_row = TRUE, annotation_names_col = TRUE, drop_levels = TRUE, show_rownames = T, show_colnames = T, main = NA, fontsize = 10, fontsize_row = fontsize, fontsize_col = fontsize, display_numbers = F, number_format = "%.2f", number_color = "grey30", fontsize_number = 0.8 * fontsize, gaps_row = NULL, gaps_col = NULL, labels_row = NULL, labels_col = NULL, filename = NA, width = NA, height = NA, silent = FALSE)




























 M2=t(apply(mat.e.result,1,sort))
  M2=t(apply(M2,2,sort))
view(M2)

help(pheatmap)

view(mat.e.result)












     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    5    6    7   11   17   23
[2,]    1    9   15   18   21   24
[3,]    2    3    8   13   19   22
[4,]    4   10   12   14   16   20
Nice, elements are sorted by row. But for symmetric reasons, I also wanted to sort them by column. So from this sorted matrix, I decided to sort elements by column,

 (M3=apply(M2,2,sort))
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    3    7   11   16   20
[2,]    2    6    8   13   17   22
[3,]    4    9   12   14   19   23
[4,]    5   10   15   18   21   24





> set.seed(1)
> u=sample(1:(nc*nl))
> (M1=matrix(u,nl,nc))
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    7    5   11   23    6   17
[2,]    9   18    1   21   24   15
[3,]   13   19    3    8   22    2
[4,]   20   12   14   16    4   10
I had to sort elements in this matrix, by row.

> (M2=t(apply(M1,1,sort)))
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    5    6    7   11   17   23
[2,]    1    9   15   18   21   24
[3,]    2    3    8   13   19   22
[4,]    4   10   12   14   16   20
Nice, elements are sorted by row. But for symmetric reasons, I also wanted to sort them by column. So from this sorted matrix, I decided to sort elements by column,

> (M3=apply(M2,2,sort))
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    1    3    7   11   16   20
[2,]    2    6    8   13   17   22
[3,]    4    9   12   14   19   23
[4,]    5   10   15   18   21   24



























































###############################################################
















# dataframe w/ values (AllwdAmt)
df <- structure(list(X = c(2078L, 2079L, 2080L, 2084L, 2085L, 2086L, 
2087L, 2092L, 2093L, 2094L, 2095L, 4084L, 4085L, 4086L, 4087L, 
4088L, 4089L, 4091L, 4092L, 4093L, 4094L, 4095L, 4096L, 4097L, 
4098L, 4099L, 4727L, 4728L, 4733L, 4734L, 4739L, 4740L, 4741L, 
4742L, 4743L, 4744L, 4745L, 4746L, 4747L, 4748L, 4749L, 4750L, 
4751L, 4752L, 4753L, 4754L, 4755L, 4756L, 4757L, 4758L), AllwdAmt = c(34.66, 
105.56, 105.56, 473.93, 108, 1669.23, 201.5, 62.67, 61.54, 601.28, 
236.96, 108, 40.28, 29.32, 483.6, 236.96, 6072.4, 25.97, 120.9, 
61.54, 32.18, 473.93, 302.25, 0, 8.48, 3140.18, 0, 0, 6.83, 6.83, 
895.44, 895.44, 24.11, 24.11, 32.18, 32.18, 236.96, 236.96, 11.96, 
11.96, 80.08, 80.08, 3140.18, 3140.18, 163.62, 163.62, 236.96, 
236.96, 216.01, 216.01)), .Names = c("X", "AllwdAmt"), row.names = c(1137L, 
1138L, 1139L, 1140L, 1141L, 1142L, 1143L, 1144L, 1145L, 1146L, 
1147L, 1945L, 1946L, 1947L, 1948L, 1949L, 1950L, 1951L, 1952L, 
1953L, 1954L, 1955L, 1956L, 1957L, 1958L, 1959L, 2265L, 2266L, 
2267L, 2268L, 2269L, 2270L, 2271L, 2272L, 2273L, 2274L, 2275L, 
2276L, 2277L, 2278L, 2279L, 2280L, 2281L, 2282L, 2283L, 2284L, 
2285L, 2286L, 2287L, 2288L), class = "data.frame")

# get class intervals. This shows where the breaks should be.
library(classInt)
classIntervals(df$AllwdAmt, n = 10, style = 'jenks')


foo <- classIntervals(df$AllwdAmt, n=10, style='jenks')
names(foo)

foo$brks 
df

cut(df$AllwdAmt, breaks = foo$brks, labels=as.character(1:10))



















view(discodata)
view(seg.df)



aabn_xls("./statistik/R/moneca/vores/discodata_nyeste.xlsx")



view(discodata)
view(seg.qual.final)




#########################################################

























view(data)
data <- read.csv("http://protzkeule.de/data.csv")
p <- ggplot(data=data, aes(x=variable, y=meas)) + geom_tile(aes(fill=value))

p + scale_fill_gradient2(low="blue", mid="white", high="red", guide="colorbar", limits=c(-.1,.1))

p + scale_fill_gradient2(low="blue", mid="white", high="red", guide="colorbar", limits=c(-.1,.3))


library("scales")
p + scale_fill_gradientn(colours = c("blue","white","red"), 
                         values = rescale(c(-.1,0,.3)),
                         guide = "colorbar", limits=c(-.1,.3))




############ stack exchange hjælp 

rname <- c("subject one", "subject two", "subject three", "subject four","subject five")
var_percent <- c(0.51, 0.60, 0.70,0.86,0.9)
var_count <- seq(5,25,5)
mat <- cbind(var_percent, var_count)
mat <- cbind(var_percent, rname)
mat <- tbl_df(mat)
mat$var_percent <-  as.numeric(mat$var_percent)


p <- ggplot(data=mat, aes(x=var_count,y=rname,fill=var_percent)) + geom_tile(aes(fill=var_percent)) 

p + scale_fill_gradientn(colours = c("indianred","white","darkseagreen"), values=rescale(c(0.51,0.55,0.6,0.65,0.9))                       ,  guide = "colorbar")

view(mat)





ledbeskaeft.hist.count <- ggplot(hist.led,aes(x=factor(disco),y=ledbeskaeft.gns,fill=ledbeskaeft.gns)) + geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label = ledbeskaeft.gns), size = 2, hjust = -0.1, position = "dodge") +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) +
  theme(text = element_text(size=9),axis.text.y = element_text(angle=0, vjust=0, size=4)) 



ggplot(data=mat, aes(mat$var_count)) + geom_histogram() + 
scale_fill_gradientn(colours=c("red","white","green"),)





scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob.seg))), name = "intern mobilitet i segment", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab, 
                     guide="legend", values= rescale(c(0.1,0.2))   )        #)     #  guide = ""colorbar"", )





















 



############### forsøg med select af udvalgte variable



test <- 

is.matrix(mob.mat2)



library(circlize)




klynge <- 3
undergr <- 24


########## simpel 

cut.off.default <-  1 #skal måske ikke være 1 her jo
wm1            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE)
wm1[is.na(wm1)] <- 0

# wm1  <-  round(wm1, digits=0)


circlelist <-  seg$segment.list[[klynge]][[undergr]]
segmentcircle.rr <- wm1[circlelist,circlelist]
segmentcircle.tot <- mob.mat[circlelist,circlelist]
segmentcircle.tot <- cbind(segmentcircle.tot,colnames(segmentcircle.tot))
segmentcircle <- segmentcircle.tot[,-ncol(segmentcircle.tot)]
segmentcircle <- segmentcircle.rr





################ avanceret

cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) #skal måske ikke være 1 her jo
wm.desk.2            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm.desk.2[is.na(wm.desk.2)] <- 0
wm.desk.2 <- round(wm.desk.2,1)
work.list.2 <- sort(unique(unlist(lapply(work.list, function(x) which(wm.desk.2[x,] != 0)))))
sub.mat <- wm.desk.2 [work.list.2,work.list.2]
sub.mat <-  cbind(colnames(sub.mat),sub.mat)
view(sub.mat)
 


segmentcircle <- sub.mat

is.tibble(segmentcircle)

 test <- 





diag(segmentcircle) <- 0
df.c <- get.data.frame(graph.adjacency(segmentcircle,weighted=TRUE))
view(df.c)
farve <-  brewer.pal(ncol(segmentcircle),"Set1")
 # farve <- c("#000000", "#FFDD89", "#957244", "#F26223")
chordDiagram(x = df.c, 
  #grid.col = farve, 
  transparency = 0.0,
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
# farve <-  getPalette(length(circlelist))
  

########## forsøg med alle grupper 


circos.clear()
circos.par(start.degree = 90, gap.degree = 0.8, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))


getPalette = colorRampPalette(brewer.pal(12,"Paired"))






wm1            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = TRUE, small.cell.reduction = small.cell.default, diagonal=NULL)
wm1[is.na(wm1)] <- 0
# wm1  <-  round(wm1, digits=3)

# view(segmentcircle)
g <- graph.adjacency(segmentcircle,weighted=TRUE)
df <- get.data.frame(g)
chordDiagram(x = df)

farve <-  getPalette(144)


chordDiagram(x = df, grid.col = farve, transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)






##################### forsøg med circle ############


library("circlize")
##
##default chord diagram
##






########################################################



view(test)

wm2 <-  cbind(wm1,label.short[-144])
view(wm2)


seg

   seg$segment.list[[4]][[5]]


length(seg$segment.list[[4]])


 rownames(mob.mat2)


######################




2 2 2



d_1 <-  discodata$disco_1cifret 


view(d_1)





mob.mat         <- rbind(mob.mat, totalbeskaeft1997.2009)

view(mob.mat)




totalbeskaeft1996.2008[144] <- 0
# Her januar 2016: problemer med at cbind og rbind ikke længere vil 




mob.mat          <- cbind(mob.mat, totalbeskaeft1996.2008)



mob.mat      <- read.csv("./statistik/R/moneca/vores/voresdata/mobilitymatrix_1cifret_disco_udenmili.csv", row.names = 1, header = TRUE, sep = ',', fileEncoding  ="UTF-8", check.names = FALSE)



view(mob.mat2)
view(mob.mat)






wm2 <- wm1 


cut.off.default          <- 0.1
small.cell.default       <- 5



wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = TRUE, small.cell.reduction = small.cell.default, diagonal=TRUE)






help(weight.matrix)
view(wm1)
view(wm2)








ungkort.dk































##########################################################################













new_column_order <- c(144,1:143) 
wm1 <- wm1[,new_column_order] 


view(wm1)

head(wm1)

library(igraph) 


g <-  get.edgelist(g)


view(df)


view(g)








<- rearrange(wm1,V144, all())


wm1 <-  wm1 %>%
  select(V144,everything())


is.matrix(wm1)

View(test)

  <-  label.short


library(data.table)


setDT(wm1, keep.rownames = TRUE)[]



get.data.frame(wm1)



library(igraph)
set.seed(1)                # for reproducible example
myAdjacencyMatrix <- matrix(runif(400),nc=20,nr=20)
View(myAdjacencyMatrix)
is.matrix(myAdjacencyMatrix)


g  <- graph.adjacency(myAdjacencyMatrix,weighted=TRUE)

View(g)


 x <-  graph.data.frame(g)




view(g)

df <- get.data.frame(x)


head(df)
view(df)




library("migest")

cfplot_reg2

demo(cfplot_reg2, package = "migest", ask = FALSE)



1
2
dev.copy2pdf(file = "cfplot_reg2.pdf", height=10, width=10)






file.show("cfplot_reg2.pdf")






help(weight.matrix)

weight.matrix(seg, cut.off=)

##
##install packages if not already done so (uncomment)
##
# install.packages("circlize")

library("circlize")

##
##load data (df0: 2010-15 global flows, df1: meta data for plot)
##global flow data based on estimates from:
##
#
# Abel, G.J. (2016) Estimates of global bilateral migration glows by gender between 1960 and 2015. 
# Vienna Institute of Demography Working Papers 2/2016.
#

df0 <- read.csv(system.file("vidwp", "reg_flow.csv", package = "migest"), stringsAsFactors=FALSE)
df1 <- read.csv(system.file("vidwp", "reg_plot.csv", package = "migest"), stringsAsFactors=FALSE)



view(df1)

##
##default chord diagram
##

chordDiagram(x = df0)

##
##plot parameters
##

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

#increasing the gaps between sectors, start at 12 o'clock, ensure no gap between the chord and the sector at the begining
# subdue warning messages and have no margins in the plot

##
##chord diagram with user selected adjustments for bilateral migration flow data
##

chordDiagram(x = df0, grid.col = df1$col, transparency = 0.25,
             order = df1$region, directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

# First line of arguments reads in the data (df0) and sets the colours base on the meta data (df1).
# Second line provides the order of outer sectors and indicates that chords should be directional.
# Third line indicates that the direction of the chords will be illustrated by both arrows and a difference in height. The
#  height difference is negative to make the chord shorter at the end (with the arrow head).
# Fourth line ensures the annotations outside the sectors are not plotted, but provides a track measures to later add 
#  annotatinos such as labels and axis (see below).
# Fifth line indicates the plot should use big arrows, sort the chords left to right in each sector and 
#  plots the smallest chords first.

##
##add in labels and axis
##

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = df1$reg1[df1$region == sector.index]
    reg2 = df1$reg2[df1$region == sector.index]
    
    circos.text(x = mean(xlim), y = ifelse(test = nchar(reg2) == 0, yes = 5.2, no = 6.0), 
                labels = reg1, facing = "bending", cex = 1.2)
    circos.text(x = mean(xlim), y = 4.4, 
                labels = reg2, facing = "bending", cex = 1.2)
    circos.axis(h = "top", 
                major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
                minor.ticks = 1, major.tick.percentage = 0.5,
                labels.niceFacing = FALSE)
  }
)

# First line indicates that first track (rather than any other that may have been created) will be used.
# Second line ensures no borders are plotted on the track.
# Third line adds a track.
# Fourth and fifth line collect individual track meta data from plot object.
# Sixth and seventh line collect matching name information from plot data frame (df1).
# The first circos.text adds text from (reg1) either at y = 6 (if there is a second part of the name in reg2) or 5.2.
# The second circost.text adds text (reg2).
# The circos.axis add axis with major and minor ticks, without flipping the axis labels in the bottom half.

##
##Printing
##

# text(x = -1.1, y = -1, pos = 4, cex = 0.6, 
#      labels = "Based on estimates from:")
# text(x = -1.1, y = -1 - 1*0.03, pos = 4, cex = 0.6, 
#      labels = expression(paste(
#        plain(" Abel G.J. (2016) "), italic("Estimates of Global Bilateral Migration Flows by Gender")
#      )))
# text(x = -1.1, y = -1 - 2*0.03, pos = 4, cex = 0.6, 
#      labels = expression(paste(
#          italic(" between 1960 and 2015. "), plain("Vienna Institute of Demography Working Papers. 2/2016")
#      )))
# 
# dev.copy2pdf(file = "cfplot_reg2.pdf", height=10, width=10)
# file.show("./cfplot_reg2.pdf")
#  
# dev.print(png, file = "cfplot_reg2.png", height=25, width=25, units = "cm", res=1000)
# file.show("cfplot_reg2.png")






############## d. 14/09/2016





view(discodata)
view(beskaeft.samlet)


beskaeft.samlet$membership <- cbind(seg.mem.df$membership



nrow(discodata)
view(d)


is.character(beskaeft.samlet$disco)
is.character(seg.mem.df$disco)
is.character(seg.mem.df$membership)

view(seg.mem.df)

test <-  discodata %>% distinct(membership,.keep_all=TRUE) 
test <-  discodata %>% distinct(disco,.keep_all=TRUE) 

view(discodata)

nrow(test)
view(test)






unique(discodata$membership)



# seg.mem.df$membership  <-  as.factor(as.numeric(seg.mem.df$membership)) #ødelægger levels måske pga punktummet?!?
# seg.mem.df$membership  <-  as.factor(seg.mem.df$membership)

view(beskaeft.samlet)



seg.mem.df$membership
test <- distinct(seg.mem.df,membership,.keep_all=TRUE)
nrow(test)
seg.mem.df$membership
test <- distinct(seg.mem.df,membership,.keep_all=TRUE)
nrow(test)


# view(seg.mem.df)
# der er kun 49 levels hvorfor ikke 51?!?

#view(seg.mem.df)



















