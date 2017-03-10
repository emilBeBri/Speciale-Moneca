




at_levels <- Map(function(i, x) cbind(i=i, x=unlist(x)), seq_along(seg$segment.list), seg$segment.list)

 seg.mem.2 <-  aggregate(i~x, do.call("rbind", at_levels), max)
view(seg.mem.2)
view(seg.mem)



segment.list.1 <-  seg$segment.list[1]
segment.list.2 <-  seg$segment.list[2]
segment.list.3 <-  seg$segment.list[3]
segment.list.4 <-  seg$segment.list[4]
segment.list.5 <-  seg$segment.list[5]


do.call(rbind, lapply(segment.list.2, data.frame, stringsAsFactors=FALSE))

plyr::ldply(segment.list.2, data.frame)


view(append(unlist(segment.list.1),as.character(discodata$disco)))
view(unlist(segment.list.2))
view(unlist(segment.list.3))
view(unlist(segment.list.4))


view(as.numeric(strtrim(as.character(discodata$membership),1)))

view(discodata$membership)
view(discodata$disco)
discodata$disco[128]
discodata$disco[161]
discodata$disco[161]




view(unlist(segment.list.5[1]))


view(discodata)




top


seg$segment.list[[5]]




128 161

viw

suppose I have a list of lists, which consists of the same elements, although fewer and fewer as we "level up" in the grouping of elements:

level.list <-  list(
list(1,2,3,4,5,6,7,8,9,10,11,12,13,14), # base level 
list(c(1,2,3),c(4,5),c(6,7),c(13,14)),     # level 2 groups 
list(c(1,2,3,6,7),c(4,5,9)),      # level 3 groups    
list(c(4,5,9,12))    # level 4 groups 
)


so, each list in the list contains some of the elements from the list before, merging them in larger groups.  


The thing is, if a group in a list isn''t present in the "higher level" list, then that group is the final one. This is highly inconvient, since the lists elements are indexes for a dataset, that groups them at higher and higher levels, and I need to make a list, that 


If an element is present in a higher level list, the group of elements that is merged at level 2 [6,7] is merged with the group of elements [1,2,3] at level 3, then the level 2 group containing [6,7] and the other level 2 group containing [1,2,3] should not be part of the final list, since both are present in the shared group [1,2,3,6,7], and this is given priority. 

So in this toy example, I'' having a hard time finding a way to merge the first list with the other lists, removing "lower order" groups, so I get a matrix/df like this:

group.matrix <- matrix(c(
1     , "3.1" ,
2     , "3.1" ,
3     , "3.1" ,
4     , "4.1" ,
5     , "4.1" ,
6     , "3.1" ,
7     , "3.1" ,
8     , "1.1" ,
9     , "4.1" ,
10    , "1.2" ,
11    , "1.3" ,
12    , "4.1" ,
13    , "2.2" , 
14    , "2.2" 
          ), 
           nrow = 14, ncol = 2, byrow = TRUE)
colnames(group.matrix) <- c("first.level","group")

Hope this is clear to you. And that you can help me! I have to weeks to hand in my master thesis and I''m simply in over my head with this problem, but I can''t finish this   :/ .







these lists contains the index numbers of a vector, and tells me which group a given index belongs to.




list(c(1,2,3,4),c(5,6),c(7,8))


list(c(1,2,3,4),c(5,6),c(7,8))


list(c(1,2,3,4,5,6))



1,2,4,5,7,8,9,10



5,9,10






















up vote
219
down vote
accepted
The function match works on vectors :

x <- sample(1:10)
x
# [1]  4  5  9  3  8  1  6 10  7  2
match(c(4,8),x)

# [1] 1 5
match only returns the first encounter of a match, as you requested.

For multiple matching, %in% is the way to go :

x <- sample(1:4,10,replace=TRUE)
x
# [1] 3 4 3 3 2 3 1 1 2 2
which(x %in% c(2,4))
# [1]  2  5  9 10




























Examples
library(pheatmap)
# Create test matrix
test = matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
colnames(test) = paste("Test", 1:10, sep = "")
rownames(test) = paste("Gene", 1:20, sep = "")
# Draw heatmaps
pheatmap(test)
pheatmap(test, kmeans_k = 2)
pheatmap(test, scale = "row", clustering_distance_rows = "correlation")
pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
pheatmap(test, cluster_row = FALSE)
pheatmap(test, legend = FALSE)
# Show text within cells
pheatmap(test, display_numbers = TRUE)
pheatmap(test, display_numbers = TRUE, number_format = "\%.1e")
pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test)))
pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0",
"1e-4", "1e-3", "1e-2", "1e-1", "1"))
# Fix cell sizes and save to file with correct size
pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap")
pheatmap(test, cellwidth = 15, cellheight = 12, fontsize = 8)
# Generate annotations for rows and columns
annotation_col = data.frame(
CellType = factor(rep(c("CT1", "CT2"), 5)),
Time = 1:5
)
rownames(annotation_col) = paste("Test", 1:10, sep = "")
annotation_row = data.frame(
GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
)
rownames(annotation_row) = paste("Gene", 1:20, sep = "")
# Display row and color annotations
pheatmap(test, annotation_col = annotation_col)

view(annotation_col)
view(test)

pheatmap(test, annotation_col = annotation_col, annotation_legend = FALSE)
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row)
# Specify colors
ann_colors = list(
Time = c("white", "firebrick"),
CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)
pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors, main = "Title")
pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row,
annotation_colors = ann_colors)

is.factor(ann_colors$Time)

pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors[2])
# Gaps in heatmaps
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14))
pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14),
cutree_col = 2)
# Show custom strings as row/col names
labels_row = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "Il10", "Il15", "Il1b")
pheatmap(test, annotation_col = annotation_col, labels_row = labels_row)
# Specifying clustering from distance matrix
drows = dist(test, method = "minkowski")
dcols = dist(t(test), method = "minkowski")
pheatmap(test, clustering_distance_rows = drows, clustering_distance_cols = dcols)
# Modify ordering of the clusters using clustering callback option
callback = function(hc, mat){
sv = svd(t(mat))$v[,1]
dend = reorder(as.dendrogram(hc), wts = sv)
as.hclust(dend)
}
pheatmap(test, clustering_callback = callback)
## Not run:
# Same using dendsort package
library(dendsort)
callback = function(hc, ...){dendsort(hc)}
pheatmap(test, clustering_callback = callback)
## End(Not run)












































length(unique(sort(e.seg.niveau.5)))
length(unique(sort(klynge.liste.e.seg.manuel.til.fra)))


view(e.mobmat.seg.manuel.til.fra)
tail(sort(diag(e.mobmat.seg.manuel.til.fra)))


# mat.e <- wm1
mat.e <-  mob.mat[-274,-274]



rene.manuelle.klynger <- c(
  "3.25", "3.9", "5.1",  #lav manuel
  "1.204", "2.40", "3.14", "3.15", "4.8", "5.2",  #høj manuel 
  "4.4" #blandet manuelt 
  ) # "2.79", "3.24", "3.30", "4.2", "4.10"  #blandet 
klynger <- rene.manuelle.klynger

#lav liste over grupper 
tmp.df <- e.mobility(klynger)


klynger.liste.til <-  tmp.df %>% group_by(membership,indeks.seg) %>% summarise_each(funs(sum), without.mob.andel.seg=without.mob.andel  ) %>% filter(without.mob.andel.seg>=(4.7/100)) %>%  arrange(desc(membership)) %>% .[[1]] %>% as.character(.)


samlet.klynge.liste <- append(klynger.liste.til, klynger)  
work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% samlet.klynge.liste) %>%     select(indeks))))





mat.e <- mat.e[work.list, work.list]
view(mat.e)



























##
##install packages if not already done so (uncomment)
##
# install.packages("circlize")

library(migest)
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

??migest
view(df0)
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