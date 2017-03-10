

mobilitetanalysemotor(manuelle.klynger.lav)







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
pheatmap(test, cellwidth = 15, cellheight = 12, fontsize = 8, filename = "test.pdf")
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
6 pheatmap
# Display row and color annotations
pheatmap(test, annotation_col = annotation_col)
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


























#################################################

DAT <- matrix(c(
771,1176,1018,1042,275,949,823,1250,348,622,1244,
465,984,766,790,573,784,592,1250,1222,421,1103,
171,288,194,80,547,528,249,959,72,81,224,
132,122,90,50,44,206,127,108,52,59,144,
0,9,5,9,0,10,17,0,0,0,0),
           nrow = 5, ncol = 11, byrow = TRUE)

group <-  c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","grp9","grp9")

annotation_c <- as.data.frame(group)

colnames(DAT) <- paste(group," ", "no.",seq_len(11),sep="")
rownames(DAT) <- paste(rep(c("grp"),5),91:95,sep="_")

rownames(annotation_c) <- colnames(DAT)
# annotation_r <- as.data.frame(rownames(DAT))

pheatmap::pheatmap(DAT, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE ,  
    annotation_col=annotation_c)




















rest.gruppe.disco.df <-  matrix(ncol=2,nrow=4)



rest.gruppe.disco.df[,1] <- rest.gruppe.disco.v
colnames(rest.gruppe.disco.df[,1]) <- c(Resterende erhvervsgrupper)


mat.e.result <-  mat.e.result[,beskaeringsindeks.tmp]



rowSums(mat.e.result) + rest.gruppe.disco.df[,1] == test.af.restgruppe

view(mat.e.result)
rownames(mat.e.result)


###############################################





is.numeric(test[,1])

is.numeric(test.m[,1])

test <-  tbl_df(test)


test.m <-  matrix(ncol=ncol(test),nrow=nrow(test))

test.m <- tbl_df(test.m)

test.m[,1] <- unname(unlist(test[,1]))



vektor  <-  unname(unlist(test[,1]))



vektor  <-  as.data.frame(vektor)




is.numeric(vector)



vektor  <- test[,1]

names(vektor)


is.numeric(vektor)

#########################
test   <-  mat.e.result[,summeringsindeks]
rowSums.test <- rowSums(test)
colnames.tmp <-  discodata$indeks[which(discodata$disco %in% colnames(test))]
colnames.tmp <-  as.character(discodata$membership[which(discodata$disco %in% colnames(test))])
colnames(test) <- colnames.tmp
test  <-  test[,order(colnames(test))]
r <- rle(colnames(test))
changes <- rep(seq_along(r$lengths), r$lengths)
test <-  t(test)
################################













is.data.frame(test.2)

is.numeric(test.2[,2])




is.character(test[,1])


as.numeric[test]


for





is.data.frame(test)



test <- cbind(test,rownames(test))


colnames(test)[ncol(test)] <- c("membership")


head(test)



aggregate(test[, 1:4], list(test[,5]), sum)



data.frame(test)



d <- read.table(text='Name     Month  Rate1     Rate2
Aira       1      12        23
Aira       2      18        73
Aira       3      19        45
Ben        1      53        19
Ben        2      22        87
Ben        3      19        45
Cat        1      22        87
Cat        2      67        43
Cat        3      45        32', header=TRUE)






<

# rownames.tmp <- rownames(test)
# colnames.tmp <- colnames(test)
test.2 <-  matrix(as.numeric(test),ncol=ncol(test),nrow=nrow(test))
# rownames(test.2) <- rownames.tmp# colnames(test.2) <- colnames.tmp

test <-  data.frame(test) 




DAT <- matrix(c(
771,1176,1018,1042,275,949,823,1250,348,622,1244,
465,984,766,790,573,784,592,1250,1222,421,1103,
171,288,194,80,547,528,249,959,72,81,224,
132,122,90,50,44,206,127,108,52,59,144,
0,9,5,9,0,10,17,0,0,0,0),
           nrow = 5, ncol = 11, byrow = TRUE)

grouping.vector <-  paste(rep(c("grp"),11),seq_len(11),sep="")

DAT  <-  t(DAT)
DAT <- tbl_df(DAT) 

DAT$membership <- grouping.vector


aggregate(DAT[, 1:4], list(DAT$membership), sum)



is.numeric(DAT$V1)


is.numeric(DAT[,1])







DAT <- as.numeric(DAT) # doesn't work 







xlsx::write.xlsx(DAT, "./desperateattempt.xlsx", sheetName="Sheet1", col.names = TRUE, row.names = FALSE, append = FALSE, showNA = TRUE)
test.2 <-  readxl::read_excel("./desperateattempt.xlsx")







is.numeric(DAT[,1])







mat.e.result[,11]
view(mat.e.result)


DAT <- matrix(c(
771,1176,1018,1042,275,949,823,1250,348,622,1244,
465,984,766,790,573,784,592,1250,1222,421,1103,
171,288,194,80,547,528,249,959,72,81,224,
132,122,90,50,44,206,127,108,52,59,144,
0,9,5,9,0,10,17,0,0,0,0),
           nrow = 5, ncol = 11, byrow = TRUE)

group <-  c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","grp9","grp9")

annotation_c <- as.data.frame(group)

colnames(DAT) <- paste(group," ", "no.",seq_len(11),sep="")
rownames(DAT) <- paste(rep(c("grp"),5),91:95,sep="_")

# annotation_r <- as.data.frame(rownames(DAT))

pheatmap::pheatmap(DAT, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE ,  
    annotation_col=annotation_c)



    # gaps_row=append(first.changes(rownames(DAT)),length(rownames(DAT))), gaps_col=first.changes(strtrim(colnames(DAT),6)),
Error in check.length("fill") : 
  'gpar' element 'fill' must not be length 0





    ,annotation_row=annotation)




dat














antal.disco <- nrow(heat.krit.df)
antal.seg <- length(seg.klynger)



til.heat.krit.df <- tbl_df(matrix(append(sort(seg.klynger),sort(seg.klynger)), 
           nrow = length(seg.klynger), ncol = 2, byrow = FALSE))

colnames(til.heat.krit.df) <- c("disco","membership")
heat.krit.df$membership <- as.character(heat.krit.df$membership)
heat.krit.df$disco <- as.character(heat.krit.df$disco)
index_for_seg <-  seq ((1+ncol(mat.e.result))-length(seg.klynger), ncol(mat.e.result), 1 ) 
colnames(mat.e.result)[index_for_seg]  <-  sort(seg.klynger)

# her afgøres rækkefølgen 
heat.krit.df <-  rbind(heat.krit.df,til.heat.krit.df)
antal.ialt <- nrow(heat.krit.df)
heat.krit.df$sortingraekkefoelge <- seq_len(nrow(heat.krit.df))
heat.krit.df$sortingraekkefoelge[seq((antal.ialt-antal.seg+1),antal.ialt,1)] <- seq_len(antal.seg)
heat.krit.df$sortingraekkefoelge[seq_len(antal.disco)] <- seq((antal.seg+1),antal.ialt,1)


heat.krit.df <-  arrange(heat.krit.df,sortingraekkefoelge)
korrekt <- match(colnames(mat.e.result),heat.krit.df$disco)
mat.e.result <-  mat.e.result[order(korrekt), order(korrekt)]
# max(as.vector(mat.e.result)) # fint 


save.image("./statistik/R/moneca/vores/voresdata/tmp4_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp4_allebeskaeft250.Rdata")


# #annotation og gaps dataframes 
    # til_anno_og_gaps_df <-  df %>% filter(indeks %in% as.numeric(names(Tset[which((Tset%in%pop))]))) %>% select(disco,membership) %>% arrange (membership)
    # # view(til_anno_og_gaps_df)
    
    # # til gaps 
    # heatmap.gaps <- til_anno_og_gaps_df %>% arrange (membership) %>% select(membership)
    # # view(heatmap.gaps)
    
    # til.heatmap.gaps <- tbl_df(matrix(sort(seg.klynger), nrow = 9, ncol = 1, byrow = FALSE))
    # colnames(til.heatmap.gaps) <- c("membership")
    # # ncol(heatmap.gaps)
    # # ncol(til.heatmap.gaps)
    # heatmap.gaps <-  rbind(til.heatmap.gaps,heatmap.gaps)
    
    # heatmap.gaps <- heatmap.gaps %>% arrange(membership)
    
    
    
    # # view(heatmap.gaps)
    # #til anno 
    # heatmap.anno <-  til_anno_og_gaps_df
    # til.heatmap.anno <- tbl_df(matrix(sort(seg.klynger ), nrow = 9, ncol = 2, byrow = FALSE))
    # colnames(til.heatmap.anno) <- c("disco","membership")
    # heatmap.anno <-  rbind(til.heatmap.anno,heatmap.anno)
    # heatmap.anno$membership <- as.factor(heatmap.anno$membership)
    # heatmap.anno_row <-    data.frame(heatmap.anno$membership)
    # rownames(heatmap.anno_row) <- heatmap.anno$disco
    # colnames(heatmap.anno_row) <- c("segment")
    
    # #annotationsfarver
    # anno_farver = list(segment = sample(xmen ,size=length(levels(heatmap.anno_row$segment)),replace=TRUE) )
    # names(anno_farver$segment) <- as.character(unique(heatmap.anno_row$segment))











mob.mat[151,249] # gamle 


mat.e.tmp[151,249] #ny spicy version 


999999  8311 Lokomotiv- og elektrofoererarbejde
9999999 8312 Arbejde m. styring af togtrafik, rangeringsarbejde samt jernbanebetjentarbejde



aug.work.list[92]
discodata$disco[151]
which(irr.job.indices %in% aug.work.list[92]) 

irr.job.indices[119]

mat.e.result


  y  <-  mat.e.result
  diag(y) <- 0
  min(colSums(y))
  colSums(y)
  which(colSums(y) %in%  c(7))







 names(
 klynger
 which(pop.seg %in% Tset.seg)
 l(pop.seg)
 unlist(pop.seg)[6]
 pop.seg <-  diag(e.mobmat.seg.niveau.5)
 


 which(pop.seg %in%  )
 which(c(1,2,3) %in% c(1,2,3,4)  )
 names(pop.seg)[niveau.1.seg.indeks]
 which((Tset%in%pop.seg))
 pop.seg%in%Tset
 seg.klynger <-  rev(sort(klynger))





length(seg.klynger)==length(seg.index) # de to skal være lige lange ellers problemer 
length(seg.klynger)+length(ikke.seg.index)==ncol(mat.e.result) #ligeledes her 

view(mat.e.result)










## omorganiser matrix 

mat.e <- mob.mat[-274,-274]
work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks))))
resten  <-  setdiff(seq_len(273),work.list)
sort.list.indeks  <-  append(work.list, resten)
korrekt <-  match(seq_len(273),sort.list.indeks)
mat.e <- mat.e[order(korrekt), order(korrekt)]
view(mat.e)



# REAL_aug_work_list <-  read_excel("./statistik/R/moneca/vores/voresdata/REAL_aug_work_list.xlsx")
REAL_aug_work_list   <- read.csv("./statistik/R/moneca/vores/voresdata/REAL_aug_work_list.csv", sep = ";")
view(sort(REAL_aug_work_list$REAL_aug_work_list))
view(as.character(test.list)) # du kan godt stole på aug.work.list. fint. hvad er det så, der er galt? 

v










test.list.1 <- sort(test.list.1)
view(sort(test.list.1))


l(aug.work.list)




test.list.2 <- sort(test.list.2)
l(test.list.2)
test.list.2 <- matrix(test.list.2)
test.list.2 <- cbind(test.list.2,test.list.2 %in% test.list.1)
view(test.list.2)


view(test.list.2)














view(discodata$disco[94])


l(names(Tset[which((Tset%in%pop))]))

seg.index <- unlist(lapply(names(Tset[which(!(Tset%in%pop))]), function(x) grep(paste0(x), colnames(mat.e.result))))








which(duplicated(klynge.liste.e.seg.manuel.til.fra))







l()



names((Tset[which((Tset%in%pop))]))




sdf

length(seg.klynger)+length(ikke.seg.index)==ncol(mat.e.result) 




ikke.seg.index


work.list




match()







length(unlist(klynge.liste.samlet.indeks))==length(work.list)
# De to her skal være ens. Det er de, når man fjerner "segmenter" med kun en discogruppe, dvs niveau 1 

length(which(duplicated(klynge.liste.e.seg.manuel.til.fra))) +length(klynger) - length(work.list) # de her skal være 0 trukket fra hinanden 

view(mat.e.result)




[151,249]



jobnames <-  c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")
jobdat <- matrix(c(
99, 5, 5, 0, 0, 5, 0,
5, 99, 2, 5, 5, 0, 0,
1, 5, 99, 5, 0, 0, 1,
1, 0, 5, 99, 8, 0, 1,
0, 5, 0, 0, 99, 5, 1,
0, 0, 5, 5, 0, 99, 5,
5, 0, 0, 0, 5, 1, 99
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(jobnames,jobnames
                ))
jobdat
mat.e <-  jobdat 
work.list <- c(1,2)










library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)

# use the NPR story data file ---------------------------------------------
# and be kind to NPR's bandwidth budget
url <- "http://apps.npr.org/dailygraphics/graphics/women-cs/data.csv"
fil <- "gender.csv"
if (!file.exists(fil)) download.file(url, fil)
 
gender <- read.csv(fil, stringsAsFactors=FALSE)

gender <- mutate_each(gender, funs(as.numeric))

colnames(gender) <- str_replace(colnames(gender), "\\.", " ")
 
gender_long <- mutate(gather(gender, area, value, -date),
                      area=factor(area, levels=colnames(gender)[2:5],
                                  ordered=TRUE))

gender_colors <- c('#11605E', '#17807E', '#8BC0BF','#D8472B')
names(gender_colors) <- colnames(gender)[2:5]

chart_title <- expression(atop("What Happened To Women In Computer Science?",
                               atop(italic("% Of Women Majors, By Field"))))
 
gg <- ggplot(gender_long)
gg <- gg + geom_line(aes(x=date, y=value, group=area, color=area))
gg <- gg + scale_color_manual(name="", values=gender_colors)
gg <- gg + scale_y_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL, title=chart_title)
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg

last_vals <- sapply(colnames(gender)[2:5], function(x) last(na.exclude(gender[,x])))
last_date <- tail(gender$date)+1 # doing this ^ wld have made it a double
names(last_vals)

gg <- gg + theme(legend.position="none")
gg <- gg + theme(plot.margin = unit(c(1, 7, 2, 1), "lines"))

for (i in 1:length(last_vals)) {
  gg <- gg + annotation_custom(grob=textGrob(names(last_vals)[i], hjust=0,
                                             gp=gpar(fontsize=8, 
                                                     col=gender_colors[names(last_vals)[i]])),
                               xmin=2014, xmax=2014,
                               ymin=last_vals[i], ymax=last_vals[i])
}

gb <- ggplot_build(gg)
gt <- ggplot_gtable(gb)
 
gt$layout$clip[gt$layout$name=="panel"] <- "off"
 
grid.draw(gt)


































































mat.e <- wm1
klynger <- c("3.8")
work.list <-  sort(as.vector(unlist(seg.selector.df %>% filter(membership %in% klynger) %>%     select(indeks))))
################ avanceret1: segment + ties 
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))


work.list <-  c(8,24,98,153,154,155,258,259)



################ avanceret2: segment + ties (uden edges ml ties) 
irr.job.indices <- which(!(seq_len(273) %in% work.list))
## first, keep diagonal values for irr.job.indices
dvals <- diag(mat.edges)[irr.job.indices]
## set sub-matrix to zero (this will also set diagnal elements to zero)
mat.edges[irr.job.indices,irr.job.indices] <- 0
diag(mat.edges)[irr.job.indices] <- dvals




























> 
asdf

segment.edges <- 
function (segmenter, cut.off = 1, mode = "directed", niveau = seq(segmenter$segment.list), 
    segment.reduction = seq(segmenter$segment.list), method = "all", 
    top = 3, diagonal = NULL, small.cell.reduction = 0) 
{
    mx <- segmenter$mat.list[[1]]

########## mit bidrag hehe 

    klynger <- c("5.2","5.1","4.8")

    work.list <-  sort(as.vector(unlist(seg.selector.df %>% filter(membership %in% klynger) %>%     select(indeks))))
################ avanceret1: segment + ties 
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mx[,x] != 0)))))
mx <- mx[aug.work.list, aug.work.list]
################ avanceret2: segment + ties (uden edges ml ties) 
irr.job.indices <- which(!(aug.work.list %in% work.list))
## first, keep diagonal values for irr.job.indices
dvals <- diag(mx)[irr.job.indices]
## set sub-matrix to zero (this will also set diagnal elements to zero)
mx[irr.job.indices,irr.job.indices] <- 0
## replace diagonal elements
diag(mx)[irr.job.indices] <- dvals



    seg <- segmenter
    seg$segment.list <- segmenter$segment.list[niveau]
    seg$mat.list <- segmenter$mat.list[niveau]
    mx.edges <- weight.matrix(mx, cut.off = cut.off, symmetric = FALSE, 
        diagonal = diagonal, small.cell.reduction = small.cell.reduction)
    mx.edges[is.na(mx.edges)] <- 0
    if (identical(segment.reduction, 0) == FALSE) {
        segments <- unlist(seg$segment.list[segment.reduction], 
            recursive = FALSE)
        for (i in 1:length(segments)) {
            mx.edges[segments[[i]], segments[[i]]] <- 0
        }
    }
    if (identical(method, "top.out")) {
        mx.sort <- matrix(nrow = top, ncol = ncol(mx.edges))
        mx.sort[1:top, ] <- apply(mx.edges, 1, sort, decreasing = TRUE)[1:top, 
            ]
        for (i in 1:(nrow(mx.edges))) {
            mx.edges[i, ][(mx.edges[i, ] %in% mx.sort[, i]) == 
                FALSE] <- 0
        }
    }
    if (identical(method, "top.in")) {
        mx.sort <- matrix(nrow = top, ncol = ncol(mx.edges))
        mx.sort[1:top, ] <- apply(mx.edges, 2, sort, decreasing = TRUE)[1:top, 
            ]
        for (i in 1:(nrow(mx.edges))) {
            mx.edges[, i][(mx.edges[, i] %in% mx.sort[, i]) == 
                FALSE] <- 0
        }
    }
    return(mx.edges)
}
<environment: namespace:MONECA>


###################

layout=

gg.emil <-  function (

segmenter =   seg

  segmenter, niveau = seq(segmenter$segment.list), layout = layout.matrix(segmenter), 
    edges = log(segment.edges(segmenter) + 1), mode = "directed", 
    vertex.size = "total", vertex.fill = "segment", vertex.alpha = 1, 
    vertex.color = "black", vertex.shape = 21, show.edges = TRUE, 
    edge.size = 1, edge.alpha = "weight", edge.color = "weight", 
    edge.line = "solid", show.text = TRUE, text.size = 3, text.color = "black", 
    text.alpha = 1, text.vjust = 1.5, show.borders = TRUE, border.size = 1, 
    border.fill = NA, border.color = "black", border.alpha = 1, 
    border.padding = 1, border.text = TRUE, border.labels = "segments", 
    border.text.size = 4, border.text.color = "black", border.text.vjust = -0.2, 
    border.text.hjust = 1, midpoints = TRUE, midpoint.arrow = arrow(angle = 20, 
        length = unit(0.33, "cm"), ends = "last", type = "closed"), 
    legend = "side", ...) 
{
    if (identical(border.labels, "segments")) {
        membership <- segment.membership(segmenter, niveau = niveau)[, 
            2]
        layout <- data.frame(layout, membership = membership)
        colnames(layout) <- c("X", "Y", "Membership")
    }
    if (length(border.labels) == nrow(layout)) {
        layout <- data.frame(layout, membership = border.labels)
        colnames(layout) <- c("X", "Y", "Membership")
    }
    niveau <- niveau[niveau != 1]
    seg <- segmenter
    seg$segment.list <- segmenter$segment.list[niveau]
    seg$mat.list <- segmenter$mat.list[niveau]
    segments <- unlist(seg$segment.list, recursive = FALSE)
    mat.edges <- edges

#     ######## mit bidrag hehe 

# work.list <- c(
# 173,174,175,176,177,178,179,180,208,209,210,211,212,214,215,216,217,218,220
# ,221,222,223,224,225,226,227,231,236,237,238,239,240,241,242,243,244,245,246
# ,247,253,254,260,270,271,272)

# aug.work.list <- c(1,5,6,7,21,22,23,27,41,77,81,82,84,86,87,102,134,154
# ,160,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183
# ,185,186,187,188,189,190,191,192,193,194,195,197,198,199,200,202,207,208
# ,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,226
# ,227,228,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245
# ,246,247,250,252,253,254,255,257,258,259,260,262,263,265,266,267,268,269
# ,270,271,272,273)

# #mat.edges <- mat.edges[aug.work.list, aug.work.list]
# ################ avanceret2: segment + ties (uden edges ml ties) 
# irr.job.indices <- which(!(aug.work.list %in% work.list))
# ## first, keep diagonal values for irr.job.indices
# dvals <- diag(mat.edges)[irr.job.indices]
# ## set sub-matrix to zero (this will also set diagnal elements to zero)
# mat.edges[irr.job.indices,irr.job.indices] <- 0
# ## replace diagonal elements
# diag(mat.edges)[irr.job.indices] <- dvals

mat.edges <- 0


## slut 

    gra.edges <- graph.adjacency(mat.edges, mode = mode, weighted = TRUE, 
        diag = NULL)
    scale_modifications <- list()
    if (identical(edge.color, "weight")) {
        edge.color <- E(gra.edges)$weight
        scale_modifications$edge.color <- scale_color_continuous(high = "darkblue", 
            low = "azure1")
    }
    if (identical(edge.alpha, "weight")) 
        edge.alpha <- E(gra.edges)$weight
    if (identical(vertex.fill, "segment")) {
        vertex.fill <- segment.membership(segmenter, niveau = niveau)$membership
        scale_modifications$vertex.fill <- scale_fill_discrete(guide = "none")
    }
    if (identical(vertex.size, "total")) {
        mat <- segmenter$mat.list[[1]]
        totals <- (mat[nrow(mat), ] + mat[, nrow(mat)])/2
        totals <- totals[-length(totals)]
        vertex.size <- totals
        scale_modifications$vertex.size <- scale_size_continuous(range = c(4, 
            10))
    }
    if (identical(vertex.size, "col.total")) {
        col.total <- data.frame(t(segmenter$mat.list[[1]]))$Total
        vertex.size <- row.total[-length(col.total)]
        scale_modifications$vertex.size <- scale_size_continuous(range = c(4, 
            10))
    }
    p <- graph.plot(gra.edges, layout = layout, vertex.color = vertex.color, 
        vertex.fill = vertex.fill, vertex.shape = vertex.shape, 
        vertex.size = vertex.size, vertex.alpha = vertex.alpha, 
        edges = show.edges, edge.color = edge.color, edge.alpha = edge.alpha, 
        edge.size = edge.size, edge.line = edge.line, edge.order = FALSE, 
        text = show.text, text.size = text.size, text.colour = text.color, 
        text.alpha = text.alpha, legend = legend, text.vjust = text.vjust, 
        midpoints = midpoints, midpoint.arrow = midpoint.arrow)
    circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
        r = diameter/2
        tt <- seq(0, 2 * pi, length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
    }
    segment.circles.hull <- function(layout, group, diameter) {
        x <- layout[group, 1:2]
        membership.seg <- unique(as.character(layout$Membership[group]))
        list.of.circles <- apply(x, 1, circleFun, diameter = diameter)
        all.circle.coordinates <- do.call(rbind, list.of.circles)
        circle.hull <- all.circle.coordinates[chull(all.circle.coordinates), 
            ]
        cbind(circle.hull, group = runif(1, min = 0, max = 999999999), 
            membership = membership.seg)
    }
    annotate_segments <- function(layout, seg.list, diameter, 
        border.alpha) {
        segment.circles <- lapply(seg.list, segment.circles.hull, 
            layout = layout, diameter = max(layout[, 1:2])/diameter)
        segment.circles <- do.call(rbind, segment.circles)
        annotate(geom = "polygon", x = segment.circles$x, y = segment.circles$y, 
            group = segment.circles$group, fill = NA, color = "black", 
            alpha = border.alpha)
    }
    if (identical(show.borders, TRUE)) {
        list.annotate <- lapply(seg$segment.list, annotate_segments, 
            layout = layout, diameter = (1/border.padding) * 
                20, border.alpha = border.alpha)
        p <- p + list.annotate
    }
    if (identical(border.text, TRUE) & length(niveau) > 0) {
        border.padding.diameter <- max(layout[, 1:2])/((1/border.padding) * 
            20)
        seg.circles <- list()
        for (i in 1:length(seg$segment.list)) {
            segment.circles <- lapply(seg$segment.list[[i]], 
                segment.circles.hull, layout = layout, diameter = border.padding.diameter)
            segment.circles <- do.call(rbind, segment.circles)
            seg.circles[[i]] <- segment.circles
        }
        segment.circles <- do.call(rbind, seg.circles)
        max.circles <- aggregate(segment.circles$y, by = list(segment.circles$membership), 
            FUN = max)
        max.segments <- segment.circles[(segment.circles$y %in% 
            max.circles$x) & (segment.circles$membership %in% 
            max.circles$Group.1), ]
        max.segments$xend <- max.segments$x + ((border.padding.diameter * 
            2) * (border.text.size/3.9))
        list.annotate <- list(annotate(geom = "text", x = max.segments$xend, 
            y = max.segments$y, label = max.segments$membership, 
            color = border.text.color, size = border.text.size, 
            vjust = border.text.vjust, hjust = border.text.hjust), 
            annotate(geom = "segment", x = max.segments$x, xend = max.segments$xend, 
                y = max.segments$y, yend = max.segments$y, color = border.color, 
                alpha = border.alpha))
        p <- p + list.annotate
        tab.mem <- table(layout$Membership)
        singles.layout <- layout[layout$Membership %in% names(tab.mem)[tab.mem == 
            1], ]
        singles.layout <- data.frame(x = singles.layout$X, y = singles.layout$Y, 
            group = runif(1, min = 0, max = 999999999), membership = singles.layout$Membership)
        singles.layout$y <- singles.layout$y + (border.padding.diameter * 
            0.25)
        singles.layout$xend <- singles.layout$x + ((border.padding.diameter * 
            2) * (border.text.size/3.9))
        singles.layout$x <- singles.layout$x + (border.padding.diameter * 
            0.25)
        list.annotate <- list(annotate(geom = "text", x = singles.layout$xend, 
            y = singles.layout$y, label = singles.layout$membership, 
            color = border.text.color, size = border.text.size, 
            vjust = border.text.vjust, hjust = border.text.hjust), 
            annotate(geom = "segment", x = singles.layout$x, 
                xend = singles.layout$xend, y = singles.layout$y, 
                yend = singles.layout$y, color = border.color, 
                alpha = border.alpha))
        p <- p + list.annotate
    }
    p + scale_modifications
}
<environment: namespace:MONECA>




###  Graph plot
graph.plot <- function(graph, layout = layout.fruchterman.reingold(graph),
                       vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
                       edges = TRUE, edge.color = "black", edge.alpha = 0.2, edge.size = 1, edge.line = "solid", edge.order = FALSE,
                       text = FALSE, text.size = 3, text.colour = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
                       midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed")){
  
  
  
  vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
  
  vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
  v.i                     <- unlist(lapply(vertex.l, length)) == 1
  vertex.attributes       <- vertex.l[v.i]
  vertex.aes              <- vertex.l[v.i==FALSE]
  vertex.aes$x            <- vertex.coords$x
  vertex.aes$y            <- vertex.coords$y
  
  
  if(identical(edges, TRUE)){
    
    edge.coords             <- edge.coord(graph, layout)
    edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
    e.i                     <- unlist(lapply(edge.l, length)) == 1
    edge.attributes         <- edge.l[e.i]
    edge.attributes$lineend <- "butt"
    edge.aes                <- edge.l[e.i==FALSE]
    edge.aes$x              <- edge.coords$start.x
    edge.aes$y              <- edge.coords$start.y
    edge.aes$xend           <- edge.coords$slut.x
    edge.aes$yend           <- edge.coords$slut.y
    
    if(identical(edge.order, FALSE) == FALSE){
      edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
    } 
  }
  
  if(identical(midpoints, TRUE)){
    midpoint.attributes         <- edge.attributes
    midpoint.attributes$arrow   <- midpoint.arrow 
    midpoint.aes                <- edge.aes
    midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
    midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
    
#     ax                          <- edge.coords$slut.x - midpoint.aes$x
#     ay                          <- edge.coords$slut.y - midpoint.aes$y
#     crazy                       <- (edge.coords$slut.y / 10000) * 0.001
#     els                         <- edge.coords$slut.y < midpoint.aes$y

# Her finder bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
# l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
# x3 = x1 + (1/l) * (x2 - x1)
# y3 = y1 + (1/l) * (y2 - y1)

    L                            <- sqrt(((edge.coords$slut.x - midpoint.aes$x)^2) + ((edge.coords$slut.y - midpoint.aes$y)^2))
    midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
    midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
    #midpoint.aes$xend           <- midpoint.aes$x + ((ax / ay) * crazy)
    #midpoint.aes$yend           <- midpoint.aes$y + crazy
#    midpoint.aes$yend[els]      <- midpoint.aes$y[els] - crazy
    midpoint.aes$group          <- paste(midpoint.aes$x, midpoint.aes$y)
    
    }
  
  text.l                  <- list(size=text.size, color=text.colour, alpha=text.alpha, vjust=text.vjust, lineheight=1)
  t.i                     <- unlist(lapply(text.l, length)) == 1
  text.attributes         <- text.l[t.i]
  text.aes                <- text.l[t.i==FALSE]
  text.aes$x              <- vertex.coords$x
  text.aes$y              <- vertex.coords$y
  text.aes$label          <- rownames(vertex.coords)
  
  # Plot edges
  p <- ggplot()
  
  if(identical(edges, TRUE)){
    edge.attributes$mapping     <- do.call("aes", edge.aes)
    p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
  }
  
  # Plot midpoints
  
  if(identical(midpoints, TRUE)){
    midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
    p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
  }
  
  # Plot vertices
  vertex.attributes$mapping     <- do.call("aes", vertex.aes)
  p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
  
  # Plot text
  if(text==TRUE){
    text.attributes$mapping     <- do.call("aes", text.aes)
    p <- p + do.call("geom_text", text.attributes, quote=TRUE)
  }
  
  # Formatting
  p <- p + theme_bw()
  p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
  
  if(legend == "bottom")  p <- p + theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal")
  if(legend == "none")    p <- p + theme(legend.position = "none")
  
  p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
  
  
  
}

vertex.coord <- function(graph, layout=layout.fruchterman.reingold(graph)){
  rownames(layout)  <- V(graph)$name
  layout            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  layout
}















##########################
























plot.df <-  beskaeft.tid.seg.df %>%     filter(
    membership=="3.9"     |
    membership=="5.1"       | 
    membership=="3.25"      | 
    membership=="3.24"      |
    membership=="3.15"      | 
    membership=="5.2"       | 
    membership=="4.8"     |
    membership=="3.10"      | 
    membership=="4.4"
  ) %>% gather(key, value, -membership) %>%   rename(segmenter=membership)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) +
                      geom_line(size=1.3) +
                      # geom_point(size=2) + 
                     xlab("") + ylab("") +
                     theme_tufte() +
                     theme(panel.grid.major= element_line(colour="black", size=0.05)) +
                      scale_colour_manual(values=colorRampPalette(xmen)(length(unique(as.matrix(plot.df[,1]))))) +
                      geom_rangeframe(color="black",size=0.80) +
                      scale_y_continuous(breaks = extended_range_breaks()(plot.df$value))




















library(treemap)
library(highcharter)

tree.df <- select(df, disco_1cifret, disco,beskaeft.andel.gns)
tree.df$label <- paste(tree.df$disco,"(",sep=" ")
tree.df$label <- paste(tree.df$label,round2(tree.df$beskaeft.andel.gns*100,1),"%",")",sep="")
tree.map <- treemap(tree.df, index = c("disco_1cifret","label"), vSize = "beskaeft.andel.gns", palette = "HCL",force.print.labels = TRUE,fontsize.labels = c(25,12), title="")
# TEST_ <- treemap(select(df, disco,beskaeft.andel.gns), index = "disco", vSize = "beskaeft.andel.gns", palette = "Spectral",type="index")
# hTEST_hc <- highchart() %>% hctreemap(TEST_, name = "urine", layoutAlgorithm = "squarified") %>% 
#   hc_title(text = "Composition of human urine (50 g  dry weight / L)")










325938- 22748








library(pheatmap)

mat.e.result  <-  e.pheat.dataprep(2,77)
heat.krit.df <-  df %>% filter(membership==3.20 | membership==3.4 |membership==3.18  ) %>% select(disco,membership,klasse_oesch16)
   
heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df <-  gdata::drop.levels(heat.krit.df)
mat.e.result <-  mat.e.result[order(heat.krit.df$membership), order(heat.krit.df$membership)]
# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)
annotation <- as.data.frame(heat.krit.df$klasse_oesch16)
rownames(annotation) <- heat.krit.df$disco 
colnames(annotation) <- c("klynge")
# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))
mat.e.result.bak <- mat.e.result 
mat.e.result <- mat.e.result.bak 
mat.e.result[mat.e.result>=25] <- 25
pheatmap.disp <- round(mat.e.result,0)
pheatmap.disp[pheatmap.disp==25] <- c("25+")
klynge <- sample(xmen ,size=length(levels(annotation[,1])))
names(klynge) <- levels(annotation[,1])
anno_farver <- list(klynge = klynge)
breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
rownames(mat.e.result) <- strtrim(rownames(mat.e.result),15)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/seg3_4_RR_10.pdf", height = 15, width = 20)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=25,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$membership)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$membership)))))
dev.off()


































group.list <- list(grp1=c(1, 3), grp2=c(2, 4))
Given this list, construct a grouping vector

# initial grouping
groups <- seq_len(ncol(jobdat))
# map elements of second list item to values of first list item
groups[match(group.list[["grp2"]], groups)] <- group.list[["grp1"]]

groups
[1] 1 1 3 3 5 6 7
So, now groups 1 and 2 are the same as well as 3 and 4. Now, we use rowsum and a couple of transposes to calculate the output.

myMat <- t(rowsum(t(rowsum(jobdat, groups)), groups))
# add the group names
dimnames(myMat) <- list(group.jobnames,group.jobnames)

myMat
            job 1 and 2 job 3 and 4 job 5 job 6 job 7
job 1 and 2          20          12     5     6    10
job 3 and 4           7          20     8     0     2
job 5                 5           0     5     5     1
job 6                 0          10     0     5     5
job 7                 1           0     5     1     5
In response to the OP's comments below, the grouping was intended to be within list elements, rather than corresponding positions between list elements as I had originally interpreted. To accomplish this form a grouping, a repeated feeding of replace to Reduce will accomplish the task. '


With group.list as in the question,

group.list <- list(grp1=c(1, 2), grp2=c(3, 4))
group.list <- list(c(1=c(1, 2), 2=c(3, 4))








groups <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)),
                 c(list(groups), unname(group.list)))


groups
[1] 1 1 3 3 5 6 7








view(df)




















obnames <-  c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")
jobdat <- matrix(c(
5, 5, 5, 0, 0, 5, 5,
5, 5, 2, 5, 5, 1, 5,
1, 5, 5, 5, 0, 0, 1,
1, 0, 5, 5, 8, 0, 1,
0, 5, 0, 0, 5, 5, 1,
0, 0, 5, 5, 0, 5, 5,
0, 1, 0, 0, 5, 1, 5
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(jobnames,jobnames
                ))

group.list  <-  list(grp1=c(1,2) ,grp2 =c(3,4))


group.jobnames <-  c("job 1 and 2","job 3 and 4","job 5","job 6","job 7")

 group.jobdat <- matrix(c(
            20,12,5,6,10,
            7,17,8,0,2,
            5,0,5,5,1,
            0,10,0,5,5,
            1,0,5,1,5
            ),
           nrow = 5, ncol = 5, byrow = TRUE,
           dimnames = list(group.jobnames,group.jobnames
                ))




 group.list <- list(grp1=c(1, 2), grp2=c(3,4))


 # initial grouping
groups <- seq_len(ncol(jobdat))
# map elements of second list item to values of first list item
groups[match(group.list[["grp2"]], groups)] <- group.list[["grp2"]]


help(match)

x1 <- rowsum(jobdat, groups)
x2 <- colsum((x1), groups)

myMat <- t(x2)


help(rowsum)





# add the group names
dimnames(myMat) <- list(group.jobnames,group.jobnames)

































###############################
# initial grouping
groups <- seq_len(ncol(jobdat))
# map elements of second list item to values of first list item
groups[match(group.list[["grp2"]], groups)] <- group.list[["grp1"]]



groups
[1] 1 1 3 3 5 6 7


myMat <- t(rowsum(t(rowsum(jobdat, groups)), groups))
# add the group names
dimnames(myMat) <- list(group.jobnames,group.jobnames)

myMat
            job 1 and 2 job 3 and 4 job 5 job 6 job 7
job 1 and 2          20          12     5     6    10
job 3 and 4           7          20     8     0     2
job 5                 5           0     5     5     1
job 6                 0          10     0     5     5
job 7                 1           0     5     1     5











jobnames <-  c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")
jobdat <- matrix(c(
5, 5, 5, 0, 0, 5, 5,
5, 5, 2, 5, 5, 1, 5,
1, 5, 5, 5, 0, 0, 1,
1, 0, 5, 5, 8, 0, 1,
0, 5, 0, 0, 5, 5, 1,
0, 0, 5, 5, 0, 5, 5,
0, 1, 0, 0, 5, 1, 5
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(jobnames,jobnames
                ))


group.list  <-  list(grp1=c(1,2) ,grp2 =c(3,4))



group.jobnames <-  c("job 1 and 2","job 3 and 4","job 5","job 6","job 7")

 group.jobdat <- matrix(c(
            20,12,5,6,10,
            7,17,8,0,2,
            5,0,5,5,1,
            0,10,0,5,5,
            1,0,5,1,5
            ),
           nrow = 5, ncol = 5, byrow = TRUE,
           dimnames = list(group.jobnames,group.jobnames
                ))


































#############################################

















Hi,

I have a matrix, which represents mobility between various jobs:

jobnames <-  c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")
 jobdat <- matrix(c(
5, 5, 5, 0, 0, 5, 5,
5, 5, 2, 5, 5, 1, 5,
1, 5, 5, 5, 0, 0, 1,
1, 0, 5, 5, 8, 0, 1,
0, 5, 0, 0, 5, 5, 1,
0, 0, 5, 5, 0, 5, 5,
0, 1, 0, 0, 5, 1, 5
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(jobnames,jobnames
                ))


This is treated as a directed, weighted adjacency matrix in a social network analysis. The direction of the network is from rows to columns: So mobility is defined as going from a job-row to a job-column. The diagonal is relevant, since it is possible to change to the same job in another firm.


I need to collapse this matrix according to a prefigured list
containing the index of the jobs that should be combined:

group.list  <-  list(grp1=c(1,4) ,grp2 =c(2,5))


Now, since it is an adjacency matrix, it's a bit different than the other ' answers about how to collapse a matrix that I've ' found here and elsewhere. The collapse has to be simultanious on both the rows and the columns.  So the result should be like this:

 group.jobdat <- matrix(c(
            20,12,5,6,10,
            7,17,8,0,2,
            5,0,5,5,1,
            0,10,0,5,5,
            1,0,5,1,5
            ),
           nrow = 5, ncol = 5, byrow = TRUE,
           dimnames = list(group.jobnames,group.jobnames
                ))

Thank you, 





group.jobnames <-  c("job 1 and 2","job 3 and 4","job 5","job 6","job 7")

 jobdat <- matrix(c(
           295,  20,   0,    0,    0,    5,    7,
           45,   3309, 15,   0,    0,    0,    3,
           23,   221,  2029, 5,    0,    0,    0,
           0,    0,    10,   100,  8,    0,    3,
           0,    0,    0,    0,    109,  4,    4,
           0,    0,    0,    0,    4,    375,  38,
           0,    18,   0,    0,    4,    26,   260), 



rownames(jobdat) <- 
rownames


seg  <-   seg.original


seg.foer <- seg

for (i in 2:length(seg$segment.list)) {
segment.list.tmp <-   seg$segment.list[i]
work.list.korrekt <-  sort(unlist(segment.list.tmp))
seg$mat.list[[i]]  <- seg$mat.list[[1]][work.list.korrekt,work.list.korrekt]
}



sort(unlist(seg.original$segment.list[2]))


anton




seg.original$segment.list[2]

  



test <-  segment.quality(seg.foer, final=TRUE)
view(test)

view(seg$mat.list[2])




sum(as.numeric(test$`Share of mobility`))
sum(as.numeric(test$`Share of total size`))


seg.original[]



segment.quality






view(seg.df)

view(df)


Hmisc::describe(df$`2: Nodes`)


238



view(segmenter$mat.list[[1]])














seg[1][1] 
seg[2] 

length(seg)



seg$segment.list
seg$small.cell.reduction
view(seg$mat.list[1])
view(seg$mat.list)







 $segment.list[[2]]



















#################################################################










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



















