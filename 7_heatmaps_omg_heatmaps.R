
## For pheatmap_1.0.8 and later (45 graders dimser under)
library(grid)  
library(pheatmap)
draw_colnames_45 <- function (coln, gaps, ...) {
    coord = pheatmap:::find_coordinates(length(coln), gaps)
    x = coord$coord - 0.5 * coord$size
    res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = -0.025, rot = 360-45, gp = gpar(...))
    return(res)}
## 'Overwrite' default draw_colnames with your own version 
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
ns=asNamespace("pheatmap"))



# 3.4

heat.krit.df <-  df %>% filter(membership=="3.4") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)   
# view(heat.krit.df)
heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
mat.e.result.bak <- mat.e.result
# mat.e.result <- mat.e.result.bak

mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]

# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)

annotation <- as.data.frame(heat.krit.df$segkrit)
rownames(annotation) <- heat.krit.df$disco 
colnames(annotation) <- c("klynge")

# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))
mat.e.result.bak <- mat.e.result
# mat.e.result <- mat.e.result.bak
mat.e.result[mat.e.result>=10] <- 10
pheatmap.disp <- round(mat.e.result,0)
# diag(pheatmap.disp)[] <- c("-")
pheatmap.disp[pheatmap.disp==10] <- c("10+")

klynge <- sample(iwanthue ,size=length(levels(annotation[,1])))
names(klynge) <- levels(annotation[,1])
anno_farver <- list(klynge = klynge)
breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
rownames(mat.e.result) <- strtrim(rownames(mat.e.result),30)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/seg3_4_RR_10.pdf", height = 15, width = 20)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=25,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))
dev.off()


heatlabber   <-  	  colnames(mat.e.result)

# 3.33



mat.e.result <-  e.pheat.dataprep(3,33)

relativrisiko.vector.mat.e.result  <-  as.vector(t(mat.e.result))
relativrisiko.vector.mat.e.result[relativrisiko.vector.mat.e.result<=0] <- NA
quantile(relativrisiko.vector.mat.e.result, seq(0,1,0.05),na.rm=TRUE)

heat.krit.df <-  df %>% filter(membership=="3.33") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)   
# view(heat.krit.df)
heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
mat.e.result.bak <- mat.e.result
# mat.e.result <- mat.e.result.bak

mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]

# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)

annotation <- as.data.frame(heat.krit.df$segkrit)
rownames(annotation) <- heat.krit.df$disco 
colnames(annotation) <- c("klynge")

# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))
mat.e.result.bak <- mat.e.result
# mat.e.result <- mat.e.result.bak
mat.e.result[mat.e.result>=20] <- 20
pheatmap.disp <- round(mat.e.result,0)
# diag(pheatmap.disp)[] <- c("-")
pheatmap.disp[pheatmap.disp==20] <- c("20+")

klynge <- sample(iwanthue ,size=length(levels(annotation[,1])))
names(klynge) <- levels(annotation[,1])
anno_farver <- list(klynge = klynge)
breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
# colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
# rownames(mat.e.result) <- strtrim(rownames(mat.e.result),30)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/seg3_4_RR_10.pdf", height = 15, width = 20)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=25,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))
dev.off()



############### hele det damn kort #########################
library(pheatmap)
cut.off.default <- 1
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0
mat.e.result  <-  wm1
colnames(mat.e.result) <- paste0(as.character(discodata$disco_4cifret))
rownames(mat.e.result) <- paste0(as.character(discodata$disco_4cifret))

diag(mat.e.result)[] <- 5

heat.krit.df <-  df %>% rename(segkrit=membership) %>% select(disco,segkrit)   
heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]
# pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)
annotation <- as.data.frame(heat.krit.df$segkrit)
rownames(annotation) <- heat.krit.df$disco 
colnames(annotation) <- c("segment")

mat.e.result[mat.e.result>=5] <- 5
pheatmap.disp <- round(mat.e.result,0)
diag(pheatmap.disp)[] <- c("-")
pheatmap.disp[pheatmap.disp==5] <- c("5+")
klynge <-  sample(colorRampPalette(xmen)(length(levels(annotation[,1]))),size=length(levels(annotation[,1])))
names(klynge) <- levels(annotation[,1])
anno_farver <- list(klynge = klynge)
breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))), filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/heleheatmaplortet_5.png", height = 50, width = 50)

display_numbers = pheatmap.disp, fontsize_number=10,


