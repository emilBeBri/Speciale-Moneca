




### testing 



p.seg.strong.edges      <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, edge.color = "weight", midpoints = TRUE, show.text = FALSE, vertex.fill = intern.mobilitet.seg, vertex.size = vertex_stoerrelse, border.labels = seg.lab, border.text.hjust = 0.32)



length(seg.lab)

length(lay)

seg.lab2 = as.numeric(as.character(seg.lab)


is.numeric(seg.lab2)
is.factor(seg.lab)

str(seg.lab)


if (length(border.labels) == nrow(layout)) {
  layout <- data.frame(layout, membership = border.labels)
  colnames(layout) <- c("X", "Y", "Membership")
}




seg.lab

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.seglabtest1.pdf", onefile = TRUE, height = 30, width = 30)
p.seg.strong.edges
dev.off()


,
                                     


kort.test <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                      edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                      vertex.size = vertex_stoerrelse, border.labels = seg.lab, border.text.hjust = 0.32) +  
  default +
  default.disco


seg.lab.bak = seg.lab  

segment.labels <- as.factor(seg.lab)
seg.lab        <- seg.mem$membership
# seg.lab = as.factor(seg.lab)

levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$membership))])
seg.lab        <- format(as.character(seg.lab))





discodata$seg.lab

discodata <-  discodata %>% 
  mutate(seg.lab2 = factor(seg.lab, levels = seg.df$seg.lab
  )) 

view(seg.df)



levels(discodata$membership)
levels(discodata$membership)
levels(seg.df$membership)
levels(seg.df$seg.lab)
levels(seg.df$seg.lab.original)


levels(discodata$seg.lab2)
nlevels(discodata$seg.lab2)

seg



is.character(seg.lab)
is.factor(seg.lab)

is.character(seg.lab.A)
is.factor(seg.lab.A)
length(seg.lab.A)



is.factor(discodata$seg.lab)

is.factor(segment.labels.A)

segment.labels.A <- read.csv("./statistik/R/moneca/Anton_untouched/Data/Segment_labels_DK.csv", sep = ";", encoding = "latin1")
seg.lab.A        <- seg.mem$membership
levels(seg.lab.A) <- paste(levels(seg.lab.A), as.character(segment.labels.A)[order(as.character(seg.qual.final$Membership))])
seg.lab.A        <- format(as.character(seg.lab.A))
cat.lab.A       <- paste(as.character(seg.mem$membership), " . ", rownames(mob.mat), sep = "")

seg.lab
levels(seg.lab.A)


seg.lab = seg.df$seg.lab

View(seg.lab)

length(seg.lab.A)

kort.test <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.siz  e, vertex.fill = discodata$disco_1cifret,
                       vertex.size = vertex_stoerrelse, border.labels = discodata$seg.lab, border.text.hjust = 0.32) +  
  default +
  default.disco

kort.disco <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                       vertex.size = vertex_stoerrelse, border.labels = seg.lab, border.text.hjust = 0.32) +  
  default +
  default.disco




segment.labels <- read.csv("./Data/Segment_labels_DK.csv", sep = ";", encoding = "latin1")$X
seg.lab        <- seg.mem$membership
levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])
seg.lab        <- format(as.character(seg.lab))



length(discodata$seg.lab) == nrow(layout)


layout = as.data.frame(lay)

lay

length(border.labels) == nrow(layout)







function (segmenter, niveau = seq(segmenter$segment.list), layout = layout.matrix(segmenter), 
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

############### kort model ############



#hovedkort disco
edges.default.all                <- segment.edges(seg.b, mode="directed",cut.off=3,small.cell.reduction = small.cell.default, segment.reduction = 5) #før var den 3 her og 5 nedenunder
edges.default.all[edges.default.all > 30] <- 30 

kort.disco <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse) +  
          default +
          default.disco

#kort.disco
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.disco.pdf", onefile = TRUE, height = 30, width = 30)
kort.disco
dev.off()


#egp-11 kort 
kort.egp11 <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                       vertex.size = vertex_stoerrelse) +  
  #default + scale_fill_manual(values = brewer.pal(11, "Paired"), labels=egp11_lab, name="EGP-11")
  default + scale_fill_manual(values = skala_egp11, labels=egp11_lab, name="EGP-11")
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.egp11.pdf", onefile = TRUE, height = 30, width = 30)
kort.egp11
dev.off()

########### plottrappen ############

source("./statistik/R/moneca/vores/vorescripts/7_voreskort_plottrappen.R")

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/plottrappen.disco.pdf", onefile = TRUE, height = 30, width = 30)
plottrappen.disco
dev.off()
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/plottrappen.egp11.pdf", onefile = TRUE, height = 30, width = 30)
plottrappen.egp11
dev.off()


#intern mobilitet
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$within.mob,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("intern mobilitet") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob))), guide = "legend", name = "% intern mobilitet\npå nodeniveau", breaks=intern.mob_num, labels=intern.mob_lab)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.intern.mob.pdf", onefile = TRUE, height = 25, width = 25)
kort.intern.mob
dev.off()



rescale(c(0.50,0.595,  0.625,0.659,    0.68,     0.695, 0.76,    0.80,1.00)
        c(0.50,0.585,  0.625,0.699,    0.70,     0.71, 0.78,    0.80,1.00)


#intern mobilitet.seg
kort.intern.mob.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,  
                                edge.size=edge.size, vertex.fill = discodata$within.mob.seg,
                                vertex.size = vertex_stoerrelse) +  
  default + ggtitle("intern mobilitet segment")  + scale_fill_gradientn(colours = c(    "indianred4","indianred2", "white", "darkseagreen2","darkseagreen4"),
                              values= rescale(c(0.50,0.595,  0.625,0.659,    0.68,     0.695, 0.76,    0.80,1.00)), guide="colorbar"
                              , name = "% intern mobilitet\npå segmentniveau", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)



cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_intern_mob_seg.pdf", onefile = TRUE, height = 25, width = 25)
kort.intern.mob.seg
dev.off()
 #getPalette = colorRampPalette(skala.indianred.darkseagreen)
#kort.intern.mob.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                   edge.size=edge.size, vertex.fill = discodata$within.mob.seg,
#                   vertex.size = vertex_stoerrelse) +  
#          default + ggtitle("intern mobilitet segment") +
#scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob.seg))), name = "intern mobilitet i segment", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab,
#                     guide="legend")        #)     #  guide = ""colorbar"", )




#intern mobilitet difference
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.dif <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                                edge.size=edge.size, vertex.fill = discodata$within.mob.dif,
                                vertex.size = vertex_stoerrelse) +  
  default + ggtitle("intern mobilitet") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
  scale_fill_gradientn(colours = getPalette(length(discodata$within.mob.dif)), guide = "legend", name = "intern mobilitet")
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_intern_mob_dif.pdf", onefile = TRUE, height = 25, width = 25)
kort.intern.mob.dif
dev.off()


#forskel i faerdighsniveauer
 
kort.skillvl <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.size, vertex.fill = discodata$skillvl,
                       vertex.size = vertex_stoerrelse) +  
  default + scale_fill_manual(values = skala_skillvl, labels=egp11_lab, name="faerdighedsniveau")
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.skillvl.pdf", onefile = TRUE, height = 30, width = 30)
kort.skillvl
dev.off()

skala_skillvl <-  c("dodgerblue4" ,"dodgerblue1","indianred2","indianred4")


 ##### baggrundsvariable ############

#gns. timeløn ( alle beskæftigede) 



discodata <- mutate(discodata,timelon.helepop.gns.inf.cutoff=replace(timelon.helepop.gns.inf, timelon.helepop.gns.inf>=301, 300))  

kort.timelon <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
           edge.size=edge.size, vertex.fill = discodata$timelon.helepop.gns.inf.cutoff,
           vertex.size = vertex_stoerrelse) +  
   ggtitle("timeløn") + scale_fill_gradientn(colours = skala.ired.dgreen.simple,
                                                             values= rescale(timelon.rescale), guide=guide_colorbar(barwidth = 1, barheight = 12,draw.ulim = FALSE, draw.llim = FALSE), name = "timeløn", breaks=timeloen.num, labels=timeloen.lab) +  default  +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(legend.position = c(0.95, 0.9))
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_timelon.pdf", onefile = TRUE, height = 25, width = 25)
kort.timelon 
dev.off()




kort.timelon <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                          edge.size=edge.size, vertex.fill = discodata$timelon.helepop.gns.inf.cutoff,
                          vertex.size = vertex_stoerrelse) +  
  ggtitle("timeløn") + scale_fill_gradientn(colours = skala.ired.dgreen.simple,
                                            values= rescale(timelon.rescale), guide=guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE, legend.position = c(0.7, 0.9)), name = "timeløn", breaks=timeloen.num, labels=timeloen.lab) +  default  #+
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
#theme(legend.position = c(0.9, 0.9))

 discodata$timelon.helepop.gns.inf
  
  





#timeloen 
quants = seq(0,1,0.05) 
format(round(quantile(discodata$timelon.helepop.gns.inf.cutoff, quants), digits=0), big.mark=".",decimal.mark=",",nsmall=0)
Hmisc::describe(discodata$timelon.helepop.gns.inf)
timeloen.num <- seq(150,500,50) #c(70,80)
timeloen.num[1] = c(145)
timeloen.num[length(timeloen.num)] = max(discodata$timelon.helepop.gns.inf)
timeloen.num = round(timeloen.num,0)



timeloen.num = c(146,175,200,211,225,250,275,300)
timeloen.lab <- paste(timeloen.num,"kr.")  #, "%")
timeloen.lab[4] <- c("211 kr. (median)")
timeloen.lab[1] <- c("145 kr.")
timeloen.lab[length(timeloen.num)] <- c("300+ kr.")  #, "%")
#timeloen.lab[length(timeloen.lab)] <- paste(timeloen.num[length(timeloen.lab)],"+ kr.",sep="")
timelon.rescale = c(146,175,  188,201,     211,     221,259,    271,300)
timelon.rescale = c(146,175,  188,201,     211,     221,259,    271,300)


146-300


154/7

1/7



quants = seq(0,1,c(1/7))
timelon.rescale =  round(quantile(discodata$timelon.helepop.gns.inf.cutoff,quants))
timelon.rescale = insert.at(timelon.rescale, c(4),c(211))
timelon.rescale[c(2)] = timelon.rescale[c(2)] -20
timelon.rescale[c(3,4)] = timelon.rescale[c()] +20
timelon.rescale

timelon.rescale[c(6,7)] = timelon.rescale[c(6,7)] +10
timelon.rescale





seq(146,300,19.25)
test = seq(146,300,22)

test = insert.at(test,(c(4),   c(211))
insert.at(test, c(4),c(211))
                 
                 
                 
                 
# a = c(2,3,4,9,10,2,4,19)
# b = c(2,1)
# d = c(0,1)

# insert.at(a, c(3,7), b, d) #indsætter b efter 3. element og d efter 7. element i a 
# insert.at(1:10, c(4,7,9), 11, 12, 13) #andet eksempel







timelon.rescale = 1
timelon.rescale[1] = min(discodata$timelon.helepop.gns.inf)




timelon.rescale[2:8] = quantile(discodata$timelon.helepop.gns.inf,seq(0.125,0.875,0.125))
timelon.rescale[9] = max(discodata$timelon.helepop.gns.inf)

0.50,0.595,  0.625,0.659,    0.68,     0.695, 0.76,    0.80,1.00






# til limitversion: konvertering fra timeløn til månedsløn
loenvector <- numeric()
loenvector[1] <-  140 * 160.33
loenvector[2] <-  250 * 160.33
#gns. månedsløn ( alle beskæftigede) 
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.manedslon <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$timelon.helepop.gns.inf.mndr,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("månedsløn ledige") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$timelon.helepop.gns.inf.mndr))), guide = "legend", name = "månedsløn", breaks=timelon.helepop.gns.inf.mndr_num, labels=timelon.helepop.gns.inf.mndr_lab)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_manedslon.pdf", onefile = TRUE, height = 30, width = 30)
kort.manedslon
dev.off()


#gns. alder ( alle beskæftigede) 
getPalette = colorRampPalette(skala.darkseagreen.indianred)
kort.alder <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$alder.helepop.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("alder") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$alder.helepop.gns))), guide = "legend", name = "alder")
  # , breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)


cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.alder.pdf", onefile = TRUE, height = 30, width = 30)
kort.alder
dev.off()

#køn
getPalette = colorRampPalette(c("#1874CD","#FFFAF0","#E8418B"))
kort.koen <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$koen.gns.kvinder.andel,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("køn") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$koen.gns.kvinder.andel))), guide = "legend", name = "andel kvinder")
# , breaks=koen_num, labels=koen_lab)


cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.koen.pdf", onefile = TRUE, height = 30, width = 30)
kort.koen
dev.off()


help(cairo_pdf)

#gns. ledighed ( alle beskæftigede) 
getPalette = colorRampPalette(skala.darkseagreen.indianred)
kort.ledighed <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$ledighed.helepop.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("ledighed") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$ledighed.helepop.gns))), guide = "legend", name = "ledighed", limits=c(0,60))
  # , breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.ledighed.pdf", onefile = TRUE, height = 30, width = 30)
kort.ledighed
dev.off()






##### segmenteringsproces (særlig) #########

###############
list.scales             <- list()
list.scales$size        <- scale_size_continuous(range = c(5, 17.5), guide = "none") 
#list.scales$fill        <- scale_fill_continuous(high = "black", low = "white", guide = "none")
list.scales$fill        <- scale_fill_grey(start = 0, end = 1, guide = "none")
list.scales$alpha       <- scale_alpha_continuous(guide = "none", range = c(0.05, 0.9))
s1                      <- segment.membership(seg, niveau = 1)[,2]
s2                      <- segment.membership(seg, niveau = 1:2)[,2]
s3                      <- segment.membership(seg, niveau = 1:3)[,2]
s4                      <- segment.membership(seg, niveau = 1:4)[,2]
s5                      <- segment.membership(seg, niveau = 1:5)[,2]

cs1t2                   <- as.character(s1) == as.character(s2)
cs2t3                   <- as.character(s2) == as.character(s3)
cs3t4                   <- as.character(s3) == as.character(s4)
cs4t5                   <- as.character(s4) == as.character(s5)

cs                      <- cs1t2 + cs2t3 + cs3t4 +cs4t5
cs                      <- as.factor(cs)
level.group             <- substring(as.character(s5), first = 1, last = 1)




# Vertex.fill er oprindeligt farve
pt1  <- gg.jonas(seg, layout=lay, niveau=1, edges = edges.default, vertex.fill="white", edge.color = "grey30", midpoints = FALSE, vertex.size = vertex_stoerrelse, 
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt1  <- pt1 + list.scales + ggtitle("1. niveau") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1) + theme(plot.title = element_text(size=50, face="bold"))  

pt2  <- gg.jonas(seg, layout=lay, niveau=1:2, edges = edges.default, vertex.fill=cs1t2, edge.color = "grey30", midpoints = FALSE, vertex.size = vertex_stoerrelse,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt2  <- pt2 + list.scales + ggtitle("2. niveau") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1) + theme(plot.title = element_text(size=35, face="bold")) 

pt3  <- gg.jonas(seg, layout=lay, niveau=1:3, edges = edges.default, vertex.fill=cs2t3, edge.color = "grey30", midpoints = FALSE, vertex.size = vertex_stoerrelse,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt3  <- pt3 + list.scales + ggtitle("3. niveau") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1) + theme(plot.title = element_text(size=35, face="bold")) 

pt4  <- gg.jonas(seg, layout=lay, niveau=1:4, edges = edges.default, vertex.fill=cs3t4, edge.color = "grey30", midpoints = FALSE, vertex.size = vertex_stoerrelse,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt4  <- pt4 + list.scales + ggtitle("4. niveau") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1) + theme(plot.title = element_text(size=35, face="bold")) 

pt5  <- gg.jonas(seg, layout=lay, niveau=1:5, edges = edges.default, vertex.fill=cs4t5, edge.color = "grey30", midpoints = FALSE, vertex.size = vertex_stoerrelse,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt5  <- pt5 + list.scales + ggtitle("5. niveau") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1) + theme(plot.title = element_text(size=35, face="bold")) 


pt.list <- list(pt1, pt2, pt3, pt4,pt5)
for (i in 1:5) {
  cairo_pdf(paste0("./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_seg_proces",i,".pdf",sep=""),onefile=TRUE,height=15,width=15)
  print(pt.list[[i]])
  dev.off()
}  
 

 

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_seg_proces.pdf", onefile = TRUE, height = 30, width = 30)


pt.list.udentitler = pt.list


outputlist=list()

pt.list[[1]]

length(pt.list)

levels(pt.list)



cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_seg_proces.pdf", onefile = TRUE, height = 30, width = 30)
grid.arrange(pt1, pt2, pt3, pt4, nrow=2, ncol=2)
#pt.list
dev.off()

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_pt_list.pdf", onefile = TRUE, height = 30, width = 30)
pt.list
dev.off()










#forsøg på forside 

# alle beskaeftigede 250 kat
# save(lay, file="./statistik/R/moneca/vores/voresdata/lay_FORSIDEMAASKE.Rdata")












###########################################################################
# GAMMELT OM ARBEJDSLØSE
###########################################################################


# default <- list()
# default$scale_size_continuous <-   scale_size_continuous(range = c(8, 25), name="% af totalt antal arbejdsløse", breaks=beskaeft.num, labels=beskaeft.lab)
# default$scale_alpha_continuous <-   scale_alpha_continuous(range = c(0.05, 1), guide = "none")
# default$scale_colour_continuous <-   scale_colour_continuous(high = "#8FBC8F",  low = "#DCDCDC")
# test  <-  as.numeric(strtrim(discodata$disco_4cifret, 4))
# test[104] <- 3230
# getPalette = colorRampPalette(c("#8B0A50","#DCDCDC"))
# gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = test,
#                    vertex.size = vertex_stoerrelse,show.text = FALSE) +  
#           default + ggtitle("intern mobilitet") +
#   # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$disco_4cifret))), guide = "none")



# ## sammenligning af segmenter for socio/socstil og alle beskaeftigede

# # alle beskaeftigedes membership i sociosocstil
# colourCount = length(unique(discodata$alle.beskaeft.membership))
# getPalette = colorRampPalette(brewer.pal(12, "Paired"))
# kort.sociosocstil.med.alleeskaeft.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
#                    edge.size=edge.size, vertex.fill = discodata$alle.beskaeft.membership,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("alle beskaeftigede seg.mem i socstil/socio") +
#           scale_fill_manual(values = getPalette(colourCount),name="seg")
# # save(kort.sociosocstil.med.alleeskaeft.seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.sociosocstil.med.alleeskaeft.seg.Rdata")



# # socio/socstils membership i alle beskaeftigede
# colourCount = length(unique(discodata$seg.mem.sociosocstil))
# getPalette = colorRampPalette(brewer.pal(12, "Paired"))
# kort.alleeskaeft.med.sociosocstil.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
#                    edge.size=edge.size, vertex.fill = discodata$seg.mem.sociosocstil,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("socstil/socio seg.mem i alle beskaeftigede") +
#           scale_fill_manual(values = getPalette(colourCount),name="seg")
# # save(kort.alleeskaeft.med.sociosocstil.seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.alleeskaeft.med.sociosocstil.seg.Rdata")


# # gemmer dem som objekter hver især så de kan loades og ligges i samme pdf 
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.sociosocstil.med.alleeskaeft.seg.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.alleeskaeft.med.sociosocstil.seg.Rdata")



# pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/segmentihverandre.pdf", height = 25, width = 25)
# kort.sociosocstil.med.alleeskaeft.seg
# kort.alleeskaeft.med.sociosocstil.seg + ggtitle("alle beskaeftigede seg.mem i socstil/socio")
# dev.off()



# #gennemsnitlig længde af ledighedsperioder
# getPalette = colorRampPalette(skala.darkseagreen.indianred)
# kort.ledperiod.lngde <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$ledperiod.gns.lngde.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("gennemsnitlig længde af ledighedsperiode test") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$ledperiod.antal.gns))), guide = "legend", name = "gennemsnitslængde af ledighedsperioder", breaks=ledperiod.gns.lngde.gns_num, labels=ledperiod.gns.lngde.gns_lab)

# #gennemsnitligt antal af ledighedsperioder
# # getPalette = colorRampPalette(c("#484426", "#827A41", "#C7BE80", "#FFFAF0", "#EB9DAB", "#B1394F", "#862A3B"))
# # getPalette = colorRampPalette(c("#00FF7F","#FFFAF0","#FFFF00"))
# getPalette = colorRampPalette(skala.darkseagreen.indianred)
# kort.ledperiod.antal <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$ledperiod.antal.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("gennemsnitligt antal ledighedsperioder") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$ledperiod.antal.gns))), guide = "legend", name = "gns. antal ledighedsperioder", breaks=ledperiod.antal.gns_num, labels=ledperiod.antal.gns_lab)


# pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/ledighedsperioder_sociosocstil.pdf", height = 25, width = 25)
# kort.ledperiod.lngde
# kort.ledperiod.antal
# dev.off()



# #køn
# getPalette = colorRampPalette(c("#1874CD","#FFFAF0","#E8418B"))
# kort.koen <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$koen.gns.kvinder.andel,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("køn") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$koen.gns.kvinder.andel))), guide = "legend", name = "andel kvinder", breaks=koen_num, labels=koen_lab)


# # 
# getPalette = colorRampPalette(skala.indianred.darkseagreen)
# kort.aarsloen.led <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$within.mob,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("intern mobilitet segment") +
#   # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob))), guide = "legend", name = "intern mobilitet i segment", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)



# #gns. årsløn (perindkialt alle beskæftigede) 
# getPalette = colorRampPalette(skala.indianred.darkseagreen)
# kort.aarsloen <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$perindkialt.helepop.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("årsløn ledige") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$perindkialt.helepop.gns))), guide = "legend", name = "årsløn ledige", breaks=perindkialt.helepop.gns_num, labels=perindkialt.helepop.gns_lab, limits=c(100000, 550000))
# pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/test4.pdf", height = 20, width = 20)
# kort.aarsloen
# dev.off()


# #gns. årsløn (loenmv ledige) 
# getPalette = colorRampPalette(skala.indianred.darkseagreen)
# kort.aarsloen.led <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$loenmv.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("årsløn ledige") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$loenmv.gns))), guide = "legend", name = "årsløn ledige", breaks=loenmv.gns.helepop_num, labels=loenmv.gns.helepop_lab, limits=c(100000, 400000))

# #gns. årsløn (loenmv alle beskæftigede) 
# getPalette = colorRampPalette(skala.indianred.darkseagreen)
# kort.aarsloen.pop <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$loenmv.helepop.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("årsløn population") +
# scale_fill_gradientn(colours = getPalette(length(unique(discodata$loenmv.helepop.gns))), guide = "legend", name = "årsløn population", breaks=loenmv.gns.helepop_num, labels=loenmv.gns.helepop_lab, limits=c(100000, 550000))


# pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/test4.pdf", height = 20, width = 20)
# kort.aarsloen.led
# kort.aarsloen.pop
# kort.koen
# dev.off()


# ######################################################




# #ledighedsrisiko
# kort.ledrisk <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$ledighedsrisiko,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("ledighedsrisiko") +
# scale_fill_gradientn(colours = brewer.pal(11, "PRGn"), guide = "legend", name = "ledighedsrisiko", breaks=ledrisk_num, labels=ledrisk_lab)

# #gennemsnitlig alder
# kort.alder <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
#                    edge.size=edge.size, vertex.fill = discodata$alder.gns,
#                    vertex.size = vertex_stoerrelse) +  
#           default + ggtitle("alder") +
# scale_fill_gradientn(colours = brewer.pal(11, "PRGn"), guide = "legend", name = "", breaks=alder.gns_num, labels=alder.gns_lab)




# pdf(file = "./statistik/R/moneca/vores/output/kort.test.socstil.socio.pdf", height = 25, width = 25)
# kort.ledperiod.lngde
# dev.off()
# kort.intern.mob.seg
# kort.intern.mob



# kort.alle <- list(kort.disco, kort.intern.mob,kort.intern.mob.seg,kort.aarsloen,kort.ledrisk,kort.alder,kort.koen)
# pdf(file = "./statistik/R/moneca/vores/output/kort.alle.socstil.socio.version2.pdf", height = 25, width = 25)
# kort.alle
# plottrappen
# dev.off()





