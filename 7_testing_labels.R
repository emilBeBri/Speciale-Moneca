

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
                      
                      #egp-11 kort 
                      kort.test <- gg.jonas(seg, layout = lay,border.labels = seg.lab)   
                      
                      #default + scale_fill_manual(values = brewer.pal(11, "Paired"), labels=egp11_lab, name="EGP-11")
                      default + scale_fill_manual(values = skala_egp11, labels=egp11_lab, name="EGP-11") + theme(legend.position = c(0.95, 0.9))
                      
                      
                      nrow(layout)
                      
                      
                      kort.test <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                                            edge.size=edge.siz  e, vertex.fill = discodata$disco_1cifret,
                                            vertex.size = vertex_stoerrelse, border.labels = seg.lab, border.text.hjust = 0.32) +  
                        default +
                        default.disco
                      
                      
                      
                      segment.labels <- read.csv("./statistik/R/moneca/vores/voresdata/Segment_labels_DK_test_52.csv", sep = ";", encoding = "latin1")$X
                      seg.lab        <- seg.mem$membership
                      levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])
                      seg.lab        <- format(as.character(seg.lab))
                      
                      seg.lab 
                      seg.lab.2
                      seg.lab.2 = seg.lab 
                      
                      levels(seg.lab)
                      is.character(seg.lab)
                      
                      length(seg.df$membership)
                      
                      
                      is.factor((seg.mem$membership))
                      
                      levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])
                      seg.lab        <- format(as.character(seg.lab))
                      
                      
                      
                      segment.labels <- read.csv("./statistik/R/moneca/vores/voresdata/Segment_labels_DK_test.csv", sep = ";", encoding = "latin1")
                      
                      is.factor(segment.labels)
                      
                      
                      seg.lab = test 
                      length(levels(test))
                      
                      
                      
                      
                      length(discodata$seg.lab) == nrow(layout)
                      
                      test
                      
                      layout = as.data.frame(lay)
                      
                      lay
                      
                      length(border.labels) == nrow(layout)
                      
                      
                      
                      length(seg.lab)
                      
                      
                      
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
                      