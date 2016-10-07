# Plots
#setwd("./statistik/R/moneca/vores/")
#source("./statistik/R/moneca/vores/2_analyse_untouched.R")
#save.image("./statistik/R/moneca/vores/workspaces/altfremtil3_plots_untouched.RData")
#load("./statistik/R/moneca/vores/workspaces/altfremtil3_plots_untouched.RData")
#getwd()

############
# TODO
# Skal der andre farver til grupperingen?
# Reducer padding ? 
# Zoom plots har cuttede labels - kan vi haandtere det?

# Layout
lay                          <- beskaeftigede.andel.seg[-l, 1:2]
#View(lay)
#LO.andel.seg[-l, 1:2]
lay[, 1]                     <- lay[, 1] * -10000
lay[, 2]                     <- lay[, 2] * 10000
lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000) #fler niveauer p? attraction hvis vi kommer over 5 niveauer
lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000)
# save(lay, file = "./statistik/R/moneca/vores/layout_test.Rda")
# write.csv(lay, file = "./statistik/R/moneca/vores/output_emil/layout_se.csv")
# den layout fil man f?r ud ved hj?lp af de her scripts er ikke den samme som den der l? der i forvejen.
#load(file = "./statistik/R/moneca/vores/layout.Rda")



# Attributes
# Blaa / Lilla
vertex.fill.high     <- "navyblue"      
vertex.fill.low      <- "whitesmoke"
vertex.na            <- "navyblue"      

# Roed
# vertex.fill.high     <- "red"      
# vertex.fill.low      <- "white"

edge.color.high      <- "#081d58"
edge.color.high      <- "#49006a"

edge.color.low       <- "#ffffd9"
vertex.shape         <- 21
edge.size            <- 0.7

gradient.colors      <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", 
                          "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")

gradient.colors      <- c(brewer.pal(5, "RdBu")[5:1])

gradient.gul         <- c("white", brewer.pal(7, "YlOrBr"))
gradient.lilla       <- c("white", "white", "white", brewer.pal(9, "RdPu"))
gradient.lilla2      <- c("white",brewer.pal(5, "RdPu"))
gradient.roed         <- c("white","white", brewer.pal(9, "Reds"))
gradient.roed.fald    <- c("white", brewer.pal(8, "Reds"))
gradient.groen        <- c("white", brewer.pal(9, "Greens"))
gradient.blaa         <- c("white", brewer.pal(7, "Blues"))
gradient.blaa         <- gradient.blaa[length(gradient.blaa):1]

#save.image("./statistik/R/moneca/vores/workspaces/tmp.RData")
#load("./statistik/R/moneca/vores/workspaces/tmp.RData")

# Edges
#help(segment.edges)  #skaber koordinater p? edges, ved ikke helt hvad det vil sige, men noget med ggplot2s tegning af grafikken g?r jeg ud fra. vigtigt? #sp?rganton
edges.default                <- segment.edges(seg.b, small.cell.reduction = small.cell.default)
edges.default[edges.default > 5] <- 5 
#help(segment.edges)
#write.table(edges.default, file="./statistik/R/moneca/vores/output/edges.default1.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
edges.default.all                <- segment.edges(seg.b, small.cell.reduction = small.cell.default, segment.reduction = 1)
#View(edges.default.all)

edges.default.all[edges.default.all > 5] <- 5 #hvorfor goer han det her?
#write.table(edges.default, file="./statistik/R/moneca/vores/output_emil/edges.default2.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)


defaults                     <- list()
defaults$scale_size          <- scale_size_continuous(range = c(5, 29), guide = "none")
defaults$scale_alpha         <- scale_alpha_continuous(range = c(0.05, 0.2), guide = "none")
defaults$scale_colour        <- scale_colour_continuous(high = edge.color.high,  low = edge.color.low, guide = "none")
#defaults$legend              <- theme(legend.position = "none")

gradient                     <- list()
gradient$scale_gradient      <- scale_fill_gradientn(colours = gradient.colors, limits = c(0, 1), guide = "legend", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1))
gradient$guide               <- guides(fill = guide_legend(override.aes = list(size = 5)))

guide.default                <- list()
guide.default$fill           <- guides(fill = guide_legend(override.aes = list(size = 5)))

arrow.default                <- arrow(angle = 15, length = unit(0.30, "cm"), ends = "last", type = "closed")


###########################################################
# Plots

# Trappen 3
#source("./statistik/R/moneca/vores/vorescripts/3_a_trappen_3.R")
# Trappen 4
#source("./statistik/R/moneca/vores/vorescripts/3_a_trappen_4.R")
# Trappen 5
source("./statistik/R/moneca/vores/vorescripts/3_a_trappen_5.R")
# Trappen 6
#source("./statistik/R/moneca/vores/vorescripts/3_a_trappen_6.R")




# Hovedkort

p.seg.tydelige.edges.no.border    <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, 
                                              edge.color = edge.color.high, show.text = TRUE, vertex.size = stor.beskaeftigede, vertex.fill = intern.mobilitet.seg,
                                              midpoint.arrow = arrow.default, show.borders = FALSE, border.text = FALSE, border.alpha = 0) + scale_alpha_continuous(range = c(0.4, 1))
p.seg.tydelige.edges.no.border    <- p.seg.tydelige.edges.no.border + ggtitle("Alle forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default

p.seg.tydelige.edges    <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, 
                                    edge.color = edge.color.high, show.text = TRUE, vertex.size = stor.beskaeftigede, vertex.fill = intern.mobilitet.seg,
                                    midpoint.arrow = arrow.default) + scale_alpha_continuous(range = c(0.4, 1))


p.seg.tydelige.edges    <- p.seg.tydelige.edges + ggtitle("Alle forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default

p.seg.strong.edges      <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, edge.color = "weight", midpoints = TRUE, show.text = FALSE, vertex.fill = intern.mobilitet.seg, vertex.size = stor.beskaeftigede,
                                    border.labels = seg.lab, border.text.hjust = 0.32) 
p.seg.strong.edges      <- p.seg.strong.edges + ggtitle("Intern mobilitet og staerke forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default + scale_color_gradientn(colours = gradient.lilla2, guide = "none")


p.seg.tydelige.edges.labels    <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, 
                                           edge.color = edge.color.high, show.text = TRUE, vertex.size = stor.beskaeftigede, vertex.fill = intern.mobilitet.seg,
                                           midpoint.arrow = arrow.default, border.labels = seg.lab, border.text.hjust = 0.32) + scale_alpha_continuous(range = c(0.4, 1))
p.seg.tydelige.edges.labels    <- p.seg.tydelige.edges.labels + ggtitle("Alle forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default
