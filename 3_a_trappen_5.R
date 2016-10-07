
# Trappen 5
trappe.niveaus          <- list("1" = 1, "2" = seq(1:2), "3" = seq(1:3), "4" = seq(1:4), "5" = seq(1:5)) 
plot.trappe.list        <- lapply(trappe.niveaus, gg.jonas, layout = lay, segmenter = seg, edges = edges.default.all, legend = "none", show.text = FALSE, edge.size = 0.5, midpoint.arrow = arrow.default )

###############
list.scales             <- list()
list.scales$size        <- scale_size_continuous(range = c(2, 4.5), guide = "none")
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

cs                      <- cs1t2 + cs2t3 + cs3t4 + cs4t5
cs                      <- as.factor(cs)
level.group             <- substring(as.character(s5), first = 1, last = 1)

# Vertex.fill er oprindeligt farve
pt1  <- gg.jonas(seg, layout=lay, niveau=1, edges = edges.default, vertex.fill="white", edge.color = "grey30", midpoints = FALSE, vertex.size = 4, 
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt1  <- pt1 + list.scales + ggtitle("1st Level") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)

pt2  <- gg.jonas(seg, layout=lay, niveau=1:2, edges = edges.default, vertex.fill=cs1t2, edge.color = "grey30", midpoints = FALSE, vertex.size = 4,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt2  <- pt2 + list.scales + ggtitle("2nd Level") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)

pt3  <- gg.jonas(seg, layout=lay, niveau=1:3, edges = edges.default, vertex.fill=cs2t3, edge.color = "grey30", midpoints = FALSE, vertex.size = 4,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt3  <- pt3 + list.scales + ggtitle("3rd Level") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)

pt4  <- gg.jonas(seg, layout=lay, niveau=1:4, edges = edges.default, vertex.fill=cs3t4, edge.color = "grey30", midpoints = FALSE, vertex.size = 4,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt4  <- pt4 + list.scales + ggtitle("4th Level") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)

pt5  <- gg.jonas(seg, layout=lay, niveau=1:5, edges = edges.default, vertex.fill=cs4t5, edge.color = "grey30", midpoints = FALSE, vertex.size = 4,
                 edge.size = 0.3, border.padding = 1.5, show.text = FALSE, border.text.size = 3, border.text = FALSE)
pt5  <- pt5 + list.scales + ggtitle("5th Level") + annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)



pt.list <- list(pt1, pt2, pt3, pt4, pt5)

pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/segmentlevelsBW_sociosocstil.pdf", height = 25, width = 25)
pt.list
dev.off()



