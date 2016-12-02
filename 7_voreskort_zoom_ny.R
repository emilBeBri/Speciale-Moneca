#kort zoom 


#intern mobilitet
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.z <- gg.jonas(seg, layout = lay, edges=edges.default.zoom, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$within.mob,
                   vertex.size = discodata$ledbeskaeft.andel.gns,
                       border.text = FALSE,border.padding = 0.3,
                       show.text = TRUE, text.size=4, text.vjust=1.2
                       ) +   
          default.zoom + ggtitle("intern mobilitet zoom") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob))), guide = "legend", name = "intern mobilitet", breaks=intern.mob_num, labels=intern.mob_lab)




# zoom version 
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.z <- gg.jonas(seg, layout = lay, edges=edges.default.zoom, midpoint.arrow = arrow.default, 
                            edge.size=edge.size, vertex.fill = discodata$within.mob,
                            vertex.size = vertex_stoerrelse) +  
  default.zoom + ggtitle("intern mobilitet") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
  scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob))), guide = "legend", name = "% intern mobilitet\npå nodeniveau", breaks=intern.mob_num, labels=intern.mob_lab)




zoom.seg <- c(2.78)
# enkelt segment
pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/zoom/kort_intern_mob_test.pdf", height = 15, width = 15)
kort.intern.mob.z
dev.off()


zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem = seg.mem$membership == "5.1", distance=400)


lapply(p.zoom.org.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)


zoom.to.segment(kort.intern.mob.z,lay=lay, zoom.mem = seg.mem$membership == "5.1", distance=400)



zoom.to.segment(kort.intern.mob.z, lay = lay, zoom.mem = seg.mem$membership == "2.78", distance = 400)

zoom.to.segment
