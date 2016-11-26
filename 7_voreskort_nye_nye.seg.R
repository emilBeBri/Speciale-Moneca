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


# fagforeningsmedlemskab seg
kort.fagf.seg  <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,  
                                edge.size=edge.size, vertex.fill = discodata$fagf.seg.gns.ja.andel,
                                vertex.size = vertex_stoerrelse) +  
  default + ggtitle("intern mobilitet segment")  + scale_fill_gradientn(colours = c(    "indianred4","indianred2", "white", "darkseagreen2","darkseagreen4"))
                                                                        
                                                                        
                                                                        ,
                                                                        values= rescale(c(0.50,0.595,  0.625,0.659,    0.68,     0.695, 0.76,    0.80,1.00)), guide="colorbar"
                                                                        , name = "% intern mobilitet\npå segmentniveau", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)



cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_fagf_seg.pdf", onefile = TRUE, height = 25, width = 25)
kort.fagf.seg
dev.off()




