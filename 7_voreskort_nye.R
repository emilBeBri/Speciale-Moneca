

############### kort model ############

#alle 
# vertex_stoerrelse <-  vertex_stoerrelse
#ledige 
vertex_stoerrelse <-  discodata$ledbeskaeft.andel.gns



#hovedkort disco
kort.disco <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse) +  
          default +
          default.disco


#plottrappen
plottrappen <- list()
plottrappen$et <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("1. niveau")
plottrappen$to <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:2),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("2. niveau")
plottrappen$tre <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:3),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("3. niveau")
plottrappen$fire <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:4),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("4. niveau")
plottrappen$fem <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:5),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("5. niveau")


# plottrappen$seks <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:6),
#                    edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
#                    vertex.size = vertex_stoerrelse) +  
#           default +
#             default.disco + ggtitle("6. niveau")         


pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/hovedkort_sociosocstil.pdf", height = 25, width = 25)
kort.disco
#plottrappen
dev.off()

#intern mobilitet
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$intern.mobilitet,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("intern mobilitet") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
scale_fill_gradientn(colours = getPalette(length(unique(discodata$intern.mobilitet))), guide = "legend", name = "intern mobilitet", breaks=intern.mob_num, labels=intern.mob_lab)

#intern mobilitet.seg
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$within.mob,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("intern mobilitet segment") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
scale_fill_gradientn(colours = getPalette(length(unique(discodata$within.mob))), guide = "legend", name = "intern mobilitet i segment", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)


pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/intern.mobilitet_sociosocstil.pdf", height = 25, width = 25)
kort.intern.mob
kort.intern.mob.seg
dev.off()


## sammenligning af segmenter for socio/socstil og alle beskaeftigede

# alle beskaeftigedes membership i sociosocstil
colourCount = length(unique(discodata$alle.beskaeft.membership))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
kort.sociosocstil.med.alleeskaeft.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                   edge.size=edge.size, vertex.fill = discodata$alle.beskaeft.membership,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("alle beskaeftigede seg.mem i socstil/socio") +
          scale_fill_manual(values = getPalette(colourCount),name="seg")
# save(kort.sociosocstil.med.alleeskaeft.seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.sociosocstil.med.alleeskaeft.seg.Rdata")



# socio/socstils membership i alle beskaeftigede
colourCount = length(unique(discodata$seg.mem.sociosocstil))
getPalette = colorRampPalette(brewer.pal(12, "Paired"))
kort.alleeskaeft.med.sociosocstil.seg <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                   edge.size=edge.size, vertex.fill = discodata$seg.mem.sociosocstil,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("socstil/socio seg.mem i alle beskaeftigede") +
          scale_fill_manual(values = getPalette(colourCount),name="seg")
# save(kort.alleeskaeft.med.sociosocstil.seg, file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.alleeskaeft.med.sociosocstil.seg.Rdata")


# gemmer dem som objekter hver især så de kan loades og ligges i samme pdf 
load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.sociosocstil.med.alleeskaeft.seg.Rdata")
load(file="./statistik/R/moneca/vores/voresdata/seg_objekter/objekt_kort.alleeskaeft.med.sociosocstil.seg.Rdata")



pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/segmentihverandre.pdf", height = 25, width = 25)
kort.sociosocstil.med.alleeskaeft.seg
kort.alleeskaeft.med.sociosocstil.seg + ggtitle("alle beskaeftigede seg.mem i socstil/socio")
dev.off()



##### baggrundsvariable ############

#gennemsnitlig længde af ledighedsperioder
getPalette = colorRampPalette(skala.darkseagreen.indianred)
kort.ledperiod.lngde <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$ledperiod.gns.lngde.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("gennemsnitlig længde af ledighedsperiode test") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$ledperiod.antal.gns))), guide = "legend", name = "gennemsnitslængde af ledighedsperioder", breaks=ledperiod.gns.lngde.gns_num, labels=ledperiod.gns.lngde.gns_lab)

#gennemsnitligt antal af ledighedsperioder
# getPalette = colorRampPalette(c("#484426", "#827A41", "#C7BE80", "#FFFAF0", "#EB9DAB", "#B1394F", "#862A3B"))
# getPalette = colorRampPalette(c("#00FF7F","#FFFAF0","#FFFF00"))
getPalette = colorRampPalette(skala.darkseagreen.indianred)
kort.ledperiod.antal <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$ledperiod.antal.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("gennemsnitligt antal ledighedsperioder") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$ledperiod.antal.gns))), guide = "legend", name = "gns. antal ledighedsperioder", breaks=ledperiod.antal.gns_num, labels=ledperiod.antal.gns_lab)


pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/ledighedsperioder_sociosocstil.pdf", height = 25, width = 25)
kort.ledperiod.lngde
kort.ledperiod.antal
dev.off()



#køn
getPalette = colorRampPalette(c("#1874CD","#FFFAF0","#E8418B"))
kort.koen <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$koen.gns.kvinder.andel,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("køn") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$koen.gns.kvinder.andel))), guide = "legend", name = "andel kvinder", breaks=koen_num, labels=koen_lab)


#gns. årsløn (loenmv ledige) 

#årsløn
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.aarsloen.led <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$loenmv.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("årsløn ledige") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$loenmv.gns))), guide = "legend", name = "årsløn ledige", breaks=loenmv.gns.helepop_num, labels=loenmv.gns.helepop_lab, limits=c(100000, 400000))

#gns. årsløn (loenmv alle beskæftigede) 

#årsløn
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.aarsloen.pop <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$loenmv.helepop.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("årsløn population") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$loenmv.helepop.gns))), guide = "legend", name = "årsløn population", breaks=loenmv.gns.helepop_num, labels=loenmv.gns.helepop_lab, limits=c(100000, 550000))


pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/test4.pdf", height = 20, width = 20)
kort.aarsloen.led
kort.aarsloen.pop
kort.koen
dev.off()


######################################################




#ledighedsrisiko
kort.ledrisk <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$ledighedsrisiko,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("ledighedsrisiko") +
scale_fill_gradientn(colours = brewer.pal(11, "PRGn"), guide = "legend", name = "ledighedsrisiko", breaks=ledrisk_num, labels=ledrisk_lab)

#gennemsnitlig alder
kort.alder <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$alder.gns,
                   vertex.size = vertex_stoerrelse) +  
          default + ggtitle("alder") +
scale_fill_gradientn(colours = brewer.pal(11, "PRGn"), guide = "legend", name = "", breaks=alder.gns_num, labels=alder.gns_lab)




pdf(file = "./statistik/R/moneca/vores/output/kort.test.socstil.socio.pdf", height = 25, width = 25)
kort.ledperiod.lngde
dev.off()
kort.intern.mob.seg
kort.intern.mob



kort.alle <- list(kort.disco, kort.intern.mob,kort.intern.mob.seg,kort.aarsloen,kort.ledrisk,kort.alder,kort.koen)
pdf(file = "./statistik/R/moneca/vores/output/kort.alle.socstil.socio.version2.pdf", height = 25, width = 25)
kort.alle
plottrappen
dev.off()









