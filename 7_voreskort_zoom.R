

#intern mobilitetd
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.z <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$intern.mobilitet,
                   vertex.size = discodata$ledbeskaeft.andel.gns,
                       border.text = FALSE,border.padding = 0.3,
                       show.text = TRUE, text.size=4, text.vjust=1.2
                       ) +   
          default.zoom + ggtitle("intern mobilitet zoom") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$intern.mobilitet))), guide = "legend", name = "intern mobilitet", breaks=intern.mob_num, labels=intern.mob_lab)

# save(kort.intern.mob.z, file="./statistik/R/moneca/vores/voresdata/objekt_kort.intern.mob.z.Rdata")
# load(file="./statistik/R/moneca/vores/voresdata/objekt_kort.intern.mob.z.Rdata.Rdata")


zoom.seg <- c(2.78)
# enkelt segment
pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/zoom/kort_intern_mob_test.pdf", height = 15, width = 15)
zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem=discodata$membership==2.78, distance=400)
dev.off()


## flere segmenter efter hinanden

zoom.seg.fler <- c(3.5,5.4)


pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/zoom/kort_intern_mob_test.pdf", height = 15, width = 15)
for(i in zoom.seg.fler){
p.z <-  zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem=seg.mem$membership==i, distance=800)
print(p.z)
}
dev.off()


## alle segmenter (virker ikke af gud ved hvilken årsag)


membership.z <- seg.df$membership
membership.z <- as.numeric(as.character(membership.z))
membership.z <-  subset(membership.z, membership.z >= 2)


pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg4.pdf", height = 15, width = 15)
for(i in membership.z){
p.z <-  zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem=seg.mem$membership==i, distance=800)
print(p.z)
}
dev.off()

#### flere segmenter på en gang samme kort

zoom.seg.m <- c("3.3","4.6")

# enkelt segment
pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg3.pdf", height = 15, width = 15)
zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem=seg.mem$membership %in% zoom.seg.m, distance=800)
dev.off()



### forsøg med loops, virker slet ikke




kort.intern.mob.z.i <- list() 
counter <- 0 
for(i in membership.z){    
kort.intern.mob.z.i[[counter]] <-  kort.intern.mob.z + ggtitle(paste("Segment :", i))
counter <- counter+1
}


letters[seq( from = 1, to = 10 )]


counter 


kort.intern.mob.z.i <- list()

kort.intern.mob.z.i[[1]] <-  kort.intern.mob.z + ggtitle(paste("Segment :", 5.3))

kort.intern.mob.z.i[[2]] <-  kort.intern.mob.z + ggtitle(paste("Segment :", 3.5))

kort.intern.mob.z.i <-  kort.intern.mob.z + ggtitle(paste("Segment :", 5.3))


pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg.alle.test4.pdf", height = 15, width = 15)
# zoom.to.segment(kort.intern.mob.z.i[1], lay=lay, zoom.mem=seg.mem$membership=="5.3", distance=800)
zoom.to.segment(kort.intern.mob.z.i, lay=lay, zoom.mem=seg.mem$membership=="3.5", distance=800)
dev.off()





pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg.alle.test2.pdf", height = 15, width = 15)
for(i in membership.z){
p.z <-  zoom.to.segment(kort.intern.mob.z.i, lay=lay, zoom.mem=seg.mem$membership==i, distance=800)
print(p.z)
}
dev.off()







length(kort.intern.mob.z.i)


for(j in length(membership.z)){
for(i in membership.z){  
kort.intern.mob.z.i[j] <-  kort.intern.mob.z + ggtitle(paste("Segment :", i))
}
}














kort.intern.mob.z.i <- list() 
for(j in length(membership.z)){
  

}
}

kort.intern.mob.z.i <- list() 
for(i in membership.z){
for(j in length(membership.z)){
kort.intern.mob.z.i[j] <-  kort.intern.mob.z + ggtitle(paste("Segment :", i))


j
i



kort.intern.mob.z.i <- NULL 

str(kort.intern.mob.z.i)


pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomsegtest4.pdf", height = 15, width = 15)
print(kort.intern.mob.z.i)
dev.off()



+
 




pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg.alle.test.pdf", height = 15, width = 15)
for(i in membership.z){
p.z <-  zoom.to.segment(kort.alder.z, lay=lay, zoom.mem=seg.mem$membership==i, distance=800)
print(p.z)
}
dev.off()


