


v(mat.e.result)
names(mat.e.result)


v(df)
  

library(imager)
library(deldir)
library(dplyr)
library(ggplot2)
library(scales)
 
# Download the image
file="http://ereaderbackgrounds.com/movies/bw/Frankenstein.jpg"


download.file(file, destfile = "frankenstein.jpg", mode = 'wb')





 
# Read and convert to grayscale
load.image("emiltest.jpg") %>% grayscale() -> x
 
# This is just to define frame limits
x %>% 
  as.data.frame() %>% 
  group_by() %>% 
  summarize(xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) %>% 
  as.vector()->rw
 
# Filter image to convert it to bw
x %>%
  threshold("45%") %>% 
  as.data.frame() -> df
 
# Function to compute and plot Voronoi tesselation depending on sample size
doPlot = function(n)
{
  #Voronoi tesselation
  df %>% 
  sample_n(n, weight=(1-value)) %>% 
  select(x,y) %>% 
  deldir(rw=rw, sort=TRUE) %>% 
  .$dirsgs -> data
 
  # This is just to add some alpha to lines depending on its longitude
  data %>% 
    mutate(long=sqrt((x1-x2)^2+(y1-y2)^2),
         alpha=findInterval(long, quantile(long, probs = seq(0, 1, length.out = 20)))/21)-> data
 
  # A little bit of ggplot to plot results
  data %>% 
    ggplot(aes(alpha=(1-alpha))) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color="black", lwd=1) +
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0), trans=reverse_trans())+
    theme(legend.position  = "none",
            panel.background = element_rect(fill="white"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())->plot
 
return(plot)
}
 
# I call the previous function and store resulting plot in jpeg format
i=150000
name=paste0("emil",i,".jpeg")
jpeg(name, width = 600, height = 800, units = "px", quality = 100)
doPlot(i)
dev.off()




 
# Once all images are stored I can create gif
library(magick)
frames=c()
images=list.files(pattern="jpeg")
 
for (i in length(images):1)
{
  x=image_read(images[i])
  x=image_scale(x, "300")
  c(x, frames) -> frames
}
animation=image_animate(frames, fps = 2)
image_write(animation, "Frankenstein.gif")











































#intern mobilitet
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.z <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                   edge.size=edge.size, vertex.fill = discodata$intern.mobilitet,
                   vertex.size = discodata$ledbeskaeft.andel.gns,
                       border.text = FALSE,border.padding = 0.3,
                       show.text = TRUE, text.size=4, text.vjust=1.2
                       ) +   
          default.zoom + ggtitle("intern mobilitet zoom") +
scale_fill_gradientn(colours = getPalette(length(unique(discodata$intern.mobilitet))), guide = "legend", name = "intern mobilitet", breaks=intern.mob_num, labels=intern.mob_lab)


zoom.seg <- c(3.5)

# enkelt segment
pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg3.pdf", height = 15, width = 15)
zoom.to.segment(kort.intern.mob.z, lay=lay, zoom.mem=seg.mem$membership==zoom.seg, distance=400)
dev.off()


## flere segmenter

zoom.seg.fler <- c(3.5,5.4)


pdf(file = "./statistik/R/moneca/vores/output/zoom/socio_socstil/zoomseg3.pdf", height = 15, width = 15)
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


