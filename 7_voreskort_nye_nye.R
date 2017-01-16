



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


#egp11 kort, klasse 
kort.egp11 <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                       vertex.size = vertex_stoerrelse) +  
  #default + scale_fill_manual(values = brewer.pal(11, "Paired"), labels=egp11_lab, name="EGP-11")
  default + scale_fill_manual(values = skala_egp11, labels=egp11_lab, name="EGP-11") + theme(legend.position = c(0.9, 0.9)) 
#+ theme(legend.key.width ="2 cm", legend.key.height="2 cm",legend.text=50,legend.key.size=100 )


cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_egp11.pdf", onefile = TRUE, height = 30, width = 30)
kort.egp11
dev.off()

#oesch16 kort, klasse

# oesch

skala_oesch16 <-  c("dodgerblue4", "dodgerblue2", "dodgerblue1","mediumpurple4","mediumpurple1","firebrick4","firebrick2","grey","grey71","yellow3","yellow1","olivedrab4","olivedrab1","darkorange3" ,"darkorange1")


skala_oesch16 <- c("yellow4", "yellow3","yellow1", # selvstændige
"mediumpurple4", "mediumpurple1", # teknikere -  mediumpink? slateblue?
"firebrick4","firebrick1", #arbejdere
"dodgerblue4", "dodgerblue1", #managers
"grey40", "grey88", #clerks
"palegreen4", "palegreen1", #sociokulturelle
"rosybrown4", "rosybrown1") #servicearbejdere
kort.oesch16 <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
                       edge.size=edge.size, vertex.fill = discodata$klasse_oesch16,
                       vertex.size = vertex_stoerrelse) +  
  #default + scale_fill_manual(values = brewer.pal(11, "Paired"), labels=egp11_lab, name="EGP-11")
  default + scale_fill_manual(values = skala_oesch16, labels=oesch16_lab, name="Oesch 16") + theme(legend.position = c(0.9, 0.9)) 
#+ theme(legend.key.width ="2 cm", legend.key.height="2 cm",legend.text=50,legend.key.size=100 )
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_oesch16.pdf", onefile = TRUE, height = 25, width = 25)
kort.oesch16
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

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_intern_mob.pdf", onefile = TRUE, height = 30, width = 30)
kort.intern.mob
dev.off()



rescale(c(0.50,0.595,  0.625,0.659,    0.68,     0.695, 0.76,    0.80,1.00)
        c(0.50,0.585,  0.625,0.699,    0.70,     0.71, 0.78,    0.80,1.00)





#intern mobilitet difference
getPalette = colorRampPalette(skala.indianred.darkseagreen)
kort.intern.mob.dif <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                                edge.size=edge.size, vertex.fill = discodata$within.mob.dif,
                                vertex.size = vertex_stoerrelse) +  
  default + ggtitle("intern mobilitet") +
  # scale_fill_gradientn(colours = c("#575155",brewer.pal(8, "Purples")), guide = "legend", name = "intern mobilitet")
  scale_fill_gradientn(colours = getPalette(length(discodata$within.mob.dif)), guide = "legend", name = "intern mobilitet")
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_intern_mob_dif.pdf", onefile = TRUE, height = 30, width = 30)
kort.intern.mob.dif
dev.off()


#forskel i faerdighsniveauer
#kort.skillvl <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default,
#                       edge.size=edge.size, vertex.fill = discodata$skillvl,
#                       vertex.size = vertex_stoerrelse) +  
#  default + scale_fill_manual(values = skala_skillvl, labels=egp11_lab, name="faerdighedsniveau")
#cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort.skillvl.pdf", onefile = TRUE, height = 30, width = 30)
#kort.skillvl
#dev.off()



 ##### baggrundsvariable ############

#gns. timeløn ( alle beskæftigede) 

discodata <- mutate(discodata,timelon.mean.gns.cutoff=replace(timelon.mean.gns, timelon.mean.gns>=351, 300))  

#timeloen 
timeloen.num = c(146,175,200,208,225,250,275,300,325,346)
timeloen.lab <- paste(timeloen.num,"kr.")  #, "%")
timeloen.lab[4] <- c("208 kr. (median)")
timeloen.lab[1] <- c("145 kr.")
timeloen.lab[length(timeloen.num)] <- c("350+ kr.")  #, "%")
#timeloen.lab[length(timeloen.lab)] <- paste(timeloen.num[length(timeloen.lab)],"+ kr.",sep="")
#timelon.rescale = c(146,175,  188,201,     208,     221,271,    290,346)
timelon.rescale = c(146,160,  175,199,     208,     217,265,    290,346)

kort.timelon <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
           edge.size=edge.size, vertex.fill = discodata$timelon.mean.gns.cutoff,
           vertex.size = vertex_stoerrelse) +  
   ggtitle("timeløn") + scale_fill_gradientn(colours = skala.ired.dgreen.simple,
                                                             values= rescale(timelon.rescale), guide=guide_colorbar(barwidth = 1, barheight = 12,draw.ulim = FALSE, draw.llim = FALSE), name = "timeløn", breaks=timeloen.num, labels=timeloen.lab) +  default  +
#  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(legend.position = c(0.95, 0.9))

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_timelon.pdf", onefile = TRUE, height = 30, width = 30)
kort.timelon 
dev.off()


# kun edges-kort

edges.eksp                <- segment.edges(seg.b, mode="directed",cut.off=1,small.cell.reduction = small.cell.default, segment.reduction = 0.1) #før var den 3 her og 5 nedenunder
edges.eksp[edges.eksp > 7.5] <- 7.5
rr.breaks.edge <- seq(1,7,1) #c(70,80)
rr.lab.edge <- paste(rr.breaks.edge)  #, "%")
rr.lab.edge[length(rr.lab.edge)] <- paste(rr.breaks.edge[length(rr.lab.edge)],"+",sep="")
rr.breaks.edge[1] <- c(1.01)
kort.edges.default <- list()
kort.edges.default$scale_size_continuous <-   scale_size_continuous(range = c(5, 30), name="antal beskaftigede", breaks=beskaeft.num, labels=beskaeft.lab,guide="legend")
kort.edges.default$scale_alpha_continuous <-   scale_alpha_continuous(range = c(0.3, 0.6), guide="none")
kort.edges.default$scale_colour_gradient2 = scale_colour_gradient2(low = "#575155", mid = "darkorange1", high = muted("darkred"), 
                                                        midpoint =3.5 , space = "Lab", na.value = "pink", guide =guide_colorbar(barwidth = 1, barheight = 7,draw.ulim = FALSE, draw.llim = FALSE),name="Styrke af forbindelse", breaks=rr.breaks.edge,labels=rr.lab.edge)
kort.edges <-  gg.jonas(seg, layout = lay, edges=edges.eksp, midpoint.arrow = arrow.default, 
                          edge.size=edge.size,
                          vertex.size = vertex_stoerrelse) +  
  ggtitle("edges")  +  kort.edges.default  + theme(legend.position = c(0.95, 0.9)) + scale_fill_manual(values =   replicate(42, "grey22"),guide="none")
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_edges.pdf", onefile = TRUE, height = 25, width = 25)
kort.edges
dev.off()





# koen
# 
#quants=seq(0,1,0.05)
#round(quantile(discodata$koen.gns.kvinder.andel,quants)*100,0)
#Hmisc::describe(round(100*discodata$koen.gns.kvinder.andel),0)
#ggplot(discodata,aes(x=koen.gns.kvinder.andel)) + geom_histogram(binwidth=0.005) + theme_bw()
#koen 
skala.koen =  c(    "firebrick4","firebrick2", "whitesmoke", "dodgerblue2","dodgerblue4")
koen.num = seq(0,100,10)
koen.num[4] = 32
koen.num=koen.num/100
koen.lab <- paste(100*koen.num)  #, "%")
koen.lab[4] <- c("32 (median)")
koen.rescale = c(0,2.5,  15,25,     50,     75,85,    97.5,100)
koen.rescale= koen.rescale/100
kort.koen <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                          edge.size=edge.size, vertex.fill = discodata$koen.gns.kvinder.andel,
                          vertex.size = vertex_stoerrelse) +  
  ggtitle("køn") + scale_fill_gradientn(values=koen.rescale,colours = skala.koen, name = "andel kvinder i %", breaks=koen.num, labels=koen.lab) +  default  +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(legend.position = c(0.9, 0.9))

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_koen.pdf", onefile = TRUE, height = 30, width = 30)
kort.koen
dev.off()


# fagforeninger - gule


discodata <- mutate(discodata,gule.mean.gns.cutoff=replace(gule.mean.gns, gule.mean.gns>=0.1, 0.1))


quants=seq(0,1,0.01)
round(quantile(discodata$gule.mean.gns,quants)*100,2)
Hmisc::describe(round(100*discodata$gule.mean.gns),3)
ggplot(discodata,aes(x=gule.mean.gns)) + geom_histogram(binwidth=0.003) + theme_bw()
#koen 




skala.gule =  c("grey30","white",  "yellow")
gule.num = seq(0,10,1)
gule.num=gule.num/100
gule.lab <- paste(100*gule.num)  #, "%")
#gule.lab = insert.at(gule.lab,4,c("3.6 (median)"))
#gule.num <- insert.at(gule.num,4,median(discodata$gule.mean.gns.cutoff))
gule.lab[5] = c("4 (median: 3,6)")
gule.lab[11] <- c("10+")
gule.rescale = c(0,.25,  3.25,   8)
gule.rescale= gule.rescale/100
kort.gule <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                       edge.size=edge.size, vertex.fill = discodata$gule.mean.gns.cutoff,
                       vertex.size = vertex_stoerrelse) +  
  ggtitle("gule") + scale_fill_gradientn(values=rescale(gule.rescale),colours = skala.gule, name = "andel gule fagf i %", breaks=gule.num, labels=gule.lab) +  
  default  +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(legend.position = c(0.9, 0.9))
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_gule.pdf", onefile = TRUE, height = 30, width = 30)
kort.gule
dev.off()


# fagforeninger - roede


RColorBrewer.RdYlBu <-   c("#A50026", "#D73027", "#F46D43" ,"#FDAE61" ,"#FEE090" ,"#FFFFBF" ,"#E0F3F8" ,"#ABD9E9" ,"#74ADD1" ,"#4575B4" ,"#313695")

skala.roed =  c("gray20" ,"gray40" ,"gray60" ,"gray80" ,"indianred1","indianred2","indianred3", "indianred4")




roed.num = seq(5,85,10)
roed.num[6] = 54
roed.num=roed.num/100
roed.lab <- paste(100*roed.num)  #, "%")
roed.lab[6] <- c("54 (median)")
roed.num[9] = 0.80
roed.num[1] = 0.1


roed.rescale = c(5,15,   16,25,  26,35,  36,45,   50,55, 56,65,   66,74, 75,85)

quants=seq(0,1,0.1)
round(quantile(discodata$roede.mean.gns,quants)*100,0)
Hmisc::describe(round(100*discodata$roede.mean.gns),0)
#ggplot(discodata,aes(x=roed.mean.gns)) + geom_histogram(binwidth=0.005) + theme_bw()
#koen 


skala.roed =  c("grey30","white", "indianred2", "indianred4")
roed.num = c(5,25,50,75,85)
roed.num=roed.num/100
roed.lab <- paste(100*roed.num)  #, "%")
roed.lab = insert.at(roed.lab,3,c("54 (median)"))
roed.num <- insert.at(roed.num,3,median(discodata$roede.mean.gns))
#roed.lab[5] = c("4 (median: 3,6)")
#roed.lab[11] <- c("10+")
roed.rescale = c(0,25,  51,   70)
roed.rescale= roed.rescale/100
kort.roed <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                       edge.size=edge.size, vertex.fill = discodata$roede.mean.gns,
                       vertex.size = vertex_stoerrelse) +  
  ggtitle("roed") + scale_fill_gradientn(values=rescale(roed.rescale),colours = skala.roed, name = "andel roede fagf i %", breaks=roed.num, labels=roed.lab) +  
  default  +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE)) +
  theme(legend.position = c(0.9, 0.9))
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_roed.pdf", onefile = TRUE, height = 30, width = 30)
kort.roed
dev.off()




################# ledighed ######################


  
Hmisc::describe(discodata$ledighed.mean.gns.cutoff)


natur.interval.ledighed.quantile.cut[9] = c(6.75/100)
natur.interval.ledighed.quantile.cut[5] = c(2.686/100)
ledighed.num = c(1,2,3,4,5,6,7)
ledighed.num[3] = c(2.686)
#ledighed.num[1] = c(0.0001)
#ledighed.num[7] = c(7.49999)
ledighed.num=ledighed.num/100
ledighed.lab <- paste(ledighed.num*100)  #, "%")
ledighed.lab[3] <- c("2,7 (median)")


natur.interval.ledighed.quantile.cut*100
jenks fisher quantile.cut kmeans 


natur.interval.ledighed = natur.interval.ledighed.jenks
natur.interval.ledighed = natur.interval.ledighed.kmeans
natur.interval.ledighed = natur.interval.ledighed.fisher






natur.interval.ledighed.quantile.cut= c(0.0006022257, 0.0114662349, 0.0165955527, 0.0201292872, 0.0268579598, 0.0339174744, 0.0412942476, 0.0529877454, 0.0750000000)


2.7/4

a = seq(0,2.6,2.6/3)
b = seq(2.8,7.5,(7.5-2.8)/4)
a = append(a,c(2.7))
b=append(b,c(7.5))
natur.interval.ledighed.emil = append(a,b)

natur.interval.ledighed.emil

natur.interval.ledighed.emil = c(0,0.4,   0.7,1.3,   1.633,2.25,   2.5,2.6,   2.65,2.75,   3.0,4,     4.5,5.5,     6,6.7,     6.9,7.5)  

natur.interval.ledighed.emil=natur.interval.ledighed.emil/100  


natur.interval.ledighed = natur.interval.ledighed.emil




getPalette = colorRampPalette(skala.darkseagreen.indianred)
kort.ledighed <-  gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, 
                       edge.size=edge.size, vertex.fill = discodata$ledighed.mean.gns.cutoff,
                       vertex.size = vertex_stoerrelse) +  
  ggtitle("ledighed") + scale_fill_gradientn(values=rescale(natur.interval.ledighed
),colours = getPalette(length(natur.interval.ledighed.quantile.cut
)), name = "ledighed af året i %", breaks=ledighed.num, labels=ledighed.lab) +  default  +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = TRUE, draw.llim = TRUE)) +
  theme(legend.position = c(0.9, 0.9))
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/kort_ledighed.pdf", onefile = TRUE, height = 30, width = 30)
kort.ledighed
dev.off()




 #########################################################################################


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





