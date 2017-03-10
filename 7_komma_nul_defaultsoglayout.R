






vertex.shape         <- 21
edge.size            <- 0.5
arrow.default                <- arrow(angle = 15, length = unit(0.30, "cm"), ends = "last", type = "closed")


#mere usikre
edges.default.unalt                <- segment.edges(seg.b, small.cell.reduction = small.cell.default, segment.reduction = 1)
edges.default                <- segment.edges(seg.b, small.cell.reduction = small.cell.default, segment.reduction = 1)
edges.default[edges.default > 10] <- 10 
edges.default.all                <- segment.edges(seg.b, mode="directed",cut.off=3,small.cell.reduction = small.cell.default, segment.reduction = 0.1) #før var den 3 her og 5 nedenunder
edges.default.all[edges.default.all > 30] <- 30 





edges.default.zoom                <- segment.edges(seg.b, mode="directed",cut.off=3,small.cell.reduction = small.cell.default, segment.reduction = 3) #før var den 3 her og 5 nedenunder
edges.default.zoom[edges.default.zoom > 15] <- 15

#help("segment.edges")





# defaults alm. kort

rr.breaks <- seq(3,30,3) #c(70,80)
rr.lab <- paste(rr.breaks)  #, "%")
rr.lab[length(rr.lab)] <- paste(rr.breaks[length(rr.lab)],"+",sep="")
rr.breaks[1] <- c(3.01)

default <- list()
default$scale_size_continuous <-   scale_size_continuous(range = c(5, 30), name="antal beskaftigede", breaks=beskaeft.num, labels=beskaeft.lab,guide="legend")
default$scale_alpha_continuous <-   scale_alpha_continuous(range = c(0.05, 0.75), guide="none")
#default$scale_colour_continuous <-   scale_colour_continuous(high = "orange",  low = "#575155", name="Styrke af forbindelse", breaks=c(3.1,5,7.5,10,12.5,15),labels=c("3","5","7,5","10","12,5","15+"))
default$scale_colour_gradient2 = scale_colour_gradient2(low = "#575155", mid = "darkorange1", high = muted("darkred"), 
                       midpoint = 15, space = "Lab", na.value = "pink", guide =guide_colorbar(barwidth = 1, barheight = 7,draw.ulim = FALSE, draw.llim = FALSE),name="Styrke af forbindelse", breaks=rr.breaks,labels=rr.lab)





#guide=guide_colorbar(barwidth = 1, barheight = 15,draw.ulim = FALSE, draw.llim = FALSE,


default.zoom = list()
default.zoom$scale_size_continuous <-   scale_size_continuous(range = c(5, 40), name="% af totalt antal ledige", breaks=beskaeft.num, labels=beskaeft.lab, guide = "none")
default.zoom$scale_alpha_continuous <-   scale_alpha_continuous(range = c(0.05, 1), guide = "none")
default.zoom$scale_colour_continuous <-   scale_colour_continuous(high = "orange",  low = "#575155", name="Styrke af forbindelse", breaks=c(3.1,5,7.5,10,12.5,15),labels=c("3","5","7,5","10","12,5","15+"))





# defaults alm. kort map array
default.maparray <- list()
default.maparray$scale_size_continuous <-   scale_size_continuous(range = c(3, 17), name="% af totalt antal ledige", breaks=beskaeft.num, labels=beskaeft.lab, guide = "none")
default.maparray$scale_alpha_continuous <-   scale_alpha_continuous(range = c(0.05, 1), guide = "none")
default.maparray$scale_colour_continuous <-   scale_colour_continuous(high = "orange",  low = "#575155", name="Styrke af forbindelse", guide = "none")




#disco defaults
default.disco <- list()

#default.disco$disco <-   scale_fill_manual(values = brewer.pal(9, "Paired"), labels=disco_1cifret_lab, name="Disco 1-cifret")
default.disco$disco <-   scale_fill_manual(values = xmen_ext, labels=disco_1cifret_lab, name="Disco 1-cifret")


 




#disco defaults map array
default.disco.maparray <- list()

default.disco.maparray$disco <-   scale_fill_manual(values = brewer.pal(9, "Paired"), labels=disco_1cifret_lab, name="Disco 1-cifret", guide = "none")

#egp defaults map array
default.egp11.maparray <- list()

default.egp11.maparray$egp11 <-  scale_fill_manual(values = skala_egp11, labels=egp11_lab, name="EGP-11",guide="none")



#leg med farver til disco-kort

# display.pal(disco.theme.1.cifret,pointsize=20)

# help(display.pal)

# q
# install.packages("plotKML")
# install.packages("rgdal")
# library(plotKML)

# #disco farvetema
# disco.theme.1.cifret <- c(
# "#CD5555", #indianred
# "#8FBC8F", #darkseagreen 
# "#EEEE00", #yellow2  
# "#8968CD", #mediumpurple3 
# "#CD6090", #hotpink3 
# "#4F94CD", #steelblue3  
# "#CD853F", #tan3 
# "#8B795E", #navajowhite4 
# "#00868B") #turquise4 







##### label sets og breaks ####



# alder

















######### layout fil 






######## layout #####################

#### layout



#lay                          <- beskaeftigede.andel.smooth[-l, 1:2]
#lay[, 1]                     <- lay[, 1] * -10000
#lay[, 2]                     <- lay[, 2] * 10000
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, niter=10000)
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, niter=10000)





#lay_checkfile <- runif(1, 1, 100000)
#lay_checkfile = round(lay_checkfile,0)
#layout.moneca = list(lay, lay_checkfile)

#save(layout.moneca, file = "./statistik/R/moneca/vores/voresdata/layout_igrah701.Rdata")
load(file = "./statistik/R/moneca/vores/voresdata/layout_igrah701.Rdata")

lay_checkfile = layout.moneca[2]
lay_checkfile == 52344
lay = layout.moneca[1]























#test <- seq(from=440, to=10, by=-90)

#test <- c(7000,4500,2500,1500,1400)
#lay                          <- beskaeftigede.andel.seg[-l, 1:2]
#lay[, 1]                     <- lay[, 1] * -10000
#lay[, 2]                     <- lay[, 2] *  10000
#lay                          <-  layout.matrix(seg,start=lay)
#lay[, 1]                     <- lay[, 1] / 1000000
#lay[, 2]                     <- lay[, 2] / 1000000


#gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, edge.size=edge.size, vertex.fill = discodata$disco_1cifret, vertex.size = vertex_stoerrelse) +   default + scale_fill_gradient(colours = getPalette(length(unique(tester))),guide="legend")
#view(discodata)











#help("layout.matrix")


#lay                          <- beskaeftigede.andel.seg[-l, 1:2]
#lay[, 1]                     <- lay[, 1] * -10000
#lay[, 2]                     <- lay[, 2] * 10000
#lay                          <-  layout.matrix(seg) #fler niveauer p? attraction 
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), weight.adjustment = 1.14, start = lay, tie.adjustment = 0.4, niter=10000) #fler niveauer p? attraction hvis vi kommer over 5 niveauer
# help(layout.matrix)


# d. 19/08/2016 Anton kommentar: Det er tie.adjustment der er vigtigst. Den står nu på 0.4 - jo lavere man sætter den desto rundere sky får man. Men så optimeres der mindre på overlap og længde af edges. Omvendt jo højere desto mindre rund sky, men også mere meningsfuld sky. 
# d. 19/08/2016: tidligere skulle  iter = 50000000000000000 og area.size=7000000 også bruges, men nu siger den det er "unused arguments" i stedet. Men virker vist fint uden så ah the the hell.
# lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000)




# # test <-  c(450, 125, 40, 22, 12, 10)
# test <-  c(500, 150, 80, 40, 16, 10)



# lay                          <- beskaeftigede.andel.seg[-l, 1:2]
# lay[, 1]                     <- lay[, 1] * -10000
# lay[, 2]                     <- lay[, 2] * 10000
# lay                          <-  layout.matrix(seg, attraction = test, area.size=8000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000) #fler niveauer p? attraction hvis vi kommer over 5 niveauer
# lay                          <-  layout.matrix(seg, attraction = test, area.size=8000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000)

# alle beskaeftigede 250 kat
# save(lay, file="./statistik/R/moneca/vores/voresdata/lay_FORSIDEMAASKE.Rdata")





#write.csv(lay, file = "./statistik/R/moneca/vores/voresdata/layout_igraph701.csv")
# den layout fil man f?r ud ved hj?lp af de her scripts er ikke den samme som den der l? der i forvejen.
#load(file = "./layout.Rda")


# Layout
#lay                          <- beskaeftigede.andel.seg[-l, 1:2]
#lay[, 1]                     <- lay[, 1] * -10000
#lay[, 2]                     <- lay[, 2] * 10000
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000)
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000)
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=9900000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, mode="directed")
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=9900000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, mode="directed")


#help(layout.matrix)


