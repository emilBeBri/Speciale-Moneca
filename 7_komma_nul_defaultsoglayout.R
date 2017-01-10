
vertex_stoerrelse <-  discodata$beskaeft.gns 


#ret sikre defaults, behÃ¸ver ikke Ã¦ndres


 
# disco 1 cifret
disco_1cifret_lab <- c("1: Ledelse", "2: Viden pÃ¥ hÃ¸jeste niveau", "3: Viden pÃ¥ mellemniveau", "4: Kontorarbejde", "5: Detailsalg, service og omsorgsarb.", "6: Landbrug, skovbrug, jagt, \nfiskeri (grundniveau)", "7: HÃ¥ndvÃ¦rksprÃ¦get arbejde", "8: Proces- og maskinoperatÃ¸rarbejde \nsamt transport og anlÃ¦g", "9: Andet manuelt arbejde")
# andel stÃ¸rrelse (numerisk)
beskaeft.num <- c(0.005,0.01,0.02,0.03,0.04,0.05)
# andel stÃ¸rrelse (labels)
beskaeft.lab <- c("0,5 %", "1 %", "2 %","3 %", "4 %", "5 %")
#intern mobilitet seg



# intern mobilitet segment

# discodata$within.mob.seg
# quants = seq(0,1,0.05) 
# format(round(quantile(discodata$within.mob.seg, quants), digits=2), big.mark=".",decimal.mark=",",nsmall=0)
# summary(discodata$within.mob.seg)

# view(seg.qual.final %>% select(membership, within.mob.seg,everything()) %>% 	 arrange(desc(within.mob.seg)))



intern.mob.seg_num <- seq(50,100,5) #c(70,80)
intern.mob.seg_num <- intern.mob.seg_num/100
intern.mob.seg_lab <- paste(100*intern.mob.seg_num)#, "%")



#intern mobilitet noder 
intern.mob_num <- c(5,10,20,30,40,50,55,60)
intern.mob_num <- intern.mob_num/100
intern.mob_lab <- c("5 %","10 %", "20 %","30 %", "40 %", "50 %", "55 %","60 %")






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

### farvevalg


#farveskaler


# grøn-rød 

skala.indianred.darkseagreen         <- c( "indianred4", "indianred3", "indianred2", "indianred1", "whitesmoke", "darkseagreen1", "darkseagreen2", "darkseagreen3", "darkseagreen4")
skala.darkseagreen.indianred <-  rev(skala.indianred.darkseagreen)


#skala.indianred.darkseagreen.3col         <- c( "indianred4", "indianred3", "indianred2","whitesmoke","darkseagreen2", "darkseagreen3", "darkseagreen4")
#skala.darkseagreen.indianred.3col <-  rev(skala.indianred.darkseagreen)


skala.ired.dgreen.simple =  c(    "indianred4","indianred2", "whitesmoke", "darkseagreen2","darkseagreen4")
skala.dgreen.ired.simple =  rev(skala.ired.dgreen.simple)



# xmen 
xmen = c("#026CCBFF", "#F51E02FF" ,"#05B102FF" ,"#FB9F53FF" ,"#9B9B9BFF", "#FB82BEFF" ,"#BA6222FF"  ,    "#EEC229FF" )
# xmen xtended 
xmen_ext = c("#026CCBFF", "#F51E02FF" ,"#05B102FF" ,"#FB9F53FF" ,"#9B9B9BFF", "#FB82BEFF" ,"#BA6222FF"  ,    "#EEC229FF"   , "#9370DB")


#egp 
# dodgerblue1 dodgerblue2                 serviceklasse
# darkseagreen1 darkseagreen4             middelklassen
# indianred1 indianred3 indianred4        arbejderklassen
# yellow1 yellow3 yellowgreen             småborgerskabet
skala_egp11 <-  c("dodgerblue4" ,"dodgerblue1","darkseagreen4","darkseagreen1","yellow3", "yellow1","grey","indianred4","indianred3","indianred1","pink")





#"1" = "1 Store forretningsdrivende" ,     "dodgerblue4"
#"2" = "2 Selvstaendige profesionelle" ,    
#"3" = "3 Smaa forretningsdrivende m. ansatte" , "dodgerblue2"
#"4" = "4 Smaa forretningsdrivende u. ansatte" , "dodgerblue1"
#"5" = "5 Teknikere (eksperter)" ,  "mediumpurple4"
#"6" = "6 Teknikere" , "mediumpurple1"
#"7" = "7 Manuelle arbejdere, hoejt niveau" , "indianred4"
#"8" = "8 Manuelle arbejdere, lavt niveau" ,  "indianred2"
#"9" = "9 Managere, hoejt niveau" , "steelblue4"
#"10" = "10 Managere, lavt niveau" , "steelblue1"
#"11" = "11 Clerks, hoejt niveau" , "yellow4"
#"12" = "12 Clerks, lavt niveau" , "yellow1"
#"13" = "13 Socio-kulturelle profesionelle" , "darkseagreen4"
#"14" = "14 Socio-kulutrelle semiprofesineolle" , "darkseagreen1"
#"15" = "15 Servicearbejdere, hoejt niveau" , "chocolate4" 
#"16" = "16 Servicearbejdere, lavt niveau") "chocolate1"








 levels(discodata$klasse_egp11)

 
 
 

# help(brewer.pal)
# display.brewer.all()
# brewer.pal.info

# colourCount = length(unique(mtcars$hp))

#  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
 
# colourCount = length(unique(discodata$koen.gns.kvinder.andel))


# # ggplot(mtcars) + 
# #   geom_histogram(aes(factor(hp)), fill=getPalette(colourCount)) + 
# #   theme(legend.position="right")


# bpal.standardset  = colorRampPalette(brewer.pal(8, "Set2"))

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














# #perindkialt breaks alle beskaeftigede
perindkialt.helepop.gns_num <- c(160000,250000,280000,325000,368000,458000,970000)
perindkialt.helepop.gns_lab <- c("160000 kr","250000 kr", "280000 kr", "325000 kr", "368000 kr", "458000 kr", "970000 kr")
# quants = seq(0,1,0.05) 
# format(round(quantile(discodata$perindkialt.helepop.gns, quants), digits=0), big.mark=".",decimal.mark=",",nsmall=0)
#kan mÃ¥ske slettes 
# #Ã¥rslÃ¸n breaks ledige loenmv
# loenmv.gns_num <- c(100000,150000,200000,250000,300000, 350000,400000)
# loenmv.gns_lab <- c("100.000 kr","150.000 kr", "200.000 kr", "250.000 kr", "300.000 kr", "350.000kr", "400.0000 kr")
# #Ã¥rslÃ¸n breaks alle beskaeftigede
# loenmv.gns.helepop_num <- c(130000,150000,200000,250000,300000, 350000,400000,550000)
# loenmv.gns.helepop_lab <- c("130.000 kr","150.000 kr", "200.000 kr", "250.000 kr", "300.000 kr", "350.000kr", "400.0000 kr","550.000 kr")





#ledighedsrisiko breaks
ledrisk_num <- c(10,20,25,30,40,50)
ledrisk_num <- ledrisk_num/100
ledrisk_lab <- c("10 %", "20 %","25 %", "30 %", "40 %", "50 %")
#alder breaks
alder.gns_num <- c(30,35,40,45,50,55)
alder.gns_lab <- c("30 Ã¥r", "35 Ã¥r", "40 Ã¥r", "45 Ã¥r", "50 Ã¥r", "55 Ã¥r")
# ledighedsperiode antal
ledperiod.antal.gns_num <- c(1.00,1.15,1.17,1.20,1.25,1.30)
ledperiod.antal.gns_lab <- c("1,00","1,15", "1,17","1,20","1,25","1.30")
# ledighedsperiode lÃ¦ngde
ledperiod.gns.lngde.gns_num <- c(1.20,1.40,1.50,1.65,1.70,1.80,1.9,2.0)
ledperiod.gns.lngde.gns_lab <- c("1,20","1,40","1,50","1,65","1,70","1,80","1,9","2,0")




#kÃ¸n breaks
koen_num <- c(1,5,10,25,50,75,90,99)
koen_num <- koen_num/100
koen_lab <- c("5 %", "5 %","10 %", "25 %", "50 %", "75 %", "90 %", "99 %")

# format(round(as.numeric(1000.64)), nsmall=1, big.mark=".",decimal.mark=",") 
# # # #til at lave breaks og labels med 
# format(summary(discodata$loenmv.helepop.gns), big.mark=".",decimal.mark=",")
# format(round(quantile(discodata$loenmv.helepop.gns, c(0.01, 0.10, 0.15, 0.25, 0.30, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98,0.985, .99)), digits=3), big.mark=".",decimal.mark=",",nsmall=0)
# # # #til at lave breaks og labels med 



#hÃ¸rer vist ikke til her, find ud af det senere (januar 2016)
# 100*summary(discodata$koen.gns.kvinder.andel)
# 100*round(quantile(discodata$koen.gns.kvinder.andel, c(0.01, 0.10, 0.15, 0.25, 0.30, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98,0.985, .99)), digits=3)


######### layout fil 






######## layout #####################

#### layout



#lay                          <- beskaeftigede.andel.seg[-l, 1:2]
#lay[, 1]                     <- lay[, 1] * -10000
#lay[, 2]                     <- lay[, 2] * 10000
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, niter=10000)
#lay                          <-  layout.matrix(seg, attraction = c(450, 125, 40, 22, 12), area.size=7000000, weight.adjustment = 1.14, start = lay, iter = 50000000000000000, niter=10000)





#lay_checkfile <- runif(1, 1, 100000)
#lay_checkfile = round(lay_checkfile,0)
#layout.moneca = list(lay, lay_checkfile)
#save(layout.moneca, file = "./statistik/R/moneca/vores/voresdata/layout_igrah701.Rda")
load(file = "./statistik/R/moneca/vores/voresdata/layout_igrah701.Rda")

lay_checkfile = layout.moneca[2]
lay_checkfile == 59047
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


