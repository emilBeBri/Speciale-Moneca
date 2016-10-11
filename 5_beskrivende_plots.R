






########################histogrammer #####################


#View(discodata[,c("ledbeskaeft.gns", "ledbeskaeft1996", "ledbeskaeft1997", "ledbeskaeft1998", "ledbeskaeft1999" , "ledbeskaeft2000" , "ledbeskaeft2001", "ledbeskaeft2002", "ledbeskaeft2003", "ledbeskaeft2004", "ledbeskaeft2005", "ledbeskaeft2006", "ledbeskaeft2007", "ledbeskaeft2008", "ledbeskaeft2009")])
hist.led <- select(discodata,ledbeskaeft.gns, disco)
hist.led <- transform(hist.led, 
                  disco = reorder(disco, ledbeskaeft.gns))
hist.beskaeft <- select(discodata,beskaeft.gns, disco_s)
hist.beskaeft <- transform(hist.beskaeft, 
                  disco = reorder(disco_s, beskaeft.gns))



summary(discodata$beskaeft.gns)
summary(discodata$ledbeskaeft.gns)
round(quantile(discodata$ledbeskaeft.andel.gns, c(0.01, 0.10, 0.25, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98,0.985, .99)), digits=3)
round(quantile(discodata$beskaeft.andel.gns, c(0.01, 0.10, 0.25, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98, 0.985,.99)), digits=3)
discodata %>% top_n(10, ledbeskaeft.andel.gns) %>% arrange(desc(ledbeskaeft.andel.gns)) %>%  select(one_of(c("ledbeskaeft.andel.gns", "beskaeft.andel.gns")))

ledbeskaeft.hist.count <- ggplot(hist.led,aes(x=factor(disco),y=ledbeskaeft.gns,fill=ledbeskaeft.gns)) + geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label = ledbeskaeft.gns), size = 2, hjust = -0.1, position = "dodge") +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) +
  theme(text = element_text(size=9),axis.text.y = element_text(angle=0, vjust=0, size=4)) 


beskaeft.hist.count <- ggplot(hist.beskaeft,aes(x=factor(disco),y=beskaeft.gns,fill=beskaeft.gns)) + geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label = beskaeft.gns), size = 2, hjust = -0.1, position = "dodge") +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) + 
  theme(text = element_text(size=9),axis.text.y = element_text(angle=0, vjust=0, size=4)) 



pdf(file = "./latexopgave/fig/metode/hist_beskaeftigede_allekategorier.pdf", height = 16, width = 16)
grid.arrange(beskaeft.hist.count, ledbeskaeft.hist.count, nrow=2)
dev.off()

# total 
hist.beskaeft <- select(discodata,beskaeft.gns, disco_s)
hist.beskaeft <- transform(hist.beskaeft, 
                  disco = reorder(disco_s, beskaeft.gns))
# andel 
hist.beskaeft <- select(discodata,beskaeft.andel.gns, disco_s)
hist.beskaeft <- transform(hist.beskaeft, 
                  disco = reorder(disco_s, beskaeft.andel.gns))
hist.beskaeft <- rename(hist.beskaeft,beskaeft.gns=beskaeft.andel.gns)
hist.beskaeft$beskaeft.gns <- hist.beskaeft$beskaeft.gns*100
hist.beskaeft$beskaeft.gns <- round(hist.beskaeft$beskaeft.gns,2)

# *fjern det med paste(hist.beskaeft$beskaeft.gns,"%") hvis du bruger totalen
ggplot(hist.beskaeft,aes(x=factor(disco),y=beskaeft.gns,fill=beskaeft.gns)) + geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label = paste(hist.beskaeft$beskaeft.gns,"%")), size = 2.5, hjust = -0.1, position = "dodge") +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) + 
 theme(text = element_text(size=10),axis.text.y = element_text(angle=0, vjust=0, size=7) , 
  panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                colour = "lightgrey"), 
  panel.grid.minor = element_blank(),
  axis.line = element_blank()  ) 


  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                colour = "black"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                colour = "black"),





############################# andre versioner #######################


#her bruges
 colourCount.hist = length(unique(hist.led$disco))
 getPalette.hist = colorRampPalette(brewer.pal(9, "Set1"))
 pdf(file = "./statistik/R/moneca/vores/output/histogram/alleledbeskaeft.pdf", height = 16, width = 16)
 ggplot(hist.led,aes(x=factor(disco),y=ledbeskaeft.gns,fill=factor(disco_1cifret))) + geom_bar(stat="identity") +
   coord_flip() + geom_text(aes(label = ledbeskaeft.gns), size = 3, hjust = -0.3, position = "dodge") +
   scale_fill_manual(values = brewer.pal(9, "Set1"), guide="none")  
 dev.off()






################################



mob.mat.weight1          <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight2          <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = TRUE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight3          <- weight.matrix(mob.mat, cut.off = 0.5, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight1          <- round(mob.mat.weight1, digits=1)
mob.mat.weight2          <- round(mob.mat.weight2, digits=1)
mob.mat.weight3          <- round(mob.mat.weight3, digits=1)

pdf(file = "./statistik/R/moneca/vores/output/tileplot.ledige.150.pdf", height = 80, width = 80)
tile.plot(mob.mat.weight1)
tile.plot(mob.mat.weight2)
tile.plot(mob.mat.weight3)
#tile.plot(mob.mat.weight4)
#tile.plot(mob.mat.weight5)
dev.off()




### histogrammer ###
#--------------
# Create Theme
#--------------

# BASIC THEME
theme.car_chart <- 
  theme(legend.position = "none") +
  theme(plot.title = element_text(size=26, face="bold", hjust=0, color="#666666")) +
  theme(axis.title = element_text(size=18, face="bold", color="#666666")) +
  theme(axis.title.y = element_text(angle=0)) 


# SCATTERPLOT THEME
theme.car_chart_SCATTER <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# HISTOGRAM THEME
theme.car_chart_HIST <- theme.car_chart +
  theme(axis.title.x = element_text(hjust=0, vjust=-.5))

# SMALL MULTIPLE THEME
theme.car_chart_SMALLM <- theme.car_chart +
  theme(panel.grid.minor = element_blank()) +
  theme(strip.text.x = element_text(size=16, face="bold", color="#666666")) 




####################### smooth plots ###

# for ledige på segment-niveau





#for ledige på disco-niveau
pdf(file = "./statistik/R/moneca/vores/output/test/smooths_array.ledbeskaeft.pdf", height = 11, width = 17)
for(i in 1:length(label)){
  pl  <- list()
  pl[[1]]   <- line.plot(data.frame(original = ledbeskaeft[i,], smooth = ledbeskaeft.smooth[i,]), sort = FALSE) + ggtitle("ledbeskaeft") + scale_x_reverse(labels = aar[length(aar):1], breaks = 1:length(aar)) + ylim(0, max(c(max(ledbeskaeft[i,]), max(ledbeskaeft.smooth[i,]))))
  pl[[2]]   <- line.plot(data.frame(original = ledbeskaeft.andel[i,], smooth = ledbeskaeft.andel.smooth[i,]), sort = FALSE) + ggtitle("ledbeskaeft") + scale_x_reverse(labels = aar[length(aar):1], breaks = 1:length(aar)) + ylim(0, max(c(max(ledbeskaeft.andel[i,]), max(ledbeskaeft.andel.smooth[i,])))) + scale_y_continuous(labels = percent)  
  map.array(pl, fixed.coord = FALSE, ncol = 2, title = label[i])
}
dev.off()






#for alle beskaeftigede
pdf(file = "./statistik/R/moneca/vores/output/smooths/smooths_array.allebeskaeft.pdf", height = 11, width = 17)
for(i in 1:length(label)){
  pl  <- list()
  pl[[1]]   <- line.plot(data.frame(original = beskaeftigede[i,], smooth = beskaeftigede.smooth[i,]), sort = FALSE) + ggtitle("beskaeftigede") + scale_x_reverse(labels = aar[length(aar):1], breaks = 1:length(aar)) + ylim(0, max(c(max(beskaeftigede[i,]), max(beskaeftigede.smooth[i,]))))
  pl[[2]]   <- line.plot(data.frame(original = beskaeftigede.andel[i,], smooth = beskaeftigede.andel.smooth[i,]), sort = FALSE) + ggtitle("beskaeftigede") + scale_x_reverse(labels = aar[length(aar):1], breaks = 1:length(aar)) + ylim(0, max(c(max(beskaeftigede.andel[i,]), max(beskaeftigede.andel.smooth[i,])))) + scale_y_continuous(labels = percent)  
  map.array(pl, fixed.coord = FALSE, ncol = 2, title = label[i])
}
dev.off()

#for alle alder
pdf(file = "./statistik/R/moneca/vores/output/smooths/smooths_array.alder.pdf", height = 11, width = 17)
for(i in 1:length(label)){
  pl  <- list()
  pl[[1]]   <- line.plot(data.frame(original = alder[i,], smooth = alder.smooth[i,]), sort = FALSE) + ggtitle("alder") + scale_x_reverse(labels = aar[length(aar):1], breaks = 1:length(aar)) + ylim(18, max(c(max(alder[i,]), max(alder.smooth[i,]))))
  map.array(pl, fixed.coord = FALSE, ncol = 1, title = label[i])
}
dev.off()





#### SNA temaplots #### 


# Organisationsgrad for hvert aar i hele perioden !!! Labels
p.alder.periode.l  <- list()


for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = alder.smooth[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("alder: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(alder.smooth, na.rm = TRUE), max(alder.smooth, na.rm = TRUE)), name = "total", na.value = vertex.na)
  p.alder.periode.l[[i]] <- p.p
}






alder.smooth[-l, i]



l

nrow(alder.smooth)


# Organisationsgrads kort
pdf(file = "./statistik/R/moneca/vores/output/alderoveraar.pdf", height = 20, width = 20)
p.alder.periode.l
dev.off()




View(intern.mobilitet)


##
#eksperimenter
p.seg.strong.edges      <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, edge.color = "weight", midpoints = TRUE, show.text = FALSE, vertex.fill = intern.mobilitet.seg, vertex.size = stor.ledbeskaeft,
                                    border.labels = seg.lab, border.text.hjust = 0.32) 
p.seg.strong.edges      <- p.seg.strong.edges + ggtitle("Intern mobilitet og staerke forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default + scale_color_gradientn(colours = gradient.lilla2, guide = "none")




### test med hovedplots  ###

#eksperimenter
p.seg.strong.edges      <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, edge.color = "weight", midpoints = TRUE, show.text = FALSE, vertex.fill = intern.mobilitet.seg, vertex.size = stor.ledbeskaeft,
                                    border.labels = seg.lab, border.text.hjust = 0.32) 
p.seg.strong.edges      <- p.seg.strong.edges + ggtitle("Intern mobilitet og staerke forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default + scale_color_gradientn(colours = gradient.lilla2, guide = "none")




p.seg.strong.edges      <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, edge.color = "weight", midpoints = TRUE, show.text = FALSE, vertex.fill = intern.mobilitet.seg, vertex.size = stor.ledbeskaeft.andel, border.labels = seg.lab, border.text.hjust = 0.32) 
# edges               styrer hvor stærke forbindelser skal vises, to defaults er edge.default for at sløre de svare forbindelse og edges.default.all for at vise alle forbindelser.    
# vertex.fill         er farven, her: intern mobilitet.
# vertex.size         er størrelsen, her: andelen af beskæftigede.
# edge.color          ? 
# midpoints           T/F kontrollerer om der skal være pile med retningen på forbindelsen.
# show.text           T/F kontrollerer om der skal disco-labels med. 
# border.labels       objekt der indeholder labels til segmenter, kan stilles på når de er analyseret.
# border.text.hjust   kontrollerer noget med placeringen (og måske størrelsen?) på titlen.
p.seg.strong.edges      <- p.seg.strong.edges + ggtitle("Intern mobilitet og staerke forbindelser") + defaults + scale_fill_gradientn(colours = gradient.lilla2, guide = "legend", name = "Intern mobilitet") + guide.default + scale_color_gradientn(colours = gradient.lilla2, guide = "none")

pdf(file = "./statistik/R/moneca/vores/output/p.seg.strong.edges.test.pdf", height = 15, width = 15)
p.seg.strong.edges
dev.off()



myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))          



defaults                     <- list()
defaults$scale_size          <- scale_size_continuous(range = c(3, 15), guide = "legend") #størrelsen på edges, rammerne for hvor store/små de kan blive.
defaults$scale_alpha         <- scale_alpha_continuous(range = c(0.05, 0.2), guide = "none")
defaults$scale_colour        <- scale_colour_continuous(high = edge.color.high,  low = edge.color.low, guide = "none")
#defaults$legend              <- theme(legend.position = "none")





# #alderover45 - TEST
# alderover45 <- loadWorkbook("./statistik/DST/DST_output/04 april/baggrund_alderover45.xlsx")
# lst = readWorksheet(alderover45, sheet = getSheets(alderover45))
# #lst
# #nrowallbeskaeft = 151
# alderover45 <- data.frame(matrix(unlist(lst), nrow=nrowallbeskaeft),stringsAsFactors=FALSE)
# #View(alderover45)
# columns <- c(1, 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54)
# alderover45 <- alderover45[,c(columns)]
# label_moneca_   <- list("moneca_label", "1996" ,"1997" , "1998" , "1999" , "2000" , "2001", "2002" , "2003" , "2004" , "2005", "2006",  "2007",  "2008",  "2009")
# colnames(alderover45) <- label_moneca_[]
# l1_r            <- nrow(alderover45)
# alderover45[l1_r,1]            <- c(9999)
# alderover45 <- sapply(alderover45, as.numeric)
# #View(alderover45)
# alderover45           <- as.matrix(alderover45[, -1]) 
# rownames(alderover45) <- label[] #tager label-objektet og s?tter labels på fra det

# alderover45.andel     <- alderover45 / ledbeskaeft 


# alderover45.andel

# #View(alderover45.andel)
# alderover45.smooth                                        <- smoothie(alderover45)
# #erstatter de forudsagte v?rdier med regressioner for hver periode - antal
# alderover45.smooth[, aar %in% smooth.periode.1996.2001]    <- smoothie(alderover45[, aar %in% smooth.periode.1996.2001])
# alderover45.smooth[, aar %in% smooth.periode.2001.2009]    <- smoothie(alderover45[, aar %in% smooth.periode.2001.2009])
# #write.table(alderover45.smooth, file="./statistik/R/moneca/vores/output_emil/alderover45.smooth1.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
# #erstatter de forudsagte v?rdier med regressioner for hver periode - andel 
# alderover45.andel.smooth                                        <- smoothie(alderover45.andel, max.value = 1)
# alderover45.andel.smooth[, aar %in% smooth.periode.1996.2001]    <- smoothie(alderover45.andel[, aar %in% smooth.periode.1996.2001], max.value = 1) 
# alderover45.andel.smooth[, aar %in% smooth.periode.2001.2009]    <- smoothie(alderover45.andel[, aar %in% smooth.periode.2001.2009], max.value = 1)
# #uklart hvad der sker her #sp?rganton. han ender med en smoothede kategorier uden decimaler, der ligger semit?t p? den tidligere smoothede version
# alderover45.smooth             <- alderover45.smooth[, aar %in% periode]
# alderover45                    <- alderover45[, aar %in% periode]
# alderover45.andel              <- alderover45.andel[, aar %in% periode]
# alderover45.andel.smooth       <- alderover45.andel.smooth[, aar %in% periode]
# alderover45.smooth <- round(alderover45.smooth)
# #kopier til excelark
# writeWorksheetToFile("./statistik/R/moneca/vores/output/baggrund/alderover45.xlsx", data = list(alderover45, alderover45.smooth, alderover45.andel,alderover45.andel.smooth),
#                      sheet = c("count", "count.smooth", "andel", "andel.smooth"),
#                      startRow = c(1,1,1,1), startCol = c(1,1,1,1), header=TRUE)







# #dst.plotq(alderover45.andel.smooth,4:5)

# #View(ledbeskaeft.andel.smooth)
# #View(alderover45.andel.smooth)
