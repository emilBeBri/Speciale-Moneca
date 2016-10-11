 # socio/socstil VORES MONECA REAL AU

desk.aupop.ledsoc_tael.voresmoneca <- NULL 
desk.aupop.ledsoc_tael.voresmoneca <- loadWorkbook("./statistik/DST/DST_output/08 august/ledsoc_tael_desk_socstil_socio_AU.xlsx")
lst = readWorksheet(desk.aupop.ledsoc_tael.voresmoneca, sheet = getSheets(desk.aupop.ledsoc_tael.voresmoneca))
# view(lst[[1]])
desk.aupop.ledsoc_tael.voresmoneca <- data.frame(matrix(unlist(lst), nrow=2),stringsAsFactors=FALSE)
columns <- c(2,6,10,14,18,22,26,30,34,38,42,46)
desk.aupop.ledsoc_tael.voresmoneca <- desk.aupop.ledsoc_tael.voresmoneca[1,c(columns)]
desk.aupop.ledsoc_tael.voresmoneca <-  t(desk.aupop.ledsoc_tael.voresmoneca)
desk.aupop.ledsoc_tael.voresmoneca <- tbl_df(data.frame(desk.aupop.ledsoc_tael.voresmoneca))
desk.aupop.ledsoc_tael.voresmoneca$X1 <- as.numeric(as.character(desk.aupop.ledsoc_tael.voresmoneca$X1))
desk.aupop.ledsoc_tael.voresmoneca$seq <- seq_len(nrow(desk.aupop.ledsoc_tael.voresmoneca))
desk.aupop.ledsoc_tael.voresmoneca <- rbind(desk.aupop.ledsoc_tael.voresmoneca,data.frame(X1=0, seq=0.1))
desk.aupop.ledsoc_tael.voresmoneca <- rbind(desk.aupop.ledsoc_tael.voresmoneca,data.frame(X1=0, seq=12.1))
desk.aupop.ledsoc_tael.voresmoneca <- desk.aupop.ledsoc_tael.voresmoneca[order(desk.aupop.ledsoc_tael.voresmoneca$seq),]
# grp <-  rep("ledsoc_tael_voresmoneca", 14)
grp <-  rep("4", 14)
desk.aupop.ledsoc_tael.voresmoneca <-  cbind(grp,desk.aupop.ledsoc_tael.voresmoneca)
desk.aupop.ledsoc_tael.voresmoneca$grp <-  as.numeric(as.character(desk.aupop.ledsoc_tael.voresmoneca$grp))
desk.aupop.ledsoc_tael.voresmoneca$Aar  <-  seq(1996,2009,by=1)
desk.aupop.ledsoc_tael.voresmoneca$seq <- NULL 
desk.aupop.ledsoc_tael.voresmoneca$N <- desk.aupop.ledsoc_tael.voresmoneca$X1 
desk.aupop.ledsoc_tael.voresmoneca$X1 <- NULL 
# view(desk.aupop.ledsoc_tael.voresmoneca)



## plot 
test1 <- NULL 
test1  <-  ggplot(desk.aupop.ledsoc_tael.voresmoneca, aes(x=Aar, y=N) )


discodata$loenmv.gns





	+ 
                      geom_line(size=1.6) +
                      geom_point(size=3) + 
                      xlab("") + ylab("") +
                      theme(legend.margin = unit(0, "cm")) # +
                      # scale_colour_manual(name = "arbejdsløse", labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=col.desk.au) #+  
                      # scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")) + 
                      # theme_bw() 
test1



######## histogram løngennemsnit

hist.loenmv <- select(discodata,loenmv.gns, disco)
hist.loenmv <- transform(hist.loenmv, 
                  disco = reorder(disco, loenmv.gns))


# hist.loenmv <- NULL 

view(discodata)

plot.hist.loenmv <- ggplot(hist.loenmv,aes(x=factor(disco),y=loenmv.gns,fill=loenmv.gns)) + geom_bar(stat="identity") +
  coord_flip() + geom_text(aes(label = loenmv.gns), size = 2, hjust = -0.1, position = "dodge") +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) +
  theme(text = element_text(size=9),axis.text.y = element_text(angle=0, vjust=0, size=7)) 
  
pdf(file = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/test5.pdf", height = 20, width = 20)
plot.hist.loenmv
dev.off()



geom_bar(stat = "identity", position = position_dodge(width = NULL)) +
  geom_text(position = position_dodge(width = 0.9), size = 10)




beskaeft.hist.count <- ggplot(hist.beskaeft,aes(x=factor(disco),y=beskaeft.gns,fill=beskaeft.gns)) + geom_bar(stat="identity", position = position_dodge(width = NULL)) +
  coord_flip() + geom_text(aes(label = beskaeft.gns), size = 2, hjust = -0.1, position = position_dodge(width = 0.9), size = 10) +
  scale_fill_gradientn(colours = c("#4A4A4A", "darkorange"), guide = "none", breaks = c(0.9, 0.75, 0.5, 0.25, 0.1)) + 
  theme(text = element_text(size=9),axis.text.y = element_text(angle=0, vjust=0, size=4)) 









## Loenmv - gennemsnitsloen (alle)

loenmv.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/loenmv_kat150__helepop.xlsx")
lst = readWorksheet(loenmv.helepop, sheet = getSheets(loenmv.helepop))
#lst
loenmv.helepop <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
#View(loenmv.helepop)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
loenmv.helepop <- loenmv.helepop[,c(columns)]
colnames(loenmv.helepop) <- label_moneca_[]
#l1_r            <- nrow(loenmv.helepop)
#l1_r
loenmv.helepop <- sapply(loenmv.helepop, as.numeric)
#View(loenmv.helepop)
moneca.labels.num <- as.vector(loenmv.helepop[, 1])
#View(moneca.labels.num)
loenmv.helepop           <- as.matrix(loenmv.helepop[, -1]) 

loenmv.helepop           <- rbind(loenmv.helepop, (colSums(loenmv.helepop)/nrowputexcel))

rownames(loenmv.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_loenmv.helepop   <- list("loenmv.helepop1996" ,"loenmv.helepop1997" , "loenmv.helepop1998" , "loenmv.helepop1999" , "loenmv.helepop2000" , "loenmv.helepop2001", "loenmv.helepop2002" , "loenmv.helepop2003" , "loenmv.helepop2004" , "loenmv.helepop2005", "loenmv.helepop2006",  "loenmv.helepop2007",  "loenmv.helepop2008",  "loenmv.helepop2009")

loenmv.helepop            <- disco.df(loenmv.helepop, label_moneca_loenmv.helepop)
loenmv.helepop     <- loenmv.helepop[-nrowtab2xl,]

is.factor(discodata$membership)



#### histogrammer på DST 
library(ggplot2)
set.seed(6298) 
diamonds_small <- diamonds[sample(nrow(diamonds), 1000), ] 
hist_cut <- ggplot(diamonds_small, aes(x=price, fill=cut))
hist_cut + geom_bar() # defaults to stacking

a4<-structure(list(MAACP = c(2.81, 2.28, 2.38, 3.96, 1.99, 4.69, 
3.07, 3.41, 4.18, 1.67, 3.88, 2.89, 3.32, 4.15, 5.53, 2.19, 2.36, 
5.07, 3.18, 1.58, 2.81, 3.09, 4.9, 3.69, 3.48, 4.58, 4.54, 3.42, 
2.92, 3.83, 3.42, 2.51, 2.95, 3.97, 2.95, 5.55, 5.54, 3.39, 0, 
2.06, 5.21, 2.64, 5.66, 2.15, 3.87, 4.09, 2.81, 5.19, 3.55, 5.17, 
3.55, 2.19, 3.51, 4.9, 4.95, 2.91, 3.61, 3.06, 3.02, 3.49, 3.14, 
4.23, 4.11, 2.36, 2.81, 2.82, 2.94, 4.17, 4.14, 4.35, 3.52, 3.35, 
5.45, 3.41, 4.31, 4.31, 2.45, 3.35, 4.11, 3.21, 2.52, 4.91, 3.92, 
4.84, 2.84, 2.95, 2.98, 3.67, 4.75, 3.62, 2.88, 3.09, 3.36, 3.29, 
2.47, 3.59, 3.59, 2.6, 1.85, 5.06), type = structure(c(1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L, 
1L, 1L), .Label = c("1", "2"), class = "factor")), .Names = c("MAACP", 
"type"), row.names = c(NA, 100L), class = "data.frame")

hist_cut<-ggplot(a4,aes(x="MAACP",fill=type),binwidth=.1)
hist_cut+geom_bar()

hist_cut <- ggplot(a4, aes(x = MAACP, fill = type))
hist_cut + geom_bar(binwidth = .1)








dat <- data.frame(key = c("a1-a3", "a1-a2"), position = 1:100, value = rlnorm(200, 0, 1))

view(dat)
#Get quantiles
quants <- quantile(dat$value, c(0.95, 0.99))

dat$quant  <- with(dat, factor(ifelse(value < quants[1], 0, 
                                  ifelse(value < quants[2], 1, 2))))



ggplot(dat, aes(position, value)) + geom_point(aes(colour = quant)) + facet_wrap(~key) +
  scale_colour_manual(values = c("black", "blue", "red"), 
                      labels = c("0-95", "95-99", "99-100")) + theme_bw()










