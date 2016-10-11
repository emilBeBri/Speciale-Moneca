#########lønvariable###


## timelon 
timelon.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop.xlsx")
timelon.seg.helepop <- data.frame(matrix(unlist(timelon.seg.helepop), nrow=51),stringsAsFactors=FALSE)
# aabn_xls("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop.xlsx")
tmp1 <-  c(4,5,6,7,8,9)
tmp2 <-  tmp1 +	rep(c(9),each=6)
tmplist <- list() 
for (i in seq(1:12)) { 
tmp <-  tmp2 + (rep(c(9),each=6)*i)
i <- i+1
tmplist[[i]] <- tmp  
}
tmplist[[1]] <-  c(1,4,5,6,7,8,9)
emilsvector <-  unlist(tmplist)
timelon.seg.helepop <- timelon.seg.helepop[,c(emilsvector)]
view(timelon.seg.helepop)

view(df)

fix_seg_labels <- timelon.seg.helepop[,1]
colnames(timelon.seg.helepop) <- seg.df$membership[]


#l1_r            <- nrow(timelon.seg.helepop)
#l1_r
timelon.seg.helepop <- sapply(timelon.seg.helepop, as.numeric)
# View(timelon.seg.helepop)
moneca.labels.num <- as.vector(timelon.seg.helepop[, 1])
#View(moneca.labels.num)
timelon.seg.helepop           <- as.matrix(timelon.seg.helepop[, -1]) 

timelon.seg.helepop           <- rbind(timelon.seg.helepop, (colSums(timelon.seg.helepop)/49))

rownames(timelon.seg.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_timelon.seg.helepop   <- list("timelon.seg.helepop1996" ,"timelon.seg.helepop1997" , "timelon.seg.helepop1998" , "timelon.seg.helepop1999" , "timelon.seg.helepop2000" , "timelon.seg.helepop2001", "timelon.seg.helepop2002" , "timelon.seg.helepop2003" , "timelon.seg.helepop2004" , "timelon.seg.helepop2005", "timelon.seg.helepop2006",  "timelon.seg.helepop2007",  "timelon.seg.helepop2008",  "timelon.seg.helepop2009")

timelon.seg.helepop            <- disco.df(timelon.seg.helepop, label_moneca_timelon.seg.helepop)
timelon.seg.helepop     <- timelon.seg.helepop[-nrowtab2xl,]
# view(timelon.seg.helepop)



# source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_loenvariable.R")
# 


## alder 

alder.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_aldernov2_kat250_helepop.xlsx")
alder.seg.helepop <- data.frame(matrix(unlist(alder.seg.helepop), nrow=51),stringsAsFactors=FALSE)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121)
alder.seg.helepop <- alder.seg.helepop[,c(columns)]
colnames(alder.seg.helepop) <- label_moneca_[]


alder.seg.helepop$moneca_label <- fix_disco_labels
alder.seg.helepop <- sapply(alder.seg.helepop, as.numeric)
# view(alder.seg.helepop)
moneca.labels.num <- as.vector(alder.seg.helepop[, 1])
# View(moneca.labels.num)
alder.seg.helepop           <- as.matrix(alder.seg.helepop[, -1]) 
alder.seg.helepop           <- rbind(alder.seg.helepop, (colSums(alder.seg.helepop)/49))

rownames(alder.seg.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_alder.seg.helepop   <- list("alder.seg.helepop1996" ,"alder.seg.helepop1997" , "alder.seg.helepop1998" , "alder.seg.helepop1999" , "alder.seg.helepop2000" , "alder.seg.helepop2001", "alder.seg.helepop2002" , "alder.seg.helepop2003" , "alder.seg.helepop2004" , "alder.seg.helepop2005", "alder.seg.helepop2006",  "alder.seg.helepop2007",  "alder.seg.helepop2008",  "alder.seg.helepop2009")

alder.seg.helepop            <- disco.df(alder.seg.helepop, label_moneca_alder.seg.helepop)
alder.seg.helepop     <- alder.seg.helepop[-nrowtab2xl,]
# view(alder.seg.helepop)


## koen 
koen.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_koen_kat250__helepop.xlsx")
koen.seg.helepop <- data.frame(matrix(unlist(koen.seg.helepop), nrow=51),stringsAsFactors=FALSE)
columns <- seq(2,56,2)
columns <-  append(columns, 1, after = 0)
koen.seg.helepop <- koen.seg.helepop[,c(columns)]
label_moneca_koen <- c("moneca_label","koen1996total", "koen1996kvinder","koen1997total", "koen1997kvinder","koen1998total", "koen1998kvinder","koen1999total", "koen1999kvinder","koen2000total","koen2000kvinder","koen2001total","koen2001kvinder","koen2002total","koen2002kvinder","koen2003total","koen2003kvinder","koen2004total","koen2004kvinder","koen2005total","koen2005kvinder","koen2006total","koen2006kvinder","koen2007total","koen2007kvinder","koen2008total","koen2008kvinder","koen2009total","koen2009kvinder")
colnames(koen.seg.helepop) <- label_moneca_koen[]
#l1_r            <- nrow(koen.seg.helepop)
#l1_r
koen.seg.helepop <- sapply(koen.seg.helepop, as.numeric)
 # View(koen.seg.helepop)
moneca.labels.num <- as.vector(koen.seg.helepop[, 1])
# View(moneca.labels.num)
koen.seg.helepop           <- as.matrix(koen.seg.helepop[, -1]) 
koen.seg.helepop           <- rbind(koen.seg.helepop, (colSums(koen.seg.helepop)/49))

rownames(koen.seg.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_koen.seg.helepop   <- list("koen1996total", "koen1996kvinder","koen1997total", "koen1997kvinder","koen1998total", "koen1998kvinder","koen1999total", "koen1999kvinder","koen2000total","koen2000kvinder","koen2001total","koen2001kvinder","koen2002total","koen2002kvinder","koen2003total","koen2003kvinder","koen2004total","koen2004kvinder","koen2005total","koen2005kvinder","koen2006total","koen2006kvinder","koen2007total","koen2007kvinder","koen2008total","koen2008kvinder","koen2009total","koen2009kvinder")

koen.seg.helepop            <- disco.df(koen.seg.helepop, label_moneca_koen.seg.helepop)
koen.seg.helepop     <- koen.seg.helepop[-nrowtab2xl,]
# View(koen.seg.helepop)


## ledighed 
ledighed.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_arledgr_kat250_helepop.xlsx")
ledighed.seg.helepop <- data.frame(matrix(unlist(ledighed.seg.helepop), nrow=51),stringsAsFactors=FALSE)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121)
ledighed.seg.helepop <- ledighed.seg.helepop[,c(columns)]
colnames(ledighed.seg.helepop) <- label_moneca_[]
#l1_r            <- nrow(ledighed.seg.helepop)
#l1_r
# view(ledighed.seg.helepop)

ledighed.seg.helepop$moneca_label <- fix_disco_labels
ledighed.seg.helepop <- sapply(ledighed.seg.helepop, as.numeric)
 # View(ledighed.seg.helepop)
moneca.labels.num <- as.vector(ledighed.seg.helepop[, 1])
# View(moneca.labels.num)
ledighed.seg.helepop           <- as.matrix(ledighed.seg.helepop[, -1]) 

ledighed.seg.helepop           <- rbind(ledighed.seg.helepop, (colSums(ledighed.seg.helepop)/49))

rownames(ledighed.seg.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_ledighed.seg.helepop   <- list("ledighed.seg.helepop1996" ,"ledighed.seg.helepop1997" , "ledighed.seg.helepop1998" , "ledighed.seg.helepop1999" , "ledighed.seg.helepop2000" , "ledighed.seg.helepop2001", "ledighed.seg.helepop2002" , "ledighed.seg.helepop2003" , "ledighed.seg.helepop2004" , "ledighed.seg.helepop2005", "ledighed.seg.helepop2006",  "ledighed.seg.helepop2007",  "ledighed.seg.helepop2008",  "ledighed.seg.helepop2009")

ledighed.seg.helepop            <- disco.df(ledighed.seg.helepop, label_moneca_ledighed.seg.helepop)
ledighed.seg.helepop     <- ledighed.seg.helepop[-nrowtab2xl,]
# view(ledighed.seg.helepop)



###################### join på disco #########################


discodata     <- left_join(discodata, timelon.seg.helepop)
discodata     <- left_join(discodata, alder.seg.helepop)
discodata     <- left_join(discodata, koen.seg.helepop)
discodata     <- left_join(discodata, ledighed.seg.helepop)
