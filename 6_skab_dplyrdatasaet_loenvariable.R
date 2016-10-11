# ### disco lavtløn 
discolavloen <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/discolavloen_kat150__helepop.xlsx")
discolavloen <- data.frame(matrix(unlist(discolavloen), nrow=nrowtab2xl),stringsAsFactors=FALSE)
# view(discolavloen)
# colbesk <- c(1, 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54)
# discolavloen <- discolavloen[,c(colbesk)]
# label_moneca_   <- list("moneca_label", "discolavloen1996" ,"discolavloen1997" , "discolavloen1998" , "discolavloen1999" , "discolavloen2000" , "discolavloen2001", "discolavloen2002" , "discolavloen2003" , "discolavloen2004" , "discolavloen2005", "discolavloen2006",  "discolavloen2007",  "discolavloen2008",  "discolavloen2009")
# colnames(discolavloen) <- label_moneca_[]
# l1_r            <- nrow(discolavloen)
# discolavloen[l1_r,1]            <- c(9999)
# discolavloen <- sapply(discolavloen, as.numeric)
# rownames(discolavloen) <- label[]
# discolavloen <-  discolavloen[,-1]
# # View(discolavloen)
# discolavloen           <- as.matrix(discolavloen) 
# discolavloen               <-   discolavloen[-nrowtab2xl,]
# label_moneca_discolavloen   <- list("discolavloen1996" ,"discolavloen1997" , "discolavloen1998" , "discolavloen1999" , "discolavloen2000" , "discolavloen2001", "discolavloen2002" , "discolavloen2003" , "discolavloen2004" , "discolavloen2005", "discolavloen2006",  "discolavloen2007",  "discolavloen2008",  "discolavloen2009")
# discolavloen <- disco.df(discolavloen, label_moneca_discolavloen)
# # View(discolavloen)


# # det her virker men uden column names 
# tilexcel <- discolavloen 
# for(var in 1:14) {
# tilexcel[[var+15]] <-  tilexcel[[var]]/sum(tilexcel[[var]])
# }


# tilexcel <- tilexcel %>% select(disco, discolavloen1996, V16, discolavloen1997, V17, discolavloen1998, V18, discolavloen1999, V19, discolavloen2000, V20, discolavloen2001, V21, discolavloen2002, V22, discolavloen2003, V23, discolavloen2004, V24, discolavloen2005, V25, discolavloen2006, V26, discolavloen2007, V27, discolavloen2008, V28, discolavloen2009, V29)


# view(tilexcel)
# ## lønanalyse lavtløn 

# tilexcel2 <- tilexcel[,-15]

# dd <- addmargins(tilexcel2, FUN = list(Total = sum), quiet = TRUE)






## joblon - gennemsnitsloen (alle alle hele populationen)

joblon.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/joblon_kat150__helepop.xlsx")
lst = readWorksheet(joblon.helepop, sheet = getSheets(joblon.helepop))
#lst
joblon.helepop <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
#View(joblon.helepop)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
joblon.helepop <- joblon.helepop[,c(columns)]
colnames(joblon.helepop) <- label_moneca_[]
#l1_r            <- nrow(joblon.helepop)
#l1_r
joblon.helepop <- sapply(joblon.helepop, as.numeric)
#View(joblon.helepop)
moneca.labels.num <- as.vector(joblon.helepop[, 1])
#View(moneca.labels.num)
joblon.helepop           <- as.matrix(joblon.helepop[, -1]) 

joblon.helepop           <- rbind(joblon.helepop, (colSums(joblon.helepop)/nrowputexcel))

rownames(joblon.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_joblon.helepop   <- list("joblon.helepop1996" ,"joblon.helepop1997" , "joblon.helepop1998" , "joblon.helepop1999" , "joblon.helepop2000" , "joblon.helepop2001", "joblon.helepop2002" , "joblon.helepop2003" , "joblon.helepop2004" , "joblon.helepop2005", "joblon.helepop2006",  "joblon.helepop2007",  "joblon.helepop2008",  "joblon.helepop2009")

joblon.helepop            <- disco.df(joblon.helepop, label_moneca_joblon.helepop)
joblon.helepop     <- joblon.helepop[-nrowtab2xl,]
# view(joblon.helepop)

## perindkialt - gennemsnitsloen (alle alle hele populationen)

# alle 
# perindkialt.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/perindkialt_kat150__helepop.xlsx")
# uden laveste 5.000
perindkialt.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/perindkialt_u5000_kat150__helepop.xlsx")

lst = readWorksheet(perindkialt.helepop, sheet = getSheets(perindkialt.helepop))
#lst
perindkialt.helepop <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# View(perindkialt.helepop)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
perindkialt.helepop <- perindkialt.helepop[,c(columns)]
colnames(perindkialt.helepop) <- label_moneca_[]
#l1_r            <- nrow(perindkialt.helepop)
#l1_r
perindkialt.helepop <- sapply(perindkialt.helepop, as.numeric)
#View(perindkialt.helepop)
moneca.labels.num <- as.vector(perindkialt.helepop[, 1])
#View(moneca.labels.num)
perindkialt.helepop           <- as.matrix(perindkialt.helepop[, -1]) 
perindkialt.helepop           <- rbind(perindkialt.helepop, (colSums(perindkialt.helepop)/nrowputexcel))
rownames(perindkialt.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_perindkialt.helepop   <- list("perindkialt.helepop1996" ,"perindkialt.helepop1997" , "perindkialt.helepop1998" , "perindkialt.helepop1999" , "perindkialt.helepop2000" , "perindkialt.helepop2001", "perindkialt.helepop2002" , "perindkialt.helepop2003" , "perindkialt.helepop2004" , "perindkialt.helepop2005", "perindkialt.helepop2006",  "perindkialt.helepop2007",  "perindkialt.helepop2008",  "perindkialt.helepop2009")

perindkialt.helepop            <- disco.df(perindkialt.helepop, label_moneca_perindkialt.helepop)
perindkialt.helepop     <- perindkialt.helepop[-nrowtab2xl,]
# view(perindkialt.helepop)

# 321930.87960357


## DISPON_NY - gennemsnitsloen (alle alle hele populationen)

disponny.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/DISPON_NYny_kat150__helepop.xlsx")
lst = readWorksheet(disponny.helepop, sheet = getSheets(disponny.helepop))
#lst
disponny.helepop <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
#View(disponny.helepop)
columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
disponny.helepop <- disponny.helepop[,c(columns)]
colnames(disponny.helepop) <- label_moneca_[]
#l1_r            <- nrow(disponny.helepop)
#l1_r
disponny.helepop <- sapply(disponny.helepop, as.numeric)
#View(disponny.helepop)
moneca.labels.num <- as.vector(disponny.helepop[, 1])
#View(moneca.labels.num)
disponny.helepop           <- as.matrix(disponny.helepop[, -1]) 

disponny.helepop           <- rbind(disponny.helepop, (colSums(disponny.helepop)/nrowputexcel))

rownames(disponny.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

#dplyr conversion
label_moneca_disponny.helepop   <- list("disponny.helepop1996" ,"disponny.helepop1997" , "disponny.helepop1998" , "disponny.helepop1999" , "disponny.helepop2000" , "disponny.helepop2001", "disponny.helepop2002" , "disponny.helepop2003" , "disponny.helepop2004" , "disponny.helepop2005", "disponny.helepop2006",  "disponny.helepop2007",  "disponny.helepop2008",  "disponny.helepop2009")

disponny.helepop            <- disco.df(disponny.helepop, label_moneca_disponny.helepop)
disponny.helepop     <- disponny.helepop[-nrowtab2xl,]


## Loenmv - gennemsnitsloen (alle hele populationen)

loenmv.helepop <- XLConnect::loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/loenmv_kat150__helepop.xlsx")
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

# ################## gamle lønvariable med MONECA løn ######

# ## Loenmv - gennemsnitsloen (ledige)

# loenmv <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/sociosocstil/baggrundsvar/loenmv_kat150__socstilsocio_version1.xlsx")
# lst = readWorksheet(loenmv, sheet = getSheets(loenmv))
# #lst
# loenmv <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# #View(loenmv)
# columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
# loenmv <- loenmv[,c(columns)]
# colnames(loenmv) <- label_moneca_[]
# #l1_r            <- nrow(loenmv)
# #l1_r
# loenmv <- sapply(loenmv, as.numeric)
# #View(loenmv)
# moneca.labels.num <- as.vector(loenmv[, 1])
# #View(moneca.labels.num)
# loenmv           <- as.matrix(loenmv[, -1]) 

# loenmv           <- rbind(loenmv, (colSums(loenmv)/nrowputexcel))

# rownames(loenmv) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

# #dplyr conversion
# label_moneca_loenmv   <- list("loenmv1996" ,"loenmv1997" , "loenmv1998" , "loenmv1999" , "loenmv2000" , "loenmv2001", "loenmv2002" , "loenmv2003" , "loenmv2004" , "loenmv2005", "loenmv2006",  "loenmv2007",  "loenmv2008",  "loenmv2009")

# loenmv            <- disco.df(loenmv, label_moneca_loenmv)
# loenmv     <- loenmv[-nrowtab2xl,]
# # view(loenmv.tmp1)

# # ## Loenmv - gennemsnitsloen (alle beskæftigede)

# loenmv.helepop <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/loenmv_kat150__helepop_fikset.xlsx")
# lst = readWorksheet(loenmv.helepop, sheet = getSheets(loenmv.helepop))
# #lst
# loenmv.helepop <- data.frame(matrix(unlist(lst), nrow=nrowputexcel),stringsAsFactors=FALSE)
# #View(loenmv.helepop)
# columns <- c(1, 4, 13, 22, 31, 40, 49, 58, 67, 76, 85, 94, 103, 112, 121) #, 26, 30, 34, 38, 42, 46, 50, 54)
# loenmv.helepop <- loenmv.helepop[,c(columns)]
# colnames(loenmv.helepop) <- label_moneca_[]
# #l1_r            <- nrow(loenmv.helepop)
# #l1_r
# loenmv.helepop <- sapply(loenmv.helepop, as.numeric)
# #View(loenmv.helepop)
# moneca.labels.num <- as.vector(loenmv.helepop[, 1])
# #View(moneca.labels.num)
# loenmv.helepop           <- as.matrix(loenmv.helepop[, -1]) 

# loenmv.helepop           <- rbind(loenmv.helepop, (colSums(loenmv.helepop)/nrowputexcel))

# rownames(loenmv.helepop) <- label[] #tager label-objektet og s?tter labels på fra det, minus totalen

# #dplyr conversion
# label_moneca_loenmv.helepop   <- list("loenmv.helepop1996" ,"loenmv.helepop1997" , "loenmv.helepop1998" , "loenmv.helepop1999" , "loenmv.helepop2000" , "loenmv.helepop2001", "loenmv.helepop2002" , "loenmv.helepop2003" , "loenmv.helepop2004" , "loenmv.helepop2005", "loenmv.helepop2006",  "loenmv.helepop2007",  "loenmv.helepop2008",  "loenmv.helepop2009")

# loenmv.helepop            <- disco.df(loenmv.helepop, label_moneca_loenmv.helepop)
# loenmv.helepop     <- loenmv.helepop[-nrowtab2xl,]
# # view(loenmv.helepop)

