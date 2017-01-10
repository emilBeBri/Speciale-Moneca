# labels
lab.disco.data <- select(discodata,disco_s) %>% 	 arrange(disco_s)
# is.factor.df$membership)
lab.disco.data$tmp.raek <- seq(1:273)

# vector til udvælgelse af variable
tmp1 <-  c(4,5,6,7,8,9)
tmp2 <-  tmp1 +	rep(c(9),each=6)
tmplist <- list() 
for (i in seq(1:12)) { 
tmp <-  tmp2 + (rep(c(9),each=6)*i)
i <- i+2
tmplist[[i]] <- tmp  
}
tmplist[[1]] <-  c(1,4,5,6,7,8,9)
tmplist[[2]] <-  tmp1 +	rep(c(9),each=6)
emilsvector <-  unlist(tmplist)

emilsvector.bin <- seq(5,56,4)  
# emilsvector.bin <- append(emilsvector.bin,c(1))
# emilsvector.bin <- sort(emilsvector.bin)


##### fagforening - gul ###
gule.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/fagforening_gulesataner.xlsx")
gule.helepop <- data.frame(matrix(unlist(gule.helepop), nrow=274),stringsAsFactors=FALSE)
nyvector <- c(1,2,6,9,12,16,20,24,28,31,34,38,41,45,48)
gule.helepop <- gule.helepop[-274,c(nyvector)]
colnames(gule.helepop)[1] <- c("disco_s")
allbeskaeft_tmp <- tbl_df(allbeskaeft)
colnames(allbeskaeft_tmp)[1] <- c("disco_s")
gule.helepop <- left_join(gule.helepop,allbeskaeft_tmp)
gule.helepop <- gule.helepop %>%  select(disco,
X2, `1996`,
X6, `1997`,
X9, `1998`,
X12, `1999`,
X16, `2000`,
X20, `2001`,
X24, `2002`,
X28, `2003`,
X31, `2004`,
X34, `2005`,
X38, `2006`,
X41, `2007`,
X45, `2008`,
X48, `2009`)
gule.helepop <- gule.helepop %>% mutate(
gule.andel.1996 = X2 / `1996`,
gule.andel.1997 = X6 / `1997`,
gule.andel.1998 = X9 / `1998`,
gule.andel.1999 = X12 / `1999`,
gule.andel.2000 = X16 / `2000`,
gule.andel.2001 = X20 / `2001`,
gule.andel.2002 = X24 / `2002`,
gule.andel.2003 = X28 / `2003`,
gule.andel.2004 = X31 / `2004`,
gule.andel.2005 = X34 / `2005`,
gule.andel.2006 = X38 / `2006`,
gule.andel.2007 = X41 / `2007`,
gule.andel.2008 = X45 / `2008`,
gule.andel.2009 = X48 / `2009`) 
gule.helepop$gule.mean.gns <- gule.helepop %>%  select(contains("andel")) %>% rowMeans()

gule.helepop.udv <- gule.helepop %>%  select(disco_s,contains("gns")) 

gule.helepop.udv$disco_s <- as.character(gule.helepop.udv$disco_s)




#########alder ###


## alder 
alder.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/aldernov2_kat250_helepop.xlsx")
alder.helepop <- data.frame(matrix(unlist(alder.helepop), nrow=273),stringsAsFactors=FALSE)

alder.helepop <- alder.helepop[,c(emilsvector)]
# view(alder.helepop)
alder.helepop[,1] <- strtrim(alder.helepop[,1], 4)
alder.helepop[1,1] <- 110
# colnames(alder.helepop) <-.df$membership[]
#l1_r            <- nrow(alder.helepop)
#l1_r
alder.helepop <- sapply(alder.helepop, as.numeric)
# view(alder.helepop)
# moneca.labels.num <- as.vector(alder.helepop[, 1])
#View(moneca.labels.num)
# alder.helepop           <- as.matrix(alder.helepop[, -1]) 
# view(alder.helepop)

# alder.helepop           <- rbind(alder.helepop, (colSums(alder.helepop)/51))
# view(cbind(alder.helepop,lab.disco.data$tmp.raek))

rownames(alder.helepop) <- as.character(lab.disco.data$disco_s) #tager label-objektet og s?tter labels på fra det, minus totalen

yearly.name.template <- c(
  "alder.mean.", 
  "alder.var.",
  "alder.sd.",
  "alder.min.",
  "alder.max.",
  "alder.total."
)
# create the names for your list titles.
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)

# apply(x,1,FUN,...) works row-wise on the data frame or matrix.
# 'collapse' is needed to merge the entire line.
all.names <- apply(all.names.df, 1, paste0, collapse="")

# now create a list of blank entries, one entry per list title.
# alder.list <- vector("list", length(all.names))
# and push the names in.
# names(alder.list) <- all.names

label_alder.helepop <-  append(all.names,c("disco_s"),after=0)

alder.helepop            <- disco.df.s(alder.helepop, label_alder.helepop)




alder.helepop$alder.mean.gns <- alder.helepop %>% 	select(contains("mean")) %>% rowMeans()
alder.helepop$alder.var.gns <- alder.helepop %>% 	select(contains("var")) %>% rowMeans()
alder.helepop$alder.sd.gns <- alder.helepop %>% 	select(contains("sd")) %>% rowMeans()
alder.helepop$alder.min.gns <- alder.helepop %>% 	select(contains("min")) %>% rowMeans()
alder.helepop$alder.max.gns <- alder.helepop %>% 	select(contains("max")) %>% rowMeans()
alder.helepop$alder.total.gns <- alder.helepop %>% 	select(contains("total")) %>% rowMeans()
# view(alder.helepop)

#udvælger variabel til discodata
alder.helepop.udv <- alder.helepop %>% 	select(disco_s,contains("gns")) %>%   select(-contains("var"))



#########timelon ###


## timelon 
timelon.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/timelon_udenti_kat250__helepop_INF.xlsx")
timelon.helepop <- data.frame(matrix(unlist(timelon.helepop), nrow=273),stringsAsFactors=FALSE)

timelon.helepop <- timelon.helepop[,c(emilsvector)]
# view(timelon.helepop)
timelon.helepop[,1] <- strtrim(timelon.helepop[,1], 4)
timelon.helepop[1,1] <- 110
# colnames(timelon.helepop) <-.df$membership[]
#l1_r            <- nrow(timelon.helepop)
#l1_r
timelon.helepop <- sapply(timelon.helepop, as.numeric)
# view(timelon.helepop)
# moneca.labels.num <- as.vector(timelon.helepop[, 1])
#View(moneca.labels.num)
# timelon.helepop           <- as.matrix(timelon.helepop[, -1]) 
# view(timelon.helepop)

# timelon.helepop           <- rbind(timelon.helepop, (colSums(timelon.helepop)/51))
# view(cbind(timelon.helepop,lab.disco.data$tmp.raek))

rownames(timelon.helepop) <- as.character(lab.disco.data$disco_s) #tager label-objektet og s?tter labels på fra det, minus totalen

yearly.name.template <- c(
  "timelon.mean.", 
  "timelon.var.",
  "timelon.sd.",
  "timelon.min.",
  "timelon.max.",
  "timelon.total."
)
# create the names for your list titles.
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)

# apply(x,1,FUN,...) works row-wise on the data frame or matrix.
# 'collapse' is needed to merge the entire line.
all.names <- apply(all.names.df, 1, paste0, collapse="")

# now create a list of blank entries, one entry per list title.
# timelon.list <- vector("list", length(all.names))
# and push the names in.
# names(timelon.list) <- all.names

label_timelon.helepop <-  append(all.names,c("disco_s"),after=0)

timelon.helepop            <- disco.df.s(timelon.helepop, label_timelon.helepop)


timelon.helepop$timelon.mean.gns <- timelon.helepop %>% 	select(contains("mean")) %>% rowMeans()
timelon.helepop$timelon.var.gns <- timelon.helepop %>% 	select(contains("var")) %>% rowMeans()
timelon.helepop$timelon.sd.gns <- timelon.helepop %>% 	select(contains("sd")) %>% rowMeans()
timelon.helepop$timelon.min.gns <- timelon.helepop %>% 	select(contains("min")) %>% rowMeans()
timelon.helepop$timelon.max.gns <- timelon.helepop %>% 	select(contains("max")) %>% rowMeans()
timelon.helepop$timelon.total.gns <- timelon.helepop %>% 	select(contains("total")) %>% rowMeans()
# view(timelon.helepop)

#udvælger variabel til discodata
timelon.helepop.udv <- timelon.helepop %>% 	select(disco_s,contains("gns")) %>%   select(-contains("var"))

#########ledighed ###


## ledighed 
ledighed.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/arledgr_kat250_helepop.xlsx")
ledighed.helepop <- data.frame(matrix(unlist(ledighed.helepop), nrow=273),stringsAsFactors=FALSE)
ledighed.helepop <- ledighed.helepop[,c(emilsvector)]
# view(ledighed.helepop)
ledighed.helepop[,1] <- strtrim(ledighed.helepop[,1], 4)
ledighed.helepop[1,1] <- 110
# colnames(ledighed.helepop) <-.df$membership[]
#l1_r            <- nrow(ledighed.helepop)
#l1_r
ledighed.helepop <- sapply(ledighed.helepop, as.numeric)
ledighed.helepop[,-1] <- ledighed.helepop[,-1]/1000 # fra promille til procent

# moneca.labels.num <- as.vector(ledighed.helepop[, 1])
#View(moneca.labels.num)
# ledighed.helepop           <- as.matrix(ledighed.helepop[, -1]) 
# view(ledighed.helepop)

# ledighed.helepop           <- rbind(ledighed.helepop, (colSums(ledighed.helepop)/51))
# view(cbind(ledighed.helepop,lab.disco.data$tmp.raek))

rownames(ledighed.helepop) <- as.character(lab.disco.data$disco_s) #tager label-objektet og s?tter labels på fra det, minus totalen

yearly.name.template <- c(
  "ledighed.mean.", 
  "ledighed.var.",
  "ledighed.sd.",
  "ledighed.min.",
  "ledighed.max.",
  "ledighed.total."
)
# create the names for your list titles.
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)

# apply(x,1,FUN,...) works row-wise on the data frame or matrix.
# 'collapse' is needed to merge the entire line.
all.names <- apply(all.names.df, 1, paste0, collapse="")

# now create a list of blank entries, one entry per list title.
# ledighed.list <- vector("list", length(all.names))
# and push the names in.
# names(ledighed.list) <- all.names

label_ledighed.helepop <-  append(all.names,c("disco_s"),after=0)

ledighed.helepop            <- disco.df.s(ledighed.helepop, label_ledighed.helepop)


ledighed.helepop$ledighed.mean.gns <- ledighed.helepop %>% 	select(contains("mean")) %>% rowMeans()
ledighed.helepop$ledighed.var.gns <- ledighed.helepop %>% 	select(contains("var")) %>% rowMeans()
ledighed.helepop$ledighed.sd.gns <- ledighed.helepop %>% 	select(contains("sd")) %>% rowMeans()
ledighed.helepop$ledighed.min.gns <- ledighed.helepop %>% 	select(contains("min")) %>% rowMeans()
ledighed.helepop$ledighed.max.gns <- ledighed.helepop %>% 	select(contains("max")) %>% rowMeans()
ledighed.helepop$ledighed.total.gns <- ledighed.helepop %>% 	select(contains("total")) %>% rowMeans()
# view(ledighed.helepop)


#udvælger variabel til discodata
ledighed.helepop.udv <- ledighed.helepop %>% 	select(disco_s,contains("gns")) %>%   select(-contains("var"))


#tidsserier 

led.tid.df <-  ledighed.helepop %>% select(disco_s,contains("mean.199"),contains("mean.200"))
noeglevar.df <-  discodata %>% select(disco_s, membership, `2: Segment`,disco) 
led.tid.df <-  left_join(led.tid.df,noeglevar.df) %>%    select(-disco_s) %>%   select(disco,membership, `2: Segment`,everything()) 
# led.sd.tid <-  ledighed.helepop %>% select(contains("mean.199"),contains("mean.200")) %>%   apply(.,1,sd)
# led.tid.df <- cbind(led.tid.df,led.sd.tid)


led.tid.df <- led.tid.df %>%  arrange(membership,`2: Segment`)





# view(ledighed.helepop.udv)


#########koen ###

koen.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/koen_kat250__helepop.xlsx")
koen.helepop <- data.frame(matrix(unlist(koen.helepop), nrow=273),stringsAsFactors=FALSE)

koen.helepop <- koen.helepop[,-c(emilsvector.bin)]
# view(koen.helepop)
koen.helepop[,1] <- strtrim(koen.helepop[,1], 4)
koen.helepop[1,1] <- 110
# colnames(koen.helepop) <-.df$membership[]
#l1_r            <- nrow(koen.helepop)
#l1_r
koen.helepop <- sapply(koen.helepop, as.numeric)
# view(koen.helepop)
# moneca.labels.num <- as.vector(koen.helepop[, 1])
#View(moneca.labels.num)
# koen.helepop           <- as.matrix(koen.helepop[, -1]) 
# view(koen.helepop)

# koen.helepop           <- rbind(koen.helepop, (colSums(koen.helepop)/51))
# view(cbind(koen.helepop,lab.disco.data$tmp.raek))

rownames(koen.helepop) <- as.character(lab.disco.data$disco_s) #tager label-objektet og s?tter labels på fra det, minus totalen

yearly.name.template <- c( "koen.total.",  "koen.maend.", "koen.kvinder." )
# create the names for your list titles.
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)

# apply(x,1,FUN,...) works row-wise on the data frame or matrix.
# 'collapse' is needed to merge the entire line.
all.names <- apply(all.names.df, 1, paste0, collapse="")

# now create a list of blank entries, one entry per list title.
# koen.list <- vector("list", length(all.names))
# and push the names in.
# names(koen.list) <- all.names

label_koen.helepop <-  append(all.names,c("disco_s"),after=0)
koen.helepop            <- disco.df.s(koen.helepop, label_koen.helepop)



koen.helepop$koen.total.gns <- koen.helepop %>% 	select(contains("total")) %>% rowMeans()
koen.helepop$koen.maend.gns <- koen.helepop %>% 	select(contains("maend")) %>% rowMeans()
koen.helepop$koen.kvinder.gns <- koen.helepop %>% 	select(contains("kvinder")) %>% rowMeans()


koen.helepop <-    koen.helepop %>%
    mutate(koen.gns.kvinder.andel = koen.kvinder.gns/koen.total.gns)

sd_bernoulli <- function(x){
	sqrt(x*(1-x))
}

koen.helepop <- koen.helepop %>% 	 rowwise()  %>% 	 mutate_each(., funs( koen.gns.kvinder.andel.sd = sd_bernoulli), contains("koen.gns.kvinder.andel"))

#udvælger variabel til discodata
koen.helepop.udv <- koen.helepop %>% 	select(disco_s,contains("gns")) %>%   select(-contains("var")) 


view(koen.helepop)

###################### join på disco #########################


#udvælgelse af variable

# discodata <- discodata %>% 	select(-contains("timelon"),-contains("alder"),-contains("koen"),-contains("ledighed"))

discodata     <- left_join(discodata, timelon.helepop.udv)
discodata     <- left_join(discodata, alder.helepop.udv)
discodata     <- left_join(discodata, koen.helepop.udv)
discodata     <- left_join(discodata, ledighed.helepop.udv)
discodata     <- left_join(discodata, gule.helepop.udv)












