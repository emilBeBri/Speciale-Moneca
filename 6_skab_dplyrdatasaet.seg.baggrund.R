
# labels til segmenter 
lab.seg.data <- select(seg.df,membership) %>% 	 arrange(membership)
# is.factor(seg.df$membership)
lab.seg.data$tmp.raek <- seq(1:51)

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


#########lønvariable###


## timelon 
timelon.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop.xlsx")
timelon.seg.helepop <- data.frame(matrix(unlist(timelon.seg.helepop), nrow=51),stringsAsFactors=FALSE)
# aabn_xls("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop.xlsx")

timelon.seg.helepop <- timelon.seg.helepop[,c(emilsvector)]
# view(timelon.seg.helepop)
# fix_seg_labels <- timelon.seg.helepop[,1]
# colnames(timelon.seg.helepop) <- seg.df$membership[]
#l1_r            <- nrow(timelon.seg.helepop)
#l1_r
timelon.seg.helepop <- sapply(timelon.seg.helepop, as.numeric)
# View(timelon.seg.helepop)
# moneca.labels.num <- as.vector(timelon.seg.helepop[, 1])
#View(moneca.labels.num)
# timelon.seg.helepop           <- as.matrix(timelon.seg.helepop[, -1]) 
# view(timelon.seg.helepop)

# timelon.seg.helepop           <- rbind(timelon.seg.helepop, (colSums(timelon.seg.helepop)/51))
# view(cbind(timelon.seg.helepop,lab.seg.data$tmp.raek))

rownames(timelon.seg.helepop) <- as.character(lab.seg.data$membership) #tager label-objektet og s?tter labels på fra det, minus totalen

yearly.name.template <- c(
  "timelon.seg.mean.", 
  "timelon.seg.var.",
  "timelon.seg.sd.",
  "timelon.seg.min.",
  "timelon.seg.max.",
  "timelon.seg.total."
)
# create the names for your list titles.
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)

# apply(x,1,FUN,...) works row-wise on the data frame or matrix.
# 'collapse' is needed to merge the entire line.
all.names <- apply(all.names.df, 1, paste0, collapse="")

# now create a list of blank entries, one entry per list title.
# timelon.seg.list <- vector("list", length(all.names))
# and push the names in.
# names(timelon.seg.list) <- all.names

label_seg_timelon.seg.helepop <-  append(all.names,c("membership"),after=0)

timelon.seg.helepop            <- make.seg.df(timelon.seg.helepop, label_seg_timelon.seg.helepop)

## alder 

alder.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_aldernov2_kat250_helepop.xlsx")
alder.seg.helepop <- data.frame(matrix(unlist(alder.seg.helepop), nrow=51),stringsAsFactors=FALSE)
alder.seg.helepop <- alder.seg.helepop[,c(emilsvector)]
# view(alder.seg.helepop)
alder.seg.helepop <- sapply(alder.seg.helepop, as.numeric)
rownames(alder.seg.helepop) <- as.character(lab.seg.data$membership) 
yearly.name.template <- c(
  "alder.seg.mean.", 
  "alder.seg.var.",
  "alder.seg.sd.",
  "alder.seg.min.",
  "alder.seg.max.",
  "alder.seg.total."
)
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
all.names <- apply(all.names.df, 1, paste0, collapse="")
label_seg_alder.seg.helepop <-  append(all.names,c("membership"),after=0)
alder.seg.helepop            <- make.seg.df(alder.seg.helepop, label_seg_alder.seg.helepop)
                                            

## koen 

koen.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_koen_kat250_helepop.xlsx")
koen.seg.helepop <- data.frame(matrix(unlist(koen.seg.helepop), nrow=51),stringsAsFactors=FALSE)
columns <- seq(2,56,2)
columns <-  append(columns, 1, after = 0)
koen.seg.helepop <- koen.seg.helepop[,c(columns)]
# view(koen.seg.helepop)
koen.seg.helepop <- sapply(koen.seg.helepop, as.numeric)
rownames(koen.seg.helepop) <- as.character(lab.seg.data$membership) 
yearly.name.template <- c(
  "koen.seg.total.", 
  "koen.seg.kvinder."
)

all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
all.names <- apply(all.names.df, 1, paste0, collapse="")
label_seg_koen.seg.helepop <-  append(all.names,c("membership"),after=0)
koen.seg.helepop            <- make.seg.df(koen.seg.helepop, label_seg_koen.seg.helepop)

## ledighed 
ledighed.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_arledgr_kat250_helepop.xlsx")
ledighed.seg.helepop <- data.frame(matrix(unlist(ledighed.seg.helepop), nrow=51),stringsAsFactors=FALSE)
ledighed.seg.helepop <- ledighed.seg.helepop[,c(emilsvector)]
# view(ledighed.seg.helepop)
ledighed.seg.helepop <- sapply(ledighed.seg.helepop, as.numeric)
rownames(ledighed.seg.helepop) <- as.character(lab.seg.data$membership) 
yearly.name.template <- c(
  "ledighed.seg.mean.", 
  "ledighed.seg.var.",
  "ledighed.seg.sd.",
  "ledighed.seg.min.",
  "ledighed.seg.max.",
  "ledighed.seg.total."
)
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
all.names <- apply(all.names.df, 1, paste0, collapse="")
label_seg_ledighed.seg.helepop <-  append(all.names,c("membership"),after=0)
ledighed.seg.helepop            <- make.seg.df(ledighed.seg.helepop, label_seg_ledighed.seg.helepop)
            

###################### join på disco #########################

discodata     <- inner_join(discodata, timelon.seg.helepop)
discodata     <- inner_join(discodata, alder.seg.helepop)
discodata     <- inner_join(discodata, koen.seg.helepop)
discodata     <- inner_join(discodata, ledighed.seg.helepop)




view(discodata)
