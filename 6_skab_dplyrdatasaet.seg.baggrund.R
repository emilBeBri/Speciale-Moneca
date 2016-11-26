
# labels til segmenter 
lab.seg.data <- select(seg.df,membership) %>% 	 arrange(membership)
# is.factor(seg.df$membership)
lab.seg.data$tmp.raek <- seq(1:54)

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


#########lønvariable###


## timelon 
timelon.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter54/segmenter51_timelon_udenti_kat250__helepop_INF.xlsx")
timelon.seg.helepop <- data.frame(matrix(unlist(timelon.seg.helepop), nrow=54),stringsAsFactors=FALSE)

timelon.seg.helepop <- timelon.seg.helepop[,c(emilsvector)]
timelon.seg.helepop <- sapply(timelon.seg.helepop, as.numeric)
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


timelon.seg.helepop$timelon.mean.seg.gns <- timelon.seg.helepop %>%   select(contains("mean")) %>% rowMeans()
timelon.seg.helepop$timelon.var.seg.gns <- timelon.seg.helepop %>%  select(contains("var")) %>% rowMeans()
timelon.seg.helepop$timelon.sd.seg.gns <- timelon.seg.helepop %>%   select(contains("sd")) %>% rowMeans()
timelon.seg.helepop$timelon.min.seg.gns <- timelon.seg.helepop %>%  select(contains("min")) %>% rowMeans()
timelon.seg.helepop$timelon.max.seg.gns <- timelon.seg.helepop %>%  select(contains("max")) %>% rowMeans()
timelon.seg.helepop$timelon.total.seg.gns <- timelon.seg.helepop %>%  select(contains("total")) %>% rowMeans()
# view(timelon.seg.helepop)

#udvælger variabel til segdata
timelon.seg.helepop.udv <- timelon.seg.helepop %>%  select(membership,contains("gns")) 

timelon.seg.helepop.udv <- timelon.seg.helepop.udv %>%  select(-contains("var")) 



#########koen ###

koen.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter54/segmenter51_koen_kat250_helepop.xlsx")
koen.seg.helepop <- data.frame(matrix(unlist(koen.seg.helepop), nrow=54),stringsAsFactors=FALSE)
koen.seg.helepop <- koen.seg.helepop[,-c(emilsvector.bin)]
koen.seg.helepop[,1] <- strtrim(koen.seg.helepop[,1], 4)
koen.seg.helepop[1,1] <- 110
koen.seg.helepop <- sapply(koen.seg.helepop, as.numeric)
rownames(koen.seg.helepop) <- as.character(lab.seg.data$membership) 
yearly.name.template <- c( "koen.total.",  "koen.maend.", "koen.kvinder." )
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
all.names <- apply(all.names.df, 1, paste0, collapse="")
label_koen.seg.helepop <-  append(all.names,c("membership"),after=0)
koen.seg.helepop            <- make.seg.df(koen.seg.helepop, label_koen.seg.helepop)
koen.seg.helepop$koen.total.seg.gns <- koen.seg.helepop %>%   select(contains("total")) %>% rowMeans()
koen.seg.helepop$koen.maend.seg.gns <- koen.seg.helepop %>%   select(contains("maend")) %>% rowMeans()
koen.seg.helepop$koen.kvinder.seg.gns <- koen.seg.helepop %>%   select(contains("kvinder")) %>% rowMeans()
koen.seg.helepop <-    koen.seg.helepop %>%
    mutate(koen.seg.gns.kvinder.andel = koen.kvinder.seg.gns/koen.total.seg.gns)
sd_bernoulli <- function(x){
  sqrt(x*(1-x))
}
koen.seg.helepop <- koen.seg.helepop %>%   rowwise()  %>%    mutate_each(., funs( koen.seg.gns.kvinder.andel.sd = sd_bernoulli), contains("koen.seg.gns.kvinder.andel"))

#udvælger variabel til segdata
koen.seg.helepop.udv <- koen.seg.helepop %>%  select(membership,contains("gns")) %>%   select(membership,-contains("var")) 


#########koen ###

fagf.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter54/monecaseg_fagfmedl_kat250__helepop.xlsx")
fagf.seg.helepop <- data.frame(matrix(unlist(fagf.seg.helepop), nrow=54),stringsAsFactors=FALSE)
fagf.seg.helepop <- fagf.seg.helepop[,-c(emilsvector.bin)]
fagf.seg.helepop[,1] <- strtrim(fagf.seg.helepop[,1], 4)
fagf.seg.helepop[1,1] <- 110
fagf.seg.helepop <- sapply(fagf.seg.helepop, as.numeric)
rownames(fagf.seg.helepop) <- as.character(lab.seg.data$membership) 
yearly.name.template <- c( "fagf.total.",  "fagf.nej.", "fagf.ja." )
all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
all.names <- apply(all.names.df, 1, paste0, collapse="")
label_fagf.seg.helepop <-  append(all.names,c("membership"),after=0)
fagf.seg.helepop            <- make.seg.df(fagf.seg.helepop, label_fagf.seg.helepop)

fagf.seg.helepop$fagf.total.seg.gns <- fagf.seg.helepop %>%   select(contains("total")) %>% rowMeans()
fagf.seg.helepop$fagf.ja.seg.gns <- fagf.seg.helepop %>%   select(contains("ja")) %>% rowMeans()
fagf.seg.helepop$fagf.nej.seg.gns <- fagf.seg.helepop %>%   select(contains("nej")) %>% rowMeans()
fagf.seg.helepop <-    fagf.seg.helepop %>%
    mutate(fagf.seg.gns.ja.andel = fagf.ja.seg.gns/fagf.total.seg.gns)

sd_bernoulli <- function(x){
  sqrt(x*(1-x))
}
fagf.seg.helepop <- fagf.seg.helepop %>%   rowwise()  %>%    mutate_each(., funs( fagf.seg.gns.ja.andel.sd = sd_bernoulli), contains("fagf.seg.gns.fagf.andel"))

#udvælger variabel til segdata
fagf.seg.helepop.udv <- fagf.seg.helepop %>%  select(membership,contains("gns")) %>%   select(membership,-contains("var")) 


###################### join på disco #########################


# #udvælgelse af variable




discodata     <- inner_join(discodata, timelon.seg.helepop.udv)
discodata     <- inner_join(discodata, koen.seg.helepop.udv)
discodata     <- inner_join(discodata, fagf.seg.helepop.udv)




