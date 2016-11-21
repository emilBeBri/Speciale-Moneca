
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


#########lønvariable###


## timelon 
timelon.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop_INF.xlsx")
timelon.seg.helepop <- data.frame(matrix(unlist(timelon.seg.helepop), nrow=54),stringsAsFactors=FALSE)
#aabn_xls("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_timelon_udenti_kat250__helepop_INF.xlsx")

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

#view(timelon.seg.helepop)

# ## alder 

# alder.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_aldernov2_kat250_helepop.xlsx")
# alder.seg.helepop <- data.frame(matrix(unlist(alder.seg.helepop), nrow=51),stringsAsFactors=FALSE)
# alder.seg.helepop <- alder.seg.helepop[,c(emilsvector)]
# alder.seg.helepop <- sapply(alder.seg.helepop, as.numeric)
# # view(alder.seg.helepop)
# rownames(alder.seg.helepop) <- as.character(lab.seg.data$membership) 
# yearly.name.template <- c(
#   "alder.seg.mean.", 
#   "alder.seg.var.",
#   "alder.seg.sd.",
#   "alder.seg.min.",
#   "alder.seg.max.",
#   "alder.seg.total."
# )
# all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
# all.names <- apply(all.names.df, 1, paste0, collapse="")
# label_seg_alder.seg.helepop <-  append(all.names,c("membership"),after=0)
# rownames(alder.seg.helepop) <- as.character(lab.seg.data$membership) 
# alder.seg.helepop            <- make.seg.df(alder.seg.helepop, label_seg_alder.seg.helepop)
# # view(alder.seg.helepop)                                            

## koen 

# koen.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_koen_kat250_helepop.xlsx")
# koen.seg.helepop <- data.frame(matrix(unlist(koen.seg.helepop), nrow=51),stringsAsFactors=FALSE)
# columns <- seq(2,56,2)
# columns <-  append(columns, 1, after = 0)
# koen.seg.helepop <- koen.seg.helepop[,c(columns)]
#  # view(koen.seg.helepop)
# koen.seg.helepop <- sapply(koen.seg.helepop, as.numeric)
# rownames(koen.seg.helepop) <- as.character(lab.seg.data$membership) 
# yearly.name.template <- c(
#   "koen.seg.total.", 
#   "koen.seg.kvinder."
# )

# all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
# all.names <- apply(all.names.df, 1, paste0, collapse="")
# label_seg_koen.seg.helepop <-  append(all.names,c("membership"),after=0)
# koen.seg.helepop            <- make.seg.df(koen.seg.helepop, label_seg_koen.seg.helepop)

# ## ledighed 
# ledighed.seg.helepop <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/baggrundsvar/segmenter51_arledgr_kat250_helepop.xlsx")
# ledighed.seg.helepop <- data.frame(matrix(unlist(ledighed.seg.helepop), nrow=51),stringsAsFactors=FALSE)
# ledighed.seg.helepop <- ledighed.seg.helepop[,c(emilsvector)]
# # view(ledighed.seg.helepop)
# ledighed.seg.helepop <- sapply(ledighed.seg.helepop, as.numeric)
# rownames(ledighed.seg.helepop) <- as.character(lab.seg.data$membership) 
# yearly.name.template <- c(
#   "ledighed.seg.mean.", 
#   "ledighed.seg.var.",
#   "ledighed.seg.sd.",
#   "ledighed.seg.min.",
#   "ledighed.seg.max.",
#   "ledighed.seg.total."
# )
# all.names.df <- merge(yearly.name.template, 1996:2009, all=TRUE)
# all.names <- apply(all.names.df, 1, paste0, collapse="")
# label_seg_ledighed.seg.helepop <-  append(all.names,c("membership"),after=0)
# ledighed.seg.helepop            <- make.seg.df(ledighed.seg.helepop, label_seg_ledighed.seg.helepop)
            
# discodata <- mutate(discodata,
#                     seg.names.woho[i].helepop.gns =  (seg.names.woho[i].helepop1996 + seg.names.woho[i].helepop1997 + seg.names.woho[i].helepop1998 + seg.names.woho[i].helepop1999 +  seg.names.woho[i].helepop2000 +  seg.names.woho[i].helepop2001 + seg.names.woho[i].helepop2002 + seg.names.woho[i].helepop2003 + seg.names.woho[i].helepop2004 + seg.names.woho[i].helepop2005 + seg.names.woho[i].helepop2006 + seg.names.woho[i].helepop2007 + seg.names.woho[i].helepop2008 + seg.names.woho[i].helepop2009)/14 )



############# manipulationer ###########################


# vars <- names(timelon.seg.helepop)[seq(2,80,6)]  # choose which columns should be mutated
# vars <- setNames(vars, paste0(vars, "_sum")) # create new column names


#lav hele timelon.df til 2009 priser. \#todo







# timelon gns 
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2008.inf=timelon.seg.mean.2008*91.6/90.4)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2007.inf=timelon.seg.mean.2007*91.6/88)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2006.inf=timelon.seg.mean.2006*91.6/85.9)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2005.inf=timelon.seg.mean.2005*91.6/84.4)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2004.inf=timelon.seg.mean.2004*91.6/82.8)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2003.inf=timelon.seg.mean.2003*91.6/81.8)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2002.inf=timelon.seg.mean.2002*91.6/80.5)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2001.inf=timelon.seg.mean.2001*91.6/78.3)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop2000.inf=timelon.seg.mean.2000*91.6/76.9)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop1999.inf=timelon.seg.mean.1999*91.6/74.9)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop1998.inf=timelon.seg.mean.1998*91.6/72.8)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop1997.inf=timelon.seg.mean.1997*91.6/71.6)
# timelon.seg.helepop <- mutate(timelon.seg.helepop, timelon.seg.helepop1996.inf=timelon.seg.mean.1996*91.6/70.1)



timelon.seg.helepop.tiny <-   timelon.seg.helepop %>%   
  mutate(timelon.seg.gns.mean = rowSums(.[seq(2,80,6)]/14)) %>%
  mutate(timelon.seg.gns.sd = rowSums(.[seq(4,82,6)]/14))   %>%
  mutate(timelon.seg.gns.min = rowSums(.[seq(5,85,6)]/14))  %>%
  mutate(timelon.seg.gns.max = rowSums(.[seq(6,86,6)]/14))  %>%
  mutate(timelon.seg.gns.tot = rowSums(.[seq(7,82,7)]/14))  %>%
  select(membership,timelon.seg.gns.mean,timelon.seg.gns.sd,timelon.seg.gns.min,timelon.seg.gns.max,timelon.seg.gns.tot)
# view(timelon.seg.helepop.tiny)
# view(timelon.seg.helepop)



# alder 

# tmp <-  c("alder.seg.mean","alder.seg.var","alder.seg.sd")



# for (i in c(1,3)) {
# varname <- paste0(tmp[i])
# alder.seg.helepop.tiny <-  mutate_(alder.seg.helepop, .dots= list(varname,rowSums([seq(1+i,79+i,6)]/14)))
#     }  



      # alder.seg.helepop$alder.seg.tmp <- paste0(tmp[i])
     # rename_(alder.seg.helepop$alder.seg.tmp,.dots=paste0(tmp[i]))
# varname <- paste0(tmp[i])
#     rename_(alder.seg.helepop.tiny, .dots= setNames(c("alder.seg.tmp"), varname))
# colnames(alder.seg.helepop.tiny)[length(alder.seg.helepop.tiny)] <- tmp[i]
# names(alder.seg.helepop.tiny[length(alder.seg.helepop.tiny)] <- tmp[i])
# dots <- list(c("alder.seg.tmp"),tmp[i])
# alder.seg.helepop.tiny <- rename_(alder.seg.helepop.tiny,.dots=setNames(dots))
}



# view(alder.seg.helepop.tiny)

# setNames(c("alder.seg.tmp"),tmp[i]))




# view(alder.seg.helepop.tiny)


# ?rename

# n <- 10
# dots <- list(~mean(mpg), ~n)
# summarise_(mtcars, .dots = dots)
# #>   mean(mpg)  n
# #> 1  20.09062 10

# summarise_(mtcars, .dots = setNames(dots, c("mean", "count")))
# #>       mean count
# #> 1 20.09062    10


# head(select_(iris, quote(-Petal.Length), quote(-Petal.Width)))
# head(select_(iris, .dots = list(quote(-Petal.Length), quote(-Petal.Width))))
# head(iris)


# view(alder.seg.helepop.tiny)

# # --- dplyr version 0.3 and beyond ---
# multipetal <- function(df, n) {
    



# }








# iris %>% rename_(.dots=setNames(names(.), tolower(gsub("\\.", "_", names(.)))))
# ?parse0

#   setNames(names(.), tolower(gsub("\\.", "_", names(.))))) %>%   head()


# head(iris)

# iris %>% head %>% mutate(sum = .[[1]] + .[[2]])


# names(employ.data)[3] <- 'firstday'

# rename(iris, petal_length = Petal.Length)
# }
# view(alder.seg.helepop.tiny)


# view(alder.seg.helepop)
# alder.seg.helepop.tiny <-   alder.seg.helepop %>%   
#   mutate(alder.seg.gns = rowSums(.[seq(2,80,6)]/14)) %>%   select(alder.seg.gns,membership)


###################### join på disco #########################


timelon.seg.helepop.tiny$membership <-  as.factor(timelon.seg.helepop.tiny$membership)

discodata     <- inner_join(discodata, timelon.seg.helepop.tiny)
#discodata     <- inner_join(discodata, alder.seg.helepop.tiny)
# discodata$membership <-  as.factor(discodata$membership)

#view(discodata)


# # discodata     <- inner_join(discodata, timelon.seg.helepop)
# # discodata     <- inner_join(discodata, alder.seg.helepop)
# # discodata     <- inner_join(discodata, koen.seg.helepop)
# # discodata     <- inner_join(discodata, ledighed.seg.helepop)



# view(test)

# view(alder.seg.helepop)





# sum

# periode


# I have a list of variables, and I need to automatically cr

#  timelon.seg.helepop  %>%     mutate(vars, timelon.seg.helepop.gns = (sum_each(vars)/14))


#  mutate(mtcars, displ_l = disp / 61.0237)



# emilfun   <- function(x) {


# multipetal <- function(df, n) {
#     varname <- paste("petal", 5 , sep=".")
#     varval <- lazyeval::interp(~Petal.Width * 5, n=5)
#     mutate_(df, .dots= setNames(list(varval), varname))
# }





# for(i in 2:5) {
#     iris <- multipetal(df=iris, n=i)
# }

# view(iris)

# }

# round_df <- function(x, digits) {
#     # round all numeric variables
#     # x: data frame 
#     # digits: number of digits to round
#     numeric_columns <- sapply(x, class) == 'numeric'
#     x[numeric_columns] <-  round(x[numeric_columns], digits)
#     x
# }

# open_command <- switch(Sys.info()[['sysname']],
#                            Windows= 'open',
#                            Linux  = 'xdg-open',
#                            Darwin = 'open')
# temp_file <- paste0(x) #kunne også 
# system(paste(open_command, temp_file))
# }


# iris %>% mutate_each_(funs(sum), vars) %>% head 







