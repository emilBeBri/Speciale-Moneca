

#  kan lige så godt installeres med det samme:

#sudo apt-get install libxml2-dev libcurl4-openssl-dev build-essential libcurl4-gnutls-dev libcurl4-gnutls-dev libssl-dev

# setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
# setwd("/home/emil")

# rm(list=ls())



###installeres til sidst
#install.packages("readODS") #kan ikke installeres - maaske problem, tjek senere
#install.packages("soc.ca")
#install.packages("XLConnect")

#installerer version af igraph kompatibel med moneca - ihvertfald som det så ud i efterår 2015, måske fikset nu
# require(devtools)
# install_version("igraph", version = "0.7.1", repos = "http://cran.us.r-project.org")
# remove.packages("igraph")

#og nødvendigt at downgrade soc.elite også 
# install_github("antongrau/soc.elite", ref = "15246d6")


######## Library #############

library(devtools)
#library(digest) #slet igen
library(MASS) #slet igen
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(scales)
library(reshape2)
#nlibrary(soc.ca) #(måske ikke så vigtigt)
library(plyr)
library(dplyr)
#library(readstata13)
library(readxl)
# library(rmarkdown)
 library(igraph) 
#library(soc.elite)
# library(soc.report) #denne her bruger xlsx-pakken, der fucker med XLConnect
library(toOrdinal)
library(MONECA)
#library(XLConnect)
# library(circlize)
# library(RDocumentation)
library(yarrr)



###################################### egne funktioner #########


# Emils funktioner 

#skal laves til funktion på et tidspunkt
# objekt <- discodata %>% select(disco, timelon.helepop.gns.inf)

# write.xlsx2(timelon.helepop,"test2.xlsx") 


#funktion til at åbne excel fil fra R. kan godt forbedres så man ikke behøver skrive .extension ind, men det kan jeg ikke lige overskue nu. 
aabn_xls <- function(x) {
open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')
temp_file <- paste0(x) #kunne også 
system(paste(open_command, temp_file))
}

#fx:
# aabn_xls("./tmp_segmenter2.xlsx")




# funktion til at indsætte element i vector på designeret sted (duer ikke til starten af en vector, kan du lave if statement selv og fikse det?)

insert.at <- function(a, pos, ...){
    dots <- list(...)
    stopifnot(length(dots)==length(pos))
    result <- vector("list",2*length(pos)+1)
    result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
    result[c(FALSE,TRUE)] <- dots
    unlist(result)
}


# a = c(2,3,4,9,10,2,4,19)
# b = c(2,1)
# d = c(0,1)

# insert.at(a, c(3,7), b, d) #indsætter b efter 3. element og d efter 7. element i a 
# insert.at(1:10, c(4,7,9), 11, 12, 13) #andet eksempel


# funktion til at runde procent op så det summer til 100 fremfor round() der skærer det sidste af - ikke afprøvet


round_to <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

#eksempel
# > sum(c(0.333, 0.333, 0.334))
# [1] 1
# > sum(round(c(0.333, 0.333, 0.334), 2))
# [1] 0.99
# > sum(round_preserve_sum(c(0.333, 0.333, 0.334), 2))
# [1] 1




### til at fjerne alle de pakker en pakke afhaenger af 
library("tools")
removeDepends <- function(pkg, recursive = FALSE){
    d <- package_dependencies(,installed.packages(), recursive = recursive)
    depends <- if(!is.null(d[[pkg]])) d[[pkg]] else character()
    needed <- unique(unlist(d[!names(d) %in% c(pkg,depends)]))
    toRemove <- depends[!depends %in% needed]
    if(length(toRemove)){
         toRemove <- select.list(c(pkg,sort(toRemove)), multiple = TRUE,
                                 title = "Select packages to remove")
         remove.packages(toRemove)
         return(toRemove)
    } else {
        invisible(character())
    }
}

# # Example
# install.packages("YplantQMC") # installs an unneeded dependency "LeafAngle"
# c("YplantQMC","LeafAngle") %in% installed.packages()[,1]
# ## [1] TRUE TRUE
# removeDepends("YplantQMC")
# c("YplantQMC","LeafAngle")  %in% installed.packages()[,1]
# ## [1] FALSE FALSE




##funktion der rounder alle numeriske variable i en discodataframe
round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, class) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}


#funktion til at indsætte en ny row et bestemt sted i en dataframe (ikke testet endnu, d. 27 marts 2016)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


# fordel at det ikke kræver java og alt muligt pis, bruger readxl pakken 
read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
}
# example
# mysheets <- read_excel_allsheets("foo.xls")



#funktion til at åbne data on the go
#indsæt det her som option: test <-  cbind(colnames(sub.mat),sub.mat) dvs til mobility table 


view <- function(data, autofilter=TRUE) {
    # data: data frame
    # autofilter: whether to apply a filter to make sorting and filtering easier  
    data <- as.data.frame(data) #to handle dplyr datasets
    open_command <- switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')
    require(XLConnect)
    temp_file <- paste0(tempfile(), '.xlsx')
    wb <- XLConnect::loadWorkbook(temp_file, create = TRUE)
    XLConnect::createSheet(wb, name = "temp")
    XLConnect::writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
    XLConnect::saveWorkbook(wb, )
    system(paste(open_command, temp_file))
# Return the name of the temporaray Excel file when executing the function
return(temp_file)
}


#original version før fiks der specificerer at den skal bruge XLConnect-pakken
# view <- function(data, autofilter=TRUE) {
#     # data: data frame
#     # autofilter: whether to apply a filter to make sorting and filtering easier  
#     data <- as.data.frame(data) #to handle dplyr datasets
#     open_command <- switch(Sys.info()[['sysname']],
#                            Windows= 'open',
#                            Linux  = 'xdg-open',
#                            Darwin = 'open')
#     require(XLConnect)
#     temp_file <- paste0(tempfile(), '.xlsx')
#     wb <- loadWorkbook(temp_file, create = TRUE)
#     createSheet(wb, name = "temp")
#     writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
#     if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
#     saveWorkbook(wb, )
#     system(paste(open_command, temp_file))
# # Return the name of the temporaray Excel file when executing the function
# return(temp_file)
# }


#write.table(seg.mem, file="./statistik/R/moneca/vores/output/seg.mem.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA) #output

#dst.wt <- function(x,y) {
#  write.table(x, file=./output/y.csv, sep = ";", fileEncoding = "UTF-8", col.names=NA)  
#}


  
#if (negative == FALSE)  px[px < 0] <- 0.00000001
  

# Funktion der forbereder til ggplot2 
dst.plotq <- function(x,y) {
  x.df = data.frame(x[y,])
  x.df <- cbind(disco = rownames(x.df), x.df)
  rownames(x) <- NULL
  x.m <- melt(x.df, id.vars='disco')    
  ggplot(data = x.m, aes(x = variable, y = value, colour = disco)) +       
    geom_line(aes(group = disco)) + geom_point()
}


#laver specifik form for variable om til dplyr datasæt
disco.df <- function(x,y) {
  colnames(x) <- y[]
  x <- as.data.frame(x)
  x$disco = rownames(x)
  rownames(x) = NULL
  x <- tbl_df(x)
}
disco.df.s <- function(x,y) {
  colnames(x) <- y[]
  x <- as.data.frame(x)
  x$disco_s = rownames(x)
  rownames(x) = NULL
  x <- tbl_df(x)
}


#laver specifik form for variable om til dplyr datasæt
make.seg.df <- function(x,y) {
  colnames(x) <- y[]
  x <- as.data.frame(x)
  x$membership = rownames(x)
  rownames(x) = NULL
  x <- tbl_df(x)
}

#dplyr dataframe konvertion - eksempel
#label_moneca_alder   <- list("alder1996" ,"alder1997" , "alder1998" , "alder1999" , "alder2000" , "alder2001", "alder2002" , "alder2003" , "alder2004" , "alder2005", "alder200#6",  "alder2007",  "alder2008",  "alder2009")
#colnames(alder) <- label_moneca_alder[]
#alder <- as.data.frame(alder)
#alder$disco = rownames(alder)
#rownames(alder) = NULL
#alder <- tbl_df(alder)
#alder


#######################################
# Functions for sorting factor levels #
# (janhove.github.io)                 #
# https://janhove.github.io/analysis/2016/08/18/ordering-factor-levels 
# R tip: Ordering factor levels more easily
#######################################


# Sort factor levels by the factor level mean of another covariate
sortLvlsByVar.fnc <- function(oldFactor, sortingVariable, ascending = TRUE) {
  
  require("dplyr")
  require("magrittr")
  
  # Combine into data frame
  df <- data.frame(oldFactor, sortingVariable)
  
  ###
  ### If you want to sort the levels by, say, the median, sd etc. instead of the mean,
  ### just change 'mean(sortingVariable)' below to, say, 'median(sortingVariable)'.
  ###
  
  # Compute average of sortingVariable and arrange (ascending)
  if (ascending == TRUE) {
    df_av <- df %>% group_by(oldFactor) %>% summarise(meanSortingVariable = median(sortingVariable)) %>% 
      arrange(meanSortingVariable)
  }
  
  # Compute average of sortingVariable and arrange (descending)
  if (ascending == FALSE) {
    df_av <- df %>% group_by(oldFactor) %>% summarise(meanSortingVariable = median(sortingVariable)) %>% 
      arrange(desc(meanSortingVariable))
  }
  
  # Return factor with new level order
  newFactor <- factor(oldFactor, levels = df_av$oldFactor)
  return(newFactor)
}

# Sort factor levels by their frequency of occurrence
sortLvlsByN.fnc <- function(oldFactor, ascending = TRUE) {

  require("magrittr")
  
  # Return factor with new level order
  newFactor <- factor(oldFactor, levels = table(oldFactor)  %>% sort(., decreasing = !ascending)  %>% names())
  return(newFactor)
}

# Sort factor levels arbitrarily
sortLvls.fnc <- function(oldFactor, levelOrder) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  
  if(!is.numeric(levelOrder)) stop("'order' should be a numeric vector.")
  
  if(max(levelOrder) > length(levels(oldFactor))) stop("The largest number in 'order' can't be larger than the number of levels in the factor.")
  
  if(length(levelOrder) > length(levels(oldFactor))) stop("You can't have more elements in 'order' than there are levels in the factor.")
  
  if(length(levelOrder) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrder])
  }
  
  if(length(levelOrder) < length(levels(oldFactor))) {
    levelOrderAll <- c(levelOrder, (1:length(levels(oldFactor)))[-levelOrder])
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderAll])
  }
  
  return(reorderedFactor)
}


## antons funktioner

#######################################################
# Funktioner

# Networks share of size
neighborhood.share.of       <- function(segmenter, x, small.cell.reduction = 5, mode = "all", symmetric = FALSE){
  wm            <- weight.matrix(segmenter$mat.list[[1]], cut.off = 1, symmetric = symmetric, diagonal = NULL, small.cell.reduction = small.cell.reduction)
  wm[is.na(wm)] <- 0
  net           <- graph.adjacency(wm, mode = "directed", weighted = TRUE)
  neigh         <- neighborhood(net, 1, mode = mode)
  ff            <- function(x, ind) sum(x[ind])
  sapply(neigh, ff, x = x)
}

#breakdown af funktionen under beskaeftigede.smooth i 2_analyse-afsnittet
aggregate.membership <- function(x, y){
  yy                    <- y
  levels(yy)            <- aggregate(x = x, by = list(y), sum)[,2]
  yy                    <- as.numeric(as.character(yy))
  names(yy)             <- y
  yy
}

break.up <- function(segmenter, values, niveau = seq(segmenter$segment.list)){
  mem                  <- as.character(segment.membership(segmenter, niveau = niveau)$membership)
  mem.less             <- as.character(segment.membership(segmenter, niveau = niveau[-length(niveau)])$membership)
  mem[mem %in% values] <- mem.less[mem %in% values]
  as.factor(mem)
}

#breakdown af funktionen længere nede
smoothie   <- function(x, negative = FALSE, max.value = FALSE){
  xm         <- melt(x)
  sxm        <- split(xm, f = xm$Var1)
  lx         <- lapply(sxm, FUN = lm, formula = value ~ Var2)
  px         <- t(sapply(lx, predict))
  colnames(px) <- colnames(x)
  if (negative == FALSE)  px[px < 0] <- 0.00000001
  if (max.value != FALSE)  px[px > max.value] <- max.value
  px
}

zoom.to.segment <- function(p.p, lay, zoom.mem, distance = 1000){
  layout.z <- lay[zoom.mem,]
  max.z.x <- max(layout.z[,1]) + distance
  min.z.x <- min(layout.z[,1]) - distance
  max.z.y <- max(layout.z[,2]) + distance
  min.z.y <- min(layout.z[,2]) - distance
  
  p.z     <- p.p + scale_x_continuous(limits = c(min.z.x, max.z.x))
  p.z     <- p.z + scale_y_continuous(limits = c(min.z.y, max.z.y))
  p.z
}



##################################################################
 
