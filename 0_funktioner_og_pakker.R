

#  kan lige så godt installeres med det samme:

#sudo apt-get install libxml2-dev libcurl4-openssl-dev build-essential libcurl4-gnutls-dev libcurl4-gnutls-dev libssl-dev

# setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
# setwd("/home/emil")

# rm(list=ls())


#etest matrice 
etest <- function(x) {
  y  <-  x
  diag(y) <- 0
  min(colSums(y))
}

#forkortelse til length 
l <-  length



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
#library(ggrepel)
library(ggthemes)
#library(grid)  
#library(pheatmap)
#library(reshape)
library(digest) #slet igen
library(MASS) #slet igen
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(scales)
library(reshape2)
#library(soc.ca) #(måske ikke så vigtigt)
library(tidyr)
library(plyr)
library(dplyr)
# library(forcats)
#library(readstata13)
library(readxl)
# library(rmarkdown)
 library(igraph) 
#library(soc.elite)
# library(soc.report) #denne her bruger xlsx-pakken, der fucker med XLConnect
library(toOrdinal)
library(MONECA)
#library(XLConnect)
#library(circlize)
# library(RDocumentation)
#library(yarrr)
library(DescTools)
library(knitr)


############ farveskalaer/farvepaletter #####################

iwanthue <-  c("#6db643","#bdad45","#7263d0","#e28337","#7183ca","#d2413f","#d14385","#5c7c37","#c66774","#45b0cf","#b15b38")

#RColorBrewer
RColorBrewer.Set1 <-  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")


RColorBrewer.RdYlBu <-   c("#A50026", "#D73027", "#F46D43" ,"#FDAE61" ,"#FEE090" ,"#FFFFBF" ,"#E0F3F8" ,"#ABD9E9" ,"#74ADD1" ,"#4575B4" ,"#313695")




# xmen 
xmen = c("#026CCBFF", "#F51E02FF" ,"#05B102FF" ,"#FB9F53FF" ,"#9B9B9BFF", "#FB82BEFF" ,"#BA6222FF"  ,    "#EEC229FF" )






###################################### egne funktioner #########


## ideer til funktioner

# sådan her kan man lave en pipe der "gør" noget ved de subgrupper man har delt op tidligere:
# We define our “window function” (function we want applied to sub-groups of data in a given order) and then use dplyr to apply the function to grouped and arranged data:
# # define our windowed operation, in this case ranking
# rank_in_group <- . %>% mutate(constcol=1) %>%
#           mutate(rank=cumsum(constcol)) %>% select(-constcol)
# # calculate
# res <-
#   iris %>% group_by(Species) %>% arrange(desc(Sepal.Length)) %>% 
#   rank_in_group
# # display first few results
# res %>% filter(rank<=2) %>% arrange(Species,rank)


# simpel procent funktion, til dplyr 
  tilprocent <- function(x){
    x <-  round2(x*100,2)
    x
  }


#funktioner til at se hvor mobilitet går hen i Moneca
e.mobility  <- function(klynger,fra=TRUE)  {
mat.e <-  mob.mat[-274,-274]
work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks))))
irr.job.indices <- which(!(seq_len(nrow(mat.e)) %in% work.list))
#fjerner diagonal og  ties mellem irrelevante  
mat.e[irr.job.indices,irr.job.indices] <- 0
#fjerner interne ties i segmenter af interesse (hvis vi bare vil se hvor de går hen)
diag(mat.e) <- 0
mat.e[work.list,work.list] <- 0

ifelse(
  fra==TRUE,
mat.e[irr.job.indices,work.list] <- 0,
mat.e[work.list,irr.job.indices] <- 0)


ifelse(
  fra==TRUE,
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[x,] != 0))))),
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))
  )


mob.mat.u.diag <- mob.mat[-274,-274] 
diag(mob.mat.u.diag) <- 0
#udregniner
mob.til.l <-  setNames(as.list(lapply(aug.work.list,function(x) sum(mat.e[,x]))), discodata$disco[aug.work.list])
mob.til.l.andel <- setNames(as.list(round2(unlist(mob.til.l)/
  sum(mat.e),6)
  ), discodata$disco[aug.work.list])
mob.til.l.andel.tot.m.diag <- setNames(as.list(unlist(mob.til.l)/
  sum(mob.mat.u.diag)
  ), discodata$disco[aug.work.list])
without.mobility.df <- tbl_df(data.frame(
  discodata$disco[aug.work.list],
  discodata$membership[aug.work.list],
  discodata$membership_lab[aug.work.list],
  unlist(mob.til.l.andel),
  unlist(mob.til.l),
  unlist(mob.til.l.andel.tot.m.diag),
  discodata$klasse_oesch8[aug.work.list],
  discodata$klasse_oesch16[aug.work.list],
  discodata$manuel.klassemix.seg[aug.work.list]
  ))
colnames(without.mobility.df) <- c("disco","membership","membership_lab","without.mob.andel","without.mob","without.mob.andel.tot","klasse_oesch8","klasse_oesch16","manuel.klassemix.seg")

#saetter indeks paa 
without.mobility.df <-  inner_join(without.mobility.df,select(discodata,indeks.seg,disco))
return(without.mobility.df)
}


e.mobility.seg <- function(tmp.df) {
tmp.df %>% group_by(membership) %>% summarise_each(funs(sum), 
  without.mob.andel.seg=without.mob.andel,
  without.mob.seg=without.mob,
  without.mob.andel.tot.seg=without.mob.andel.tot
  ) %>% arrange(desc(without.mob.andel.seg))
}












#pheatmap funktion, simpel 

e.pheat.dataprep <- function(klynger)  {
    cut.off.default <- 1
    wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
    wm1[is.na(wm1)] <- 0

    # mat.e <-  mob.mat
    mat.e <- wm1


# work.list <-  seg$segment.list[[klynge]][[undergr]]
# work.list <- sort(work.list)

work.list <-  sort(as.vector(unlist(seg.selector.df %>% filter(membership %in% klynger) %>%     select(indeks))))

mat.e.result <- mat.e[work.list,work.list]

# relativrisiko.vector.mat.e.result  <-  as.vector(t(mat.e.result))
# relativrisiko.vector.mat.e.result[relativrisiko.vector.mat.e.result<=0] <- NA
# quantile(relativrisiko.vector.mat.e.result, seq(0,1,0.05),na.rm=TRUE)

diag(mat.e.result)[] <- 0
diag(mat.e.result)[] <- round_any(max(mat.e.result), 5, ceiling)

return(mat.e.result)
}



# forsøg på emulering af statas tab1 command. Hvordan henter man en variabel inde i en df på en generisk måde? #todoiR
# tab1 <-  function(df,var) {  

# df <- df
# var <- var 
# generic.df = select(generic.df,var)

# # generic.df  <-  select(df,var)
#   cbind(Freq=table(generic.df[,1]), Cumul=cumsum(table(generic.df[,1])), relative=prop.table(table(generic.df[,1]))*100,cumrela=cumsum(table(generic.df[,1])/nrow(generic.df))*100) 

# }




# to funktioner til at se på memoryforbrug af objekter, 
#fra:  http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session


ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
#lsos() #bruges sådan her, lissom getwd()

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
    areal <- length(data)+25
    areal <- append(areal,length(data)+25)
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', areal))
    XLConnect::saveWorkbook(wb, )
    system(paste(open_command, temp_file))
    # system(paste("libreoffice5.2 --norestore", temp_file))

# Return the name of the temporaray Excel file when executing the function
return(temp_file)
}

v <- function(data, autofilter=TRUE) {
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
    areal <- length(data)+25
    areal <- append(areal,length(data)+25)
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', areal))
    XLConnect::saveWorkbook(wb, )
    system(paste(open_command, temp_file))
    # system(paste("libreoffice5.2 --norestore", temp_file))

# Return the name of the temporaray Excel file when executing the function
return(temp_file)
}


# # Open file in Excel and save filename of temporary Excel file 
# filename <- view(data)
# # Go to Excel and browse the file and/or do some changes
# # Import changes to R after closing Excel
# wb <- loadWorkbook(xlsxdata_filename)
 # changed_data <- readWorksheet(wb, sheet=1)


## For pheatmap_1.0.8 and later (45 graders dimser under)
# draw_colnames_45 <- function (coln, gaps, ...) {
#     coord = pheatmap:::find_coordinates(length(coln), gaps)
#     x = coord$coord - 0.5 * coord$size
#     res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3,"bigpts"), vjust = 0.5, hjust = -0.025, rot = 360-45, gp = gpar(...))
#     return(res)}
# ## 'Overwrite' default draw_colnames with your own version 
# assignInNamespace(x="draw_colnames", value="draw_colnames_45",
# ns=asNamespace("pheatmap"))





# ikke brugt endnu d. d. 26/12/2016

# One of my latest problems with R involved trying to make a two-way table of relative frequencies by column with weighted data… yes, a simple contingency table! The table() function cannot even compare with Stata’s tabulate twoway command, since:
# it does not handle weights;
# it does not report marginal distributions in the last row and column of the table (which I always find helpful);
# it calculates cell frequencies but not relative frequencies by row or column.
# Luckily, writing an R function that can achieve this is not too hard:
# col.table <- function(var1, var2, weights=rep(1,length(var1)), margins=TRUE){
# Creating table of (weighted) relative frequencies by column, and adding row variable margins as the last column
# crosstab <- prop.table(xtabs(weights ~ var1 + var2), margin=2)
# t <- cbind(crosstab, Total=prop.table(xtabs(weights ~ var1)))
# # Adding column sums in the last row
# t <- rbind(t,Total = colSums(t))
# # Naming rows and columns of the table after var1 and var2 used, and returning result
# names(dimnames(t)) <- c(deparse(substitute(var1)), deparse(substitute(var2)))
# return(round(100*t,2))
# }
# col.table(x,y,w) gives the same output as Stata’s “tabulate x y [fw=w], col nofreq”. Note that the weight argument is optional so that: col.table(x,y) is equivalent to tabulate x y, col nofreq.
# Here’s the same function, but for relative distributions by row:
row.table <- function(var1, var2, weights=rep(1,length(var1)), margins=TRUE){
t <- rbind(prop.table(xtabs(weights ~ var1 + var2), margin=1),
Total=prop.table(xtabs(weights ~ var2)))
t <- cbind(t,Total = rowSums(t))
names(dimnames(t)) <- c(deparse(substitute(var1)), deparse(substitute(var2)))
return(round(100*t,2))
}


#rund up til nærmeste "lækre" tal, dvs 1:10. 
# http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}
# The above doesn't work when x is a vector - too late in the evening right now :)
# roundUpNice(0.0322)
# roundUpNice(3.22)
# roundUpNice(32.2)
# roundUpNice(42.2)
# roundUpNice(422.2)


# finder første sted der sker en ændring i en vektor
first.changes <- function(d) {
  p <- cumsum(rle(d)$lengths)
  p[-length(p)]
}
#v = c(1, 1, 1, 1, 1, 1, 1, 1.5, 1.5, 2, 2, 2, 2, 2)
#first.changes(v)
#sæt et +1 på efter cumsum-linjen hvis det skal være *efter* skiftet og ikke før det. 

##



em.heatmap <-  function(x,max=500,min=0,nbreaks=10,stylebreak="jenks",mis="pink") {
  require(stringr)
  require(tibble)
  require(tidyr)
  dat <- x 
diag(dat)[] <- 0
diag(dat)[] <- round_any(max(dat), 5, ceiling)
  dat[dat > max] <- max
  dat[dat < min] <- min
heatmap.rescale = classInt::classIntervals(as.vector(t(dat)), n = nbreaks, style = stylebreak )$brks
  ## reshape data (tidy/tall form)
  # colnames(dat2) <- colnames(dat2)
  dat <- cbind(dat,label.short[work.list])
  dat <-  tbl_df(dat) %>%  rename(Var1=) %>%
        gather(Var2, value, -Var1)
  dat$Var1 <- as.factor(dat$Var1)
  dat$Var2 <- as.factor(dat$Var2)    
  dat$value <-  as.numeric(dat$value)
    ggplot(dat, aes(Var1, Var2)) + geom_raster(aes(fill = value))  + scale_fill_gradientn(colors=skala.heatmap,values= rescale(heatmap.rescale),na.value=mis) + theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          )
  }


#original
# em.heatmap <-  function(x,y=30,z=0,mis=c("black")) {
#   require(stringr)
#   require(tibble)
#   require(tidyr)
#   dat <- x 
#   ## reshape data (tidy/tall form)
#   # colnames(dat2) <- colnames(dat2)
#   dat[dat > y] <- y
#   dat[dat < z] <- z
#   dat <- cbind(dat,label.short[-274])
#   dat <- tbl_df(dat) %>%  rename(Var1=) %>%   
#       gather(Var2, value, -Var1)
#   dat$Var1 <- as.factor(dat$Var1)
#   dat$Var2 <- as.factor(dat$Var2)    
#   dat$value <-  as.numeric(dat$value)
#     ggplot(dat, aes(Var1, Var2)) + geom_raster(aes(fill = value))  + scale_fill_gradientn(colors=skala.heatmap,values= rescale(heatmap.rescale),na.value=mis) 
#   }








em.vis.ties <- function(klynge,undergr) {
mat.e <- wm1
work.list <-  seg$segment.list[[klynge]][[undergr]]
########## simpel: kun segmentet
mat.e.result <- mat.e[work.list,work.list]
################ avanceret1: segment + ties 
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))

vis.ties.e <- discodata[aug.work.list,] 
vis.ties.e <-  select(vis.ties.e,disco,membership) %>%     arrange(desc(membership))   
view(vis.ties.e)
    }



em.circlize <-  function(segmentcircle) {
require("circlize")
getPalette = colorRampPalette(xmen)
diag(segmentcircle) <- 0
df.c <- get.data.frame(graph.adjacency(segmentcircle,weighted=TRUE))
df.c <- arrange(df.c,from)
n <-  unique(as.vector(df.c[2]))
n2 <- unique(as.vector(df.c[1]))
nfarve <-  length(unique(unlist(append(n,n2))))
chordia.e <-  chordDiagram(x = df.c, grid.col = sample(getPalette(nfarve)),
 transparency = 0.2, directional = 1, symmetric=FALSE, direction.type = c("arrows", "diffHeight"), diffHeight  = -0.065, link.arr.type = "big.arrow", link.largest.ontop = TRUE, link.border="black", link.sort=FALSE)
 # farve <- c("#000000", "#FFDD89", "#957244", "#F26223")
             #,
             # self.link=1
             # link.lwd = 2, 
             # link.lty = 2
return(chordia.e)
}



######### boxplot ##############



######################## 
whisk.emil <- function(x) {
  r <- quantile(x, probs=c(0.1,0.9) , na.rm=TRUE)
  # r = c(r[1:2], exp(mean(log(x))), r[3:4]) #hvorfor tage log, exp og mean af x??
  # r = c(r[1:2], mean(x), r[3:4]) #hvorfor tage log, exp og mean af x??
  r
}
sd.ned.emil <- function(x) {
 mean(x) - sd(x)
}

sd.op.emil <- function(x) {
 mean(x) + sd(x)
} 

# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - .5 * IQR(x) | x > quantile(x, 0.75) + .5 * IQR(x))
# }

is_outlier <- function(x,y=.1,z=4) {
  return(x < quantile(x, y) - (sd(x)/z) | x > quantile(x, 1-y)+ (sd(x)/z)) 
}

# is_outlier <- function(x,y=0.1) {
# return(x < quantile(x,y) | x > quantile(x,1-y) )
# }

bp.vals <- function(x, probs=c(0.1, 0.25,0.5, 0.75, .9)) {
  r <- quantile(x, probs , na.rm=TRUE)
  # r = c(r[1:2], exp(mean(log(x))), r[3:4]) #hvorfor tage log, exp og mean af x??
  # r = c(r[1:2], mean(x), r[3:4]) #hvorfor tage log, exp og mean af x??
  names(r) <- c("ymin", "lower", "middle","upper", "ymax")
  r
}



col_select <- function(df,
ret = c("df_select", "m_kode"),
top_n = 100) {
require(shiny)
require(miniUI)
require(dplyr)
require(DT) 
ret <- match.arg(ret)
stopifnot(is.data.frame(df))
df_head <- head(df, top_n)
ui <- miniPage(
gadgetTitleBar("Have your pick"),
miniContentPanel(
dataTableOutput("selection_df", height = "100%")
)
)
server <- function(input, output, session){
options(DT.options = list(pageLength = 10))
output$selection_df <- renderDataTable(
df_head, server = FALSE, selection = list(target = "column")
)
observeEvent(input$done, stopApp( input$selection_df_columns_selected))
}
cols_selected <- runGadget(ui, server)
if (ret == "df_select") {
return( df %>% select(cols_selected) )
} else {
df_name <- deparse(substitute(df))
colnames_selected <- colnames(df)[cols_selected] %>%
paste(collapse = ", ")
rstudioapi::insertText(
paste(df_name, " %>% select(", colnames_selected, ")", sep = "")
)
}
}

# Batting_rel <- col_select(discodata) #uden kode til at reproducere
# Batting_rel <- col_select(discodata, ret = 'm_code') #med kode til at reproducere

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


round2 <- function(x, digits = 0) {
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
# percent_df <- function(x) { lav den her funktion senere, til at afrunde procent
#     # round all numeric variables
#     # x: data frame 
#     # digits: number of digits to round
#     numeric_columns <- sapply(df, class) == 'numeric'



#     x[100*numeric_columns] <-  x[numeric_columns]
#     x
# }

##funktion der afrunder alle numeriske variable i en discodataframe
round_df <- function(x,digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, class) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}



#funktion til at indsætte en row eller col et bestemt sted i en df 
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
insertCol <- function(existingDF, newcol, c) {
  existingDF[,seq(c+1,ncol(existingDF)+1)] <- existingDF[,seq(c,ncol(existingDF))]
  existingDF[,c] <- newcol
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
    # if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
    autofilemil = dim(data)+5
    if (autofilter) setAutoFilter(wb, 'temp', aref('A1', autofilemil))
    XLConnect::saveWorkbook(wb, )
    system(paste(open_command, temp_file))
# Return the name of the temporaray Excel file when executing the function
return(temp_file)
}


# view <- function(data, autofilter=TRUE) {
#     # data: data frame
#     # autofilter: whether to apply a filter to make sorting and filtering easier  
#     data <- as.data.frame(data) #to handle dplyr datasets
#     open_command <- switch(Sys.info()[['sysname']],
#                            Windows= 'open',
#                            Linux  = 'xdg-open',
#                            Darwin = 'open')
#     require(XLConnect)
#     temp_file <- paste0("/tmp/",deparse(substitute(data)), '.xlsx')
#     # temp_file <- paste0(tempfile(), '.xlsx')
#     wb <- XLConnect::loadWorkbook(temp_file, create = TRUE)
#     XLConnect::createSheet(wb, name = "temp")
#     XLConnect::writeWorksheet(wb, data, sheet = "temp", startRow = 1, startCol = 1)
#     # if (autofilter) setAutoFilter(wb, 'temp', aref('A1', dim(data)))
#     autofilemil = dim(data)+5
#     if (autofilter) setAutoFilter(wb, 'temp', aref('A1', autofilemil))
#     XLConnect::saveWorkbook(wb, )
#     system(paste(open_command, temp_file))
# # Return the name of the temporaray Excel file when executing the function
# return(temp_file)
# }





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







## rettelse af Antons funktion 

# segment.quality <- 
# function (segmenter, final.solution = FALSE) 
# {
#     mat <- segmenter$mat.list[[1]]
#     l <- nrow(mat)
#     mat <- mat[-l, -l]
#     number.of.levels <- length(segmenter$mat.list)
#     segment.qual.onelevel <- function(segmenter, niveau) {
#         names <- rownames(segmenter$mat.list[[1]])
#         names <- names[-length(names)]
#         seg.list.niveau <- segmenter$segment.list[[niveau]]
#         mat.niveau <- segmenter$mat.list[[niveau]]
#         totals.niveau <- (mat.niveau[nrow(mat.niveau), ] + mat.niveau[, 
#             nrow(mat.niveau)])/2
#         totals.niveau <- totals.niveau[-length(totals.niveau)]
#         mat.niveau <- mat.niveau[-nrow(mat.niveau), -nrow(mat.niveau)]
#         edge.matrix <- segment.edges(segmenter, cut.off = 1, 
#             niveau = 0, small.cell.reduction = 5, segment.reduction = 0)
#         net.edge <- graph.adjacency(edge.matrix, weighted = TRUE)
#         seg <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) seg[seg.list.niveau[[i]]] <- i
#         niveau.qual <- round(diag(mat.niveau)/(rowSums(mat.niveau)), 10)
#         quality <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) quality[seg.list.niveau[[i]]] <- niveau.qual[i]
#         niveau.size <- round(((rowSums(mat.niveau) + colSums(mat.niveau))/2)/sum(colSums(mat.niveau)), 
#             4)
#         size <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) size[seg.list.niveau[[i]]] <- niveau.size[i]
#         niveau.density <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) niveau.density[seg.list.niveau[[i]]] <- graph.density(net.edge - 
#             which(((1:vcount(net.edge) %in% seg.list.niveau[[i]]) == 
#                 FALSE)))
#         nodes <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) nodes[seg.list.niveau[[i]]] <- length(seg.list.niveau[[i]])
#         max.path <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) max.path[seg.list.niveau[[i]]] <- diameter(net.edge - 
#             which(((1:vcount(net.edge) %in% seg.list.niveau[[i]]) == 
#                 FALSE)), weights = NA)
#         share.of.total <- rep(NA, length(names))
#         for (i in 1:length(seg.list.niveau)) share.of.total[seg.list.niveau[[i]]] <- (totals.niveau/sum(totals.niveau))[i]
#         out.frame <- data.frame(Segment = seg, within.mobility = quality, 
#             share.of.mobility = size, Density = niveau.density, 
#             Nodes = nodes, Max.path = max.path, share.of.total = round(share.of.total, 
#                 4))
#         colnames(out.frame) <- paste(niveau, ": ", colnames(out.frame), 
#             sep = "")
#         out.frame
#     }
#     qual.list <- lapply(1:number.of.levels, segment.qual.onelevel, 
#         segmenter = segmenter)
#     out.mat <- do.call(cbind, qual.list)
#     out.mat <- cbind(Membership = segment.membership(segmenter)[, 
#         2], out.mat)
#     rownames(out.mat) <- rownames(mat)
#     order.mat <- out.mat[, grep("share.of.total", colnames(out.mat))]
#     order.mat <- order.mat[, ncol(order.mat):1]
#     out.mat <- out.mat[do.call(order, -order.mat), ]
#     if (final.solution == TRUE) {
#         small.mat <- out.mat[duplicated(out.mat$Membership) == 
#             FALSE, ]
#         small.mat[sapply(small.mat, is.nan)] <- Inf
#         tsm <- as.matrix(small.mat)[, -1]
#         collapse.mat <- function(row, n) tail(na.omit(row), n)
#         tsm <- as.data.frame(t(apply(tsm, 1, collapse.mat, n = 7)))
#         colnames(tsm) <- c("Membership", "Within mobility", "Share of mobility", 
#             "Density", "Nodes", "Max.path", "Share of total size")
#         tsm$Membership <- small.mat$Membership
#         out.mat <- tsm
#     }
#     out.mat
# }
# <environment: namespace:MONECA>


##################################################################
 
