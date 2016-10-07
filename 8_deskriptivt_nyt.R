

################################## forberedelse


# Funktion der forbereder til ggplot2 
dst.plotq <- function(x,y) {
  x.df = data.frame(x[y,])
  x.df <- cbind(disco = rownames(x.df), x.df)
  rownames(x) <- NULL
  x.m <- melt(x.df, id.vars='disco')    
  ggplot(data = x.m, aes(x = variable, y = value, colour = disco)) +       
    geom_line(aes(group = disco)) + geom_point()
}







### deskriptivt om segmenter i socio/socstil moneca

# tael antal af kategorier i en factor 
# length(as.list(table(discodata$membership)))






## udvikling i segmenter


#arrangeret efter niveau 3 så 2 
view(arrange(seg.qual.df, `3: Segment`, `2: Segment`, `1: Segment`))

#arrangeret efter niveau 4 så 3 så 2 
view(arrange(seg.qual.df, `4: Segment`,`3: Segment`, `2: Segment`, `1: Segment`))



# df.t <-  seg.qual.df %>% select(disco, raekkefoelge,`1: Density`,`1: within.mobility`,`2: Density`,`2: within.mobility`,`3: Density`,`3: within.mobility`,`4: Density`,`4: within.mobility`,`5: Density`,`5: within.mobility`,`6: Density`,`6: within.mobility`,`7: Density`,`7: within.mobility`)
# view(df.t)

# view(discodata)

#membership df 
### deskriptivt om segmenter i socio/socstil moneca
# seg.df <- NULL 
# seg.df <- seg.qual.final
# View(seg.df)





# seg.df.tmp <-  lapply(seg.df[,-1],as.character)
# seg.df.tmp <-  sapply(seg.df.tmp,as.numeric)
# seg.df.tmp <- as.data.frame(seg.df.tmp)
# seg.df.tmp <- tbl_df(seg.df.tmp)
# seg.df.tmp$membership <- seg.df$membership
# seg.df <- seg.df.tmp
# seg.df.tmp <- NULL 
# seg.df <-   select(seg.df,membership, everything())
# view(seg.df)
# view(seg.qual.final)

# view(discodata)

####
# arrange(seg.df, desc(Density), desc(intern.mobilitet.seg)) %>% 	print(,n=39)

# view(mob.mat)




view(seg.mem)

arrange(seg.df, desc(within.mob),desc(Density) ) %>% 	print(,n=39)
#hiv segment ud og tjek indhold
desk.tmp1 <- c("5.2")
tmp  <-  discodata %>% select(disco, membership) %>% 	filter(disco==desk.tmp1) %>% print(,n=30)
discodata %>% select(disco, beskaeft.andel.gns, intern.mobilitet, membership) 		%>% 	filter(membership==desk.tmp1)
View(discodata %>% select(disco, beskaeft.andel.gns, intern.mobilitet, membership))    #%>%   filter(membership==desk.tmp1))


#check enkelt disco indhold
desk.tmp2 <- 110
discodata %>% select(disco, membership) %>% filter(grepl(desk.tmp2,disco))
discodata %>% select(disco_4cifret, ledbeskaeft.andel.gns, intern.mobilitet, membership) %>% filter(grepl(desk.tmp2,disco_4cifret)) #!grepl kan bruges modsat
#fra position i label/monecatabel
discodata[103,] %>% select(disco,membership)





### beskrivelser af variable (ku godt laves til funktion)

var1 <- c("loenmv.helepop.gns")
var2 <- 
desk.tmp1 <- 3.11
desk.tmp1 <- 5.5

viewer <-  discodata %>% filter(membership==desk.tmp1) %>%  select(disco, membership, loenmv.helepop.gns,disponny.helepop.gns) %>%    arrange( desc(loenmv.helepop.gns) ) %>%  print(,n=39)

View(viewer)


arrange(seg.df, desc(within.mob),desc(Density) ) %>%  print(,n=39)

discodata %>% select(disco, membership) %>%   filter(membership==desk.tmp1) %>% print(,n=30)

#### loen/løn udforskning

discodata <- mutate(discodata,
                    loenmv.helepop.gns.prmd =  (loenmv.helepop.gns)/12 )
discodata <- mutate(discodata,
                    disponny.helepop.gns.prmd =  (disponny.helepop.gns)/12 )
discodata <- mutate(discodata,
                    perindkialt.helepop.gns.prmd =  (perindkialt.helepop.gns)/12 )
discodata <- mutate(discodata,
                    joblon.helepop.gns.prmd =  (joblon.helepop.gns)/12 )



desk.tmp1 <- 3.11
desk.tmp1 <- 5.5

viewer <-  discodata %>%  select(disco,disponny.helepop.gns,loenmv.helepop.gns,perindkialt.helepop.gns,joblon.helepop.gns) %>%    arrange( desc(perindkialt.helepop.gns) )

viewer <-  discodata %>%  select(disco,disponny.helepop.gns.prmd,loenmv.helepop.gns.prmd,perindkialt.helepop.gns.prmd,joblon.helepop.gns.prmd) %>%    arrange( desc(joblon.helepop.gns.prmd) )

viewer <-  discodata  %>%  select(disco,disponny.helepop2009,loenmv.helepop2009,perindkialt.helepop2009,joblon.helepop2009,timelon.helepop2009)
viewer <- round_df(viewer)
View(viewer)
view(viewer)

arrange(seg.df, desc(within.mob),desc(Density) ) %>%  print(,n=39)

discodata %>% select(disco, membership) %>%   filter(membership==desk.tmp1) %>% print(,n=30)

# x <-  discodata %>% select(disco,disponny.helepop.gns,loenmv.helepop.gns,perindkialt.helepop.gns,joblon.helepop.gns) #%>%    arrange( desc(joblon.helepop.gns) )
x <- round_df(x)
x <-  discodata %>% select(disco,disponny.helepop2009,loenmv.helepop2009,perindkialt.helepop2009,joblon.helepop2009) %>%    arrange(desc(perindkialt.helepop2009))
view(x)






#disco summering på 1-cifret niveau, til sammenligning med DST rapport. virker ikke fordi det ikke er på 
# seg.df <- NULL 

View(til.loen.df.1)
View(discodata)
til.loen.df.1 <- ddply(discodata,~disco_1cifret,summarise,loenmv_1cifret=sum(loenmv.helepop2009))


# til.loen.df.1 <- ddply(discodata,~disco_1cifret,summarise,loenmv_1cifret=sum(loenmv.helepop2009))
# til.loen.df.2 <- ddply(discodata,~disco_1cifret,summarise,disponny_1cifret=sum(disponny.helepop2009))
# til.loen.df.3 <- ddply(discodata,~disco_1cifret,summarise,perindkialt_1cifret=sum(perindkialt.helepop2009))
# til.loen.df.4 <- ddply(discodata,~disco_1cifret,summarise,joblon_1cifret=sum(joblon.helepop2009))

# loen.df <- left_join(til.loen.df.1, til.loen.df.2)
# loen.df <- left_join(loen.df, til.loen.df.3)
# loen.df <- left_join(loen.df, til.loen.df.4)
# loen.df$disco_1cifret <- as.factor(loen.df$disco_1cifret)

# loen.df <- mutate(loen.df,
#                     joblon.mndr =  (joblon_1cifret)/12)
# loen.df$joblon.mndr <-  round(loen.df$joblon.mndr)
# View(select(loen.df,disco_1cifret,joblon.mndr))






#########detaljer fra discodata


seg.qual.final <- rename(seg.qual.final,  = `Share of mobility`)
disco.udv <-  discodata %>% select(disco_4cifret, disco_1cifret,disco,membership, Density, max.path,share.total.size, within.mob,beskaeft.gns,beskaeft.andel.gns)
view(disco.udv)


view(seg.qual.final)




# omdÃ¸b og numerisk
seg.qual.final <- rename(seg.qual.final,  = `Within mobility`)
seg.qual.final <- rename(seg.qual.final,  = `Share of mobility`)
seg.qual.final <- rename(seg.qual.final, max.path = `Max.path`)
seg.qual.final <- rename(seg.qual.final, share.total.size = `Share of total size`)










view(discodata)

discodata %>% select(disco_4cifret, membership,koen,ledbeskaeft.gns) %>%arrange(desc(koen)) %>% 	print(,n=25)
discodata %>% select(disco_4cifret, membership,koen,ledbeskaeft.gns) %>%arrange(koen) %>% 	print(,n=25)



arrange(discodata, desc(ledbeskaeft.andel.gns)) %>% 	print(,n=25)

0.000235*100


#vælg disco ud
minidisco <-  discodata %>% select(contains("disco"), membership,beskaeft.andel.gns,Density,intern.mobilitet,intern.mobilitet.seg,koen.gns.kvinder.andel)
# reorder variables sÃ¥ disco-variable ligger fÃ¸rst
minidisco <-  minidisco %>%
  select(disco,disco_1cifret,disco_4cifret,membership, everything())

view(minidisco)



view(select(contains("kvinde"),contains("total"), everything()))

view(discodata)




## flere detlajer 
df.t <-  discodata %>% select(disco, membership, within.mob,contains("intern"),contains("within"))
view(df.t)


sum(mob.mat[-l,-l])



view(mob.mat)
94.672.532

### vægtmatricen tile.plot

# wm <- weight.matrix(seg$mat.list[[1]])
# wm[is.na(wm)] <- 0
# wm <- round(wm)
# tile.plot(wm)
# names(wm)


wm1            <- weight.matrix(mob.mat, cut.off = 0.0000000000000000000000001, symmetric = FALSE, small.cell.reduction = small.cell.default)
wm1[is.na(wm1)] <- 0
wm1  <-  round(wm1, digits=1)

view(wm1)

pdf(file = "./statistik/R/moneca/vores/output/socio_socstil/tileplot.ledige.150.pdf", height = 70, width = 70)
tile.plot(wm1)
dev.off()

help(tile.plot)



mob.mat.weight1          <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight2          <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = TRUE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight3          <- weight.matrix(mob.mat, cut.off = 0.5, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=1)
mob.mat.weight1          <- round(mob.mat.weight1, digits=1)
mob.mat.weight2          <- round(mob.mat.weight2, digits=1)
mob.mat.weight3          <- round(mob.mat.weight3, digits=1)

tile.plot(mob.mat.weight1)
tile.plot(mob.mat.weight2)
tile.plot(mob.mat.weight3)
#tile.plot(mob.mat.weight4)
#tile.plot(mob.mat.weight5)




help(filter)


view(til.seg.df)




arrange(seg.df, within.mob,Density) %>% 
	print(,n=32)	




# seg.df <-  seg.qual.final %>% select(contains("disco"), membership, within.mob, share.of.mob, Density,Nodes,max.path,share.total.size, contains("intern"))

View(seg.qual.final)



til.df.seg <-  discodata %>% select(membership,intern.mobilitet.seg



seg.df <-  semi_join(seg.qual.final, til.df.seg)



view(seg.df)










# Open file in Excel and save filename of temporary Excel file 
file10173e783af <- view(df.t)
file10173e783af #location of file 

# Go to Excel and browse the file and/or do some changes

# Import changes to R after closing Excel
wb <- loadWorkbook(file10173e783af)
df.t <- readWorksheet(wb, sheet=1)








