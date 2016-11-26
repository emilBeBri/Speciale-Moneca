



# nøgletal for intern mobilitet (delanalyse1)
view(seg.opsummering)
view(seg.qual.final)
view(seg.qual)
view(seg.df)
view(disco.df)



disco.inseg <- filter(discodata, !grepl("^1.*", membership))
disco.not.inseg <- filter(discodata, grepl("^1.*", membership))

Hmisc::describe(seg.df$within.mob.seg*100)
sd(discodata$within.mob)*100
Hmisc::describe(discodata$within.mob*100) 
sd(seg.df$within.mob.seg)*100

quants <- seq(0.1,0.2,0.01)
format(round(quantile(seg.df$within.mob.seg, quants, na.rm=TRUE), digits=3), big.mark=".",decimal.mark=",",nsmall=0)

view(seg.df)

### 

under68.df <- filter(df, within.mob.seg <= .68)
view(under68.df)

# relativ risiko (delanalyse1)


Hmisc::describe(relativrisiko.vector)
quants <- seq(0.9,1,0.005)
format(round(quantile(relativrisiko.vector, quants, na.rm=TRUE), digits=3), big.mark=".",decimal.mark=",",nsmall=0)





### 


# find navn der matcher mønster
filter(df,grepl(".*mili*.", as.character(disco),ignore.case=TRUE)) %>% 	select(disco)

#check enkelt disco indhold
desk.tmp2 <- c("disco","disco_s","membership","within.mob","within.mob.seg","alder.helepop.gns","koen.gns.kvinder.andel")
desk.tmp1 <- c(5131:5133,5113)
df.t <-  df %>% select_(.dots=desk.tmp2) %>% filter(grepl(paste(desk.tmp1,collapse="|"),disco_s)) %>% 	 mutate(disco_s2 = disco_s) 




desk.tmp1 <- c("3.24")
df.t <-  df %>% select_(.dots=desk.tmp2) %>% filter(grepl(paste(desk.tmp1,collapse="|"),membership)) %>%    mutate(disco_s2 = disco_s) 
view(df.t)





### density #########

dens1 <- discodata %>%  filter(Density==1) %>%  select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns)

deskall <- discodata %>% select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns)
  
view(segment.quality(seg,final.solution=TRUE))


tmp  <-  tbl_df(segment.quality(seg.original,final.solution=TRUE))

tmp$Density <- as.numeric(tmp$Density)
view(tmp)

tbl_df156,9204


lavdens.df <- filter(df, Density <= .52)
view(lavdens.df)


### delanalyse 2: indkomst 

quants = seq(0.7,1,0.01) 
round(quantile(discodata$timelon.mean.gns,quants),0)
Hmisc::describe(discodata$timelon.mean.gns)



# quantil histogram d
farve = c("indianred4","indianred3","darkseagreen3","darkseagreen4")
quants = c(0,0.1,0.25,0.5,0.75,0.9)
quants = quantile(discodata$timelon.mean.gns,quants,na.rm=TRUE)
discodata$quants = NULL
discodata$quants = with(discodata,factor(ifelse(timelon.mean.gns<quants[1],0, ifelse(timelon.mean.gns<quants[2],1, ifelse(timelon.mean.gns<quants[3],2, ifelse(timelon.mean.gns<quants[4],3, ifelse(timelon.mean.gns<quants[5],4, ifelse(timelon.mean.gns<quants[6],5,6 ))))))))
getPalette= colorRampPalette(farve)
ggplot(discodata,aes(x=timelon.mean.gns,fill=quants)) + geom_histogram(binwidth=1) +
  # coord_cartesian(xlim=c(50,500)) + 
  scale_x_continuous(breaks=seq(75,500,25)) +
theme_bw() + scale_fill_manual(values=getPalette(6), name="Udvalgte \npercentiler",
                               labels=c("0.-10.","11.-25.","25.-50. (median)","51.-75.","76.-90.","91.-.100"),
                               guide=guide_legend(legend.position=c(0.7,0.9))
                               ) +
  xlab("timeløn i kroner") + ylab("antal observationer") 
  



# plot over hele timeloens spectret 
p1 <-   ggplot(discodata, aes(x=timelon.mean.gns, y=reorder(disco,timelon.mean.gns),label=disco )) +
geom_point(size=1) +
theme_bw() +
theme(panel.grid.major.x = element_blank(),
panel.grid.minor.x = element_blank(),
panel.grid.major.y = element_line(colour="grey60", linetype="dashed")) 
 p2 <-  p1  + theme(
      axis.text.y=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none"
      )



library(ggrepel)
p2 + geom_label(hjust=-0.1,aes(size=beskaeft.andel.gns)) + scale_size_continuous(range=c(1,3))
p2 + geom_text_repel(size=2,force=0.3,nudge_x=1)
p2 + geom_text(nudge_x=1,hjust=-0.05,aes(size=beskaeft.andel.gns),check_overlap=TRUE) + scale_size_continuous(range=c(1,3))

ggplot(discodata,aes(x=beskaeft.andel.gns,y=timelon.mean.gns)) + geom_point(size=2.5, shape=19,colour=discodata$disco_1cifret, position="jitter")


#############################################




######### boxplot ##############

view(seg.df)
view(plot.df)

brewer.pal(nfarve, "Paired")

plot.df <- filter(discodata, !grepl("^1.*", membership), !grepl("^2.*", membership))
pirateplot(formula=timelon.mean.gns~membership, data=plot.df)

nfarve <-  length(unique(plot.df$membership))


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

##############################


iwanthue <- c("#6673c2","#6db543","#c651bb","#5bbe7d","#8263d3","#c6ac36","#5ea1d8","#d64737","#4bb7a7","#db457a","#457d42","#d98fcb","#777629","#9c518c","#b7b368","#a8485a","#e3893c","#e0867b","#a8773b","#aa522c")
plot.df <- filter(discodata, grepl("^3.*", membership)) #%>%   


plot.df$plot.order <- sortLvlsByVar.fnc(plot.df$membership, plot.df$koen.gns.kvinder.andel, ascending = TRUE)
p <- ggplot(plot.df, aes(x=plot.order, y=koen.gns.kvinder.andel,fill=membership,label=plot.df$disco))

plot.df$plot.df.order <- sortLvlsByVar.fnc(plot.df$membership, plot.df$ledighed.mean.gns, ascending = TRUE)
p <- ggplot(plot.df, aes(x=plot.df.order, y=ledighed.mean.gns,fill=membership,label=plot.df$disco))
p1 <-  p + scale_fill_manual(values=iwanthue)
p2 <-  p1 +  sta t_summary(fun.data=bp.vals, geom="boxplot",alpha=0.7,color="black") + geom_point(position= position_jitter(width=0.2),aes(size=beskaeft.andel.gns),alpha=.75)
p3 <-  p2 + stat_summary(fun.y=whisk.emil, geom="point",shape=95,size=8,color="black") + theme_bw() + theme(legend.position="none") 
p3
p4 <-  p3 + stat_summary(fun.y=sd.op.emil, geom="point",shape=2,size=2,color="black") + stat_summary(fun.y=sd.ned.emil, geom="point",shape=6,size=2,color="black") + scale_size_continuous(range = c(0.5,8)) + stat_summary(fun.y="mean", geom="point",shape=95,size=8,color="black") 
p4
p3  + geom_text(size=2.5)

p4 + geom_text_repel(aes(membership, timelon.mean.gns, label = disco),size=2.5,force=10)




###############################3

min.mean.sd.max(x)

sd(x) + mean(x)









test(x)


quantile(plot.df$timelon.mean.gns,0.1)



fyn.y=


+ theme_bw()







xmen  <-  piratepal("xmen")

   <-   as.vector(xmen)






bp.vals(discodata$timelon.mean.gns)


ggplot(plot.df, aes(x = membership, y = timelon.mean.gns)) + 
        geom_boxplot(fill = nfarve,
                     alpha = 0.3) + scale_fill_manual(values=brewer.pal(nfarve, "Paired")) + geom_point(position= position_jitter(width=0.3))





nfarve  <-  piratepal("xmen")




fill <- "pink"
line <- "gold"

x = plot.df$timelon.mean.gns

bp.vals <- function(x, probs=c(0.1, 0.25,0.5, 0.75, .9)) {
  r <- quantile(x, probs=probs , na.rm=TRUE)
  # r = c(r[1:2], exp(mean(log(x))), r[3:4]) #hvorfor tage log, exp og mean af x??
  # r = c(r[1:2], mean(x), r[3:4]) #hvorfor tage log, exp og mean af x??
  names(r) <- c("ymin", "lower", "middle","upper", "ymax")
  r
}


bp.vals(x)



bp.vals(x)

p1  


# Return the desired percentiles plus the geometric mean
bp.vals <- function(x, probs=c(0.1, 0.25, 0.75, .9)) {
  r <- quantile(x, probs=probs , na.rm=TRUE)
  r = c(r[1:2], exp(mean(log(x))), r[3:4])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# Sample usage of the function with the built-in mtcars data frame
ggplot(mtcars, aes(x=factor(cyl), y=mpg)) +
  stat_summary(fun.data=bp.vals, geom="boxplot")












pirateplot(formula = timelon.mean.gns~membership,
           data = plot.df,
           theme = 1,
           bean.f.o = 0.5, # Bean fill
           # point.o = .3, # Points
           inf.f.o = .0, # Inference fill
           inf.b.o = .0, # Inference border
           # avg.line = 1, # Average line
           # bar.f.o = .5, # Bar
           # inf.f.col = "white", # Inf fill col
           # inf.b.col = "black", # Inf border col
           # avg.line.col = "black", # avg line col
           # bar.f.col = gray(.8), # bar filling color
           # point.pch = 21,
           # point.bg = "white",
           point.col = "black",
           point.o = .65,
           # point.f = "pink",
           # point.cex = .7,
           quant=c(0.2,0.5,0.8),
           quant.col="black",
           quant.lwd=1
           )

b.males <- c(6, 7, 8, 8, 8, 9, 10, 10, 11, 11, 12, 12, 12, 12, 13, 14, 15)
b.females <- c(14, 13, 12, 12, 11, 10, 10, 9, 9, 9, 9, 9, 8, 8, 8, 7, 7, 7, 7)
b.total<-c(b.males,b.females)

b.m<-data.frame(b.males)
b.f<-data.frame(b.females)
b.t<-data.frame(b.total)

myList<-list(b.m, b.f, b.t)
df<-melt(myList)

colnames(df) <- c("class","count")
plt<-ggplot(df, aes(x=class,y=count))+geom_boxplot() 
plt + geom_point(aes(x = as.numeric(class) + 0, colour=class))

ggplot(df,aes(class,count))+geom_boxplot()+
  geom_dotplot(aes(fill=class),binaxis="y",stackdir="center",dotsize=0.5)











### antal beskæftigede 

deskall <- discodata %>% select(disco, max.path,membership,Nodes,Density,within.mob,within.mob.seg,share.of.mob,beskaeft.andel.gns,beskaeft.gns) %>% arrange(desc(beskaeft.andel.gns))
view(deskall)




## intern mobilitet seg 
seg.df <- seg.qual.final
seg.df$membership  <-  as.factor(seg.df$membership)
til.seg.df.1 <- ddply(discodata,~membership,summarise,beskaeft.gns=sum(beskaeft.gns))
til.seg.df.2 <- ddply(discodata,~membership,summarise,beskaeft.andel.gns=sum(beskaeft.andel.gns))
seg.df <- left_join(seg.df, til.seg.df.1)
seg.df <- left_join(seg.df, til.seg.df.2)
# lapply(seg.df,is.numeric)
seg.df <-   select(seg.df,membership, everything())

view(seg.df)





til.seg.qual.detailed <- select(discodata,disco,membership,within.mob.seg,Density,max.path,within.mob,within.mob.dif) 
seg.qual.detailed.df  <-  left_join(seg.qual.detailed,til.seg.qual.detailed)
view(seg.qual.detailed.df)








## segment labels 


# egp11_lab  =  c("I"="I: Oevre Serviceklasse","II"="II: Nedre Serviceklasse","IIIa"="IIIa: Rutinepraeget, ikke-manuelt arbejde hoejeste niveau","IIIb"="IIIb: Rutinepraeget, ikke-manuelt arbejde laveste niveau","Iva"="IVa: små selvstaendige med ansatte","Ivb"="IVb: små selvstaendige uden ansatte","Ivc"="IVc: Landmaend og andre selvstaendige i primaer produktion ","V"="V: Teknikere af laveste grad, supervisorer af manuelt arbejde","VI"="VI: Faglaerte, manulle arbejdere","VIIa"="VIIa: Ikke-faglaerte, manuelle arbejdere","VIIb"="VIIb: landbrugsarbejdere")

order(as.character(seg.df$membership))

is.factor(seg.df$membership)


seg.lab.tmp <- seg.df$membership
view(seg.lab.tmp)

view(seg.df)

levels()

help(order)


seg.df$seg.lab <- recode_factor(seg.df$membership, 
"5.1"	= "5.1",
"5.2"	= "Bygningsarbejdere I",
"4.8"	= "4.8",
"4.10"	= "4.10",
"4.6"	= "4.6",
"4.1"	= "4.1",
"4.9"	= "Bygningsarbejdere II",
"4.12"	= "4.12",
"4.11"	= "4.11",
"4.13"	= "4.13",
"4.4"	= "4.4",
"4.2"	= "4.2",
"3.35"	= "3.35",
"3.8"	= "3.8",
"3.36"	= "3.36",
"3.26"	= "3.26",
"3.24"	= "3.24",
"3.21"	= "3.21",
"3.14"	= "3.14",
"3.20"	= "3.20",
"3.30"	= "3.30",
"3.18"	= "3.18",
"3.37"	= "3.37",
"3.25"	= "3.25",
"3.15"	= "3.15",
"3.12"	= "3.12",
"3.2"	= "3.2",
"3.9"	= "3.9",
"2.66"	= "2.66",
"2.78"	= "2.78",
"2.61"	= "2.61",
"2.56"	= "2.56",
"2.31"	= "2.31",
"2.64"	= "2.64",
"2.24"	= "2.24",
"2.74"	= "2.74",
"2.80"	= "2.80",
"2.58"	= "2.58",
"2.40"	= "2.40",
"1.45"	= "1.45",
"1.159"	= "1.159",
"1.75"	= "1.75",
"1.93"	= "1.93",
"1.162"	= "1.162",
"1.260"	= "1.260",
"1.27"	= "1.27",
"1.194"	= "1.194",
"1.204"	= "1.204",
"1.214"	= "1.214",
"1.217"	= "1.217",
"1.223"	= "1.223",
.default=NA_character_)
# view(seg.df)

seg.lab.df <- select(seg.df,membership,seg.lab)

discodata <- inner_join(discodata,seg.lab.df)
view(discodata)


# old school metode 

view(seg.df)

segment.labels <- read.csv("./statistik/R/moneca/vores/voresdata/segment_labels.csv", sep = ";", encoding = "UTF-8")$Langt
segment.labels <-  as.factor(segment.labels)
is.factor(segment.labels) #ja
nlevels(segment.labels) # 51 levels (de 51 segmenter)
length(segment.labels) # 51 elementer, dvs antal levels og antal elementer er det samme 
levels(segment.labels) #factor levels er rangeret alfabetisk
segment.labels #men det er elementerne i vektoren ikke


seg.lab        <- seg.mem$membership
is.character(seg.lab) #charactervector med numeriske værdier for alle 273 noder
length(seg.lab) # 273 elementer svarende til de 273 noder
length(unique(seg.lab)) # 51 unikke værdier, svarende til de 51 segmenter is my guess
levels(seg.lab) # har ingen levels


seg.qual.final  <- segment.quality(seg, final.solution = TRUE)

levels(seg.lab) <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(discodata$seg.lab))])
is.character(seg.lab) #stadig charactor vector
length(seg.lab) # stadig 273 elementer 
length(unique(seg.lab)) # stadig  51 unikke værdier
levels(seg.lab) # har nu levels, og de er rangeret efter hvordan de ligger i seg.qual.final 
seg.lab

seg.lab2 <- paste(levels(seg.lab), as.character(segment.labels)[order(as.character(seg.qual.final$Membership))])



seg.lab3        <- format(as.character(seg.lab2))
levels(seg.lab3) #levels fjernes igen? wtf? og det er vist ikke engang nødvendigt.



