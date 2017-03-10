
DescTools::Desc(df$klasse_egp11)




view(segment.quality(seg,final=TRUE))





view(mob.mat)

mobmat(

  view(cbind(names(mob.mat),mob.mat))


view(as.numeric(as.character(seg.df$disco_s))
view(as.numeric(as.character(df$disco_s)))







median(seg.df$within.mob.seg)

df %>%  mutate(`2: Nodes` = ifelse(is.na(`2: Nodes`), c(1), na.rm = T), `2: Nodes`))


tmp  <-  df %>% mutate(tmp = ifelse(is.na(`2: Nodes`), c(1), `2: Nodes`)) 



view(df %>%  mutate(`2: Nodes` = ifelse(is.na(`2: Nodes`), c(1), `2: Nodes`)) %>% group_by(`2: Nodes`) %>%     mutate(test =sum(`2: Nodes`)))


mean(tmp$tmp)


view(seg.df$within.mob.beregn)
mean(seg.df$within.mob.seg)





# se et segments vigtigste variable 
view(df %>% filter(membership==3.33) %>%    arrange(`2: Segment`,disco) %>%   select(`2: Segment`, share.total.size, Nodes, disco, timelon.mean.gns, timelon.sd.gns, ledighed.mean.gns, ledighed.sd.gns, koen.gns.kvinder.andel, koen.gns.kvinder.andel.sd, alder.mean.gns, alder.sd.gns, membership ))

view(seg.df %>% filter(membership==3.33) %>%  select(membership,Nodes,within.mob.seg,share.total.size, within.mob.sd.beregn, ledighed.mean.gns.beregn, ledighed.sd.gns.beregn, timelon.mean.gns.beregn, timelon.sd.gns.beregn, koen.gns.kvinder.mean.beregn ,koen.gns.kvinder.sd.beregn))
valg.seg <-  c("3.24")
view(DST_fagbet %>% filter(membership==valg.seg) %>% select(`2: Segment`,fagbet_tekst, disco, membership, skillvl) %>% arrange(`2: Segment`,disco))




#gennemsnitsværdier for alle centrale v
mean.df <- select(df,within.mob,Nodes,max.path,
  contains("mean.gns"),contains("sd.gns")) %>%   select(-contains("beregn"),-contains("cutoff"))
round(apply(mean.df,2 ,mean),4)
round(apply(mean.df,2 ,median),4)





valg.seg <-  c("3.24")
view(DST_fagbet %>% filter(membership==valg.seg) %>% select(`2: Segment`,fagbet_tekst, disco, membership, skillvl) %>% arrange(`2: Segment`,disco))

df %>% filter(membership==valg.seg) %>% arrange(beskaeft.andel.gns) %>% select(disco)   %>%   print(n=40)
view(DST_fagbet)




view(mean.df)

view(df)


timelon.mean.gns
ledighed.mean.gns
kvinder.andel.koen.mean.gns
alder.mean.gns
within.mob.mean.gns







#relation mellem variable 

library(corrgram)

corrgram(train, order=NULL, panel=panel.shade, text.panel=panel.txt,


plot.df <- discodata %>% select(gule.mean.gns,koen.gns.kvinder.andel,ledighed.mean.gns,roede.mean.gns, timelon.mean.gns,  within.mob, 
  Nodes,Density,max.path,beskaeft.andel.gns )
corrgram(plot.df, order=TRUE,
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=)



view(df)

cor(plot.df,use="pairwise.complete.obs")

corrgram(plot.df, order=NULL, panel=panel.shade)


, text.panel=panel.txt,
           main="Correlogram") 





library(help = MONECA)



occupations
moneca_stat1 <-  overordnede.niveau(seg)
moneca_stat2 <-  first.level.summary(seg)
moneca_stat3 <- vertex.mobility(seg)
view(moneca_stat3)

# moneca_stat4 <-  neighborhood.share.of(seg,1)


# moneca_stat3 <- ego.plot(seg) # ved ikke hvad den her kan 




print.first_level_summary

ego.plot

segment.edges



vertex.mobility


view(seg.df)

view(seg.qual)




####  Delanalyse 3: Sociale klasser med Oesch   ##########


####  Metodeafsnit 2: Disco                   ###############


# bortfald 


############### tidsserier for beskæftigelse ##############

library(ggthemes)
bortfald.df <- read_excel("./statistik/R/moneca/vores/00_emilspeciale_output/fraDST/bortfaldsanalyse.xlsx")
plot.df <- bortfald.df %>%    gather(key,value,-grp) 
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/tidsserier/frafald_beskaeft.pdf", height = 10, width = 15)
ggplot(plot.df, aes(x=key, y=value, colour=as.matrix(plot.df[,1]), group=as.matrix(plot.df[,1]))) +
                      geom_line(size=2) +
                      # geom_point(size=2) + 
                     xlab("") + ylab("") +
                     theme_tufte() +                     
                     theme(panel.grid.major= element_line(colour="black", size=0.05),text=element_text(size=20)) +
                      scale_colour_manual(values=sample(iwanthue)) +
                      geom_rangeframe(color="black",na.rm=TRUE) +
                      scale_y_continuous(breaks =extended_range_breaks()(plot.df$value))
dev.off()







####### treemap 
library(treemap)
library(highcharter)

tree.df <- select(df, disco_1cifret, disco,beskaeft.andel.gns)
DescTools::Desc(tree.df$beskaeft.andel.gns*100)
tree.df$label <- paste(tree.df$disco,"(",sep=" ")
tree.df$label <- paste(tree.df$label,round2(tree.df$beskaeft.andel.gns*100,1),"%",")",sep="")
tree.df$label[tree.df$beskaeft.andel.gns <= 0.0025 ] <- c("SAMLET: Erhvervsgrupper med en andel af beskæftigede under 2,5 %") 
# tree.df$label[tree.df$beskaeft.andel.gns >= 0.0025 ] <- c("under") 
cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/treemaps/DISCO_hovedgrp_beskaeft.pdf", height = 25, width = 25)
tree.map <- treemap(tree.df, index = c("disco_1cifret","label"), vSize = "beskaeft.andel.gns", palette = "HCL",force.print.labels = TRUE,fontsize.labels = c(30,18), title="")
dev.off()


#andel beskæftigede i disco 
 discodata %>% group_by(disco_1cifret) %>% summarise(summie=sum(beskaeft.andel.gns))  %>% arrange(desc(summie))

DescTools::Desc(tmp$summie*100)


sum(mob.mat[-274,-274])



# TEST_ <- treemap(select(df, disco,beskaeft.andel.gns), index = "disco", vSize = "beskaeft.andel.gns", palette = "Spectral",type="index")
# hTEST_hc <- highchart() %>% hctreemap(TEST_, name = "urine", layoutAlgorithm = "squarified") %>% 
#   hc_title(text = "Composition of human urine (50 g  dry weight / L)")






# nøgletal for intern mobilitet (delanalyse1)
view(seg.opsummering)
view(seg.qual.final)
view(seg.qual)
view(seg.df)
view(disco.df)
view(df)

view(df %>%   arrange(`5: Segment`,`4: Segment`,`3: Segment`,`2: Segment`,`2: Segment`))
view(df %>%   arrange(`4: Segment`,`3: Segment`,`2: Segment`,`2: Segment`))


disco.inseg <- filter(discodata, !grepl("^1.*", membership))
disco.not.inseg <- filter(discodata, grepl("^1.*", membership))

Hmisc::describe(seg.df$within.mob.seg*100)
sd(discodata$within.mob)*100
Hmisc::describe(discodata$within.mob*100) 
sd(seg.df$within.mob.seg)*100

quants <- seq(0.1,0.2,0.01)
format(round(quantile(seg.df$within.mob.seg, quants, na.rm=TRUE), digits=3), big.mark=".",decimal.mark=",",nsmall=0)







### 

under68.df <- filter(df, within.mob.seg <= .68)
view(under68.df)

# relativ risiko (delanalyse1) - UDEN diagonalen 


Hmisc::describe(relativrisiko.vector)
quants <- seq(0,1,0.01)
format(round2(quantile(relativrisiko.vector, quants, na.rm=TRUE), digits=3), big.mark=".",decimal.mark=",",nsmall=0)

# kunne være sjovt at lave det her plot, og så zoome ind på området med den høje stigning #todoir

sort(relativrisiko.vector)

ggplot() + geom_point(aes(x=seq_along(sort(relativrisiko.vector)), y=sort(relativrisiko.vector)),alpha=.1,size=0.5)
ggplot() + geom_line(aes(x=seq_along(sort(relativrisiko.vector)[500:length(relativrisiko.vector)]), y=sort(relativrisiko.vector)[500:length(relativrisiko.vector)]))




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


lavdens.df <- filter(df, Density <= .52)
view(lavdens.df)

############## delanalyse 3: Klasse ###########


nabo.out          <- neighborhood.share.of(seg, stor/sum(stor), small.cell.reduction = 5, mode = "out")
view(nabo.out)


view(stor)


############## delanalyse 2: Bringin' it All back Home ###########

beta.analyse.df <- df  %>%     arrange(desc(beta.var.alle),desc(membership)) %>% select(membership,Nodes,disco,contains("beta"), max.path,Density,beskaeft.andel.gns,beskaeft.gns,beskaeft.andel.gns.beregn, within.mob,within.mob.seg, share.of.mob, share.total.size,  `1: share.of.mobility`,skillvl) %>%    select(membership,Nodes,disco,contains("beta"),everything())


beta.analyse.seg.df <- seg.df  %>%     arrange(desc(beta.var.alle.seg),desc(membership)) %>% select(membership,Nodes,contains("beta"), max.path,Density, within.mob.seg, share.of.mob, share.total.size,beskaeft.andel.gns.beregn)
view(beta.analyse.seg.df)
 



###################### delanalyse 2: fagforeninger ###############


valg.seg <-  c("2.40")
view(DST_fagbet %>% filter(membership==valg.seg) %>% select(`2: Segment`,fagbet_tekst, disco, membership, skillvl) %>% arrange(`2: Segment`,disco))

df %>% filter(membership==valg.seg) %>% arrange(beskaeft.andel.gns) %>% select(disco)   %>%   print(n=40)
view(DST_fagbet)





#gennemsnitsstatistik - noder 
Hmisc::describe(round(df$roede.mean.gns*100),2)
round2(quantile(df$roede.mean.gns,standard.percentiler,na.rm=TRUE)*100,1)
#gennemsnitsstatistik - segmenter 
Hmisc::describe(seg.df$roede.mean.gns.beregn*100)
round2(quantile(seg.df$roede.mean.gns.beregn,standard.percentiler,na.rm=TRUE)*100,1)
round2(mean(seg.df$roede.sd.gns.beregn,na.rm=TRUE)*100,1)

#mere
round2(quantile(df$roede.sd.gns.beregn,standard.percentiler,na.rm=TRUE)*100,1)

round2(mean(seg.df$roede.mean.gns.beregn) *100,1)
round2(median(seg.df$roede.mean.gns.beregn) *100,1)
round2(mean(seg.df$roede.sd.gns.beregn,na.rm=TRUE) *100,1)
round2(median(seg.df$roede.sd.gns.beregn,na.rm=TRUE) *100,1)



# udvalgte delmarkeder

udvalgt.seg.df <- df  %>% filter(membership=="5.1") %>%       arrange(desc(roede.mean.gns.beregn )) %>%   select(membership,disco,`2: Segment`,`3: Segment`,`4: Segment`,contains("roed"), beskaeft.andel.gns, max.path,Density,beskaeft.gns, within.mob,skillvl) 



xlsxdat_file  <-  view(udvalgt.seg.df)
wb <- loadWorkbook(xlsxdat_file)
nytdata <- readWorksheet(wb, sheet=1)









view(df)
roede.analyse.df <- df  %>%     arrange(desc(roede.mean.gns.beregn ),desc(membership)) %>% select(membership,Nodes,disco,contains("roed"), max.path,Density,beskaeft.andel.gns,beskaeft.gns,beskaeft.andel.gns.beregn, within.mob,within.mob.seg, share.of.mob, share.total.size,  `1: share.of.mobility`,skillvl) %>%    select(membership,Nodes,disco,roede.mean.gns.beregn ,roede.sd.gns.beregn,roede.mean.gns, beskaeft.andel.gns.beregn, beskaeft.andel.gns,everything())
view(roede.analyse.df)


view(seg.df)
roede.analyse.seg.df <- seg.df  %>%     arrange(desc(roede.mean.gns.beregn ),desc(membership)) %>% select(membership,Nodes,contains("roed"), max.path,Density, within.mob.seg, share.of.mob, share.total.size,beskaeft.andel.gns.beregn) %>%  select(membership,Nodes,roede.mean.gns.beregn ,roede.sd.gns.beregn,beskaeft.andel.gns.beregn,everything())
view(roede.analyse.seg.df)
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/df_roede.xlsx")
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/seg.df_roede.xlsx")
















###################### delanalyse 2: ledighed ################


Hmisc::describe(seg.df$seg.koen.fordeling)
Hmisc::describe(seg.df$seg.ledighed.fordeling)
Hmisc::describe(seg.df$seg.timelon.fordeling)

#hvis vi tager dem som udtryk for en "tilfældig" fordeling? Kan man det? Nok først når man har set på hvor mange der er i hver. 
Hmisc::describe(seg.df$seg.alder.fordeling)
Hmisc::describe(seg.df$seg.within.mob.fordeling)








view(DST_fagbet %>% filter(grepl("*arbejde*", disco)))
view(DST_fagbet %>% filter(grepl("*revisions*", disco))

 view(df %>% filter(membership==valg.seg) %>%    group_by(`2: Segment`) %>% summarise(lavklynge_gns = mean(ledighed.mean.gns*100)) %>%   left_join(.,df) )







#gennemsnitsstatistik - noder 
Hmisc::describe(df$ledighed.mean.gns*100)
round2(quantile(df$ledighed.mean.gns,standard.percentiler)*100,1)
#gennemsnitsstatistik - segmenter 
Hmisc::describe(seg.df$ledighed.mean.gns.beregn*100)
round2(quantile(seg.df$ledighed.mean.gns.beregn,standard.percentiler)*100,1)
round2(mean(seg.df$ledighed.sd.gns.beregn,na.rm=TRUE)*100,2)
#mere
mean(seg.df$ledighed.mean.gns.beregn) *100
median(seg.df$ledighed.mean.gns.beregn) *100
mean(seg.df$ledighed.sd.gns.beregn,na.rm=TRUE) *100
median(seg.df$ledighed.sd.gns.beregn,na.rm=TRUE) *100



view(df)
ledighed.analyse.df <- df  %>%     arrange(desc(ledighed.mean.gns.beregn ),desc(membership)) %>% select(membership,Nodes,disco,contains("ledig"), max.path,Density,beskaeft.andel.gns,beskaeft.gns, within.mob,within.mob.seg, share.of.mob, share.total.size,  `1: share.of.mobility`,skillvl) %>%   select(-ledighed.total.gns,-ledighed.mean.gns.cutoff,-ledighed.min.gns) %>% select(membership,Nodes,disco,ledighed.mean.gns.beregn ,ledighed.sd.gns.beregn,ledighed.mean.gns,ledighed.sd.gns ,everything())
view(ledighed.analyse.df)
                              

view(seg.df)
ledighed.analyse.seg.df <- seg.df  %>%     arrange(desc(ledighed.mean.gns.beregn ),desc(membership)) %>% select(membership,Nodes,contains("ledig"), max.path,Density, within.mob.seg, share.of.mob, share.total.size) %>%  select(membership,Nodes,ledighed.mean.gns.beregn ,ledighed.sd.gns.beregn, ledighed.tid.sd.seg, everything())
view(ledighed.analyse.seg.df)
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/df_ledighed.xlsx")
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/seg.df_ledighed.xlsx")






###################### delanalyse 2: køen ########################


discodata %>%    group_by(seg.koen.fordeling) %>%   summarise(sum=sum(beskaeft.andel.gns*100))
# 1/4 er i delmarkeder hvor der kun er jobs med en overvægt af kvinder eller mænd, og 2/4 er i miks. 

view(df)

# ledelse

ledelse <- df
nrow(ledelse)


ledelse <- col_select(df)
view(ledelse[2:28,])
# gns kvind andel  22 %, sd 38 %
# 16 / 27 = 59 % har kun 1/4 kvinder




view(select(seg.df,membership,within.mob.seg,share.of.mob,Density,Nodes,max.path,koen.gns.kvinder.mean.beregn,seg.antal.kvindgrp,  seg.koen.fordeling,share.total.size))


Hmisc::describe(seg.df$seg.koen.fordeling)




# plot.df <- filter(discodata, !grepl("^1.*", membership))
# ggplot(plot.df2, aes(x=timelon.mean.gns
# ,y=koen.gns.kvinder.andel,color=membership)) + geom_point(size=4) find ud af hvordan du laver en regressionslinje for hele populationen, #todo



Hmisc::describe(seg.df$seg.koen.fordeling)
seg.df %>%  group_by(seg.koen.fordeling) %>%   summarise(sum(share.total.size*100))
view(seg.df)
view(df)


koen.analyse.df <- df  %>%     arrange(desc(koen.gns.kvinder.mean.beregn
),desc(membership)) %>% select(membership,disco,contains("koen"), max.path,Density,beskaeft.andel.gns,beskaeft.gns, within.mob,within.mob.seg, share.of.mob, share.total.size,  `1: share.of.mobility`,skillvl) %>%  select(membership,disco,koen.gns.kvinder.mean.beregn,koen.gns.kvinder.sd.beregn,everything())
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/disco.koen.mean.gns.beregn.xlsx")
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/seg.df_koen.xlsx")

valg.seg <-  c("3.4")
view(DST_fagbet %>% filter(membership==valg.seg) %>%    arrange(disco_4cifret))
df %>% filter(membership==valg.seg) %>% arrange(beskaeft.andel.gns) %>% select(disco)   %>%   print(n=40)






# lineplot over kønsfordeling 




til.df.1 <-  discodata %>% group_by(membership) %>% summarise_each(funs(sd), timelon.sd.gns.beregn=timelon.mean.gns, koen.gns.kvinder.sd.beregn=koen.gns.kvinder.andel,ledighed.sd.gns.beregn=ledighed.mean.gns)

sammen med Holger! 






maend.au <- koen.helepop %>%   select(contains("koen.maend."))
maend.au <- colSums(maend.au[-length(maend.au)])
maend.au <- tbl_df(maend.au)
maend.au$Aar <- cbind(1996:2009)
maend.au$koen <- cbind(rep(c("maend"),14))
kvinder.au <- koen.helepop %>%   select(contains("koen.kvinder."))
kvinder.au <- colSums(kvinder.au[-length(kvinder.au)])
kvinder.au <- tbl_df(kvinder.au)
kvinder.au$Aar <- cbind(1996:2009)
kvinder.au$koen <- cbind(rep(c("kvinder"),14))

koen.au <-  bind_rows(maend.au,kvinder.au)
koen.au     <- tbl_df(koen.au)
koen.au$koen <- rev(as.factor(koen.au$koen))

ecol <-  rev(c("dodgerblue", "firebrick"))
### plot  ###
koen.desk <- NULL 
p1 <-  ggplot(koen.au, aes(x=Aar, y=value, colour=koen, group=rev(koen))) + 
                      geom_line(size=1.6) +
                      geom_point(size=3) + 
                      xlab("") + ylab("") +
                      theme(legend.margin = unit(0, "cm")) +
                      scale_colour_manual(name = "antal beskæftigede", labels = c("Mænd","Kvinder"),values=muted(ecol)) +  
                      # scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")) + 
                      theme_bw() + theme.desk + scale_x_continuous(breaks=c(1996:2009)) + scale_y_continuous(breaks=seq(0, 1500000, 50000),
  expand = c(0,1500000)  ) 




### delanalyse 2: indkomst 



mean(discodata$timelon.sd.gns)





view(DST_fagbet %>% filter(membership=="3.35") %>%    arrange(disco_4cifret))


view()


view(arrange(seg.df,desc(timelon.mean.gns.beregn)))

view(df)
view(seg.df)

timelon.analyse.df <-  arrange(df,desc(timelon.mean.gns.beregn),desc(membership)) %>% select(membership,disco,timelon.mean.gns.beregn, timelon.sd.gns.beregn,contains("timelon"), max.path,Density,beskaeft.andel.gns,beskaeft.gns, within.mob,within.mob.seg, share.of.mob, share.total.size,  `1: share.of.mobility`,skillvl             ) %>%select(-timelon.total.seg.gns)
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/disco.timelon.mean.gns.beregn.xlsx")

view( timelon.analyse.df %>%   arrange(disco,desc(membership)))

# segment 4.9 indeholder 
7/27
# af hovedgrp klyngerne 

##

quants = seq(0.7,1,0.01) 
round(quantile(discodata$timelon.mean.gns,quants),0)
Hmisc::describe(discodata$timelon.mean.gns)
quants = seq(0,1,0.05) 
round(quantile(discodata$timelon.mean.gns.beregn,quants),0)
Hmisc::describe(discodata$timelon.mean.gns.beregn)




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
  



################## analyse af segmenter ###############

view(seg.df)








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


###########################################




############################## boxplot koen whatever *************



plot.df <- filter(discodata, !grepl("^1.*", membership))
plot.df <- filter(discodata, grepl("^3.*", membership)) #%>%   

plot.df <- filter(discodata, !grepl("^1.*", membership), !grepl("^2.*", membership))

getPalette= colorRampPalette(xmen)
plot.df <- plot.df %>%  group_by(membership) %>%  mutate(is.outlier = is_outlier(koen.gns.kvinder.andel,0.1))
plot.df <-  plot.df %>%   mutate(outlier = ifelse(is.outlier==TRUE, as.character(disco),NA)) 

is_outlier



plot.df$plot.order <- sortLvlsByVar.fnc(plot.df$membership, plot.df$koen.gns.kvinder.andel, ascending = TRUE) 
##
p <- ggplot(plot.df, aes(x=plot.order, y=koen.gns.kvinder.andel,fill=membership,label=plot.df$disco))
p1 <-  p + scale_fill_manual(values=getPalette(length(unique(plot.df$membership))))
p2 <-  p1 +  stat_summary(fun.data=bp.vals, geom="boxplot",alpha=0.7,color="black") + geom_point(position= position_jitter(width=0.2),aes(size=beskaeft.andel.gns),alpha=.75)
p3 <-  p2 + stat_summary(fun.y=whisk.emil, geom="point",shape=95,size=8,color="black") + theme_bw() + theme(legend.position="none") 
 p_out <-  p3 + geom_text(aes(label = outlier), na.rm = TRUE, vjust = -0.6,size=2.75)

cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/boxplots/koen_seg3-5.pdf", onefile = TRUE, height = 20, width = 20)
p_out + scale_y_continuous(labels=percent)
dev.off()




p4 <-  p3 + stat_summary(fun.y=sd.op.emil, geom="point",shape=2,size=2,color="black") + stat_summary(fun.y=sd.ned.emil, geom="point",shape=6,size=2,color="black") + scale_size_continuous(range = c(0.5,8)) + stat_summary(fun.y="mean", geom="point",shape=95,size=8,color="black") 
p4
p3  + geom_text(size=2.5)

p4 + geom_text_repel(aes(membership, koen.gns.kvinder.andel, label = disco),size=2.5,force=10)


is_outlier <- function(x) {
  return(x < quantile(x, 0.05) | x > quantile(x, 0.95)) 
}

###############################3

min.mean.sd.max(x)

sd(x) + mean(x)









test(x)


quantile(plot.df$timelon.mean.gns,0.1)



fyn.y=


+ theme_bw()








   <-   as.vector(xmen)






bp.vals(discodata$timelon.mean.gns)


ggplot(plot.df, aes(x = membership, y = timelon.mean.gns)) + 
        geom_boxplot(fill = nfarve,
                     alpha = 0.3) + scale_fill_manual(values=brewer.pal(nfarve, "Paired")) + geom_point(position= position_jitter(width=0.3))





nfarve  <-  piratepal("xmen")




fill <- "pink"
line <- "gold"

x = plot.df$timelon.mean.gns



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






###  antal beskæftigede 

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



########### røde halvmåne og grønne formation #############




# analyse: er der forskel på grønne lønninger og ikke-grønne lønninger? 
e.vals <-  c("3.7" ,"3.20" ,"3.21" ,"2.64" ,"3.18" ,"3.12" ,"4.11" ,"4.9" ,"3.34" ,"2.31" ,"3.4" ,"2.61" ,"1.45" ,"1.75" ,"3.3" ,"4.1" ,"1.99" ,"1.214")

a.ind2  <-   filter(seg.df, !membership %in% e.vals)
a.ind1  <-   filter(seg.df, membership %in% e.vals)


mean(a.ind2$within.mob.seg)
mean(a.ind1$within.mob.seg)
sd(a.ind2$within.mob.seg)
sd(a.ind1$within.mob.seg)
median(a.ind2$within.mob.seg)
median(a.ind1$within.mob.seg)
#ikke meget. 


#hvem har de forbindelser til?
cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 
cut.off.default <- 1
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0


mat.e <- wm1
# mat.e.result <- mob.mat


worklist_final =NULL
worklist1 <-  seg$segment.list[[1]][[214]]
worklist2 <-  seg$segment.list[[1]][[45]]
worklist_final <- append(worklist1,worklist2)
worklist3 <-  seg$segment.list[[1]][[75]]
worklist_final <- append(worklist_final,worklist3)
worklist4 <-  seg$segment.list[[1]][[99]]
worklist_final <- append(worklist_final,worklist4)
worklist5 <-  seg$segment.list[[2]][[31]]
worklist_final <- append(worklist_final,worklist5)
worklist6 <-  seg$segment.list[[2]][[61]]
worklist_final <- append(worklist_final,worklist6)
worklist7 <-  seg$segment.list[[2]][[64]]
worklist_final <- append(worklist_final,worklist7)
worklist8 <-  seg$segment.list[[3]][[12]]
worklist_final <- append(worklist_final,worklist8)
worklist9 <-  seg$segment.list[[3]][[18]]
worklist_final <- append(worklist_final,worklist9)
worklist10 <-  seg$segment.list[[3]][[20]]
worklist_final <- append(worklist_final,worklist10)
worklist11 <-  seg$segment.list[[3]][[21]]
worklist_final <- append(worklist_final,worklist11)
worklist12 <-  seg$segment.list[[3]][[3]]
worklist_final <- append(worklist_final,worklist12)
worklist13 <-  seg$segment.list[[3]][[34]]
worklist_final <- append(worklist_final,worklist13)
worklist14 <-  seg$segment.list[[3]][[4]]
worklist_final <- append(worklist_final,worklist14)
worklist15 <-  seg$segment.list[[3]][[7]]
worklist_final <- append(worklist_final,worklist15)
worklist16 <-  seg$segment.list[[4]][[1]]
worklist_final <- append(worklist_final,worklist16)
worklist17 <-  seg$segment.list[[4]][[11]]
worklist_final <- append(worklist_final,worklist17)
worklist18 <-  seg$segment.list[[4]][[9]]
worklist_final <- append(worklist_final,worklist18)
groenneformation <- sort(worklist_final)

################ hvilke disco 1-3 er ikke med i median indtjeningerne?

disco.1_3 <-  c(2:132)

udenfordetgodeselskab <-  setdiff(disco.1_3,work.list)
view(discodata$disco[udenfordetgodeselskab])
view(discodata[udenfordetgodeselskab,] %>% select(disco,membership,within.mob,within.mob.seg,timelon.mean.gns.beregn,timelon.sd.gns.beregn))
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/udenfordetgodeselskab.xlsx")


view(discodata[work.list,] %>% select(disco,membership,within.mob,within.mob.seg,timelon.mean.gns.beregn,timelon.sd.gns.beregn))
nrow(discodata)
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/detgodeselskab.xlsx")


# ser ud til at stort set alle DISCO-2 er repræsenteret, samt alt disco-1 der er fint nok. de eneste to disco-2 der ikke er med er tandlægerne og oversygeplejerske/jordemødre. finansfolkene er her, jura, etc etc. arbejde med religion er også med, men det er ikke pga deres lønninger. kunsterisk arbejde med musik, film og illustration er også med. det er med andre ord nogle cirkler der dækker en hel del magt. 




#######################

# forberedelse
dimnames(mob.mat) <- list(label.short, label.short) 
cut.off.default <-  median(relativrisiko.vector,na.rm=TRUE) 
cut.off.default <- 1
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0

mat.e <- wm1
# mat.e.tom <- mat.e 
# mat.e.tom[,] <- 0 

#view(mat.e.backup)



groenneformation <- groenneformation #pas på, er det nu den rigtigte?
roedehalvmaane <-  setdiff(c(1:273),groenneformation)
#### case 1: interne forbindelser 


#den røde halvmånes interne forbindelser 
mat.e <- wm1  
mat.e[groenneformation,groenneformation] <- NA
roede.ties.groen<- sort(unique(unlist(lapply(groenneformation, function(x) which(mat.e[,x] !=0 )))))
mat.e[roede.ties.groen,roede.ties.groen] <- NA
mat.e.roed.intern <- mat.e
em.heatmap(mat.e.roed.intern,mis=c("pink"))
sum(mat.e.roed.intern,na.rm=TRUE)


# den grønne formations interne forbindelser
mat.e <- wm1  
mat.e[roedehalvmaane,roedehalvmaane] <- NA
groen.ties.roed<- sort(unique(unlist(lapply(roedehalvmaane, function(x) which(mat.e[,x] !=0 )))))
mat.e[groen.ties.roed,groen.ties.roed] <- NA
mat.e.groen.intern <- mat.e
em.heatmap(mat.e.groen.intern,mis=c("pink"))
sum(mat.e.groen.intern,na.rm=TRUE)
sum(mat.e.roed.intern,na.rm=TRUE)
#### case 2: eksterne forbindelser 

#den grønne formations eksterne forbindelser 

mat.e <- wm1 
dvals <-  diag(mat.e)
diag(mat.e) <- 0  
groenneformation <- work.list
roedehalvmaane <-  setdiff(c(1:273),groenneformation)
#interne forbindelser i grønne formation lig 0 
mat.e[groenneformation,groenneformation] <- NA
#forbindelser fra roede halvmåne til grøn formation 
roede.ties.groen<- sort(unique(unlist(lapply(groenneformation, function(x) which(mat.e[,x] !=0 )))))
# roede.ties.groen.r<- sort(unique(unlist(lapply(groenneformation, function(x) which(mat.e[x,] !=0 )))))
# roede.ties.groen.master <-  union(roede.ties.groen.r,roede.ties.groen.c)


#grønne interne ties plus ties til roed 
groen.plus.ties.til.roed <- sort(union(roede.ties.groen,groenneformation))

# roeds interne ties der ikke går til groen  
irr.roede.ties <- setdiff(c(1:273),groen.plus.ties.til.roed)


#fjern diagonal (for overblikkets skyld)
diag(mat.e) <- NA
#fjern de irrelevante roede ties 
mat.e[irr.roede.ties,irr.roede.ties] <- NA

#fjerner intern ties der er mellem de roede noder, der har groenne ties 
mat.e[roede.ties.groen,roede.ties.groen] <- NA

#fjerner de gronne ties-ties og roede ties internt 
groen.ties.til.roed.og.irr.roede.ties  <-  union(groen.ties.roed,irr.roede.ties)

mat.e[groen.ties.til.roed.og.irr.roede.ties,groen.ties.til.roed.og.irr.roede.ties] <- NA

mat.e.kun.roed.til.gron <- mat.e 
sum(mat.e.kun.roed.til.gron,na.rm=TRUE)



#den røde halvmånes eksterne forbindelser 

mat.e <- wm1 
dvals <-  diag(mat.e)
diag(mat.e) <- 0  
#interne forbindelser i grønne formation lig 0 
mat.e[roedehalvmaane,roedehalvmaane] <- NA
#forbindelser fra roede halvmåne til grøn formation 
groen.ties.roed<- sort(unique(unlist(lapply(roedehalvmaane, function(x) which(mat.e[,x] !=0 )))))

#roede interne ties plus ties til groen 
roed.plus.ties.til.groen <- sort(union(groen.ties.roed,groenneformation))

# groens interne ties der ikke går til roed  
irr.groen.ties <- setdiff(c(1:273),roed.plus.ties.til.groen)

#fjern diagonal (for overblikkets skyld)
diag(mat.e) <- NA
#fjern de irrelevante roede ties 
mat.e[irr.groen.ties,irr.groen.ties] <- NA

#fjerner intern ties der er mellem de roede noder, der har groenne ties 
mat.e[groen.ties.roed,groen.ties.roed] <- NA

#fjerner de roede ties-ties og groennes ties internt 
roed.ties.til.groen.og.irr.groen.ties  <-  union(groen.ties.roed, irr.groen.ties)

mat.e[groen.ties.til.roed.og.irr.roede.ties,groen.ties.til.roed.og.irr.roede.ties] <- NA

mat.e.kun.groen.til.roed <- mat.e 



sum(mat.e.groen.intern,na.rm=TRUE) 
em.heatmap(mat.e.groen.intern,mis="pink")
rr.vector.mat.e.groen.intern  <-  as.vector(t(mat.e.groen.intern))
rr.vector.mat.e.groen.intern[rr.vector.mat.e.groen.intern==0]  <-  NA
Hmisc::describe(rr.vector.mat.e.groen.intern,na.rm=TRUE)

sum(mat.e.kun.roed.til.gron,na.rm=TRUE)
em.heatmap(mat.e.kun.roed.til.gron,mis="pink")
rr.vector.mat.e.kun.roed.til.gron  <-  as.vector(t(mat.e.kun.roed.til.gron))
rr.vector.mat.e.kun.roed.til.gron[rr.vector.mat.e.kun.roed.til.gron==0]  <-  NA
Hmisc::describe(rr.vector.mat.e.kun.roed.til.gron,na.rm=TRUE)

sum(mat.e.roed.intern,na.rm=TRUE)
em.heatmap(mat.e.roed.intern,mis="pink")
rr.vector.mat.e.roed.intern  <-  as.vector(t(mat.e.roed.intern))
rr.vector.mat.e.roed.intern[rr.vector.mat.e.roed.intern==0]  <-  NA
Hmisc::describe(rr.vector.mat.e.roed.intern,na.rm=TRUE)

sum(mat.e.kun.groen.til.roed,na.rm=TRUE)
em.heatmap(mat.e.kun.groen.til.roed,mis="pink")
rr.vector.mat.e.kun.groen.til.roed  <-  as.vector(t(mat.e.kun.groen.til.roed))
rr.vector.mat.e.kun.groen.til.roed[rr.vector.mat.e.kun.groen.til.roed==0]  <-  NA
Hmisc::describe(rr.vector.mat.e.kun.groen.til.roed,na.rm=TRUE)



em.heatmap(mat.e)



## replace diagonal elements
diag(mat.e) <- -dvals

mat.e <- mat.e_bat 
mat.e + mat.e_bat  


# testdata 

# jobdat <- matrix(c(
#   1,   0,   1,   0,   1,   0,   0,
#   1,   1,   1,   0,   0,   0,   0,
#   0,   1,   1,   0,   1,   1,   0,
#   0,   0,   0,   1,   0,   1,   0,
#   0,   0,   0,   1,   1,   0,   0,
#   0,   1,   0,   0,   0,   1,   0,
#   1,   0,   0,   0,   0,   0,   1
# ), 
# nrow = 7, ncol = 7, byrow = TRUE,
# dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7"),
#                 c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")))


jobdat <- matrix(c(
1,  1,  0,  0,  0,  0,  0,  0,
1,  1,  0,  0,  0,  0,  0,  0,
0,  0,  1,  1,  0,  0,  0,  0,
0,  1,  0,  1,  0,  0,  0,  0,
1,  0,  0,  1,  1,  0,  1,  0,
0,  0,  0,  0,  0,  1,  0,  1,
1,  0,  1,  0,  0,  0,  1,  1,
0,  0,  0,  0,  0,  1,  0,  1
), 
nrow = 8, ncol = 8, byrow = TRUE,
dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7","job 8"),
                c("job 1","job 2","job 3","job 4","job 5","job 6","job 7","job 8")))
jobdat <- matrix(c(
1,  1,  0,  1,  0,  0,  1,  0,
1,  1,  0,  0,  1,  0,  0,  1,
0,  0,  1,  1,  0,  1,  1,  0,
0,  1,  0,  1,  0,  0,  0,  1,
1,  0,  0,  1,  1,  0,  1,  0,
0,  1,  0,  0,  0,  1,  0,  1,
1,  0,  1,  0,  0,  0,  1,  1,
0,  1,  0,  0,  0,  0,  0,  1
), 
nrow = 8, ncol = 8, byrow = TRUE,
dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7","job 8"),
                c("job 1","job 2","job 3","job 4","job 5","job 6","job 7","job 8")))

sum(jobdat)
# jobdat <-  jobdat_bak 
dvals <-  diag(jobdat)
diag(jobdat) <- 0  
groenneformation <- c(1,5,7)
roedehalvmaane <-  setdiff(c(1:8),groenneformation)
#interne forbindelser i grønne formation lig 0 
jobdat[groenneformation,groenneformation] <- 0
#forbindelser fra roede halvmåne til grøn formation 
roede.ties.groen<- sort(unique(unlist(lapply(groenneformation, function(x) which(jobdat[,x] !=0 )))))
# roede.ties.groen.r<- sort(unique(unlist(lapply(groenneformation, function(x) which(jobdat[x,] !=0 )))))
# roede.ties.groen.master <-  union(roede.ties.groen.r,roede.ties.groen.c)


#grønne interne ties plus ties til roed 
groen.plus.ties.til.roed <- sort(union(roede.ties.groen,groenneformation))

# roeds interne ties der ikke går til groen  
irr.roede.ties <- setdiff(c(1:8),groen.plus.ties.til.roed)


#fjern diagonal (for overblikkets skyld)
diag(jobdat) <- 0 
#fjern de irrelevante roede ties 
jobdat[irr.roede.ties,irr.roede.ties] <- NA

#fjerner intern ties der er mellem de roede noder, der har groenne ties 
# jobdat[roede.ties.groen.master,roede.ties.groen.master] <- NA
jobdat[roede.ties.groen,roede.ties.groen] <- NA


#fjerner de gronne ties-ties og roede ties internt 
groen.ties.til.roed.og.irr.roede.ties  <-  union(roede.ties.groen,irr.roede.ties)

jobdat[groen.ties.til.roed.og.irr.roede.ties,groen.ties.til.roed.og.irr.roede.ties] <- NA



sum(jobdat,na.rm=TRUE)



## replace diagonal elements
diag(jobdat) <- -dvals

jobdat <- jobdat_bat 
jobdat + jobdat_bat  












