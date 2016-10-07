## deskriptivt ###


#ny variabel til formålet

# gennemsnit af beskæftigelse indenfor 1-cifret disconiveau #nyvar
discodata <- discodata %>%
  group_by(disco_1cifret) %>%
  mutate_each(funs(mean), ledbeskaeft.gns.disco.1 =ledbeskaeft.gns, beskaeft.gns.disco1=beskaeft.gns)




### at se på data

glimpse(discodata)


View(select(discodata, contains("disco")))
glimpse(select(discodata, contains("mem")))


View(discodata[,c("disco","membership")])


select(discodata, starts_with("disco"))

select(discodata, ends_with("disco"))
arrange(discodata, ledbeskaeft.gns)[ , 15]
arrange(discodata, desc(ledbeskaeft.gns))[ , 15]

str(discodata$membership)

discodata2  <- discodata
discodata  <- discodata2






discodata %>%
  group_by(disco_1cifret) %>%
  summarise_each(funs(mean), ledbeskaeft.gns, beskaeft.gns)


View(discoseg)

#




View(discodata[,145:159])



View(select(discodata, contains("nodes")))

#### om disco-kategorier

#antallet af grupper - der er ret ujævn fordeling, selvom 9'erne står for rigtig meget variation. Tjek det ud. 
View(discodata %>%
       group_by(disco_1cifret) %>%
       summarize(n=n()) %>%
       mutate(tmp=n/sum(n)) %>%
       arrange(desc(tmp)))

discodata %>%
       summarize(n=n()) %>%
  
median(discodata$beskaeft.gns)  
mean(discodata$beskaeft.gns)  
sd(discodata$beskaeft.gns)





#### deskriptivt om segmenter #######


nlevels(discodata$membership)

view(summarise(disco.mem, count=n()))

#andel af total antal segmenter
View(discodata %>%
  group_by(membership) %>%
  summarize(n=n()) %>%
  mutate(seg.andel=n/sum(n)) %>%
  arrange(desc(seg.andel)))
#antal af ledige i segmenter
View(discodata %>% group_by(membership) %>% 
       summarise( tmp=sum(ledbeskaeft.gns)) %>%
       arrange(tmp))
#andel af ledige i segmenter
View(discodata %>% group_by(membership) %>% 
       summarise( tmp=sum(ledbeskaeft.andel.gns)) %>%
       arrange(tmp))




#### udvælg variable 
list.seg1  <- select(discodata, contains("gns"))
list.seg1$disco  <- discodata$disco
#View(list.seg1)
list.seg2  <- select(discodata, disco, Nodes, membership, disco_1cifret)
udvalgte  <- left_join(list.seg2, list.seg1)
View(udvalgte)
# segment 5.3
seg_5.3  <- filter(udvalgte, membership=="5.3")
View(seg_5.3)


test  <- aggregate(udvalgte, by=list(membership), FUN=mean)

test2  <- summaryBy(membership ~  ledbeskaeft.andel.gns, fun=(mean), data=udvalgte)


#lave segment-datasæt. Men behøver ikke laves endnu! Og husk at nogle variable skal summeres, mens andre er gennemsnit!
udvalgte.seg <- ddply(udvalgte,~membership,summarise,mean=mean(alder.gns),sd=sd(alder.gns))

udvalgte.seg <- ddply(udvalgte,~membership,summarise,sum=sum(ledbeskaeft.andel.gns))



View(udvalgte.seg)

install.packages("doBy")
library(doBy)


#andel af disco 1-cifret
View(seg_5.3 %>%
       group_by(disco_1cifret) %>%
       summarize(n=n()) %>%
       mutate(tmp=n/sum(n)) %>%
       arrange(desc(tmp)))

View(seg_5.3 %>% filter(disco_1cifret=="8"))
View(udvalgte %>% filter(disco_1cifret=="8") %>% arrange(membership))



View(udvalgte %>% group_by(disco_1cifret))

View(udvalgte, disco_1cifret="8"))



discodata %>% top_n(10, ledbeskaeft.andel.gns) %>% arrange(desc(ledbeskaeft.andel.gns)) %>%  select(one_of(c("ledbeskaeft.andel.gns", "beskaeft.andel.gns")))



### se på løn  ######

udvalgte %>% View(select(discodata, disco, loenmv.gns) %>%  arrange(desc(loenmv.gns)))





