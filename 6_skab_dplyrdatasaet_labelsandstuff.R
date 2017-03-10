vertex_stoerrelse <-  discodata$beskaeft.andel.gns 


#ret sikre defaults, behÃ¸ver ikke Ã¦ndres


 
# disco 1 cifret
disco_1cifret_lab <- c("1: Ledelse", "2: Viden pÃ¥ hÃ¸jeste niveau", "3: Viden pÃ¥ mellemniveau", "4: Kontorarbejde", "5: Detailsalg, service og omsorgsarb.", "6: Landbrug, skovbrug, jagt, \nfiskeri (grundniveau)", "7: HÃ¥ndvÃ¦rksprÃ¦get arbejde", "8: Proces- og maskinoperatÃ¸rarbejde \nsamt transport og anlÃ¦g", "9: Andet manuelt arbejde")
# andel stÃ¸rrelse (numerisk)
beskaeft.num <- c(0.005,0.01,0.02,0.03,0.04,0.05)
# andel stÃ¸rrelse (labels)
beskaeft.lab <- c("0,5 %", "1 %", "2 %","3 %", "4 %", "5 %")
#intern mobilitet seg



# intern mobilitet segment

# discodata$within.mob.seg
# quants = seq(0,1,0.05) 
# format(round(quantile(discodata$within.mob.seg, quants), digits=2), big.mark=".",decimal.mark=",",nsmall=0)
# summary(discodata$within.mob.seg)

# view(seg.qual.final %>% select(membership, within.mob.seg,everything()) %>% 	 arrange(desc(within.mob.seg)))



intern.mob.seg_num <- seq(50,100,5) #c(70,80)
intern.mob.seg_num <- intern.mob.seg_num/100
intern.mob.seg_lab <- paste(100*intern.mob.seg_num)#, "%")



#intern mobilitet noder 
intern.mob_num <- c(5,10,20,30,40,50,55,60)
intern.mob_num <- intern.mob_num/100
intern.mob_lab <- c("5 %","10 %", "20 %","30 %", "40 %", "50 %", "55 %","60 %")


### farvevalg


#farveskaler


# grøn-rød 

skala.indianred.darkseagreen         <- c( "indianred4", "indianred3", "indianred2", "indianred1", "whitesmoke", "darkseagreen1", "darkseagreen2", "darkseagreen3", "darkseagreen4")
skala.darkseagreen.indianred <-  rev(skala.indianred.darkseagreen)


#skala.indianred.darkseagreen.3col         <- c( "indianred4", "indianred3", "indianred2","whitesmoke","darkseagreen2", "darkseagreen3", "darkseagreen4")
#skala.darkseagreen.indianred.3col <-  rev(skala.indianred.darkseagreen)


skala.ired.dgreen.simple =  c(    "indianred4","indianred2", "whitesmoke", "darkseagreen2","darkseagreen4")
skala.dgreen.ired.simple =  rev(skala.ired.dgreen.simple)

#egp 
# dodgerblue1 dodgerblue2                 serviceklasse
# darkseagreen1 darkseagreen4             middelklassen
# indianred1 indianred3 indianred4        arbejderklassen
# yellow1 yellow3 yellowgreen             småborgerskabet
skala_egp11 <-  c("dodgerblue4" ,"dodgerblue1","darkseagreen4","darkseagreen1","yellow3", "yellow1","grey","indianred4","indianred3","indianred1","pink")





#"1" = "1 Store forretningsdrivende" ,     "dodgerblue4"
#"2" = "2 Selvstaendige profesionelle" ,    
#"3" = "3 Smaa forretningsdrivende m. ansatte" , "dodgerblue2"
#"4" = "4 Smaa forretningsdrivende u. ansatte" , "dodgerblue1"
#"5" = "5 Teknikere (eksperter)" ,  "mediumpurple4"
#"6" = "6 Teknikere" , "mediumpurple1"
#"7" = "7 Manuelle arbejdere, hoejt niveau" , "indianred4"
#"8" = "8 Manuelle arbejdere, lavt niveau" ,  "indianred2"
#"9" = "9 Managere, hoejt niveau" , "steelblue4"
#"10" = "10 Managere, lavt niveau" , "steelblue1"
#"11" = "11 Clerks, hoejt niveau" , "yellow4"
#"12" = "12 Clerks, lavt niveau" , "yellow1"
#"13" = "13 Socio-kulturelle profesionelle" , "darkseagreen4"
#"14" = "14 Socio-kulutrelle semiprofesineolle" , "darkseagreen1"
#"15" = "15 Servicearbejdere, hoejt niveau" , "chocolate4" 
#"16" = "16 Servicearbejdere, lavt niveau") "chocolate1"








 
 
 

# help(brewer.pal)
# display.brewer.all()
# brewer.pal.info

# colourCount = length(unique(mtcars$hp))

#  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
 
# colourCount = length(unique(discodata$koen.gns.kvinder.andel))


# # ggplot(mtcars) + 
# #   geom_histogram(aes(factor(hp)), fill=getPalette(colourCount)) + 
# #   theme(legend.position="right")


# bpal.standardset  = colorRampPalette(brewer.pal(8, "Set2"))



skala_oesch16 <- c("yellow","darkorange1", # selvstændige
                   "mediumpurple4", "mediumpurple1", # teknikere -  mediumpink? slateblue?
                   "firebrick4","firebrick1", #arbejdere
                   "dodgerblue4", "dodgerblue1", #managers
                   "grey40", "grey88", #clerks
                   "palegreen4", "palegreen1", #sociokulturelle
                   "rosybrown4", "rosybrown1") #servicearbejdere
#"olivedrab4", "olivedrab1") #servicearbejdere





#beskaeft.tid.klasse.df$klasse_oesch16 <- relevel(beskaeft.tid.klasse.df$klasse_oesch16, 
#"1 Store forretningsdrivende",
#"2 Selvstaendige profesionelle",
#"3 Smaa forretningsdrivende m. ansatte",
#"4 Smaa forretningsdrivende u. ansatte",
#"5 Teknikere (eksperter)",
#"6 Teknikere",
#"7 Manuelle arbejdere, hoejt niveau",
#"8 Manuelle arbejdere, lavt niveau",
#"9 Managere, hoejt niveau",
#"10 Managere, lavt niveau",
#"11 Clerks, hoejt niveau",
#"13 Sociokulturelle profesionelle",
#"12 Clerks, lavt niveau",
#"14 Sociokulturelle semiprofessionelle",
#"15 Servicearbejdere, hoejt niveau",
#"16 Servicearbejdere, lavt niveau"



skala_oesch8 <- c(
	"darkgoldenrod1", # selvstændige
    "mediumpurple", # teknikere 
    # →   mediumpink? slateblue?
    "indianred", #arbejdere
    "royalblue", #managers
    "grey40", #clerks
    "seagreen", #sociokulturelle
    "rosybrown") #servicearbejdere
	# → "olivedrab4", "olivedrab1")

 

# #perindkialt breaks alle beskaeftigede
perindkialt.helepop.gns_num <- c(160000,250000,280000,325000,368000,458000,970000)
perindkialt.helepop.gns_lab <- c("160000 kr","250000 kr", "280000 kr", "325000 kr", "368000 kr", "458000 kr", "970000 kr")
# quants = seq(0,1,0.05) 
# format(round(quantile(discodata$perindkialt.helepop.gns, quants), digits=0), big.mark=".",decimal.mark=",",nsmall=0)
#kan mÃ¥ske slettes 
# #Ã¥rslÃ¸n breaks ledige loenmv
# loenmv.gns_num <- c(100000,150000,200000,250000,300000, 350000,400000)
# loenmv.gns_lab <- c("100.000 kr","150.000 kr", "200.000 kr", "250.000 kr", "300.000 kr", "350.000kr", "400.0000 kr")
# #Ã¥rslÃ¸n breaks alle beskaeftigede
# loenmv.gns.helepop_num <- c(130000,150000,200000,250000,300000, 350000,400000,550000)
# loenmv.gns.helepop_lab <- c("130.000 kr","150.000 kr", "200.000 kr", "250.000 kr", "300.000 kr", "350.000kr", "400.0000 kr","550.000 kr")





#ledighedsrisiko breaks
ledrisk_num <- c(10,20,25,30,40,50)
ledrisk_num <- ledrisk_num/100
ledrisk_lab <- c("10 %", "20 %","25 %", "30 %", "40 %", "50 %")
#alder breaks
alder.gns_num <- c(30,35,40,45,50,55)
alder.gns_lab <- c("30 Ã¥r", "35 Ã¥r", "40 Ã¥r", "45 Ã¥r", "50 Ã¥r", "55 Ã¥r")
# ledighedsperiode antal
ledperiod.antal.gns_num <- c(1.00,1.15,1.17,1.20,1.25,1.30)
ledperiod.antal.gns_lab <- c("1,00","1,15", "1,17","1,20","1,25","1.30")
# ledighedsperiode lÃ¦ngde
ledperiod.gns.lngde.gns_num <- c(1.20,1.40,1.50,1.65,1.70,1.80,1.9,2.0)
ledperiod.gns.lngde.gns_lab <- c("1,20","1,40","1,50","1,65","1,70","1,80","1,9","2,0")




#kÃ¸n breaks
koen_num <- c(1,5,10,25,50,75,90,99)
koen_num <- koen_num/100
koen_lab <- c("5 %", "5 %","10 %", "25 %", "50 %", "75 %", "90 %", "99 %")

# format(round(as.numeric(1000.64)), nsmall=1, big.mark=".",decimal.mark=",") 
# # # #til at lave breaks og labels med 
# format(summary(discodata$loenmv.helepop.gns), big.mark=".",decimal.mark=",")
# format(round(quantile(discodata$loenmv.helepop.gns, c(0.01, 0.10, 0.15, 0.25, 0.30, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98,0.985, .99)), digits=3), big.mark=".",decimal.mark=",",nsmall=0)
# # # #til at lave breaks og labels med 



#hÃ¸rer vist ikke til her, find ud af det senere (januar 2016)
# 100*summary(discodata$koen.gns.kvinder.andel)
# 100*round(quantile(discodata$koen.gns.kvinder.andel, c(0.01, 0.10, 0.15, 0.25, 0.30, 0.40, 0.50,0.60, 0.75, 0.90,0.95,0.96,0.97,0.98,0.985, .99)), digits=3)



