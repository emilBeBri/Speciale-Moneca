# startdokument

aabn_xls("./statistik/R/moneca/vores/voresdata/Segment_labels_250kat.xlsx")

aabn_xls("tegnet_ny.xlsx")



aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/df.xlsx")
view(seg.df)
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/seg.df.xlsx")
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/mob.mat.xlsx")
aabn_xls("./statistik/R/moneca/vores/00_emilspeciale_output/dataframes/mob.mat.original.xlsx")


aabn_xls("./latexopgave/tabel/00_outputtilExcel_LATEX.xlsx")





# ############ source files #########

# #emil:
# #setwd("/media/EcoGreen_2/Dropbox/Emil_Soeren/statistik/R/moneca/vores")
# #setwd("./statistik/R/moneca/vores")
# setwd("E:/Dropbox/Speciale/Emil_Soeren")
# setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")

setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
setwd("C:/Users/emil/Dropbox/Speciale/Emil_Soeren")
setwd("//VBOXSVR/Emil_Soeren")
getwd()
checkpoint::checkpoint("2015-06-25")

#load("./statistik/R/moneca/vores/.Rdata.Rdata")

library(XLConnect)
#fjerner alt 
rm(list=ls())
###
source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")
source("./statistik/R/moneca/vores/vorescripts/0_funktion_mobilitetsanalysemotor.multi.R")
source("./statistik/R/moneca/vores/vorescripts/0_funktion_mobilitetsanalysemotor.uni.R")
source("./statistik/R/moneca/vores/vorescripts/0_funktion_mobilitetsanalysemotor.uni.desc.R")
source("./statistik/R/moneca/vores/vorescripts/0_funktion_mobilitetsanalysemotor.uni.df.R")



#### selectors ####

#HOVEDKORT

nrowallbeskaeft <- 274
allbeskaeft <- read_excel_allsheets("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede_250kat_version1.xlsx")
mob.mat <-  read_excel("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/moneca_disco_mse_250kat_version1_allebeskaeft.xlsx")
label.kode   <- read.csv("./statistik/R/moneca/vores/voresdata/Oversat_Moneca kategorier_es_250kat.csv", sep = ";")
label <- label.kode$Disco_tekstogkode
label.short <- label.kode$Disco_kode
nrowputexcel <- 273
nrowtab2xl <- 274
source("./statistik/R/moneca/vores/vorescripts/1_data_se_ny.R")
source("./statistik/R/moneca/vores/vorescripts/2_analyse_se.R")
save.image("./foerdplyr.Rdata")
rm(list=ls())
load("./foerdplyr.Rdata")
# Dplyr datasaet essentials 
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_essentials.R")
 # Dplyr datasaet
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_udvidet.R")
# Dplyr datasaet klasser
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_klasser.R")
#segment niveau - skal først fikset ifht. ændringer af kulturklyngen
# source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet.seg.baggrund.R")
#ekstra
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_efterbehandling.R")
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_labelsandstuff.R")
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_efterbehandling2_klyngeliste.R")
nrow(discodata) == 273
nrow(seg.df) == 47 #fra 54 til 47
discodata <- tbl_df(discodata)
discodata_bak  <-  discodata 
#klassefraktioner 



 
#0 
### load ovenstaende 
save.image("./statistik/R/moneca/vores/voresdata/WORKINGPROGRESS_allebeskaeft250.Rdata")
d <- 5+7
aegteja <- 6+6


setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
setwd("//VBOXSVR/Emil_Soeren")

setwd("C:/Users/bubba/Dropbox/Speciale/Emil_Soeren")
rm(list=ls())
checkpoint::checkpoint("2015-06-25")
source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")
load("./statistik/R/moneca/vores/voresdata/WORKINGPROGRESS_allebeskaeft250.Rdata")
 
source("./statistik/R/moneca/vores/vorescripts/7_komma_nul_defaultsoglayout.R")
  
############3 til gamle igraph, 

# defaults til ggplot2 kort 

# d. 08/09/2016 lavet på Ubuntu med nyeste moneca og igraph 
save.image("./statistik/R/moneca/vores/voresdata/dplyrdata_allebeskaeft250_oldmoneca.Rdata")
load("./statistik/R/moneca/vores/voresdata/dplyrdata_allebeskaeft250_oldmoneca.Rdata")



#alle beskaeftigede 250 kat tmp med disco kort objekt og seg.df  - NY GAMMEL VERSION MED IGRAPH 0.7.1
#save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_igraph071.Rdata")
("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_igraph071")


#OLD alle beskaeftigede 250 kat tmp med disco kort objekt og seg.df 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_OLD.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_OLD.Rdata")





##################################################################



#alle beskaeftigede tmp 150 kat 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft.Rdata")



# socio/socstil tmp 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_sociosoctil.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_sociosoctil.Rdata")


#alle beskaeftigede og ledige tmp 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft_og_ledige.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft_og_ledige.Rdata")

#bare tmp 1 uden map defaults 
save.image("./statistik/R/moneca/vores/voresdata/tmp_tmp.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_tmp.Rdata")

#bare tmp 2 med map defaults 
save.image("./statistik/R/moneca/vores/voresdata/tmp_tmp2.Rdata")
load("./statistik/R/moneca/vores/voresdata/tmp_tmp2.Rdata")





################### flere ting #########################


# sletter discodataframen
discodata <- NULL
# save.image("./statistik/R/moneca/vores/voresdata/tmp_nyeste.Rdata")
load("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2.Rdata")




############gamle versioner af moneca- HUSK at gem en frisk kopi f?r du laver en ny moneca!! ##############
#helt gamle version.
#save.image("./statistik/R/moneca/vores/voresdata/moneca_kat150_version1.Rdata")


# version med 150 kategorier, men hvor nogle af socstils koder ikke var med, fx barselsdagpenge, flexydelse og delvis ledighed.



#save.image("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2.Rdata")
# load("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2.Rdata")


#gem workspace, husk at læg det et andet sted hen
save.image("./statistik/R/moneca/vores/output/moneca_data.Rdata")


rm(list=ls())
#socio/socstil kort
# save.image("./statistik/R/moneca/vores/output/socio_socstil/moneca_data.Rdata")
load("./statistik/R/moneca/vores/output/socio_socstil/moneca_data.Rdata")
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet.R")

source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")

#


# rod 

txt=['find matches of a string in a vector of strings' 'search position of a character string in an other string' 'Compare Strings']

grep(txt,'strings')
grep(txt,['strings' 'Strings'])

[r,w]=grep(txt,['strings' 'Strings'])


str = ["hat";"cat";"hhat";"chat";"hcat";"ccchat";"at";"dog"]


res <- lapply(seg, function(ch) grep("", ch))

