# startdokument

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

#load("./statistik/R/moneca/vores/.Rdata")


#fjerner alt 
rm(list=ls())
###
source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")
#### selectors ####

#HOVEDKORT
mob.mat <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/moneca_disco_mse_250kat_version1_allebeskaeft.xlsx")
# mob.mat <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede/moneca_disco_mse_250kat_version1_allebeskaeft_periode19962001.xlsx")
nrowallbeskaeft <- 274
allbeskaeft <- loadWorkbook("./statistik/DST/DST_output/00_emil_speciale/MONECAs/allebeskaeftigede_250kat_version1.xlsx")
label.kode   <- read.csv("./statistik/R/moneca/vores/voresdata/Oversat Moneca kategorier_es_250kat.csv", sep = ";")
label <- label.kode$Disco_tekstogkode
label.short <- label.kode$Disco_kode
nrowputexcel <- 273
nrowtab2xl <- 274
source("./statistik/R/moneca/vores/vorescripts/1_data_se_ny.R")
# tmp data 
source("./statistik/R/moneca/vores/vorescripts/2_analyse_se.R")

save.image("./foerdplyr")
rm(list=ls())
load("./foerdplyr")
# Dplyr datasaet essentials 
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_essentials.R")
# Dplyr datasaet 
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet.R")
# Dplyr datasaet klasser
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_klasser.R")
#ekstra
source("./statistik/R/moneca/vores/vorescripts/6_skab_dplyrdatasaet_efterbehandling.R")

discodata <- tbl_df(discodata)
discodata_newestmoneca_bak <- discodata

#0 
### load ovenstaende 
save.image("./statistik/R/moneca/vores/voresdata/WORKINGPROGRESS_allebeskaeft250")
setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
setwd("//VBOXSVR/Emil_Soeren")

setwd("C:/Users/bubba/Dropbox/Speciale/Emil_Soeren")
rm(list=ls())
checkpoint::checkpoint("2015-06-25")
source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")
load("./statistik/R/moneca/vores/voresdata/WORKINGPROGRESS_allebeskaeft250")


source("./statistik/R/moneca/vores/vorescripts/7_komma_nul_defaultsoglayout.R")


 ############3 til gamle igraph, 

# defaults til ggplot2 kort 

# d. 08/09/2016 lavet på Ubuntu med nyeste moneca og igraph 
save.image("./statistik/R/moneca/vores/voresdata/dplyrdata_allebeskaeft250_oldmoneca")
load("./statistik/R/moneca/vores/voresdata/dplyrdata_allebeskaeft250_oldmoneca")







#alle beskaeftigede 250 kat tmp med disco kort objekt og seg.df  - NY GAMMEL VERSION MED IGRAPH 0.7.1
#save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_igraph071")
("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_igraph071")


#OLD alle beskaeftigede 250 kat tmp med disco kort objekt og seg.df 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_OLD")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft250_OLD")





##################################################################



#alle beskaeftigede tmp 150 kat 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft")



# socio/socstil tmp 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_sociosoctil")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_sociosoctil")


#alle beskaeftigede og ledige tmp 
# save.image("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft_og_ledige")
load("./statistik/R/moneca/vores/voresdata/tmp_efterdefaultsoglayout_allebeskaeft_og_ledige")

#bare tmp 1 uden map defaults 
save.image("./statistik/R/moneca/vores/voresdata/tmp_tmp")
load("./statistik/R/moneca/vores/voresdata/tmp_tmp")

#bare tmp 2 med map defaults 
save.image("./statistik/R/moneca/vores/voresdata/tmp_tmp2")
load("./statistik/R/moneca/vores/voresdata/tmp_tmp2")





################### flere ting #########################


# sletter discodataframen
discodata <- NULL
# save.image("./statistik/R/moneca/vores/voresdata/tmp_nyeste")
load("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2")




############gamle versioner af moneca- HUSK at gem en frisk kopi f?r du laver en ny moneca!! ##############
#helt gamle version.
#save.image("./statistik/R/moneca/vores/voresdata/moneca_kat150_version1")


# version med 150 kategorier, men hvor nogle af socstils koder ikke var med, fx barselsdagpenge, flexydelse og delvis ledighed.



#save.image("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2")
# load("./statistik/R/moneca/vores/voresdata/gamleversioner/0_moneca_disco_mse_150kat_version2")


#gem workspace, husk at læg det et andet sted hen
save.image("./statistik/R/moneca/vores/output/moneca_data")


rm(list=ls())
#socio/socstil kort
# save.image("./statistik/R/moneca/vores/output/socio_socstil/moneca_data")
load("./statistik/R/moneca/vores/output/socio_socstil/moneca_data")
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

