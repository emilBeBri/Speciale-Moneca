



install.packages("lubridate")
install.packages("plotly")
install.packages("timevis")








## for at få devtools op at køre installer følgende
# sudo apt-get install libcairo-dev libcurl4-openssl-dev libssl-dev


#install.packages("classInt")
install.packages("mosaic")
install.packages("mosaic")
devtools::install_github("gaborcsardi/crayon")
library(crayon)
cat(blue("Hello", "world!\n"))


help(crayon)


test <-  make_style("seagreen")

error <-  test 
warn <- magenta $ underline
note <- cyan
cat(error("Error: subscript out of bounds!\n"))


ivory <- make_style("pink")
bgMaroon <- make_style("maroon", bg = TRUE)
fancy <- combine_styles(ivory, bgMaroon)

cat(fancy("This will have some fancy colors"), "\n")




### test til at få moneca til at virke

packageVersion("ggplot2")
packageVersion("igraph")

checkpoint::checkpoint("2015-06-01")
checkpoint::checkpoint("2015-05-01")



devtools::install_github("ndphillips/yarrr", 
                         build_vignettes = TRUE)



#problemer med digest og ggplot2 

# install.packages("tidyverse") #Hadley Wickham, vent med at brug den til du er færdig med Moneca 
# install.packages("haven") # lyder lovende ifht transfer mellem stata, sas, R og spss. 

install.packages("miniUI")
install.packages("shiny")
install.packages("DT")
install.packages("MASS")
install.packages("scales")
install.packages("digest")
install.packages("gridExtra")

install.packages("RDocumentation")
install.packages("gguitar")

install.packages("devtools")
library(gridExtra)

devtools::install_local("./statistik/R/moneca/tidligere_versioner_packages/25_06_2015/MONECA-bec4da65068c955eefbb8042c4c2100ac3bb3d8b")
devtools::install_local("./statistik/R/moneca/tidligere_versioner_packages/25_06_2015/MONECA-bec4da65068c955eefbb8042c4c2100ac3bb3d8b")




library(devtools,)




### commands til pakker 

 
# opdaterer samtlige pakker
update.packages(ask='graphics')
# updaterer alle user-installerede pakker, uden at spørge om hver enkelt
update.packages(ask=FALSE)

# egen funktion: removeDepends("") # fjerner alle dependencies også

#viser paths til pakke foldere 
.libPaths() 
# typisk de her 
# [1] "/home/emil/R/x86_64-pc-linux-gnu-library/3.3"
# [2] "/usr/local/lib/R/site-library"               
# [3] "/usr/lib/R/site-library"                     
# [4] "/usr/lib/R/library" 

setwd("/home/emil")
setwd("/home/emil/Dropbox/Speciale/Emil_Soeren")
getwd()

rm(list=ls())


# installer gammel version fra cran 
# require(devtools)
packageVersion("dplyr")
devtools::install_version("igraph", version = "0.7.1", repos = "http://cran.us.r-project.org")


# installer lokal pakke hentet fra fx github
devtools::install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/soc.elite_tidligereversion/soc.elite-cea7f5d3889fe7972b551e76aaf5db69619f8cf6 sept 28 2015")


#Lokal pakke alternativ metode?
# install.packages("/home/emil/Documents/Statistik/R/ Rcphsoc_0.00.002.tar.gz", repos=NULL) 



################### pakker ####################################


#p? windows skal Rtools.exe installeres f?r devtools kan hentes, den findes her (R 3.1.3 skal bruge Rtools 3.1)
# http://cran.r-project.org/bin/windows/Rtools/


install.packages("devtools") #Hadley Wickham
install.packages("checkpoint")
devtools::install_github("jalvesaq/colorout")
install.packages("plyr") #Hadley Wickham
install.packages("dplyr") #Hadley Wickham
install.packages("tibble") #Hadley Wickham
install.packages("data.table")
install.packages("reshape2") #Hadley Wickham 
install.packages("foreign")
install.packages("readr") #Hadley Wickham
install.packages("stringr") #Hadley Wickham
install.packages("migest")
install.packages("scales")
install.packages("Rcpp")
install.packages("httpuv")	#der må IKKE være noget i .Rprofilen når du installerer den her, ihvertfald ikke .First og .Last funktioner. 
install.packages("soc.ca") #Anton Grau
install.packages("circlize")
install.packages("broom") # tidy model output, god i kombi med dplyr 
install.packages("ggtheme") #not available for R 3.3.0, prøv senere (d. 23/08/2016)
install.packages("readstata13")
install.packages("RColorBrewer")
install.packages("readxl") #Hadley Wickham



#lidt specielle - skal installeres på særlig måde, hvor følgende køres først og rækkefølgen af pakkerne skal være sådan her:


# sudo apt-get install libcairo-dev 
	install.packages("gdtools")
	install.packages("svglite")
install.packages("ggplot2") #Hadley Wickham
install.packages("gdtools")

# i windows: kræver at den rigtige 64-bit version af java er installeret, nogle gange installeres 32-bit versionen uforvarende så vær sikker på det 
# i linux, følg følgende guide:

# Yossiles answer helped me on the way, but here is the newbie-friendly version, which also draws on the answer to this question

What worked for me was was this:

1) open /etc/environment in your favorite txteditor:

sudo leafpad /etc/environment

2) add a new line with "JAVA_HOME" and the path to correct jr-directory, in the case of java 8, it is:

JAVA_HOME="/usr/lib/jvm/java-8-oracle/jre"


export LD_LIBRARY_PATH=/usr/lib/jvm/java-7-oracle/lib/amd64:/usr/lib/jvm/java-7-oracle/jre/lib/amd64/server


3) source the updated enviroment and check the variable:

source /etc/environment

echo $JAVA_HOME

4) update the java-R configuration like this:

sudo R CMD javareconf

5) install RJava, XLConnect or whatever java-dependent R-package you are looking for.

# EKSTRA 
# problem når rJava-relaterede pakker ikke kan loades, så virker det her: http://people.duke.edu/~aql3/2015/02/01/rstudio-cannot-load-rjava-in-ubuntu/

6) Set LD_LIBRARY_PATH in ~/.profile to make it available to all desktop applications, including Rstudio. I /home/emil/.profile, set følgende, og vær sikker på at det er den korrekte java-version (7, 8, 9, whatever)

export LD_LIBRARY_PATH=/usr/lib/jvm/java-8-oracle/lib/amd64:/usr/lib/jvm/java-8-oracle/jre/lib/amd64/server

7) Then make R update its java configuration:

sudo R CMD javareconf

så burde det virke igen. 

###########################################


install.packages("xlsx") # noget med java, fiks hvis du har brug for den pakke 
install.packages("rJava")
install.packages("XLConnect")


# til moneca 

#  nyeste version af igraph
library(devtools)
devtools::install_github("gaborcsardi/pkgconfig")
devtools::install_github("igraph/rigraph")

# Antons pakker 
install_github("antongrau/soc.report") #Anton Grau
install_github("antongrau/soc.elite") #Anton Grau
	
	install.packages("toOrdinal")
devtools::install_github("antongrau/MONECA") #Anton Grau


#  evt. nyeste version af ggplot2 
# devtools::install_github("hadley/ggplot2")

