###Libraries

## 
install.packages("devtools")
















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




devtools::install_github("gaborcsardi/pkgconfig")
devtools::install_github("igraph/rigraph")

install_github("antongrau/MONECA")
install_github("antongrau/soc.report")
install_github("antongrau/soc.elite")


# pakke xda kan bruges til summering af data hurtigt, check den ud


# fejl med ny igraph version
require(devtools)
packageVersion("igraph")
install_version("igraph", version = "0.7.1", repos = "http://cran.us.r-project.org")

remove.packages("igraph")



install.packages("igraph")

library(igraph)
#Check version af pakke 
packageVersion("soc.elite")




packageVersion("soc.elite")
remove.packages("soc.elite")

require(devtools)
# september 29 2015 virker ikke 
install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/soc.elite_tidligereversion/soc.elite-cea7f5d3889fe7972b551e76aaf5db69619f8cf6 sept 28 2015")
# december 8 2015 virker ikke 
install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/soc.elite_tidligereversion/soc.elite-ec0c80e568cbae116e80e983fb8c522dc84db71c 8 december 2015")

# februar 16 2016 virker ikke 
install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/soc.elite_tidligereversion/soc.elite-24b415c37a2014e4daf3bdd1960256b08121ce87 16 februar 2016")

# august 12 2015 virker ikke 
install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/soc.elite_tidligereversion/soc.elite-d256df385c3cc394a4200cc5489d46c6cbe0a55c august 12 2015")


help(install_local)

packageVersion("ggplot2")
remove.packages("ggplot2")
install_version("ggplot2", version = "1.0.1", repos = "http://cran.us.r-project.org")
install.packages("ggplot2")

devtools::install_github("hadley/ggplot2")


getwd()

packageVersion("MONECA")



packageVersion("soc.elite")

remove.packages("toOrdinal")
install_local("/home/emil/Dropbox/Speciale/Emil_Soeren/statistik/R/moneca/toOrdinal_tidligere version/toOrdinal-071c1679323d87a7eb69ac057573b00c9abc0165 april 3 2015")

# removeDepends("XLConnect")



library(devtools)
install_github("antongrau/soc.elite") # når man installerer den siger den: Warning: replacing previous import ‘igraph::%>%’ by ‘stringr::%>%’ when loading ‘soc.elite’
install_github("antongrau/soc.report") #kræver rJava, se længere nede. og bruger xlsx-pakken, der fucker med XLConnect
######for at kunne installere "MONECA" skal "toOrdinal" installeres foerst
install.packages("toOrdinal")
install_github("antongrau/MONECA")
remove.packages("MONECA")



install.packages("xlsx")



remove.packages("MONECA")
remove.packages("soc.elite")
remove.packages("soc.report") 
remove.packages("igraph")



#pakker du ikke bruger for tiden
install.packages("SortableHTMLTables")
install.packages("car")
install.packages("gmodels")
install.packages("doBy")
install.packages("Zelig")
install.packages("psych")
install.packages("pastecs")
install.packages("wordcloud")				
install.packages("tm")
install.packages("gtools")
install.packages("TeachingDemos")
install.packages("rmarkdown")
install.packages("migest")
install.packages("Hmisc", dependencies=TRUE)



#pakker du bruger for tiden
install.packages("reshape", dependencies=TRUE)
install.packages("foreign", dependencies=TRUE)
install.packages("readr")

install.packages("migest")
install.packages("circlize", dependencies=TRUE)
install.packages("scales", dependencies=TRUE)
install.packages("plyr", dependencies=TRUE)
install.packages("dplyr") #uden dependencies, den vil ha mysql ting med

	install.packages("Rcpp")
	install.packages("httpuv")	#der må IKKE være noget i .Rprofilen når du installerer den her, ihvertfald ikke .First og .Last funktioner. 
	install.packages("shiny")		
install.packages("soc.ca")
	

install.packages("ggtheme", dependencies=TRUE) #not available for R 3.3.0, prøv senere (28. maj 2016)
install.packages("data.table")
install.packages("readstata13")
install.packages("RColorBrewer")
install.packages("readxl", dependencies=TRUE)
# install.packages("igraph", dependencies=TRUE)



#lidt specielle - skal installeres på særlig måde, hvor følgende køres først og rækkefølgen af pakkerne skal være sådan her:


# sudo apt-get install libcairo-dev 
	install.packages("gdtools")
	install.packages("svglite")
install.packages("ggplot2", dependencies=TRUE)


#
install.packages("xlsx") # noget med java, fiks hvis du har brug for den pakke 


#sudo apt-get install libxml2-dev libcurl4-openssl-dev build-essential libcurl4-gnutls-dev libcurl4-gnutls-dev libssl-dev

install.packages("devtools")
install.packages("XML")
install.packages("Matrix")


devtools::install_github("jalvesaq/colorout")


#sudo apt-get install libgsl0-dev
install.packages("topicmodels",dependencies=TRUE)

#p? windows skal Rtools.exe installeres f?r devtools kan hentes, den findes her (R 3.1.3 skal bruge Rtools 3.1)
http://cran.r-project.org/bin/windows/Rtools/

# i windows: kræver at den rigtige 64-bit version af java er installeret, nogle gange installeres 32-bit versionen uforvarende så vær sikker på det 
# i linux, følg følgende guide:

###########################################

Yossiles answer helped me on the way, but here is the newbie-friendly version, which also draws on the answer to this question

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


# remove.packages("xlsx")

install.packages("rJava")

install.packages("XLConnect")

install.packages("tm",dependencies=TRUE)


#library test 
library(devtools)
library(tm)
library(topicmodels)
library(igraph)
library(lattice)
library(Hmisc)
library(ggplot2)
library(SortableHTMLTables)
library(car)
library(RColorBrewer)
library(gmodels)
library(doBy)
library(Zelig)
library(reshape)
library(plyr)
library(psych)
library(pastecs)
library(wordcloud)
library(tm)
library(gtools)
library(foreign)
library(TeachingDemos)
library(readxl)
library(data.table)

# opdaterer samtlige pakker
update.packages(ask='graphics')
# updaterer alle user-installerede pakker, uden at spørge om hver enkelt
update.packages(ask=FALSE)

#Lokal pakke
install.packages("/home/emil/Documents/Statistik/R/ Rcphsoc_0.00.002.tar.gz", repos=NULL) 

## troubles nye (oktober 2014)

Cannot find curl-config

http://askubuntu.com/questions/359267/cannot-find-curl-config-in-ubuntu-13-04




