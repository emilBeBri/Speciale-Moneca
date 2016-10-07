


# lav ny levels order på dplyr måden 
 discodata <-  discodata %>% 
  mutate(klasse_egp11_2 = factor(klasse_egp11, levels = c("x","y")
)) 


### arrangerer levels i en faktor efter den rækkefølge de er i rows i dataframen 
df <- data.frame(
    num = 5:1,
    word = c("five", "four", "three", "two", "one"))

 df$word <-  as.factor(df$word)
levels(df$word) # levels are alphabetical by default
df$word # right now the order is 5,4,3,2,1
df <- df %>%
    arrange(num) %>%   # rearrange the df in the order we want (1,2,3,4,5)
    mutate(word = factor(word, word)) # this line reorders the factor in the same order
levels(df$word)  # now the levels are 1,2,3,4,5







## mulige brugbare links 

http://dr-k-lo.blogspot.dk/2014/03/the-simplest-way-to-plot-legend-outside.html?view=magazine



### global options #######

# s? der er 10 decimaler f?r lortet bruger scientific notion som er sygt irriterende
options(scipen=15)



### snippets til sublime

#fjern alle objekter
rm(list=ls()) = remove
write.table = rw 


#########

#fjernalt alt ALTT
rm(list=ls())


#gem og load workspaces
save.image("./hof.RData")
load("./hof.RData")

#working dirs
getwd()
setwd()

#lav en csv-fil
write.table(components.ind.nob.ud, file="sti/navn.csv", sep = ";", fileEncoding = "UTF-8")




#printer aller objekter - obs, hvis du har mange og store objekter tar det LANG tid, især med store matricer (fx i SNA)
for ( obj in ls() ) { print(get(obj)) }


#at ændre en factors levels ifht. en anden variabel:
Dat$Factor <- reorder(Dat$Factor, Dat$x)


#At lave nye variable fra gamle med matematiske operationer
dfm2$BuAc <- with(dfm2, Budget - Actual)
dfm2$BuAcPc <- with(dfm2, (Budget/Actual - 1) * 100)

#At droppe et level fra en factor - men kun hvis den ingen observationer har, tror jeg
levels(test2$test1)
test2$test1 <- test2$test1[,drop=TRUE]

#At ændre levels så de har den rigtige rækkefølge til et plot (http://learnr.wordpress.com/2010/03/23/ggplot2-changing-the-default-order-of-legend-labels-and-stacking-of-data/)
levels(diamonds$cut)
diamonds$cut <- factor(diamonds$cut, levels = rev(levels(diamonds$cut)))
levels(diamonds$cut)

#lav gennemsnit indenfor grupper i en factorvariabel
#p? en kontinuert variabel. Gemmes i en ny variabel:
a$grp.mean.values <- ave(a$V1, a$V2)

#Indl?s csv fil
a <-read.csv(".csv", sep=",")

#Skriv scv fil
write.csv(a,".csv")

#skriv dta fil
write.dta(a, ".dta")

#Fjern alle objekter, funktioner, *alt*
rm(list=ls())

#fjern noget specifikt
rm(x)

#list all objectsa
ls()

#slet variabel 
dataframe$var <- NULL

#giver en variabel med et 1 tal til alle observationer samt variabel med n
data3$count <- ifelse(data3$startlanguage=="da", c(1))
data3$n <- sum(data3$count)

 
#reorder levels
a$t9vaelg <- factor(a$vælg,levels(factor(a$vaelg))[c(2,1,3,4)])

a$t9vaelg <- factor(a$vælg,levels(a$vaelg)[c(2,1,3:10)])

#view a class of object

class [object]

#view structure
str(object)

#another way of subsetting data3 - in this example "=" is used, I think "<-" is better?
ds = within(ds, (cesdcut = ifelse(cesd>20, 1, 0)))


#Completely removes a variable
test$v6 <- NULL

#Remove several variables
data3[,c("unik_joh2","unik_parti2")] <- list(NULL)

#Installer en pakke lokalt
install.packages("/home/emil/Documents/Statistik/R/ Rcphsoc_0.00.002.tar.gz", repos=NULL) 

#Vis info om denne session:
sessionInfo()

#Lav analyseudvalg
au.missing <- ess4dk[c("gndr", "yrbrn", "trstprl")]

#summary statistics
sd(data$test, na.rm=TRUE)
mean(data$test, na.rm=TRUE)
quantile(data$v1, c(0, 0.01, 0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9, 0.95, 0.99, 1), na.rm=TRUE)


#subsetting et undervalg med bestemte kvaliteter
test <- au[au$alder < 20 & au$gndr==2, ]

# opdaterer samtlige pakker
update.packages(ask='graphics')


d <- describe(my.data.frame)
 d$age                   # print description for just age
 d[c('age','sex')]       # print description for two variables
 d[sort(names(d))]       # print in alphabetic order by var. names
 d2 <- d[20:30]          # keep variables 20-30
 page(d2)                # pop-up window for these variables

#Record several plots in the graphics-window
windows(record=T)

### Load the package or install if not present
if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}

#apply function til en række af variable - hvor "3:10" er nummeret på kolonnerne. er kun brugt på recode,
#duer sikkert også til andre funktioner! - her er to eksempler:
for(i in 3:10) au2[,i] <- recode(au2[,i],"88=NA ; 99=NA")
for(i in 8:21) a[,i] <- with(a,(a[,i]/1000))
for(i in 1:7) cronbach[,i] <- recode(cronbach[,i],"88=NA ; 99=NA")

#Lav ny variabel betinget p? gamle
au$stratum[au$alder > 71 & au$gndr==2] <- "K over 70"


#se på data
view(data)

# exclude 3rd and 5th variable 
newdata <- mydata[c(-3,-5)]

# include 3rd and 5th variable 
newdata <- mydata[c(3,5)]


#kig på data
View(dataset)


##################Postnumre-inddeling
#Postnumre
#Se kort nr.dk/map
#9xxx = Nordjylland
#7xxx = Midtjylland
#8xxx = Århus etc
#6xxx = Sønderhylland
#5xxx = Fyn
#4xxx = resten af sjælland
#3xxx = Nordsjælland


#Note: Kbh omegn:  (inklusive Valby, Vanløse, Brønshøj)
a$lokal_tmp <- recode(a$b1, "9400='4' ; 0:1499='1';1500:1799='2' ; 1800:2199='1' ; 2200='2' ; 2201:2399='1' 
;2400:2450='2' ; 2451:2999='3' ; 6700:6715='4' ; 8000:8299='4' ; 8355='4' ; 8361='4'; 8381='4' ; 9000:9220='4' ;
 5200:5270='4' ; 3000:5999='5' ; NA=NA ;else=6")
a$lokal6 <- ordered(a$lokal_tmp, levels = c(1,2,3,4,5,6), labels = c("Indre By, Østerbro, Ama'r, Frd.berg", 
                                                                     "Vesterbro, Nørrebro, NV", "Københavns Omegn", 
                                                                     "Århus, Aalborg, Odense, Esbjerg", "Provins: Øerne", "Provins: Jylland"))






txtStart('log1.txt') # this is similar to "log using mylogfile.txt" in Stata
txtStop() # this is similar to "log close" in Stata







################################M?ske redundante


#at ?ndre en factors levels ifht. en anden variabel:
Dat$Factor <- reorder(Dat$Factor, Dat$x)


#At lave nye variable fra gamle med matematiske operationer
dfm2$BuAc <- with(dfm2, Budget - Actual)
dfm2$BuAcPc <- with(dfm2, (Budget/Actual - 1) * 100)

#At droppe et level fra en factor - men kun hvis den ingen observationer har, tror jeg
levels(test2$test1)
test2$test1 <- test2$test1[,drop=TRUE]

#At ?ndre levels s? de har den rigtige r?kkef?lge til et plot (http://learnr.wordpress.com/2010/03/23/ggplot2-changing-the-default-order-of-legend-labels-and-stacking-of-data/)
> levels(diamonds$cut)
[1] "Fair"      "Good"      "Very Good" "Premium"
[5] "Ideal"
> diamonds$cut <- factor(diamonds$cut, levels = rev(levels(diamonds$cut)))
> levels(diamonds$cut)
[1] "Ideal"     "Premium"   "Very Good" "Good"
[5] "Fair"

#Fjern alle objekter, funktioner, *alt*
rm(list=ls())

#fjern noget specifikt
rm(x)

#list all objects
ls()

#giver en variabel med et 1 tal til alle observationer samt variabel med n
data3$count <- ifelse(data3$startlanguage=="da", c(1))
data3$n <- sum(data3$count)


#?ndre koordinater:
+ coord_flip() +opts(legend.position="top", legend.direction="horizontal")

#view a class of object

class [object]

#view structure
str(object)

#another way of subsetting data3 - in this example "=" is used, I think "<-" is better?
ds = within(ds, (cesdcut = ifelse(cesd>20, 1, 0)))


#Completely removes a variable
test$v6 <- NULL

#Remove several variables
data3[,c("unik_joh2","unik_parti2")] <- list(NULL)

#Installer en pakke lokalt
install.packages("/home/emil/Documents/Statistik/R/ Rcphsoc_0.00.002.tar.gz", repos=NULL) 

#Vis info om denne session:
sessionInfo()

#Lav analyseudvalg
au.missing <- ess4dk[c("gndr", "yrbrn", "trstprl", "contplt", "wrkprty", "wrkorg", "pbldmn", "bctprd", "mnactic", "edlvadk", "emplrel", "hinctnta")]

#summary statistics
sd(data$test, na.rm=TRUE)
mean(data$test, na.rm=TRUE)
quantile(data$test, c(0.25, 0.5, 0.6,), na.rm=TRUE)

#subsetting et undervalg med bestemte kvaliteter
test <- au[au$alder < 20 & au$gndr==2, ]

# opdaterer samtlige pakker
update.packages(ask='graphics')

#To change colour on a low-to-high plot:
ggplot(au, aes(x=trstep)) + geom_histogram(binwidth=1, aes(fill=..count..)) + scale_fill_gradient(low="Black", high="Pink")

d <- describe(my.data.frame)
 d$age                   # print description for just age
 d[c('age','sex')]       # print description for two variables
 d[sort(names(d))]       # print in alphabetic order by var. names
 d2 <- d[20:30]          # keep variables 20-30
 page(d2)                # pop-up window for these variables

#Record several plots in the graphics-window
windows(record=T)

### Load the package or install if not present
if (!require("RColorBrewer")) {
install.packages("RColorBrewer")
library(RColorBrewer)
}

#apply function til en r?kke af variable - hvor "3:10" er nummeret p? kolonnerne. er kun brugt p? recode,
#duer sikkert ogs? til andre funktioner!
for(i in 3:10) au2[,i] <- recode(au2[,i],"88=NA ; 99=NA")

#se p? data
view(data)



