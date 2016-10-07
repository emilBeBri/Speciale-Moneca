## analyse af timelon

timelon.dst <- read_excel_allsheets("./statistik/DST/var_lønvariable/timeloen_for_grupper_2.xlsx")
timelon.dst <- data.frame(matrix(unlist(timelon.dst), nrow=600),stringsAsFactors=FALSE)
timelon.dst <- tbl_df(timelon.dst) 
timelon.dst <-  rename(timelon.dst, disco = X1,discodetail=X2,timelonDST=X3)
timelon.dst$timelonDST <-as.numeric(as.character(timelon.dst$timelonDST))
timelon.test <- inner_join(timelon.dst,timelon.helepop)

View(timelon.dst)

timelon.test <- timelon.test %>% select(disco,timelonDST,timelon.helepop2009,discodetail)



timelon.test <- mutate(timelon.test, timelon.helepop2009.inf=timelon.helepop2009*93.8/91.6)

view(timelon.test)


objekt <- discodata %>% select(disco, timelon.helepop.gns.inf.dst)
View(objekt)

View(discodata %>% select(disco, beskaeft.gns,beskaeft.andel.gns))



write.xlsx2(objekt,"./statistik/DST/var_lønvariable/loentest9.xlsx") 



View(disco$timelon.helepop.gns.inf.dst)



timelon.helepop.gns.inf.dst



mutate


timelon.helepop$disco        <- strtrim(timelon.helepop$disco, 4)

timelon.helepop$disco[1] <- c("0110")

view(discodata$disco)






is.character(timelon.dst$disco)
is.character(timelon.helepop$disco)




timelon.dst$disco_test        <- strtrim(timelon.dst$disco, 4)




View(timelon.helepop)
View(timelon.dst)



View(test)



is.data.frame(timelon.dst)



### gennemsnitsloen

 <-  150.6
 <-  151.58
 <-  159.75
 <-  163.85
 <-  177.66
 <-  181.3
 <-  185.65
 <-  184.81
 <-  195.2
 <-  202.32
 <-  209.82
 <-  217.42
 <-  222.58
