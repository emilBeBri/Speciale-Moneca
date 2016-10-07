
### deskriptivt om segmenter i socio/socstil moneca


### deskriptivt om segmenter i socio/socstil moneca

seg.df <- seg.qual.final
seg.df <-  seg.df %>%
  select(membership, everything())

# gns og den slags - husk at det afhænger af om de skal summeres eller ej!!
# til.df.seg <- ddply(discodata,~membership,summarise,mean=mean(alder.gns),sd=sd(alder.gns))
til.seg.df <- ddply(discodata,~membership,summarise,ledbeskaeft.gns=sum(ledbeskaeft.gns))
seg.df <- left_join(seg.df, til.seg.df)



####


arrange(seg.df, desc(Density), desc(within.mob)) %>% 
	print(,n=32)	



view(seg.df)
view(til.seg.df)






arrange(seg.df, within.mob,Density) %>% 
	print(,n=32)	




# seg.df <-  seg.qual.final %>% select(contains("disco"), membership, within.mob, share.of.mob, Density,Nodes,max.path,share.total.size, contains("intern"))

view(seg.qual.final)



til.df.seg <-  discodata %>% select(membership,intern.mobilitet.seg



seg.df <-  semi_join(seg.qual.final, til.df.seg)



view(seg.df)










# Open file in Excel and save filename of temporary Excel file 
file10173e783af <- view(df.t)
file10173e783af #location of file 

# Go to Excel and browse the file and/or do some changes

# Import changes to R after closing Excel
wb <- loadWorkbook(file10173e783af)
df.t <- readWorksheet(wb, sheet=1)

