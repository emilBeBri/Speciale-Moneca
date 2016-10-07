

install.packages("mapDK")
library(mapDK)



mapDK()


pdf(file = "./statistik/R/moneca/vores/output/test.maps.pdf", height = 15, width = 15)
mapDK(values = "indbrud", id = "kommune", data = crime)
dev.off()


pdf(file = "./statistik/R/moneca/vores/output/test.maps.pdf", height = 15, width = 15)
mapDK(values = "stemmer", id = "id", 
      data = votes,
      detail = "zip", show_missing = FALSE,
      guide.label = "Stemmer \nSocialdemokratiet (pct)")
dev.off()

help(mapDK)



crime <- crime

crime <- votes


View(crime)

