########## for aggregerede submatricer shit ############

save.image("./statistik/R/moneca/vores/voresdata/tmp3_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp3_allebeskaeft250.Rdata")

#beskær matrice til ønskede format og med det ønskede minium antal i cellerne 
mat.e.result <- mat.e.result[seg.index,ikke.seg.index.i.submatrix]  


# fjern segmenter der er for små 
beskaeringsindeks.tmp  <-  sort(unique(unlist(which(rowSums(mat.e.result) >= 1000))))



mat.e.result <- mat.e.result[beskaeringsindeks.tmp,]


###

#antal discogrp tilbage efter udlusning
summeringsindeks  <-  sort(unique(unlist(which(colSums(mat.e.result) <= 1000))))
beskaeringsindeks.tmp  <-  sort(unique(unlist(which(colSums(mat.e.result) >= 1000))))
sum(mat.e.result) == sum(mat.e.result[,beskaeringsindeks.tmp]) +sum(mat.e.result[,summeringsindeks]) #skal være sand 




### restgrupper 
test.af.restgruppe <-   rowSums(mat.e.result)

restgrupper   <-  mat.e.result[,summeringsindeks]
rowSums.restgrupper <- rowSums(restgrupper)
colnames.tmp <-  discodata$indeks[which(discodata$disco %in% colnames(restgrupper))]
colnames.tmp <-  as.character(discodata$membership_lab[which(discodata$disco %in% colnames(restgrupper))])
colnames(restgrupper) <- colnames.tmp
restgrupper  <-  restgrupper[,order(colnames(restgrupper))]
# r <- rle(colnames(restgrupper))
# changes <- rep(seq_along(r$lengths), r$lengths)
restgrupper <-  t(restgrupper)
restgrupper <- tbl_df(restgrupper) 
restgrupper$membership <- sort(colnames.tmp)

restgrupper <-  aggregate(restgrupper[, seq_len(ncol(restgrupper)-1)], list(restgrupper$membership), sum)
restgrupper$Group.1 <- NULL
restgrupper <-  t(restgrupper)
colnames(restgrupper) <- sort(unique(colnames.tmp))
# fjern segmenter der er for små 
summeringsindeks.tmp.1  <-  sort(unique(unlist(which(colSums(restgrupper) <= 1000))))
summeringsindeks.tmp.2  <-  sort(unique(unlist(which(colSums(restgrupper) >= 1000))))
rest.rest.grupper   <-  restgrupper[,summeringsindeks.tmp.1]
rowSums( cbind(restgrupper[,summeringsindeks.tmp.2] ,rowSums(rest.rest.grupper))) == rowSums(restgrupper)


restgrupper.samlet <-  cbind(rowSums(rest.rest.grupper),restgrupper[,summeringsindeks.tmp.2] )


colnames(restgrupper.samlet)[1] <- paste(c("Andre:"),
ncol(rest.rest.grupper), "erhvervsgrupper", c("fra"), length(unique(colnames(rest.rest.grupper))), "segmenter",sep=" ")




#fjerner de små discogrp der er slået sammen nu 
mat.e.result <-  mat.e.result[,beskaeringsindeks.tmp]

################# labels 

save.image("./statistik/R/moneca/vores/voresdata/tmp5_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp5_allebeskaeft250.Rdata")

col_labels.mat.e.result <- paste(discodata$membership[which(discodata$disco%in%colnames(mat.e.result))], ":", " ", strtrim(colnames(mat.e.result) , 50), sep="")
row_labels.mat.e.result <- paste(rev(sort(unique(discodata$membership_lab[which(discodata$membership %in% rownames(mat.e.result))]))),":", " ", sep="") #OBS! Her bliver de IKKE matchet på deres (unikke) diagonal, fordi den er skåret væk, men udelukkende på position. Mulig fejlkilde. 
colnames(mat.e.result) <- col_labels.mat.e.result
rownames(mat.e.result) <- row_labels.mat.e.result
# colnames(pheatmap.display) <- col_labels.mat.e.result
# rownames(pheatmap.display) <- row_labels.mat.e.result


# save.image("./statistik/R/moneca/vores/voresdata/tmp4_allebeskaeft250.Rdata")
# rm(list=ls())
# load("./statistik/R/moneca/vores/voresdata/tmp4_allebeskaeft250.Rdata")


restgrupper.samlet <-  cbind(restgrupper.samlet,mat.e.result[,1])

#raekkefolge ifht. antal skift i segment af interesse : rows 
mat.e.result <-  mat.e.result[rev(order(rowSums(mat.e.result))),]
restgrupper.samlet <- restgrupper.samlet[order(match(restgrupper.samlet[,ncol(restgrupper.samlet)],mat.e.result[,1]) ),]

#raekkefolege ifht til segment af discogrupper - cols
mat.e.result <-  mat.e.result[,order(colnames(mat.e.result) , colSums(mat.e.result) )]

#tilføj rest.gruppe.disco men beholder mat.e.results rownames
 rownames.tmp <-  rownames(mat.e.result)
 restgrupper.samlet <- restgrupper.samlet[,-ncol(restgrupper.samlet)]
 mat.e.result <-  cbind(restgrupper.samlet,mat.e.result)

rowSums(mat.e.result) %in%  test.af.restgruppe # skal være sande 

rownames(mat.e.result) <- rownames.tmp



#ændringer rækkefølge final 
somdeter <-  data.frame(colnames(mat.e.result))
somdeter <-  data.frame(colnames(mat.e.result))
colnames(somdeter) <- c("label")
somdeter$label_kort <-  strtrim(somdeter$label,5)
somdeter$colsums <-  colSums(mat.e.result)
somdeter <- somdeter %>% group_by(label_kort) %>% mutate(colsums.grp= sum(colsums))
somdeter$colsums.grp[1] <- -9999999
somdetskalvaere <- somdeter %>% arrange(desc(colsums.grp), label_kort,label)
somdetskalvaere <-  somdetskalvaere[nrow(somdetskalvaere):1,]
korrekt <-  match(colnames(mat.e.result),somdetskalvaere$label)
mat.e.result <- mat.e.result[,order(korrekt)]




# procent 
mat.e.result.andel <- mat.e.result
mat.e.result.andel <- 100*(mat.e.result/sum(mat.e.result.andel))
sum(mat.e.result.andel)==100 #procent, skal være 100


save.image("./statistik/R/moneca/vores/voresdata/tmp6_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp6_allebeskaeft250.Rdata")


# beregning af bedst data sted og udlusning af irrelevante discogrupper - total 
mat.e.result.tmp <- as.vector(mat.e.result[mat.e.result!=0])
# beregning af bedst data sted og udlusning af irrelevante discogrupper - procent 
mat.e.result.andel.tmp <- as.vector(mat.e.result.andel[mat.e.result.andel!=0])
mat.e.result.andel.tmp <- round2(mat.e.result.andel.tmp,5)





## sæt marginaler på (ikke nødvendigvis en god ide, forsøg)

#col 
marginaler_col <-  colSums(mat.e.result)
mat.e.result  <-  rbind(mat.e.result,marginaler_col)
rownames(mat.e.result)[nrow(mat.e.result)] <- c("Række total")

marginaler_col <-  colSums(mat.e.result.andel)
mat.e.result.andel  <-  rbind(mat.e.result.andel,marginaler_col)
rownames(mat.e.result.andel)[nrow(mat.e.result.andel)] <- c("Række total")


#row
marginaler_row <-  rowSums(mat.e.result)
mat.e.result  <-  cbind(marginaler_row,mat.e.result)
colnames(mat.e.result)[1] <- c("Kolonne total")

marginaler_row <-  rowSums(mat.e.result.andel)
mat.e.result.andel  <-  cbind(marginaler_row,mat.e.result.andel)
colnames(mat.e.result.andel)[1] <- c("Kolonne total")


#display 
 pheatmap.display <- mat.e.result
 pheatmap.display.andel <- mat.e.result.andel






median(mat.e.result.tmp)
mean(mat.e.result.tmp)
quantile(mat.e.result.tmp,seq(0,1,0.05))
quantile(mat.e.result.tmp,seq(0.9,1,0.005))
Desc(mat.e.result.tmp,plotit=TRUE)
median(mat.e.result.andel.tmp)
mean(mat.e.result.andel.tmp)
quantile(mat.e.result.andel.tmp,seq(0,1,0.05))
quantile(mat.e.result.andel.tmp,seq(0.9,1,0.005))
Desc(mat.e.result.andel.tmp,plotit=TRUE)


# ceiling på høje værdier
#total 
mat.e.result[mat.e.result>=1250] <- 1250
pheatmap.display <- round(pheatmap.display,1)
# pheatmap.display[pheatmap.display==2] <- c("2+ %")
pheatmap.display[pheatmap.display==0] <- c("")
# diag(pheatmap.display) <- c("im")

#andel 
mat.e.result.andel[mat.e.result.andel>=3.5] <- 3.5
pheatmap.display.andel <- round(pheatmap.display.andel,1)
# pheatmap.display.andel[pheatmap.display.andel==2] <- c("2+ %")
pheatmap.display.andel[pheatmap.display.andel==0] <- c("")
# diag(pheatmap.display.andel) <- c("im")

#fjern værdier kolonne og række total i selve data-matricen
mat.e.result[,1] <- median(mat.e.result.tmp)
mat.e.result[nrow(mat.e.result),] <- median(mat.e.result.tmp) 
mat.e.result.andel[,1] <- median(mat.e.result.andel.tmp) - 0.5
mat.e.result.andel[nrow(mat.e.result.andel),] <- median(mat.e.result.andel.tmp) - 0.5




annotation_col.df <- data.frame(somdetskalvaere$label)
colnames(annotation_col.df) <- c("label")
annotation_col.df$label <- strtrim(annotation_col.df$label,5)
annotation_col.df <-  rbind(c("Kolonne Total"),annotation_col.df)
rownames(annotation_col.df) <- colnames(mat.e.result.andel)


# view(annotation_col.df)

# tet <- c("dsW##% :")
# tet
# # somdetskalvaere$label[1] <- c("1.245: ") 
# tet <-  strtrim(somdetskalvaere$label,5)
# tet <-  gsub(" ", "", tet)
# # tet2 <-  seg.df$klasse_oeschbegtrup[which(seg.df$membership %in% tet)]
# view(levels(discodata$klasse_oesch16))

# view(discodata$klasse_oesch16[discodata$membership %in%  c("1.45","1.75","1.99","1.162","1.27","1.204")])

# tet <-  substring(somdetskalvaere$label, 5)
# tet <-  gsub(":", "", tet)
# tet <-  gsub(" ", "", tet)
# tet <-  trim.leading(tet)
# view(tet)
# view(tet2)  
#   which(discodata$disco %in% tet)
# trim.leading <- function (x)  sub("^\\s+", "", x)
# tet <-  gsub("'/[^a-zA-Z]", "", somdetskalvaere$label)
# gsub(" ", "", tet)
# annotation_col.df$label_f <- as.factor(annotation_col.df$label) 






#gaps
# gaps_col.df <- colnames(mat.e.result.andel) 
# gaps_col.df[seq_len(ncol(restgrupper.samlet))] <- rep("store ørn",ncol(restgrupper.samlet))


CW <- 40
CH <- 60

pheatmap::pheatmap(mat.e.result.andel, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE, fontsize_number=10,gaps_row=append(first.changes(rownames(mat.e.result.andel)),length(rownames(mat.e.result.andel))), gaps_col=first.changes(annotation_col.df$label),display_numbers = pheatmap.display.andel, cellwidth = CW, cellheight = CH, annotation_col=annotation_col.df)


# farver - tøv en kende. der er mere i 9_tmp7_R
klynge <- sample(xmen ,size= length(levels(annotation_col.df$label_f)) ,replace=TRUE) 

klynge 


view(ann_colors)
anno_farver <- list(klynge = klynge)

	,annotation_colors = anno_farver)




# first.changes(strtrim(colnames(mat.e.result.andel),6))









# colnames(restgrupper.samlet)


# # annotation_col.df[seq(ncol(restgrupper.samlet)+1, ncol(mat.e.result.andel)),] <- strtrim(colnames(mat.e.result.andel),6)[seq(ncol(restgrupper.samlet)+1, ncol(mat.e.result.andel)) ]


# annotation_col.df[seq_len(ncol(restgrupper.samlet)),] <- rep("store ørn",ncol(restgrupper.samlet))







# colnames


# annotation_col=





# append(,

















# 	),6))





# )



# 	,



# view(restgrupper.samlet)



# 	,annotation_row=annotation)


# view(mat.e.result)








# # annotation - farver til segmenter 

# annotation <- as.data.frame(colnames(mat.e.result))
# annotation <- as.data.frame(c("grp1","grp2","grp3","grp4","grp5","grp6","grp7","grp8","grp9","grp9","grp9"))


# annotation[]


# colnames(annotation) <- c("segment")




# , annotation_row = annotation,annotation_col = annotation


# rownames(annotation) <- colnames(mat.e.result) 


# colnames(annotation) <- c("klynge")


# # annotation[,1] <-  as.factor(annotation[,1])

# # breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)

# save.image("./statistik/R/moneca/vores/voresdata/tmp5_allebeskaeft250.Rdata")
# rm(list=ls())
# load("./statistik/R/moneca/vores/voresdata/tmp5_allebeskaeft250.Rdata")








# cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/manuelonly.pdf", height = 15, width = 20)
# # pheatmap::pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=10,annotation_colors = anno_farver)
# pheatmap::pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(seq_len(10))), cluster_cols=FALSE,cluster_rows=FALSE, fontsize_number=10,
# 	display_numbers = pheatmap.disp,
# #ubeskåret 
# gaps_row=first.changes(heatmap.gaps$membership),gaps_col=first.changes(heatmap.gaps$membership),annotation_row=heatmap.anno_row,annotation_col=heatmap.anno_row,
# annotation_colors = anno_farver)
# dev.off()



# cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/manuelonly.pdf", height = 15, width = 20)
# # pheatmap::pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=10,annotation_colors = anno_farver)
# #medbeskåretkolonne, med beskåret række - virker ikke endnu med annotation_colors, tror ikke det er et problem jeg kan løse. Ret surt. 



# save.image("./statistik/R/moneca/vores/voresdata/tmp4_allebeskaeft250.Rdata")
# rm(list=ls())
# load("./statistik/R/moneca/vores/voresdata/tmp3_allebeskaeft250.Rdata")









# first.changes(heat.krit.df$membership_bak[index.for.disco.1])


# view(seg.df)




# view(heat.krit.df)
# view(mat.e.result)
# view(mat.e.result_thirdpoint)






# #forsøg med annotation
# annotation_row=heatmap.anno_row) 
#  heatmap.anno_row <-    data.frame(factor(heatmap.anno$membership[1:9]))
# rownames(heatmap.anno_row) <- heatmap.anno$disco[1:9]
# colnames(heatmap.anno_row) <- c("segment")
# is.factor(heatmap.anno_row$segment)
# levels(heatmap.anno_row$segment)
# ncol(mat.e.result)
# nrow(mat.e.result)
#  test  <-  heatmap.anno_row$segment[1:9]
#  test <- data.frame(test)
#  colnames(test) <- c("segment")
# rownames(test) <- sort(seg.klynger)
#  view(test)
# is.data.frame(test)

# # gaps_row=first.changes(heatmap.gaps$membership[seq_len(length(seg.klynger))]),gaps_col=first.changes(heatmap.gaps$membership[index.for.disco.ny]), annotation_row = annotation$segment,annotation_col = annotation$segment)



# #udenbeskåretkolonne, med beskåret række
# # gaps_row=first.changes(heatmap.gaps$membership[seq_len(length(seg.klynger))]),gaps_col=first.changes(heatmap.gaps$membership))






# length(annotation$segment[index.for.disco.ny])


# sort(as.numeric(as.character(heat.krit.df$membership)))

# sort(as.character(heat.krit.df$membership))

# ###################### old school metode #######################



# manuelle.klynger <- c("5.2","5.1","4.10","4.4","4.8","3.9","3.25","3.24","3.15")
# klynger <- manuelle.klynger
# klynger <- c("3.24")

# mat.e.result  <-  e.pheat.dataprep(klynger)
# # heat.krit.df <-  df %>% filter(membership %in% klynger) %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)
# # heat.krit.df <-  df %>% filter(membership %in% klynger) %>%  rename(segkrit=membership) %>% select(disco,segkrit)


# #heat.krit 
# heat.krit.df <-  df %>% filter(indeks %in% aug.work.list) %>%  rename(segkrit=membership) %>% select(disco,segkrit)
# heat.krit.df$segkrit <- ifelse(heat.krit.df$segkrit %in% klynger, c("manuelt arbejde"), c("ikke manuelt arbejde"))
# heat.krit.df <-  df %>% filter(membership=="4.1") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)


# heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
# heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)

# nrow(heat.krit.df)
# nrow(mat.e.result)


# mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]
# # pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)
# annotation <- as.data.frame(heat.krit.df$segkrit)
# rownames(annotation) <- heat.krit.df$disco 
# colnames(annotation) <- c("klynge")
# # pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))
# mat.e.result[mat.e.result>=10] <- 10
# pheatmap.disp <- round(mat.e.result,0)
# # diag(pheatmap.disp)[] <- c("-")
# pheatmap.disp[pheatmap.disp==10] <- c("10+")
# klynge <- sample(xmen ,size=length(levels(annotation[,1])),replace=TRUE)
# names(klynge) <- levels(annotation[,1])
# anno_farver <- list(klynge = klynge)
# klynge <- sample(xmen ,size=length(levels(annotation[,1])),replace=TRUE)
# annotation_colors = anno_farver



# breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
# colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
# rownames(mat.e.result) <- strtrim(rownames(mat.e.result),60)
# cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/showcase.pdf", height = 15, width = 20)
# pheatmap::pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=5,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))
# dev.off()















# ##############simpel: for en enkelt discogruppe ###############



# library(pheatmap)

# median(mat.e.result)
# mat.e.result <- e.mobmat.seg.manuel.til.fra
# mat.e.result <-  mat.e.result[order(sort(diag(mat.e.result))), order(sort(diag(mat.e.result)))]
# mat.e.result[mat.e.result>=500] <- 500
# pheatmap.disp <- round(mat.e.result,0)
# # diag(pheatmap.disp)[] <- c("-")
# pheatmap.disp[pheatmap.disp==500] <- c("500+")
# diag(mat.e.result) <- 0
# pheatmap:pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(10), cluster_cols=FALSE,cluster_rows=FALSE, fontsize_number=25)




# ## test 


# pheatmap:pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, display_numbers = pheatmap.disp, fontsize_number=25)















# #################################################################

# heatlabber   <-  	  colnames(mat.e.result)

# # 3.33



# mat.e.result <-  e.pheat.dataprep(3,33)

# relativrisiko.vector.mat.e.result  <-  as.vector(t(mat.e.result))
# relativrisiko.vector.mat.e.result[relativrisiko.vector.mat.e.result<=0] <- NA
# quantile(relativrisiko.vector.mat.e.result, seq(0,1,0.05),na.rm=TRUE)

# heat.krit.df <-  df %>% filter(membership=="3.33") %>%  rename(segkrit=`2: Segment`) %>% select(disco,segkrit)   
# # view(heat.krit.df)
# heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
# heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
# mat.e.result.bak <- mat.e.result
# # mat.e.result <- mat.e.result.bak

# mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]

# # pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)

# annotation <- as.data.frame(heat.krit.df$segkrit)
# rownames(annotation) <- heat.krit.df$disco 
# colnames(annotation) <- c("klynge")

# # pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE, annotation_col = annotation,display_numbers = round(mat.e.result,0))
# mat.e.result.bak <- mat.e.result
# # mat.e.result <- mat.e.result.bak
# mat.e.result[mat.e.result>=20] <- 20
# pheatmap.disp <- round(mat.e.result,0)
# # diag(pheatmap.disp)[] <- c("-")
# pheatmap.disp[pheatmap.disp==20] <- c("20+")

# klynge <- sample(iwanthue ,size=length(levels(annotation[,1])))
# names(klynge) <- levels(annotation[,1])
# anno_farver <- list(klynge = klynge)
# breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
# # colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
# # rownames(mat.e.result) <- strtrim(rownames(mat.e.result),30)

# cairo_pdf(filename = "./statistik/R/moneca/vores/00_emilspeciale_output/heatmaps/seg3_4_RR_10.pdf", height = 15, width = 20)
# pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,display_numbers = pheatmap.disp, fontsize_number=25,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))
# dev.off()



# ############### hele det damn kort #########################
# library(pheatmap)
# cut.off.default <- 1
# wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
# wm1[is.na(wm1)] <- 0
# mat.e.result  <-  wm1
# colnames(mat.e.result) <- paste0(as.character(discodata$disco_4cifret))
# rownames(mat.e.result) <- paste0(as.character(discodata$disco_4cifret))
# colnames(mat.e.result) <- strtrim(colnames(mat.e.result),30)
# rownames(mat.e.result) <- strtrim(rownames(mat.e.result),30)

# # diag(mat.e.result)[] <- 5

# heat.krit.df <-  df %>% rename(segkrit=membership) %>% select(disco,segkrit)   
# heat.krit.df$disco  <-  as.character(heat.krit.df$disco)
# heat.krit.df$segkrit <-  as.factor(heat.krit.df$segkrit)
# mat.e.result <-  mat.e.result[order(heat.krit.df$segkrit), order(heat.krit.df$segkrit)]
# # pheatmap(mat.e.result, cluster_cols=FALSE,cluster_rows=FALSE)
# annotation <- as.data.frame(heat.krit.df$segkrit)
# rownames(annotation) <- heat.krit.df$disco 
# colnames(annotation) <- c("segment")

# mat.e.result[mat.e.result>=5] <- 5
# pheatmap.disp <- round(mat.e.result,0)
# # diag(pheatmap.disp)[] <- c("-")
# # pheatmap.disp[pheatmap.disp==5] <- c("5+")
# klynge <-  sample(colorRampPalette(xmen)(length(levels(annotation[,1]))),size=length(levels(annotation[,1])))
# names(klynge) <- levels(annotation[,1])
# anno_farver <- list(klynge = klynge)
# breaksList = seq(0, ceiling(max(mat.e.result,na.rm=true)), by = 1)
# pheatmap(mat.e.result, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(length(breaksList)), breaks = breaksList, cluster_cols=FALSE,cluster_rows=FALSE, annotation_row = annotation,annotation_col = annotation,annotation_colors = anno_farver, gaps_row=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))),gaps_col=first.changes(sort(as.numeric(as.character(heat.krit.df$segkrit)))))


# 	, filename = "./statistik/R/moneca/vores/00_emilspeciale_output/00_tryout_nogetrod/heleheatmaplortet_5.png", height = 50, width = 50)

# display_numbers = pheatmap.disp, fontsize_number=10,


