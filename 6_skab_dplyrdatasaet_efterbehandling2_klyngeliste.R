klynge.df.tmp <- discodata %>% select(membership,indeks)

klynge.liste.niveau.5 <-  list(
list_5.1	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="5.1") %>% select(indeks))))),
list_5.2	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="5.2") %>% select(indeks))))),
list_4.7	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.7") %>% select(indeks))))),
list_4.8	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.8") %>% select(indeks))))),
list_4.10	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.10") %>% select(indeks))))),
list_4.1	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.1") %>% select(indeks))))),
list_4.9	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.9") %>% select(indeks))))),
list_4.4	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.4") %>% select(indeks))))),
list_4.2	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="4.2") %>% select(indeks))))),
list_3.35	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.35") %>% select(indeks))))),
list_3.8	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.8") %>% select(indeks))))),
list_3.36	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.36") %>% select(indeks))))),
list_3.26	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.26") %>% select(indeks))))),
list_3.34	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.34") %>% select(indeks))))),
list_3.4	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.4") %>% select(indeks))))),
list_3.24	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.24") %>% select(indeks))))),
list_3.7	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.7") %>% select(indeks))))),
list_3.21	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.21") %>% select(indeks))))),
list_3.20	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.20") %>% select(indeks))))),
list_3.14	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.14") %>% select(indeks))))),
list_3.30	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.30") %>% select(indeks))))),
list_3.18	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.18") %>% select(indeks))))),
list_3.33	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.33") %>% select(indeks))))),
list_3.37	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.37") %>% select(indeks))))),
list_3.25	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.25") %>% select(indeks))))),
list_3.12	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.12") %>% select(indeks))))),
list_3.15	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.15") %>% select(indeks))))),
list_3.3	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.3") %>% select(indeks))))),
list_3.39	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.39") %>% select(indeks))))),
list_3.9	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.9") %>% select(indeks))))),
list_3.2	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.2") %>% select(indeks))))),
list_3.38	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="3.38") %>% select(indeks))))),
list_2.66	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.66") %>% select(indeks))))),
list_2.61	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.61") %>% select(indeks))))),
list_2.56	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.56") %>% select(indeks))))),
list_2.79	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.79") %>% select(indeks))))),
list_2.64	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.64") %>% select(indeks))))),
list_2.24	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.24") %>% select(indeks))))),
list_2.58	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.58") %>% select(indeks))))),
list_2.77	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.77") %>% select(indeks))))),
list_2.40	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="2.40") %>% select(indeks))))),
list_1.45	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.45") %>% select(indeks))))),
list_1.162	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.162") %>% select(indeks))))),
list_1.75	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.75") %>% select(indeks))))),
list_1.99	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.99") %>% select(indeks))))),
list_1.204	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.204") %>% select(indeks))))),
list_1.27	<- 	(sort(as.vector(unlist(klynge.df.tmp %>% filter(membership=="1.27") %>% select(indeks)))))
)

liste.segmenter <- c("5.1",
"5.2",
"4.7",
"4.8",
"4.10",
"4.1",
"4.9",
"4.4",
"4.2",
"3.35",
"3.8",
"3.36",
"3.26",
"3.34",
"3.4",
"3.24",
"3.7",
"3.21",
"3.20",
"3.14",
"3.30",
"3.18",
"3.33",
"3.37",
"3.25",
"3.12",
"3.15",
"3.3",
"3.39",
"3.9",
"3.2",
"3.38",
"2.66",
"2.61",
"2.56",
"2.79",
"2.64",
"2.24",
"2.58",
"2.77",
"2.40",
"1.45",
"1.162",
"1.75",
"1.99",
"1.204",
"1.27")

names(klynge.liste.niveau.5) <- liste.segmenter


#
mat.e <- mob.mat[-274,-274]
s.membership <- as.character(seg.df$membership)
seg.within.mob.REAL  <- list()
for (tmem in s.membership) {
tmp.membership <- tmem
work.list <-  sort(as.vector(unlist(discodata %>%    filter(membership == tmp.membership )  %>% select(indeks))))
tmp  <-   mat.e[work.list,work.list]
tmp <- sum(tmp)
seg.within.mob.REAL[tmp.membership] <- tmp
}

seg.within.mob.REAL.1 <-  matrix(seg.within.mob.REAL)
seg.within.mob.REAL.1 <-  cbind(names(seg.within.mob.REAL),seg.within.mob.REAL.1)


################################################################
######## aggreger til niveau 5 ########
################################################################

mat.e <- mob.mat[-274,-274]

klynge.liste.samlet.indeks <- list()
for (name in sort(liste.segmenter)) {
   # print(myList[[name]]) 
klynge.liste.samlet.indeks[name]  <-  klynge.liste.niveau.5[name]
}
# klynge.liste.samlet.indeks

e.seg.niveau.5 <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(seq_len(273)), unname(klynge.liste.samlet.indeks)))
# map elements of second list item to values of first list item - NB bruges anderledes end i moneca. men måske brugbar.
# e.seg.niveau.5[match(group.list[["grp2"]], e.seg.niveau.5)] <- group.list[["grp1"]] 

e.mobmat.seg.niveau.5 <- t(rowsum(t(rowsum(mat.e, e.seg.niveau.5)), e.seg.niveau.5))
# ncol(e.mobmat.seg.niveau.5)

diag.e.mobmat.seg.niveau.5 <-  diag(e.mobmat.seg.niveau.5) 

korrekt <- match(seg.within.mob.REAL.1[,2],diag.e.mobmat.seg.niveau.5)
colnames(e.mobmat.seg.niveau.5)[korrekt] <-seg.within.mob.REAL.1[,1]
rownames(e.mobmat.seg.niveau.5)[korrekt] <-seg.within.mob.REAL.1[,1]


korrekt <-  match(colnames(e.mobmat.seg.niveau.5),seg.df$membership)
e.mobmat.seg.niveau.5 <-  e.mobmat.seg.niveau.5[order(korrekt) , order(korrekt) ]

dimnames(e.mobmat.seg.niveau.5) <- list(seg.df$membership, seg.df$membership )





# ENDELIG

within.mob.seg<- diag(e.mobmat.seg.niveau.5)/rowSums(e.mobmat.seg.niveau.5)



within.mob.seg<- diag(e.mobmat.seg.niveau.5)/rowSums(e.mobmat.seg.niveau.5)


seg.df$within.mob.seg.E <- round((within.mob.seg),4)

seg.df <- seg.df %>%  select(membership,within.mob.seg,within.mob.seg.E, everything())



# sd(seg.df$within.mob.seg.E)*100 - sd(seg.df$within.mob.seg)*100



# ### med total, til relativ risiko

# mat.e <- mob.mat

# klynge.liste.samlet.indeks <- list()
# for (name in sort(liste.segmenter)) {
#    # print(myList[[name]]) 
# klynge.liste.samlet.indeks[name]  <-  klynge.liste.niveau.5[name]
# }
# # klynge.liste.samlet.indeks

# e.seg.niveau.5 <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(seq_len(273)), unname(klynge.liste.samlet.indeks)))
# # map elements of second list item to values of first list item - NB bruges anderledes end i moneca. men måske brugbar.
# # e.seg.niveau.5[match(group.list[["grp2"]], e.seg.niveau.5)] <- group.list[["grp1"]] 

# e.mobmat.seg.niveau.5.m.total <- t(rowsum(t(rowsum(mat.e, e.seg.niveau.5)), e.seg.niveau.5))
# # ncol(e.mobmat.seg.niveau.5.m.total)

# diag.e.mobmat.seg.niveau.5.m.total <-  diag(e.mobmat.seg.niveau.5.m.total) 

# korrekt <- match(seg.within.mob.REAL.1[,2],diag.e.mobmat.seg.niveau.5.m.total)
# colnames(e.mobmat.seg.niveau.5.m.total)[korrekt] <-seg.within.mob.REAL.1[,1]
# rownames(e.mobmat.seg.niveau.5.m.total)[korrekt] <-seg.within.mob.REAL.1[,1]


# korrekt <-  match(colnames(e.mobmat.seg.niveau.5.m.total),seg.df$membership)
# e.mobmat.seg.niveau.5.m.total <-  e.mobmat.seg.niveau.5.m.total[order(korrekt) , order(korrekt) ]














