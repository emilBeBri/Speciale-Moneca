cut.off.default <- 1
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0



level.list <-  list(
list(1,2,3,4,5,6,7,8,9,10,11,12,13,14), # base level 
list(c(1,2,3),c(4,5),c(6,7),c(13,14)),     # level 2 groups 
list(c(1,2,3,6,7),c(4,5,9)),      # level 3 groups    
list(c(4,5,9,12))    # level 4 groups 
)



jobnames <-  c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")
jobdat <- matrix(c(
99, 5, 5, 0, 0, 5, 0,
5, 99, 2, 5, 5, 0, 0,
1, 5, 99, 5, 0, 0, 1,
1, 0, 5, 99, 8, 0, 1,
0, 5, 0, 0, 99, 5, 1,
0, 0, 5, 5, 0, 99, 5,
5, 0, 0, 0, 5, 1, 99
           ), 
           nrow = 7, ncol = 7, byrow = TRUE,
           dimnames = list(jobnames,jobnames
                ))
 jobdat
work.list <- c(1,2)

## aggregrer grupper i matrix (test data)

# initial grouping
groups <- seq_len(ncol(jobdat))
group.list <- list(grp1=c(5, 2), grp2=c(3, 6))
group.jobnames <-  c("job 1 and 2","job 3 and 4","job 5","job 6","job 7")

groups <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(groups), unname(group.list)))
# map elements of second list item to values of first list item - NB bruges anderledes end i moneca. men måske brugbar.
# groups[match(group.list[["grp2"]], groups)] <- group.list[["grp1"]] 

myMat <- t(rowsum(t(rowsum(jobdat, groups)), groups))
# add the group names
dimnames(myMat) <- list(group.jobnames,group.jobnames)


### aggreger grupper i submatrice - udvælg submatrice 


# jobnames <-  c("job 1","job 2","job 3","job 4","job 5","job 	6","job 7")
	←# jobdat <- matrix(c(
	# 101, 5, 5, 0, 0, 5, 0,
	# 5, 102, 2, 5, 5, 0, 0,
	# 1, 5, 103, 5, 0, 0, 1,
	# 1, 0, 5, 104, 8, 0, 1,
	# 0, 5, 0, 0, 105, 5, 1,
	# 0, 0, 5, 5, 0, 106, 5,
	# 5, 0, 0, 0, 5, 1, 107
	#            ), 
	#            nrow = 7, ncol = 7, byrow = TRUE,
	#            dimnames = list(jobnames,jobnames
	#                 ))
	#  jobdat
	# work.list <- c(1,2)
	
	# mat.e <- jobdat
	# mat.e.tmp <- jobdat

#test

# view(mat.e)
mat.e <- mob.mat[-274,-274]
mat.e[248,94] <- c(999999)  
# mat.e[249,94] <- c(9999999)  
mat.e.tmp <- mat.e
# view(mat.e)

rm(list=ls())
source("./statistik/R/moneca/vores/vorescripts/0_funktioner_og_pakker.R")
load("./statistik/R/moneca/vores/voresdata/WORKINGPROGRESS_allebeskaeft250_4.Rdata")
work.list <- NULL 
aug.work.list <- NULL 
#
mat.e.result <- mob.mat[-274,-274]
mat.e.result <- mat.e
# etest(mat.e)

# klynger <- manuelle.klynger.alle
klynger <- manuelle.klynger.hoj 
klynger <- manuelle.klynger.lav
klynger <- klynger[grep("^1.*", klynger, invert = TRUE)]





klynge.liste.samlet.indeks <- list()
for (name in sort(klynger)) {
klynge.liste.samlet.indeks[name]  <-  klynge.liste.niveau.5[name]
}
# klynge.liste.samlet.indeks
work.list.foer.sub <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks)))) #det samme, det ene som liste og det andet som vector 
sort(as.vector(unlist(klynge.liste.samlet.indeks)))==work.list.foer.sub # yes de skal være ens det er de 

# find  de disco der går 
# 1. fra segmenter af interesse
aug.work.list.foer.sub <- sort(unique(unlist(lapply(work.list.foer.sub, function(x) which(mat.e[x,] != 0)))))
# 2. til segmenter af interesse 
# aug.work.list.foer.sub <- sort(unique(unlist(lapply(work.list.foer.sub, function(x) which(mat.e[,x] != 0)))))


# liste til aggregering af disco 
klynge.liste.e.seg.manuel.til.fra <- Reduce(function(x, y) replace(x, x[x %in% y], min(y)), c(list(seq_len(273)), unname(klynge.liste.samlet.indeks)))




# det store aggregeringsnummer 
mat.e.result <- t(rowsum(t(rowsum(mat.e.result, klynge.liste.e.seg.manuel.til.fra)), klynge.liste.e.seg.manuel.til.fra))


ncol(mat.e.result) 
# view(mat.e.result)# her lagt sammen med den anden 
etest(mat.e.result)
# view(mat.e.result)
sum(mat.e.result)


# de her to skal være ens, ellers er noget galt: 
length(unique(which(diag(mat.e.result) == 0)))==length(which(diag(mat.e.result) == 0))
# view(mat.e.result)
# max(as.vector(mat.e.result))
# min(colSums(mat.e.result))
# etest(mat.e.result)
#view(mat.e.result)
sum(mat.e.result)



save.image("./statistik/R/moneca/vores/voresdata/tmp1_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp1_allebeskaeft250.Rdata")


# diagonaler til at identificere elementer 
diag.discodata <- diag(mob.mat[-274,-274])
diag.submatrix <- diag(mat.e.result)
diag.seg.df <- diag(e.mobmat.seg.niveau.5)
diag.seg.df.udisco <-diag.seg.df[1:41]
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) # skal være sand for alle elementer 


# find  de disco der går fra nye seg-index 
work.list.efter.sub <- seg.index

# 1. fra segmenter af interesse
aug.work.list.efter.sub <- sort(unique(unlist(lapply(work.list.efter.sub, function(x) which(mat.e.result[x,] != 0)))))

# test
tmp1  <-  setdiff(aug.work.list.foer.sub,work.list.foer.sub) 
test.list.1 <-  as.character(discodata$disco[tmp1])
tmp1  <-  setdiff(aug.work.list.efter.sub,work.list.efter.sub) 
test.list.2 <-  as.character(discodata$disco[tmp1])
length(test.list.1)==length(test.list.2) # de her to skal være ens 


mat.e.result <- mat.e.result[aug.work.list.efter.sub, aug.work.list.efter.sub]

# diagonaler til at identificere elementer 

diag.submatrix <- diag(mat.e.result)
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) # skal være sand for alle elementer 


# find ud af hvilket indeks der passer med diverse segmenter etc 
ikke.seg.index.i.disco <-  as.numeric(names(diag.submatrix[which((diag.submatrix%in%diag.discodata))]))
ikke.seg.index.i.submatrix <-  which((diag.submatrix%in%diag.discodata))


colnames(mat.e.result)[ikke.seg.index.i.submatrix] <- as.character(discodata$disco[ikke.seg.index.i.disco])
rownames(mat.e.result)[ikke.seg.index.i.submatrix] <- as.character(discodata$disco[ikke.seg.index.i.disco])
colnames(mat.e.result)[ 
	which(diag.submatrix %in% diag.seg.df.udisco)] <-
				 names(diag.seg.df.udisco)[
                   which(diag.seg.df.udisco %in% diag.submatrix.seg)]
rownames(mat.e.result)[ 
	which(diag.submatrix %in% diag.seg.df.udisco)] <-
				 names(diag.seg.df.udisco)[
                   which(diag.seg.df.udisco %in% diag.submatrix.seg)]
ncol(mat.e.result)				
# view(mat.e.result)
# max(as.vector(mat.e.result))

tmp.order  <-  append(colnames(mat.e.result) [seg.index] , colnames(mat.e.result)[ikke.seg.index.i.submatrix ] )
korrekt <-  match(tmp.order,colnames(mat.e.result))
mat.e.result <-  mat.e.result[korrekt, korrekt]
# head(colnames(mat.e.result)) # yes 


# ncol(mat.e.result)
# view(mat.e.result) #og her, det er desuden korrekte dimnames
# max(as.vector(mat.e.result))
# min(colSums(mat.e.result))
# etest(mat.e.result)

save.image("./statistik/R/moneca/vores/voresdata/tmp2_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp2_allebeskaeft250.Rdata")


# diagonaler til at identificere elementer 

diag.submatrix <- diag(mat.e.result)
seg.index <- which(!(diag.submatrix%in%diag.discodata))
diag.submatrix.seg  <-  diag.submatrix[seg.index]
ikke.seg.index.i.submatrix <-  which((diag.submatrix%in%diag.discodata))
ikke.seg.index.i.disco <-  which((diag.discodata%in%diag.submatrix))

 which(names(diag.seg.df.udisco)[which(diag.seg.df.udisco %in% 	diag.submatrix.seg)] %in% klynger)==seq_len(length(klynger)) 


mat.e.result[ikke.seg.index.i.submatrix,ikke.seg.index.i.submatrix] <- 0 #fjerner interne ties i segmenter af interesse (hvis vi bare vil se hvor de går hen)
etest(mat.e.result) # yep 

diag.mat.e.result <- diag(mat.e.result)  # for later use 
mat.e.result[ikke.seg.index.i.submatrix,seg.index] <- 0


save.image("./statistik/R/moneca/vores/voresdata/tmp3_allebeskaeft250.Rdata")
rm(list=ls())
load("./statistik/R/moneca/vores/voresdata/tmp3_allebeskaeft250.Rdata")




################################################################
######## aggreger grupper i matrix - behold hele matrix ########
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
ncol(e.mobmat.seg.niveau.5)
view(e.mobmat.seg.niveau.5)




# view(e.mobmat.seg.niveau.5)


# view(e.mobmat.seg.niveau.5) #YES

within.mob.seg<- diag(e.mobmat.seg.niveau.5)/rowSums(e.mobmat.seg.niveau.5)

seg.df <- arrange(seg.df,desc(membership))
seg.df$within.mob.seg.E <- round((within.mob.seg),4)
seg.df <- arrange(seg.df,(indeks.seg)) %>% select(membership,within.mob.seg,within.mob.seg.E, everything())


# sd(seg.df$within.mob.seg.E)*100 - sd(seg.df$within.mob.seg)*100
view(seg.df)


### version 1 - når hele matricen skal beholdes  ##############


mat.e <-  mob.mat[-274,-274]


# vælg segmenter af interesse 
manuelle.klynger.alle <- c(
	"3.25", "3.9", "5.1",  #lav manuel
	"1.204", "2.40", "3.14", "3.15", "4.8", "5.2",  #høj manuel 
	"4.4" #blandet manuelt 
	) # "2.79", "3.24", "3.30", "4.2", "4.10"  #blandet 
						  )
klynger <- manuelle.klynger.alle
mat.e <-  mob.mat[-274,-274]
mat.e <- wm1
# mat.e <- jobdat 
# work.list <- c(1,2)
work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks))))
irr.job.indices <- which(!(seq_len(nrow(mat.e)) %in% work.list))
#fjerner diagonal og  ties mellem irrelevante  
mat.e[irr.job.indices,irr.job.indices] <- 0
#fjerner interne ties i segmenter af interesse (hvis vi bare vil se hvor de går hen)
diag(mat.e) <- 0
diag.mat.e <- diag(mat.e)  # for later use 
mat.e[work.list,work.list] <- 0
#1. kun *fra* segmenter af interesse *til* andre segmenter 
mat.e[irr.job.indices,work.list] <- 0
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[x,] != 0)))))
#2. kun *til* segmenter af interesse *fra* andre segmenter (modstridende ihft ovenstående)
# mat.e[work.list,irr.job.indices] <- 0
# aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))
mob.mat.u.diag <- mob.mat[-274,-274] 
diag(mob.mat.u.diag) <- 0
#udregniner
mob.til.l <-  setNames(as.list(lapply(aug.work.list,function(x) sum(mat.e[,x]))), discodata$disco[aug.work.list])
mob.til.l.andel <- setNames(as.list(round2(unlist(mob.til.l)/
	sum(mat.e),6)
	), discodata$disco[aug.work.list])
mob.til.l.andel.tot.m.diag <- setNames(as.list(unlist(mob.til.l)/
	sum(mob.mat.u.diag)
	), discodata$disco[aug.work.list])
without.mobility.df <- tbl_df(data.frame(
	discodata$disco[aug.work.list],
	discodata$membership[aug.work.list],
	unlist(mob.til.l.andel),
	unlist(mob.til.l),
	unlist(mob.til.l.andel.tot.m.diag),
	discodata$klasse_oesch8[aug.work.list],
	discodata$klasse_oesch16[aug.work.list]
	))
colnames(without.mobility.df) <- c("disco","membership","without.mob.andel","without.mob","without.mob.andel.tot","klasse_oesch8","klasse_oesch16")
# view(without.mobility.df)



#aggregreret til segment.niveau


without.mobility.df.seg <-  e.mobility.seg(without.mobility.df)



################# Antal klynger, hvor kommer de fra? #########
wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
wm1[is.na(wm1)] <- 0
mat.e <- wm1
mat.e <- mob.mat[-274,-274]

manuelle.klynger.alle <- c(
	"3.25", "3.9", "5.1",  #lav manuel
	"1.204", "2.40", "3.14", "3.15", "4.8", "5.2",  #høj manuel 
	"4.4" #blandet manuelt 
	) # "2.79", "3.24", "3.30", "4.2", "4.10"  #blandet 
						  )
klynger <- manuelle.klynger.alle
mat.e <-  mob.mat[-274,-274]
mat.e <- wm1
# mat.e <- jobdat 
# work.list <- c(1,2)
work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks))))


# 1. fra segmenter af interesse
aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[x,] != 0)))))
# 2. til segmenter af interesse 
# aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))

# find dem! 
gylden.liste <-  setdiff(aug.work.list,work.list)
# df <- rename(df,klasse_oesch8=klasse_oesch_8) #hotfix
tmp.df <- df %>% filter(indeks %in% gylden.liste) %>% 	select(disco,membership,klasse_oesch16,klasse_oesch8,indeks)

#se dem!
count(tmp.df, membership) %>% arrange(desc(n))
count(tmp.df, klasse_oesch16,membership) %>% arrange(desc(n))



# #############  når en submatrice skal udvælges ###### gammelttror jeg - d. 26/02/2017

# # wm1            <- weight.matrix(mob.mat, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE) 
# # wm1[is.na(wm1)] <- 0
# mat.e <- wm1
# mat.e <-  mob.mat[-274,-274]
# # vælg segmenter af interesse 
# klynger = c("3.24")
# # forberedelse 
# work.list <-  sort(as.vector(unlist(df %>% filter(membership %in% klynger) %>%     select(indeks))))

# # find evt. de segmenter der går 
# # 1. fra segmenter af interesse
# aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[x,] != 0)))))
# # 2. til segmenter af interesse 
# aug.work.list <- sort(unique(unlist(lapply(work.list, function(x) which(mat.e[,x] != 0)))))

# mat.e <- mat.e[aug.work.list, aug.work.list]
# ################ avanceret2: segment + ties (uden edges ml ties) 
# irr.job.indices <- which(!(aug.work.list %in% work.list))
# ## first, keep diagonal values for irr.job.indices
# dvals <- diag(mat.e)[irr.job.indices]
# ## set sub-matrix to zero (this will also set diagnal elements to zero)
# mat.e[irr.job.indices,irr.job.indices] <- 0
# ## replace diagonal elements
# diag(mat.e)[irr.job.indices] <- dvals
# #view(mat.e)
# #forhindrer diagonalen i at fucke det hele op 
# diag(mat.e)[] <- 0
# diag(mat.e)[] <- round_any(max(mat.e), 5, ceiling)


















# diag(mat.e) <- 0
# sum(mat.e)/sum(mat.e[work.list,work.list])
 





# sum(diag(mat.e[work.list,work.list]))

# )

# fra.seg.til <- list()


# tmp.df <-  df %>% filter(indeks %in% aug.work.list)  %>% select(disco,membership, klasse_oesch16)



# fra.seg.til[2] <- list(unique(factor(tmp.df$membership)))
# fra.seg.til[1] <- list(length(levels(factor(tmp.df$membership))))


# fra.seg.til[2] <- list(sort(unique(factor(tmp.df$klasse_oesch16))))
# fra.seg.til[1] <- list(length(levels(factor(tmp.df$klasse_oesch16))))
# fra.seg.til






