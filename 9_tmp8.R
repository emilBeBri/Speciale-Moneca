



wm1            <- weight.matrix(mob.mat2, cut.off = cut.off.default, symmetric = FALSE, small.cell.reduction = small.cell.default, diagonal=TRUE)
wm1[is.na(wm1)] <- 0
# view(wm1)
# wm1  <-  round(wm1, digits=3)
circlelist <-  seg$segment.list[[3]][[14]]
segmentcircle_nonsym <- wm1[circlelist,circlelist]

view(round(segmentcircle_nonsym),0)

is.matrix(segmentcircle)

view(segmentcircle)

matric
help(matrix)


is.data.frame(segmentcircle)


## Example data 
     
     jobdat <- matrix(c(
               295,  20,   0,    0,    0,    5,    7,
               45,   3309, 15,   0,    0,    0,    3,
               23,   221,  2029, 5,    0,    0,    0,
               0,    0,    10,   100,  8,    0,    3,
               0,    0,    0,    0,    109,  4,    4,
               0,    0,    0,    0,    4,    375,  38,
               0,    18,   0,    0,    4,    26,   260), 
               nrow = 7, ncol = 7, byrow = TRUE,
               dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7"),
                    c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")))

work.list <-  c(1,5,7) 

work.list <- sort(unique(unlist(lapply(work.list, function(x) which(jobdat[x,] != 0)))))


jobdat[work.list,work.list]


work.list <- c(1,5,7)
jobpick_wrong <- jobdat[work.list,work.list]



jobpick_right <- matrix(c(
          295,  20,   0,    5,    7,
          45,   3309, 0,    0,    3,
          0,    0,    109,  4,    4,
          0,    0,    4,    375,  38,
          0,    18,   4,    26,   260),
          nrow = 5, ncol = 5, byrow = TRUE,
          dimnames = list(c("job 1","job 2","job 5","job 6","job 7"),
                    c("job 1","job 2","job 5","job 6","job 7")))









view(jobdat)
 
hvis jeg gerne vil udvælge job 1, 5 og 7:

jobpick_wrong <- jobdat[c(1,5,7),c(1,5,7)]

view(jobpick)

There are two problems with this. The first is the most serious: I want to get all the 

the problem is, this only selects the cells that directly influences each other, whereas what I need is *all* the cells from the three job categories that coincides with each other, since that is the actual jobmobility. 

     Since I dont know how to do it, here is a matrix constructed with the required result:


     jobdat  <- matrix(c(
     				295	,20,	0,	0,	0,	5,	7,
     				45	,3309,	15,	0,	0,	0,	3,
     				23	,221,	2029,	5,	0,	0,	0,
     				3	,0,	10,	100,	8,	0,	3,
     				2	,0,	0,	0,	109,	4,	4,
     				10	,0,	0,	0,	4,	375,	38,
     				11	,18,	0,	0,	4,	26,	260
     	), nrow = 7, ncol = 7, byrow = TRUE,
                    dimnames = list(c("job 1","job 2","job 3","job 4","job 5","job 6","job 7"),
                                    c("job 1","job 2","job 3","job 4","job 5","job 6","job 7")))
view(jobdat)






ego(graph, order, nodes = V(graph), mode = c("all", "out", "in"), mindist = 0)


view(jobpick_right)


?neighborhood

wm <-  jobdat

library(igraph)

mode <- c("all")

net           <- graph.adjacency(jobdat, mode = "directed", weighted = TRUE)

neigh         <- ego(net, 1, mode = mode)


view(neigh)



neighborhood

