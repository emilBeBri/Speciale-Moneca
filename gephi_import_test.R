#### gephi import test 

#procesting2
##### emil og soeren med 156 i stedet for 160
rr.mob.mat      <- read.csv("./statistik/R/moneca/vores/voresdata/selvudregnetrelativrisiko.csv", row.names = 1, header = TRUE, sep = ';', fileEncoding  ="UTF-8", check.names = FALSE)
rr.mob.mat    <-  as.matrix(rr.mob.mat) # transformerer til matrice
stest.l            <- ncol(stest.mob.mat) # displays no. of columns #gir os antal kolonner og l?gger den som en vektor med et tal
stest.label        <- strtrim(rownames(stest.mob.mat), 40) #fjerner alle characters udover 40
stest.label.kode   <- read.csv("./statistik/R/moneca/vores/voresdata/Oversat Moneca kategorier_es_test.csv", sep = ";", fileEncoding = "UTF-8") #laver liste med moneca labels p? dansk
stest.label        <- paste(stest.label.kode$DISCO, stest.label.kode$Dansk.Moneca.label, sep = ": ") # tager disco-koder og danske moneca labels og applic
dimnames(stest.mob.mat) <- list(stest.label, stest.label) # s?tter labels b?de p? rows og columns p? mob.mat matricen




geee <- graph.adjacency(rr.mob.mat,mode="undirected",weighted=TRUE) # this will create an 'igraph object'

geee <- graph.adjacency(stest.mob.mat,mode="directed",weighted=TRUE) # this will create an 'igraph object'

help(graph.adjacency)

geee

saveAsGEXF(geee, "output.gexf") 



cg1 <- erdos.renyi.game(5, 0.4)

saveAsGEXF(cg1, "./statistik/R/moneca/vores/output.gexf")  








#procesting2
##### emil og soeren med 156 i stedet for 160
stest.mob.mat      <- read.csv("./statistik/R/moneca/vores/voresdata/disco_m_ledig_og_alle_test.csv", row.names = 1, header = TRUE, sep = ';', fileEncoding  ="UTF-8", check.names = FALSE)
stest.mob.mat    <-  as.matrix(stest.mob.mat) # transformerer til matrice
stest.l            <- ncol(stest.mob.mat) # displays no. of columns #gir os antal kolonner og l?gger den som en vektor med et tal
stest.label        <- strtrim(rownames(stest.mob.mat), 40) #fjerner alle characters udover 40
stest.label.kode   <- read.csv("./statistik/R/moneca/vores/voresdata/Oversat Moneca kategorier_es_test.csv", sep = ";", fileEncoding = "UTF-8") #laver liste med moneca labels p? dansk
stest.label        <- paste(stest.label.kode$DISCO, stest.label.kode$Dansk.Moneca.label, sep = ": ") # tager disco-koder og danske moneca labels og applic
dimnames(stest.mob.mat) <- list(stest.label, stest.label) # s?tter labels b?de p? rows og columns p? mob.mat matricen



geee <- graph.adjacency(stest.mob.mat,mode="directed",weighted=TRUE) # this will create an 'igraph object'
geee <- graph.adjacency(stest.mob.mat,mode="directed",weighted=FALSE) # this will create an 'igraph object'

help(graph.adjacency)

geee

saveAsGEXF(geee, "output.gexf") 



cg1 <- erdos.renyi.game(5, 0.4)

saveAsGEXF(cg1, "./statistik/R/moneca/vores/output.gexf")  






# Converts the given igraph object to GEXF format and saves it at the given filepath location
#     g: input igraph object to be converted to gexf format
#     filepath: file location where the output gexf file should be saved
#
saveAsGEXF = function(g, filepath="converted_graph.gexf")
{
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Gopalakrishna Palem", description="igraph -> gexf converted file", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}


