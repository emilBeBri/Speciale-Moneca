


plottrappen.disco <- list()
plottrappen.disco$et <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("1. niveau")
plottrappen.disco$to <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:2),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("2. niveau")
plottrappen.disco$tre <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:3),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("3. niveau")
plottrappen.disco$fire <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:4),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("4. niveau")
plottrappen.disco$fem <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:5),
                   edge.size=edge.size, vertex.fill = discodata$disco_1cifret,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.disco.maparray  + ggtitle("5. niveau")


plottrappen.egp11 <- list()
plottrappen.egp11$et <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1),
                   edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.egp11.maparray  + ggtitle("1. niveau")
plottrappen.egp11$to <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:2),
                   edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.egp11.maparray  + ggtitle("2. niveau")
plottrappen.egp11$tre <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:3),
                   edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.egp11.maparray  + ggtitle("3. niveau")
plottrappen.egp11$fire <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:4),
                   edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.egp11.maparray  + ggtitle("4. niveau")
plottrappen.egp11$fem <- gg.jonas(seg, layout = lay, edges=edges.default.all, midpoint.arrow = arrow.default, niveau=seq(1:5),
                   edge.size=edge.size, vertex.fill = discodata$klasse_egp11,
                   vertex.size = vertex_stoerrelse,show.text = FALSE) +  
          default.maparray +
            default.egp11.maparray  + ggtitle("5. niveau")


