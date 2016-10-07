################### Temakort  ######################

#############################################################################
#################### ORGANISEREDE

# Organisationsgrad for hvert aar i hele perioden
p.org.periode  <- list() #laver tom liste
for(i in 1:length(periode)){ #benytter sig af periode-objektet indeholdende 1995:2012. 
p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.smooth[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
p.p   <- p.p + defaults + ggtitle(paste("Organisationsgrad: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.smooth, na.rm = TRUE), max(organiserede.andel.smooth, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
p.org.periode[[i]] <- p.p
}
#laver simpelthen en liste med ggplots. like so:
#p.org.periode[2]
#der farvelæges efter følgende, lavet i 1_data
#write.table(organiserede.andel.smooth, file="./statistik/R/moneca/vores/output_emil/organiserede.andel.smooth.csv", sep = ";", fileEncoding = "UTF-8")
#



# Organisationsgrad for hvert aar i hele perioden - kun paa segment niveau
p.org.periode.seg  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.seg[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Organisationsgrad: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.seg, na.rm = TRUE), max(organiserede.andel.seg, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.org.periode.seg[[i]] <- p.p
}

# Organisationsgrad for hvert aar i hele perioden - indeks - bemærk den bruger organiserede.andel.smooth.i, der skabes i 1_data og 
p.org.periode.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.smooth.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i organisationsgrad siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.smooth.i, na.rm = TRUE), max(organiserede.andel.smooth.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.org.periode.i[[i]] <- p.p
}

# Organisationsgrad for hvert aar i hele perioden - kun paa segment niveau - indeks
p.org.periode.seg.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.seg.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i organisationsgrad siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.seg.i, na.rm = TRUE), max(organiserede.andel.seg.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.org.periode.seg.i[[i]] <- p.p
}

#
# !! LABELS
#

# Organisationsgrad for hvert aar i hele perioden !!! Labels
p.org.periode.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.smooth[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Organisationsgrad: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.smooth, na.rm = TRUE), max(organiserede.andel.smooth, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.org.periode.l[[i]] <- p.p
}

# Organisationsgrad for hvert aar i hele perioden - kun paa segment niveau !!! Labels
p.org.periode.seg.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.seg[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Organisationsgrad: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.seg, na.rm = TRUE), max(organiserede.andel.seg, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.org.periode.seg.l[[i]] <- p.p
}

# Organisationsgrad for hvert aar i hele perioden - indeks !!! Labels
p.org.periode.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.smooth.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i organisationsgrad siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.smooth.i, na.rm = TRUE), max(organiserede.andel.smooth.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.org.periode.i.l[[i]] <- p.p
}

# Organisationsgrad for hvert aar i hele perioden - kun paa segment niveau - indeks !!! Labels
p.org.periode.seg.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = organiserede.andel.seg.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i organisationsgrad siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.seg.i, na.rm = TRUE), max(organiserede.andel.seg.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.org.periode.seg.i.l[[i]] <- p.p
}

#############################################################################
#################### Roede

# Roede for hvert aar i hele perioden
p.roede.periode  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.smooth[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Roede: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.smooth, na.rm = TRUE), max(roede.andel.smooth, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.roede.periode[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - kun paa segment niveau
p.roede.periode.seg  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.seg[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Roede: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.seg, na.rm = TRUE), max(roede.andel.seg, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.roede.periode.seg[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - indeks
p.roede.periode.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.smooth.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i roede siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.smooth.i, na.rm = TRUE), max(roede.andel.smooth.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.roede.periode.i[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - kun paa segment niveau - indeks
p.roede.periode.seg.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.seg.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i roede siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.seg.i, na.rm = TRUE), max(roede.andel.seg.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.roede.periode.seg.i[[i]] <- p.p
}

#
# !! LABELS
#

# Roede for hvert aar i hele perioden !!! Labels
p.roede.periode.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.smooth[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Roede: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.smooth, na.rm = TRUE), max(roede.andel.smooth, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.roede.periode.l[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - kun paa segment niveau !!! Labels
p.roede.periode.seg.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.seg[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Roede: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.seg, na.rm = TRUE), max(roede.andel.seg, na.rm = TRUE)), labels = percent, name = "Andel", na.value = vertex.na)
  p.roede.periode.seg.l[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - indeks !!! Labels
p.roede.periode.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.smooth.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i roede siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.smooth.i, na.rm = TRUE), max(roede.andel.smooth.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.roede.periode.i.l[[i]] <- p.p
}

# Roede for hvert aar i hele perioden - kun paa segment niveau - indeks !!! Labels
p.roede.periode.seg.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = roede.andel.seg.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i roede siden ", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed, limits = c(min(roede.andel.seg.i, na.rm = TRUE), max(organiserede.andel.seg.i, na.rm = TRUE)), labels = percent, name = "Procent", na.value = vertex.na)
  p.roede.periode.seg.i.l[[i]] <- p.p
}




#####################################################################
#################### GULE

# Andel gule for hvert aar i hele perioden med
p.gule.periode  <- list()
for(i in 1:length(periode)){
  # vertex.size = gule[-l, i]
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.smooth[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Gule: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth[-l,], na.rm = TRUE), max(gule.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden - kun paa segment niveau
p.gule.periode.seg  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.seg[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Gule: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.seg[-l,], na.rm = TRUE), max(gule.andel.seg[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.seg[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden med indeks
p.gule.periode.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.smooth.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Stigning af gule siden", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth.i[-l,], na.rm = TRUE), max(gule.andel.smooth.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.i[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden - kun paa segment niveau - med indeks
p.gule.periode.seg.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.seg.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Stigning af gule siden", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.seg.i[-l,], na.rm = TRUE), max(gule.andel.seg.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.seg.i[[i]] <- p.p
}


####################
# Andel gule for hvert aar i hele perioden - med labels og stoerrelse
p.gule.periode.label  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.smooth[-l, i], vertex.size = 7, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Gule: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth[-l,], na.rm = TRUE), max(gule.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_size_continuous(range = c(4, 25), limits = c(min(gule[-l,]), max(gule[-l,])), name = "Antal")
  p.gule.periode.label[[i]] <- p.p
}

#
# !! LABELS
#

# Andel gule for hvert aar i hele perioden med !!! LABELS
p.gule.periode.l  <- list()
for(i in 1:length(periode)){
  # vertex.size = gule[-l, i]
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.smooth[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Gule: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth[-l,], na.rm = TRUE), max(gule.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.l[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden - kun paa segment niveau !!! LABELS
p.gule.periode.seg.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.seg[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Gule: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.seg.i[-l,], na.rm = TRUE), max(gule.andel.seg.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.seg.l[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden med indeks !!! LABELS
p.gule.periode.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.smooth.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Stigning af gule siden", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth[-l,], na.rm = TRUE), max(gule.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.i.l[[i]] <- p.p
}

# Andel gule for hvert aar i hele perioden - kun paa segment niveau - med indeks !!! LABELS
p.gule.periode.seg.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = gule.andel.seg.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32)
  p.p   <- p.p + defaults + ggtitle(paste("Stigning af gule siden", periode[1], " : ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.seg.i[-l,], na.rm = TRUE), max(gule.andel.seg.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.gule.periode.seg.i.l[[i]] <- p.p
}

#####################################################################
#################### LO

####################
# Andel LO for hvert aar i hele perioden
p.LO.periode  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.smooth[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("LO: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Andel", limits = c(min(LO.andel.smooth, na.rm = TRUE), max(LO.andel.smooth, na.rm = TRUE)))
  p.LO.periode[[i]] <- p.p
}

# Andel LO for hvert aar i hele perioden - kun paa segment niveau
p.LO.periode.seg  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.seg[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("LO: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Andel", limits = c(min(LO.andel.seg, na.rm = TRUE), max(LO.andel.seg, na.rm = TRUE))) 
  p.LO.periode.seg[[i]] <- p.p
}

####################
# Andel LO for hvert aar i hele perioden - indeks
p.LO.periode.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.smooth.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "")) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Procent", limits = c(min(LO.andel.smooth.i, na.rm = TRUE), max(LO.andel.smooth.i, na.rm = TRUE)))
  p.LO.periode.i[[i]] <- p.p
}

# Andel LO for hvert aar i hele perioden - kun paa segment niveau - indeks
p.LO.periode.seg.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.seg.i[-l, i], vertex.size = 6, show.text = FALSE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "")) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Procent", limits = c(min(LO.andel.seg.i, na.rm = TRUE), max(LO.andel.seg.i, na.rm = TRUE))) 
  p.LO.periode.seg.i[[i]] <- p.p
}

#
# !!!! LABELS
#

####################
# Andel LO for hvert aar i hele perioden !!! LABELS
p.LO.periode.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.smooth[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("LO: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Andel", limits = c(min(LO.andel.smooth, na.rm = TRUE), max(LO.andel.smooth, na.rm = TRUE)))
  p.LO.periode.l[[i]] <- p.p
}

# Andel LO for hvert aar i hele perioden - kun paa segment niveau !!! LABELS
p.LO.periode.seg.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.seg[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("LO: ", periode[i], sep = "" )) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Andel", limits = c(min(LO.andel.seg, na.rm = TRUE), max(LO.andel.seg, na.rm = TRUE))) 
  p.LO.periode.seg.l[[i]] <- p.p
}

####################
# Andel LO for hvert aar i hele perioden - indeks !!! LABELS
p.LO.periode.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.smooth.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "")) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Procent", limits = c(min(LO.andel.smooth.i, na.rm = TRUE), max(LO.andel.smooth.i, na.rm = TRUE)))
  p.LO.periode.i.l[[i]] <- p.p
}

# Andel LO for hvert aar i hele perioden - kun paa segment niveau - indeks !!! LABELS
p.LO.periode.seg.i.l  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = LO.andel.seg.i[-l, i], vertex.size = 6, show.text = TRUE, midpoints = FALSE, border.labels = seg.lab, border.text.hjust = 0.32, border.padding = 1.3)
  p.p   <- p.p + defaults + ggtitle(paste("Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "")) + scale_fill_gradientn(colours = gradient.roed.fald, labels = percent, na.value = vertex.na, name = "Procent", limits = c(min(LO.andel.seg.i, na.rm = TRUE), max(LO.andel.seg.i, na.rm = TRUE))) 
  p.LO.periode.seg.i.l[[i]] <- p.p
}

###############################################################################
## ZOOM

### Basis kort

###################################
## ORG-grad

p.zoom.org  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = organiserede.andel.smooth[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.lilla, limits = c(0, max(organiserede.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.org[[i]] <- p.p
}

p.zoom.org.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = organiserede.andel.smooth.i[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.lilla, limits = c(min(organiserede.andel.smooth.i[-l,], na.rm = TRUE), max(organiserede.andel.smooth.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.org.i[[i]] <- p.p
}

# GULE
p.zoom.gule  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = gule.andel.smooth[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.gul, limits = c(0, max(gule.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.gule[[i]] <- p.p
}

p.zoom.gule.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = gule.andel.smooth.i[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.gul, limits = c(min(gule.andel.smooth.i[-l,], na.rm = TRUE), max(gule.andel.smooth.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.gule.i[[i]] <- p.p
}

################################
# LO
p.zoom.LO  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = LO.andel.smooth[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.roed.fald, limits = c(0, max(LO.andel.smooth[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.LO[[i]] <- p.p
}

p.zoom.LO.i  <- list()
for(i in 1:length(periode)){
  p.p   <- gg.jonas(seg, layout = lay, edges = edges.default.all, edge.size = edge.size, vertex.fill = LO.andel.smooth.i[-l, i], vertex.size = 13, show.text = TRUE, text.vjust = 2.2,
                    text.size = 6, midpoints = TRUE, border.text = FALSE, border.text.hjust = 0.32, border.padding = 0.3)
  p.p   <- p.p + defaults + scale_fill_gradientn(colours = gradient.roed.fald, limits = c(min(LO.andel.smooth.i[-l,], na.rm = TRUE), max(LO.andel.smooth.i[-l,], na.rm = TRUE)), na.value = vertex.na, labels = percent, name = "Procent")
  p.p   <- p.p + scale_alpha_continuous(range = c(0.2, 1), guide = "none")
  p.zoom.LO.i[[i]] <- p.p
}

#################################################
# 2.40 Omsorg/Ambulance

# ORG
p.zoom.2.40.org   <- lapply(p.zoom.org, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.org[[i]]     <-  p.zoom.2.40.org[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - Organisationsgrad: ", periode[i], sep = "" ))

p.zoom.2.40.org.i   <- lapply(p.zoom.org.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.org.i[[i]]   <-  p.zoom.2.40.org.i[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - Ã†ndring i organisationsgrad siden", periode[1], " : ", periode[i], sep = "" ))

# Gule
p.zoom.2.40.gule   <- lapply(p.zoom.gule, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.gule[[i]]    <-  p.zoom.2.40.gule[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - Gule: ", periode[i], sep = "" ))

p.zoom.2.40.gule.i   <- lapply(p.zoom.gule.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.gule.i[[i]]    <-  p.zoom.2.40.gule.i[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - Stigning af gule siden", periode[1], " : ", periode[i], sep = "" ))

# LO
p.zoom.2.40.LO   <- lapply(p.zoom.LO, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.LO[[i]]    <-  p.zoom.2.40.LO[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - LO: ", periode[i], sep = "" ))

p.zoom.2.40.LO.i   <- lapply(p.zoom.LO.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "2.40", distance = 400)
for (i in 1:length(periode)) p.zoom.2.40.LO.i[[i]]    <-  p.zoom.2.40.LO.i[[i]] + ggtitle(paste("2.40 Omsorg/Ambulance - Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "" ))

#################################################
# 3.17 Kontor

# ORG
p.zoom.3.17.org   <- lapply(p.zoom.org, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.org[[i]]    <-  p.zoom.3.17.org[[i]] + ggtitle(paste("3.17 Kontor - Organisationsgrad: ", periode[i], sep = "" ))

p.zoom.3.17.org.i   <- lapply(p.zoom.org.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.org.i[[i]]    <-  p.zoom.3.17.org.i[[i]] + ggtitle(paste("3.17 Kontor - Ã†ndring i organisationsgrad siden", periode[1], " : ", periode[i], sep = "" ))

# Gule
p.zoom.3.17.gule   <- lapply(p.zoom.gule, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.gule[[i]]    <-  p.zoom.3.17.gule[[i]] + ggtitle(paste("3.17 Kontor - Gule: ", periode[i], sep = "" ))

p.zoom.3.17.gule.i   <- lapply(p.zoom.gule.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.gule.i[[i]]    <-  p.zoom.3.17.gule.i[[i]] + ggtitle(paste("3.17 Kontor - Stigning af gule siden", periode[1], " : ", periode[i], sep = "" ))

# LO
p.zoom.3.17.LO   <- lapply(p.zoom.LO, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.LO[[i]]    <-  p.zoom.3.17.LO[[i]] + ggtitle(paste("3.17 Kontor - LO: ", periode[i], sep = "" ))

p.zoom.3.17.LO.i   <- lapply(p.zoom.LO.i, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership == "3.17", distance = 400)
for (i in 1:length(periode)) p.zoom.3.17.LO.i[[i]]    <-  p.zoom.3.17.LO.i[[i]] + ggtitle(paste("3.17 Kontor - Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "" ))

#################################################
# 3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag)

# ORG
p.zoom.3.4.org   <- lapply(p.zoom.org, zoom.to.segment, lay = lay, zoom.mem = seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.org[[i]]    <-  p.zoom.3.4.org[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - Organisationsgrad: ", periode[i], sep = "" ))

p.zoom.3.4.org.i   <- lapply(p.zoom.org.i, zoom.to.segment, lay = lay, seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.org.i[[i]]    <-  p.zoom.3.4.org.i[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - Ã†ndring i organisationsgrad siden", periode[1], " : ", periode[i], sep = "" ))

# Gule
p.zoom.3.4.gule   <- lapply(p.zoom.gule, zoom.to.segment, lay = lay, seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.gule[[i]]    <-  p.zoom.3.4.gule[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - Gule: ", periode[i], sep = "" ))

p.zoom.3.4.gule.i   <- lapply(p.zoom.gule.i, zoom.to.segment, lay = lay, seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.gule.i[[i]]    <-  p.zoom.3.4.gule.i[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - Stigning af gule siden", periode[1], " : ", periode[i], sep = "" ))

# LO
p.zoom.3.4.LO   <- lapply(p.zoom.LO, zoom.to.segment, lay = lay, seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.LO[[i]]    <-  p.zoom.3.4.LO[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - LO: ", periode[i], sep = "" ))

p.zoom.3.4.LO.i   <- lapply(p.zoom.LO.i, zoom.to.segment, lay = lay, seg.mem$membership %in% c("3.4", "3.5"), distance = 400)
for (i in 1:length(periode)) p.zoom.3.4.LO.i[[i]]    <-  p.zoom.3.4.LO.i[[i]] + ggtitle(paste("3.4 Arbejde m. trae og 3.5 BYG/ANLÃ†G (ufag) - Ã†ndring i andelen af LO-medlemmer siden", periode[1], " : ", periode[i], sep = "" ))







# Netvaerkets andel af stoerrelse
# p.nabo.out                 <- gg.jonas(seg, layout = lay, edges = edges.default, edge.size = edge.size, vertex.fill = nabo.out, vertex.size = nabo.out)
# p.nabo.out + scale_fill_continuous(high = vertex.fill.high, low = vertex.fill.low)
# 
# p.nabo.in                 <- gg.jonas(seg, layout = lay, edges = edges.default,  edge.size = edge.size, vertex.fill = nabo.in, vertex.size = nabo.in)
# p.nabo.in + scale_fill_continuous(high = vertex.fill.high, low = vertex.fill.low)
# 
# p.nabo.sym                 <- gg.jonas(seg, layout = lay, edges = edges.default,  edge.size = edge.size, vertex.fill = nabo.sym, vertex.size = nabo.sym)
# p.nabo.sym + scale_fill_continuous(high = vertex.fill.high, low = vertex.fill.low)
# 
# p.nabo.dif                 <- gg.jonas(seg, layout = lay, edges = edges.default,  edge.size = edge.size, vertex.fill = nabo.dif, vertex.size = nabo.dif)
# p.nabo.dif + scale_fill_continuous(high = vertex.fill.high, low = vertex.fill.low)

###########################################################################
# EGO kort
# # Vi har ikke styr paa raekker og kolonner her, saa vi ved ikke hvad der sender og modtager
# freq.mat    <- mob.mat[-nrow(mob.mat), -nrow(mob.mat)]
# wm          <- segment.edges(seg.b, cut.off = 0.95, segment.reduction = 1, small.cell.reduction = 5)
# 
# ids         <- 1:nrow(wm)
# exclude     <- c(12)
# ids         <- ids[-which(ids %in% exclude)]
# 
# ego.plots    <- list()
# for (id in ids){
#   
#   MR        <- wm[id,]
#   MC        <- wm[,id]
#   M         <- freq.mat[id, ] # Den her kender vi ikke retningen paa - maaske skal raekke og kolonne laegges sammen og dividers med 2, saa er det alle der udveksler...
#   nul       <- as.factor(M == 0)
#   M[id]     <- NA
#   EM        <- matrix(0 , nrow = nrow(wm), ncol = ncol(wm))
#   EM[,id]   <- MC
#   EM[id,]   <- MR
#   EM[EM <= 0.99] <- 0
#   dimnames(EM) <- dimnames(wm)
# 
#   sum.stat  <- c("Beskaeftigede" = as.numeric(stor.beskaeftigede[id]),
#                  "Andel af alle beskaeftigede %" = round(as.numeric(stor.beskaeftigede[id]/sum(stor.beskaeftigede)), 3),
#                           "Intern mobilitet %" = round(as.numeric(intern.mobilitet[id]), 2),
#                           "Organisationsgrad 2011 %" = round(organiserede.andel[id, ncol(organiserede.andel)], 2))
#   as.matrix(sum.stat)
#   sum.stat  <- paste(names(sum.stat), " :", sum.stat)
#   sum.stat  <- paste(sum.stat, collapse = "\n")
#   
#   p.ego     <- gg.jonas(seg.b, layout = lay, niveau = 1:4, edges = EM, vertex.fill = M, vertex.size = M, edge.size = 0.8, border.padding = 1, show.text = TRUE, border.text.size = 3, edge.color = edge.color.high, vertex.shape = nul)
#   p.ego     <- p.ego + scale_fill_continuous(high = vertex.fill.high, low = vertex.fill.low, na.value = "purple", guide = "legend") + scale_size_continuous(range = c(3, 10), na.value = 8) + scale_shape_manual(values = c(21, 4), guide = "none")
#   p.ego     <- p.ego + guides(size = guide_legend(override.aes = list(shape = 21)))
#   p.ego     <- p.ego + annotate("text", x = Inf, y = -Inf, color = "black", vjust = -0.5, hjust = 1 ,label = sum.stat)
#   ego.plots[[id]] <- p.ego + ggtitle(rownames(wm)[id]) + annotate("segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, color = "black", lwd = 1)
# }

#pdf(file = "gg_p_5_ego_kort.pdf", height = 15, width = 15)
#ego.plots
#dev.off()

# 
# #' Tile.plot
# #' Plots a matrix as tile with color according to intensity
# #' @param adj.mat is the input matrix, with named rows and columns and numerical cells
# #' @return a ggplot2 tile plot
# #' @export
# 
# tile.plot <- function(adj.mat){
#   mti                <- melt(adj.mat)
#   mti$name           <- factor(mti$Var1, levels = rownames(adj.mat), ordered = TRUE )
#   mti$color          <- mti$value
#   mti$color[mti$color == 0] <- NA
#   mti$color[mti$color > 5]  <- 5
#   
#   sc                 <- list()
#   sc$theme_bw        <- theme_bw()
#   sc$fill            <- scale_fill_continuous(high = "#b2182b", low = "#fddbc7", na.value = "white", guide = "none")
#   sc$axis.angle      <- theme(axis.text.x = element_text(size = 11, angle = 90, hjust = 1, color = "black"), axis.text.y = element_text(size = 11, color = "black"))
#   sc$xlab            <- xlab(NULL)
#   sc$ylab            <- ylab(NULL)
#   sc$theme           <- theme(axis.ticks = element_blank(), panel.border = element_blank())
#   
#   p                  <- ggplot(data = mti, aes(x = Var1, y = Var2, fill = color, label = value)) + geom_tile(color = "black") 
#   p                  <- p + sc #+ geom_text(alpha = 1)
#   p
# }
# 
# wm <- weight.matrix(seg$mat.list[[1]])
# wm[is.na(wm)] <- 0
# wm <- round(wm)
# tile.plot(wm)
# names(wm)
