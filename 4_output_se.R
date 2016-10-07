# Outputs
#suppressMessages(source("3_plots.R"))
#setwd("./statistik/R/moneca/Anton_untouched")



# Text output
sink(file = "./statistik/R/moneca/vores/output/1_Text.txt")

sink()
# Matrix output
write.table(seg.qual, file = "./statistik/R/moneca/vores/output/csv/segment.quality.allemobile.150.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
write.table(seg.qual.final, file = "./statistik/R/moneca/vores/output/csv/segment.quality.final.allemobile.150.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)
write.table(mob.mat, file = "./statistik/R/moneca/vores/output/csv/mob.mat.allemobile.allemobile.150.csv", sep = ";", fileEncoding = "UTF-8", col.names=NA)



#write.table(gule.dat, file = "./statistik/R/moneca/vores/output/gule.csv", sep = ";", row.names = FALSE) #giver fejl
#####################################################################################################
# Plot stor stoerrelse

# Plot trappen 
# pdf(file = "./statistik/R/moneca/vores/output/2.plot.trappen.array.allemobilejonasanton.pdf", height = 15, width = 15)
 # map.array(plot.trappe.list, fixed.coord = FALSE, ncol = 2)
 # map.array(pt.list, fixed.coord = FALSE, ncol = 2)
# dev.off()

pdf(file = "./statistik/R/moneca/vores/output/2.plot.trappen.ledige.150.pdf", height = 15, width = 15)
plot.trappe.list
dev.off()
#warnings()


# Hoved kort #removed missing values, alt muligt der #spoerganton
pdf(file = "./statistik/R/moneca/vores/output/3.plot.hoved.ledige.150.pdf", height = 15, width = 15)
p.seg.tydelige.edges.no.border  #f?lgende fejl opst?r: 1: In aesthetics["group"] <- .$geom_params$group: number of items to replace is not a multiple of replacement length #sp?rganton
p.seg.tydelige.edges #samme her og
p.seg.strong.edges #samme her
dev.off()


###############################################################
# Organisationsgrads kort
pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad.pdf", height = 12, width = 13)
p.org.periode
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_seg.pdf", height = 12, width = 13)
p.org.periode.seg
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_indeks.pdf", height = 12, width = 13)
p.org.periode.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_seg_indeks.pdf", height = 12, width = 13)
p.org.periode.seg.i
dev.off()

# !!! LABELS

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_labels.pdf", height = 15, width = 16)
p.org.periode.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_seg_labels.pdf", height = 15, width = 16)
p.org.periode.seg.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_indeks_labels.pdf", height = 15, width = 16)
p.org.periode.i.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/5_organisationsgrad_seg_indeks_labels.pdf", height = 15, width = 16)
p.org.periode.seg.i.l
dev.off()

###############################################################
# Roede kort
pdf(file = "./statistik/R/moneca/vores/output/4_roede.pdf", height = 12, width = 13)
p.roede.periode
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_seg.pdf", height = 12, width = 13)
p.roede.periode.seg
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_indeks.pdf", height = 12, width = 13)
p.roede.periode.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_seg_indeks.pdf", height = 12, width = 13)
p.roede.periode.seg.i
dev.off()

# !!! LABELS

pdf(file = "./statistik/R/moneca/vores/output/4_roede_labels.pdf", height = 15, width = 16)
p.roede.periode.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_seg_labels.pdf", height = 15, width = 16)
p.roede.periode.seg.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_indeks_labels.pdf", height = 15, width = 16)
p.roede.periode.i.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/4_roede_seg_indeks_labels.pdf", height = 15, width = 16)
p.roede.periode.seg.i.l
dev.off()

#################################################################
# Gule

pdf(file = "./statistik/R/moneca/vores/output/6_gule.pdf", height = 12, width = 13)
p.gule.periode
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_seg.pdf", height = 12, width = 13)
p.gule.periode.seg
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_indeks.pdf", height = 12, width = 13)
p.gule.periode.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_seg_indeks.pdf", height = 12, width = 13)
p.gule.periode.seg.i
dev.off()

# pdf(file = "./statistik/R/moneca/vores/output/6_gule_label.pdf", height = 16, width = 17)
# p.gule.periode.label
# dev.off()

# !!! LABELS

pdf(file = "./statistik/R/moneca/vores/output/6_gule_labels.pdf", height = 15, width = 16)
p.gule.periode.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_seg_labels.pdf", height = 15, width = 16)
p.gule.periode.seg.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_indeks_labels.pdf", height = 15, width = 16)
p.gule.periode.i.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/6_gule_seg_indeks_labels.pdf", height = 15, width = 16)
p.gule.periode.seg.i.l
dev.off()

######################################################
# Roede
# pdf(file = "./statistik/R/moneca/vores/output/7_roede.pdf", height = 12, width = 13)
# p.roede.periode
# dev.off()
# 
# pdf(file = "./statistik/R/moneca/vores/output/7_roede_seg.pdf", height = 12, width = 13)
# p.roede.periode.seg
# dev.off()
# 
# pdf(file = "./statistik/R/moneca/vores/output/7_roede_indeks.pdf", height = 12, width = 13)
# p.roede.periode.i
# dev.off()
# 
# pdf(file = "./statistik/R/moneca/vores/output/7_roede_seg_indeks.pdf", height = 12, width = 13)
# p.roede.periode.seg.i
# dev.off()

######################################################
# LO
pdf(file = "./statistik/R/moneca/vores/output/8_LO.pdf", height = 12, width = 13)
p.LO.periode
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_seg.pdf", height = 12, width = 13)
p.LO.periode.seg
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_indeks.pdf", height = 12, width = 13)
p.LO.periode.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_seg_indeks.pdf", height = 12, width = 13)
p.LO.periode.seg.i
dev.off()
# !!! LABELS
pdf(file = "./statistik/R/moneca/vores/output/8_LO_labels.pdf", height = 15, width = 16)
p.LO.periode.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_seg_labels.pdf", height = 15, width = 16)
p.LO.periode.seg.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_indeks_labels.pdf", height = 15, width = 16)
p.LO.periode.i.l
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/8_LO_seg_indeks_labels.pdf", height = 15, width = 16)
p.LO.periode.seg.i.l
dev.off()

###########################################################################
# ZOOM


######################################
# 2.40 Omsorg/Ambulance

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_organisationsgrad.pdf", height = 10, width = 10)
p.zoom.2.40.org
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_organisationsgrad_i.pdf", height = 12, width = 13)
p.zoom.2.40.org.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_gule.pdf", height = 12, width = 13)
p.zoom.2.40.gule
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_gule_i.pdf", height = 12, width = 13)
p.zoom.2.40.gule.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_LO.pdf", height = 12, width = 13)
p.zoom.2.40.LO
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_2_40_LO_i.pdf", height = 12, width = 13)
p.zoom.2.40.LO.i
dev.off()

######################################
# 3.17 Kontor

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_organisationsgrad.pdf", height = 10, width = 10)
p.zoom.3.17.org
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_organisationsgrad_i.pdf", height = 12, width = 13)
p.zoom.3.17.org.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_gule.pdf", height = 12, width = 13)
p.zoom.3.17.gule
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_gule_i.pdf", height = 12, width = 13)
p.zoom.3.17.gule.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_LO.pdf", height = 12, width = 13)
p.zoom.3.17.LO
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_17_LO_i.pdf", height = 12, width = 13)
p.zoom.3.17.LO.i
dev.off()

######################################
# 3.4 Arbejde m. trae og 3.5 BYG/ANLÆG (ufag)

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_organisationsgrad.pdf", height = 12, width = 13)
p.zoom.3.4.org
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_organisationsgrad_i.pdf", height = 12, width = 13)
p.zoom.3.4.org.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_gule.pdf", height = 12, width = 13)
p.zoom.3.4.gule
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_gule_i.pdf", height = 12, width = 13)
p.zoom.3.4.gule.i
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_LO.pdf", height = 12, width = 13)
p.zoom.3.4.LO
dev.off()

pdf(file = "./statistik/R/moneca/vores/output/ZOOM/1_ZOOM_3_4_LO_i.pdf", height = 12, width = 13)
p.zoom.3.4.LO.i
dev.off()

# # Ego plots
# pdf(file = "./statistik/R/moneca/vores/output/ego_plots.pdf", height = 15, width = 16)
# ego.plots
# dev.off()

# #############################################################################
# # Plot A4
# 
# # Plot output : A4
# pdf(file = "./statistik/R/moneca/vores/output/2_plot_alle_A4.pdf", height = 8.27, width = 11.69)
# plot.trappe.list
# dev.off()
# 
# pdf(file = "./statistik/R/moneca/vores/output/3_plot_trappen_A4.pdf", height = 8.27, width = 11.69)
# plot.trappe.list
# dev.off()
# 
# pdf(file = "./statistik/R/moneca/vores/output/4_plot_hoved.pdf", height = 8.27, width = 11.69)
# p.seg
# dev.off()
