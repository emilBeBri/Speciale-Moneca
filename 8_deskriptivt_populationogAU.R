## lav data
source("./statistik/R/moneca/vores/vorescripts/8.1_deskriptivt_populationogAU_datagenerering.R")
# view(desk.ledsoc_tael)

### theming

theme.desk <-  theme(axis.title=element_blank()
    ,plot.background = element_blank()
   # ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,panel.border = element_blank()
   ,legend.text=element_text(size=12)
  ) 

############## ledsoc_tael plot #########


# ledsoc_tael_socio_netto
# ledsoc_tael_socstil_netto
# is.factor(desk.ledsoc_tael$grp

# col.desk.au <- c("#11605E", "#17807E", "#8BC0BF","#D8472B","#FFB90F","#00008B")
# names(col.desk.au) <- c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")
# names(col.desk.au) <- c(1,2,3,4,5,6)

#farverne
col1 <- c("#FF3030") #firebrick1,
col2 <- c("#8B1A1A") #firebrick4,
col3 <- c("#1E90FF") #skyblue1,
col4 <- c("#104E8B") #skyblue4,
col5 <- c("#C1FFC1") #darkseagreen1,
col6 <- c("#698B69") #darkseagreen4
#rækkefølgen
col.desk.au <- c(col1,col3,col6,col2,col5,col4)
# names(col.desk.au) <- c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")
# names(col.desk.au) <- c(1,2,3,4,5,6)

### test 1


# opdel i forskellige grupper
# sociosocstil, nettosocio og nettosocstil
# desk.ledsoc_tael <-   filter(desk.ledsoc_tael,  grp== 1 |  grp== 5 |  grp== 6)
# desk.ledsoc_tael <-   filter(desk.ledsoc_tael,  grp== 1 |  grp== 5 |  grp== 6 |  grp== 4)

  


view(desk.ledsoc_tael)  
### plot  ###
p.desk.ledsoc_tael <- NULL 
p.desk.ledsoc_tael  <-  ggplot(desk.ledsoc_tael, aes(x=Aar, y=N, colour=grp, group=grp)) + 
                      geom_line(size=1.6) +
                      geom_point(size=3) + 
                      xlab("") + ylab("") +
                      theme(legend.margin = unit(0, "cm")) +
                      scale_colour_manual(name = "arbejdsløse", labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=col.desk.au) +  
                      # scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")) + 
                      theme_bw() 
# til N
p.desk.ledsoc_tael <-   p.desk.ledsoc_tael +  scale_y_continuous(breaks=seq(0, 250000, 25000)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
scale_x_continuous(breaks=c(1996:2009)) 
p.desk.ledsoc_tael  + theme.desk


pdf(file = "./statistik/R/moneca/vores/output/deskriptivt/desk.ledsoc_tael.pdf", height = 10, width = 16)
p.desk.ledsoc_tael  + theme.desk 
dev.off()

### backup af ovenstående

# # view(desk.ledsoc_tael)  
# ### plot  ###
# p.desk.ledsoc_tael  <-  ggplot(desk.ledsoc_tael, aes(x=Aar, y=N, colour=grp, group=grp)) + 
#                       geom_line(size=1) +
#                       # geom_point(size=5,aes(shape=grp,type=5)) + 
#                       xlab("") + ylab("") +
#                       theme(legend.margin = unit(0, "cm")) +
#                       scale_colour_manual(name = "arbejdsløse",
#                       labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=c(1,2,3,4,5,6)) +   
#                       scale_shape_manual(name = "arbejdsløse", labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=c(1,2,3,4,5,6)) +
#                       scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")) + 
#                       theme_bw() 
# # til N
# p.desk.ledsoc_tael <-   p.desk.ledsoc_tael +  scale_y_continuous(breaks=seq(0, 250000, 25000)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
# scale_x_continuous(breaks=c(1996:2009)) 
# p.desk.ledsoc_tael  + theme.desk 



# pdf(file = "./statistik/R/moneca/vores/output/deskriptivt/desk.ledsoc_tael3.pdf", height = 10, width = 10)
# p.desk.ledsoc_tael  + theme.desk 
# dev.off()




#################### test  virker ikke :'(


# view(desk.ledsoc_tael)
# data_wide <- spread(desk.ledsoc_tael, grp, N)
# view(data_wide)


# last_vals <-  setNames(c(91574,85377,57706,95756), c("ledsoc_tael_aupop_socstil
# ", "ledsoc_tael_aupop_socstil_socio","ledsoc_tael_socio_netto","ledsoc_tael_socstil_netto"))

# gg <- p.desk.ledsoc_tael  + theme.desk + theme(legend.position="none")
# gg <- gg + theme(plot.margin = unit(c(1, 7, 2, 1), "lines"))

# for (i in 1:length(last_vals)) {
#   gg <- gg + annotation_custom(grob=textGrob(names(last_vals)[i], hjust=0,
#                                              gp=gpar(fontsize=8, 
#                                                      col=gender_colors[names(last_vals)[i]])),
#                                xmin=2010, xmax=2010,
#                                ymin=last_vals[i], ymax=last_vals[i])
# }
# gg

##################



##########################################################
                      alder  
##########################################################

source("./statistik/R/moneca/vores/vorescripts/8.1_deskriptivt_populationogAU_datagenerering.R")
# view(desk.alder)


## nyt plot 
p.desk.alder  <-  ggplot(desk.alder, aes(x=Aar, y=mean, colour=grp, group=grp)) + 
                      geom_line(size=1) +
                      # geom_point(size=2.5) + 
                      xlab("") + ylab("") +
                      theme(legend.margin = unit(0, "cm")) +  
                      scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("pop","aupop","au")) +
                      coord_cartesian(ylim = c(20, 50)) +
                      theme_bw()



# uden SD
p.desk.alder <-   p.desk.alder +  scale_y_continuous(breaks=seq(20, 50, 3)) + scale_x_continuous(breaks=c(1996:2009)) 
p.desk.alder  + theme.desk 


# med SD
p.desk.alder.sd <-   p.desk.alder +  scale_y_continuous(breaks=seq(0, 65, 5)) + scale_x_continuous(breaks=c(1996:2009)) + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?



pdf(file = "./statistik/R/moneca/vores/output/deskriptivt/desk.alder.pdf", height = 10, width = 10)
p.desk.alder  + theme.desk 
dev.off()


### z-test

view(desk.alder) 

z.test(35.95,35.15,11.79,68149)

x = 35.95            # sample mean 
X = 35.15             # hypothesized value 
sigma = 11.79            # population standard deviation 
n = 68149


tmp1 = x-X
help(pnorm)
qnorm()


alfa=0.05 

z.test <- function(x,X,sigma,n,alfa=0.05) {
crit.val <-  -qnorm(alfa/2)
tmp1 <- x - X
zval <- tmp1/(sqrt(sigma/n))
pval <- pnorm(zval)*2
konf.int <- crit.val*(sqrt(sigma/n))
print(paste("Z-værdi::", round(zval,digits=3)))
print(paste("P-Værdi::", round(pval,digits=5)))
print(paste("konfidensinterval:",round(konf.int,digits=3)))
print(paste("konf.int lav:", round(x - konf.int,digits=3)))
print(paste("konf.int høj:", round(x + konf.int,digits=3)))
}

z.test(x=14.6,X=15.4,sigma=2.5,n=35)


qnorm(0.05/2)


Solution
The null hypothesis is that μ = 15.4. We begin with computing the test statistic.

xbar = 14.6            # sample mean 
mu = 15.4             # hypothesized value 
sigma = 2.5            # population standard deviation 
n = 35                 # sample size 
zter = (xbar − mu)/(sigma/sqrt(n)) 
zter                      # test statistic 


install.packages("asbio")
install.packages("tkrplot")


library(asbio)

setwd("/home/emil")

one.sample.z {asbio}


one.sample.z(null.mu = 15.4, xbar = NULL, sigma, n = NULL, 
test = c("two.sided", "lower.tail", "upper.tail"))

z.test(14.6,15.4,mu=0,sigma.x=2.5)




# sample size 

zter = tmp1/(sigma/sqrt(n)) 

zter                      # test statistic 

pnorm(zter) *2


(x − X)

tmp1 <- x - X
tmp1/(sigma/sqrt(n))


z.test <- function(x,X,sigma,n) {
tmp1 <- x - X
zval <- tmp1/(sigma/sqrt(n))
pval <- pnorm(zval)*2
konf.int <- 1.96*(sigma/sqrt(n))
print(paste("Z-værdi:", round(zval,digits=3)))
print(paste("P-Værdi:", round(pval,digits=5)))
print(paste("konfidensinterval"))
}







konf.int <- NULL 

konf.int + 14.6 


help(cat)



konf.hoj 



help(round)







zter <- 
]

# Funktion der forbereder til ggplot2 

z.test <- function(x,X,sigma,n){
tmp1 <-  (x − X)
zter <-  tmp1/(sigma/sqrt(n)) 
}

zsatest <- function(x,y){
tmp1 <-  x − y }




  x.df = data.frame(x[y,])
  x.df <- cbind(disco = rownames(x.df), x.df)
  rownames(x) <- NULL
  x.m <- melt(x.df, id.vars='disco')    
  ggplot(data = x.m, aes(x = variable, y = value, colour = disco)) +       
    geom_line(aes(group = disco)) + geom_point()



zz  <-   (14.6-15.4)/(sigma/sqrt(n))


10/2

[1] −1.8931
We then compute the critical values at .05 significance level.

> alpha = .05 
> z.half.alpha = qnorm(1−alpha/2) 
> c(−z.half.alpha, z.half.alpha) 
[1] −1.9600  1.9600
Answer
The test statistic -1.8931 lies between the critical values -1.9600 and 1.9600. Hence, at .05 significance level, we do not reject the null hypothesis that the mean penguin weight does not differ from last year.

Alternative Solution
Instead of using the critical value, we apply the pnorm function to compute the two-tailed p-value of the test statistic. It doubles the lower tail p-value as the sample mean is less than the hypothesized value. Since it turns out to be greater than the .05 significance level, we do not reject the null hypothesis that μ = 15.4.

> pval = 2 ∗ pnorm(z)    # lower tail 
> pval                   # two−tailed p−value 
[1] 0.058339








############ visualisering ##############




# #### tufte plot i R

# x <- 1967:1977
# y <- c(0.5,1.8,4.6,5.3,5.3,5.7,5.4,5,5.5,6,5)
# d <- data.frame(x, y)
# view(d)
# library(ggthemes)
# ggplot(d, aes(x,y)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) +
#   theme(axis.title=element_blank()) + geom_hline(yintercept = c(5,6), lty=2) + 
#   scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
#   scale_x_continuous(breaks=x,label=x) +
#   annotate("text", x = c(1977,1977.2), y = c(2,5.5), adj=1,  family="serif",
#            label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))



# ggplot(x.m, aes(variable,value)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) + 
# theme(axis.title=element_blank()) + geom_hline(yintercept = c(2000,2500), lty=2) + 
#   scale_y_continuous(breaks=seq(1000,4250,250))   + 
#   scale_x_continuous(breaks=x,label=x) 


#   +
#   annotate("text", x = c(1977,1977.2), y = c(2,5.5), adj=1,  family="serif",
#            label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))



# max(x.m$value)


# view(x.m)

# install_github("jrnold/ggthemes")


# install.packages('ggthemes', dependencies = TRUE)


# setwd("/home/emil")
# getwd()

# #### leg med plot

# view 


# # formatet til 
# df.t <- NULL
# x.m <- NULL
# df.t <-  discodata %>% select(disco,contains("ledbeskaeft.smooth"))
# df.t$membership <- NULL
# df.t <- df.t[21:25,]  # udvælg grupper af interesse
# # view(df.t)
# x.m <- melt(df.t, id.vars='disco')
# x.tmp <- 1996:2009
# x.tmp <-  rep(x.tmp,5) #antal grupper
# x.tmp <-  sort(x.tmp) 
# x.m$variable <- x.tmp
#  view(x.m)

# # eksperiment



# test  <-  ggplot(x.m, aes(x=variable, y=value, colour=disco, group=disco)) + 
# geom_line(size=1) +
# geom_point(size=6,aes(shape=disco,type=5)) + 
# xlab("") + ylab("") +
# theme(legend.margin = unit(0, "cm")) +
#   scale_colour_manual(name = "Treatment & State",
#                       labels = c("Control, Non-F", "Control, Flwr", "noget andet","4","5"),values=c(1,2,3,4,5)) +   
#   scale_shape_manual(name = "Treatment & State",
#                      labels = c("Control, Non-F", "Control, Flwr","noget andet","4","5"),values=c(1,2,3,4,5)) +
# scale_colour_brewer(palette="Set1",name = "Treatment & State",
#                      labels = c("Control, Non-F", "Control, Flwr", "noget andet","4","5"))  + theme_bw() 


# test2 <-  test + theme(axis.title=element_blank()) + 
#   scale_y_continuous(breaks=seq(0, 4000, 250)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
# scale_x_continuous(breaks=c(1996:2009)) #+ theme_tufte()


# test2 +   #eliminates background, gridlines, and chart border
#   theme(
#     plot.background = element_blank()
#    # ,panel.grid.major = element_blank()
#    ,panel.grid.minor = element_blank()
#    ,panel.border = element_blank()
#   ) +
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))




# ggplot(d, aes(x,y)) + geom_line() + geom_point(size=3) + theme_tufte(base_size = 15) +
#   theme(axis.title=element_blank()) + geom_hline(yintercept = c(5,6), lty=2) + 
#   scale_y_continuous(breaks=seq(1, 6, 1), label=sprintf("$%s",seq(300,400,20))) + 
#   scale_x_continuous(breaks=x,label=x) +
#   annotate("text", x = c(1977,1977.2), y = c(2,5.5), adj=1,  family="serif",
#            label = c("Per capita\nbudget expandures\nin constant dollars", "5%"))



# , name="ledighedsdefinition") +






# #virker men dobbelt labels, grr
# ggplot(x.m, aes(x=variable, y=value, colour=disco, group=disco)) + 
# geom_line(size=1) +
# scale_colour_brewer(palette="Spectral", name="ledighedsdefinition", labels=c("1","2","3")) +
# geom_point(size=6,aes(shape=disco)) + 
# xlab("antal ledige") + ylab("år") +
# theme(legend.margin = unit(0, "cm")) +
# guides(colour = "legend", 
#   size = "none", 
#   shape = "legend") + guides(colour = guide_legend("ledighedsdefinition"), 
#   shape = guide_legend("ledighedsdefinition"),
#   labels=c("1","2","3"))   +
# theme_bw() 





# scale_fill_gradientn(colours = getPalette(length(unique(discodata$intern.mobilitet.seg))), guide = "legend", name = "intern mobilitet i segment", breaks=intern.mob.seg_num, labels=intern.mob.seg_lab)




# ggplot(data = x.m, aes(x = variable, y = value, colour = disco)) +       
#     geom_line(aes(group = disco)) + geom_point() +
#     labs(x="år",y="antal beskæftigede")


# ### graphics cook book: making a basic line graph:
# df.t2 <- df.t

# library(gcookbook)

# tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))



# view(tg)

# #####################################




# ################ trash ###############################################



# ### plot  ###
# desk.aupop.alder.tmp1  <-  ggplot(desk.alder, aes(x=Aar, y=mean, colour=grp, group=grp)) + 
# geom_line(size=1) +
# geom_point(size=6,aes(shape=grp,type=5)) + 
# xlab("") + ylab("") +
# theme(legend.margin = unit(0, "cm")) +
#   scale_colour_manual(name = "alder",
#                       labels = c("au.pop","au", "pop"),values=c(1,2,3)) +   
#   scale_shape_manual(name = "alder",
#                      labels = c("au.pop","au", "pop"),values=c(1,2,3)) +
# scale_colour_brewer(palette="Set1",name = "alder",
#                      labels = c("au.pop","au", "pop")) + theme_bw() 

# # til variabel
# desk.aupop.alder.tmp1 <-   desk.aupop.alder.tmp1 +  scale_y_continuous(breaks=seq(0, 65, 5)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
# scale_x_continuous(breaks=c(1996:2009)) 

# # til N
# desk.aupop.alder.tmp1 <-   desk.aupop.alder.tmp1 +  scale_y_continuous(breaks=seq(0, 65, 5)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
# scale_x_continuous(breaks=c(1996:2009)) 



# desk.aupop.alder <-  desk.aupop.alder.tmp1  + theme.desk + geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) 

# desk.aupop.alder + theme.desk 


# pdf(file = "./statistik/R/moneca/vores/output/deskriptivt/desk.aupop.alder.pdf", height = 15, width = 15)
# desk.aupop.alder 
# dev.off()

# #  kommentar: vores analyseudvalg af arbejdsløse adskiller sig fra hele den danske befolkning, det ville også være underligt andet. Interessant at vores au rent faktisk er yngre. Det ses også at sammenkoblingen mellem disco og socio ikke har ændret alderssammensætningen nævneværdigt, hvilket er fremragende for validiteten.  
# # Når N tages med ses en stor forskel i antallet af personer, men dette er ikke forskelligt fra au og aupop. det har dog helt tydeligt noget med datakvalitet at gøre, da den i pop er nærmest konstant. 
# desk.aupop.alder.tmp1  + theme.desk + coord_cartesian(ylim = c(50000, 275000))


# # view(desk.alder)






#   +
#   #draws x and y axis line
#   # theme(axis.line = element_line(color = 'black'))












############## templates der gemmes til senere ##########################


### sådan laves farvede linjer *der samtidig* har figurer i deres punkter. Måske overkill, måske ikke. gør det hele væsentligt mere besværligt.
# # view(desk.ledsoc_tael)  
# ### plot  ###
# p.desk.ledsoc_tael  <-  ggplot(desk.ledsoc_tael, aes(x=Aar, y=N, colour=grp, group=grp)) + 
#                       geom_line(size=1) +
#                       geom_point(size=5,aes(shape=grp,type=5)) + 
#                       xlab("") + ylab("") +
#                       theme(legend.margin = unit(0, "cm")) +
#                       scale_colour_manual(name = "arbejdsløse",
#                       labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=c(1,2,3,4,5,6)) +   
#                       scale_shape_manual(name = "arbejdsløse", labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto"),values=c(1,2,3,4,5,6)) +
#                       scale_colour_brewer(palette="Set1",name = "arbejdsløse",labels = c("sociosocstil","socstil","socio","disco_sociosocstil","socio.netto","socstilnetto")) + 
#                       theme_bw() 


# # til N
# p.desk.ledsoc_tael <-   p.desk.ledsoc_tael +  scale_y_continuous(breaks=seq(0, 250000, 25000)) + #problem at den ikke starter ved nul, fix det måske med x_lim og y_lim? findes det?
# scale_x_continuous(breaks=c(1996:2009)) 

# pdf(file = "./statistik/R/moneca/vores/output/deskriptivt/desk.ledsoc_tael3.pdf", height = 10, width = 10)
# p.desk.ledsoc_tael  + theme.desk 
# dev.off()






