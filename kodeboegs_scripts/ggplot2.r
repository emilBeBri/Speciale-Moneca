#At Ã¦ndre levels sÃ¥ de har den rigtige rÃ¦kkefÃ¸lge til et plot (http://learnr.wordpress.com/2010/03/23/ggplot2-changing-the-default-order-of-legend-labels-and-stacking-of-data/)
> levels(diamonds$cut)
[1] "Fair"      "Good"      "Very Good" "Premium"
[5] "Ideal"
> diamonds$cut <- factor(diamonds$cut, levels = rev(levels(diamonds$cut)))
> levels(diamonds$cut)
[1] "Ideal"     "Premium"   "Very Good" "Good"
[5] "Fair"

#Ãndre koordinater:
+ coord_flip() +opts(legend.position="top", legend.direction="horizontal")

#reorder levels
a$t9vÃ¦lg <- factor(a$vÃ¦lg,levels(factor(a$vÃ¦lg))[c(2,1,3,4)])
a$t9vÃ¦lg <- factor(a$vÃ¦lg,levels(a$vÃ¦lg)[c(2,1,3:10)])

#To change colour on a low-to-high plot:
ggplot(au, aes(x=trstep)) + geom_histogram(binwidth=1, aes(fill=..count..)) + scale_fill_gradient(low="Black", high="Pink")
