
#############3


## Define the image location (Kill Bill Poster)
img_kb <- "http://www.moviegoods.com/Assets/product_images/1010/477803.1010.A.jpg"

library(rPlotter)
pal_r <- extract_colours("http://developer.r-project.org/Logo/Rlogo-1.png")

pal_r1 <- extract_colours("./statistik/R/moneca/vores/voresdata/color_extract1.png")
pal_r2 <- extract_colours("./statistik/R/moneca/vores/voresdata/color_extract2.png")
pal_r3 <- extract_colours("./statistik/R/moneca/vores/voresdata/color_extract3.png")
pal_r <- extract_colours("./statistik/R/moneca/vores/voresdata/color_extract3.png")

samlet.pal <- append(pal_r1,pal_r2)
samlet.pal <- append(samlet.pal,pal_r3)

par(mfrow = c(1,2))
pie(rep(1, 5), col = pal_r, main = "Palette based on R Logo")
hist(Nile, breaks = 5, col = pal_r, main = "Palette based on R Logo")



## Plot and save as PNG
png("example_kb.png", width = 1000, height = 250, res = 150)
par(mfrow = c(1,4))
display(readImage(img_kb), method = "raster")
set.seed(1234)
pie(rep(1, 3), col = extract_colours(img_kb, 3))
pie(rep(1, 5), col = extract_colours(img_kb, 5))
pie(rep(1, 7), col = extract_colours(img_kb, 7))
par(mfrow = c(1,1))
dev.off()


view(df)
view(seg.df)

view(discodata)



tmp <- df %>% 	select(disco_s,membership)
tmp$membership2 <-   as.numer+ic(tmp$membership)


view(tmp)




quants <- seq(0,1,0.05)

round(quantile(discodata$fagf.seg.gns.ja.andel,quants),2)



# Lahman::Batting %>% select(playerID, yearID, stint, teamID)



view(discodata)


til.df.seg <- ddply(discodata,~membership,summarise,mean=mean(timelon.mean.gns))

test <- left_join(til.df.seg,discodata) %>% 	 select(disco,membership,mean,timelon.mean.gns, timelon.mean.seg.gns,mean)
view(test)


view(til.df.seg)

col_select <- function(df,
ret = c("df_select", "dplyr_code"),
top_n = 100) {
require(shiny)
require(miniUI)
require(dplyr)
require(DT)	
ret <- match.arg(ret)
stopifnot(is.data.frame(df))
df_head <- head(df, top_n)
ui <- miniPage(
gadgetTitleBar("Have your pick"),
miniContentPanel(
dataTableOutput("selection_df", height = "100%")
)
)
server <- function(input, output, session){
options(DT.options = list(pageLength = 10))
output$selection_df <- renderDataTable(
df_head, server = FALSE, selection = list(target = "column")
)
observeEvent(input$done, stopApp( input$selection_df_columns_selected))
}
cols_selected <- runGadget(ui, server)
if (ret == "df_select") {
return( df %>% select(cols_selected) )
} else {
df_name <- deparse(substitute(df))
colnames_selected <- colnames(df)[cols_selected] %>%
paste(collapse = ", ")
rstudioapi::insertText(
paste(df_name, " %>% select(", colnames_selected, ")", sep = "")
)
}
}
















































