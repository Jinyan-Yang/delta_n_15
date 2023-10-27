devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
palette(c(col.df$iris[c(1,2,3,5)],col.df$daisy,col.df$beauty[c(2,5)],col.df$bushBySea[5],col.df$cityNight[4]))

# Developmental version
# CRAN version
# install.packages("ggthemes")
library(ggthemes)
c.f <- tableau_color_pal(
  palette = "Tableau 10",
  type = "regular",
  direction = 1
)
palette(c(c.f(10),col.df$cityNight[4])[c(1,4,5,9,10,8,3,7,6,2,11)])
plot(1:11,pch=15,col=palette(),cex=6)
