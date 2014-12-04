# install.packages("devtools")  # so we can install from github
library("devtools")
# install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)

### Connect plotly
py <- plotly(username="B.Tian", key="s0asjdatdm")  # open plotly connection
### ggplot
pp <- function (n,r=4) {
        x <- seq(-r*pi, r*pi, len=n)
        df <- expand.grid(x=x, y=x)
        df$r <- sqrt(df$x^2 + df$y^2)
        df$z <- cos(df$r^2)*exp(-df$r/6)
        df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p <- p + geom_tile(aes(fill=z))

py$ggplotly(p)
### ploty
belt.df  <- read.csv("belt.csv")
ggBelt  <- ggplot() + geom_polygon(aes(x = long, y = lat, group=name, fill = name),
                     #geom_point(aes(x = long, y = lat, fill = name),
                     data = belt.df) +
        scale_fill_manual(name =  "Geological belt", values =cols)
py$ggplotly(ggBelt)
