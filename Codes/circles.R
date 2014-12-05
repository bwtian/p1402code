facet_labels <- function(variable, value) {
        labels <- as.character(value)
        labels[labels == '(all)'] <- 'FOO'
        return (labels)
}

ggplot(mtcars, aes(mpg, wt)) + geom_point() +
        facet_grid(am ~ cyl, margins = "cyl", labeller = facet_labels)
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
        r = diameter / 2
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 100)
ggplot(dat,aes(x,y)) + geom_path()
g<-g+annotate("path",
              x=xc+r*cos(seq(0,2*pi,length.out=100)),
              y=yc+r*sin(seq(0,2*pi,length.out=100)))
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
        theta <- (0:npoints) * 2 * pi/npoints
        Circle <- cbind(cos(theta), sin(theta))
        t(center + scale * t(Circle %*% chol(cov)))
}
