colorsDouble <- function(colorfunc, numcolors) {
	colors0 <- rev(colorfunc((1+numcolors)/2))
	c(colors0, rev(if (numcolors %% 2 == 0) colors0 else head(colors0, -1)))
}
