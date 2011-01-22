dens <- density(daysleft+as.numeric(today))
densdays <- as.Date(dens$x, origin="1970-01-01")
