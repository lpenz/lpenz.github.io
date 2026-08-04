
f <- function(spaceleft) {
    days <- 0
    while(spaceleft > 0) {
        days <- days + 1
        spaceleft <- spaceleft - sample(dudelta, 1, replace=TRUE)
    }
    days
}

