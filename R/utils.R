pn <- function(x, y) {
    m <- prettyNum(x, big.mark = ",", scientific = FALSE)
    paste0(paste(rep(" ", y - nchar(m)), collapse = ""), m)
}

pn2 <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)


# min max scaling function
minmax_scale <- function(x) {
	if(max(x) - min(x) == 0) return(stats::setNames(rep(1, length(x)), names(x)))
    (x - min(x))/(max(x) - min(x))
}

above <- function(x, threshhold) which(x >= threshhold)

rguid <- function(n=20){
    paste(sample(c(LETTERS, letters, 0:9), n, TRUE), collapse="")
}



