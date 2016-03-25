pn <- function(x, y) {
    m <- prettyNum(x, big.mark = ",", scientific = FALSE)
    paste0(paste(rep(" ", y - nchar(m)), collapse = ""), m)
}

pn2 <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)


# min max scaling function
min_max <- function(x) (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))


above <- function(x, threshhold) which(x >= threshhold)

rguid <- function(n=20){
    paste(sample(c(LETTERS, letters, 0:9), n, TRUE), collapse="")
}

