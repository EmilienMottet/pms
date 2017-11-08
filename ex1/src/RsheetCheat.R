# Title     : TODO
# Objective : TODO
# Created by: emilien
# Created on: 06/11/17
# https://www.agroparistech.fr/IMG/pdf/Simul-VA.pdf
# https://www.agroparistech.fr/IMG/pdf/Simul-VA.pdf

sturges  <- function(lenData){
    sturges = 1 + log2(lenData)
    ifelse(sturges < 5, sturges <- 5, ifelse(sturges > 20 , sturges <- 20, sturges <- round(sturges)))
    return(sturges)
}

miniHist <- function(first,last) { first - 0.025 * (last-first) }

maxHist <- function(first,last) { last + 0.025 * (last-first) }

createHistogramSameWid <- function(d){

    sturges = sturges(length(d))

    mini = miniHist(d[1],d[length(d)])
    maxi = maxHist(d[1],d[length(d)])

    abs <- c()
    br <- seq(mini,maxi,abs(maxi-mini)/sturges)

    for(i in 2:length(br)){
        abs <- c(abs,br[i-1]+(br[i]-br[i-1])/2)
    }

    res = hist(d,prob=T,breaks=br)
    lines(abs,res$density)

}

createHistogramSameEff <- function(d){

    sturges = sturges(length(d))

    mini = miniHist(d[1],d[length(d)])
    maxi = maxHist(d[1],d[length(d)])

    abs <- c()
    br <- c(mini,quantile(d,seq(1,sturges-1)/sturges),maxi)

    print(quantile(d,seq(1,length(d)-1)/length(d)))
    print(br)

    for(i in 2:length(br)){
        abs <- c(abs,br[i-1]+(br[i]-br[i-1])/2)
    }

    res = hist(d,prob=T,breaks=br)
    lines(abs,res$density)
}

compareExp = function(data) {
    x <- sort(data)
    y <- qexp(seq(1:length(data))/length(data))
    plot(x,y)
    abline(lm(y[-length(data)] ~ x[-length(data)]))
}

compareExpCustom = function(data) {
    x <- sort(data)
    eq <- function(u,p = 1){
        (-log(1-u))/p
    }
    y <- eq(seq(1:length(data))/length(data))
    plot(x,y)
    abline(lm(y[-length(data)] ~ x[-length(data)]))
}

compareNorm = function(data) {
    x <- sort(data)
    y <- qnorm(seq(1:length(data))/length(data))
    plot(x,y)
    abline(lm(y[-length(data)] ~ x[-length(data)]))
}

compareUnif = function(data) {
    x <- sort(data)
    y <- qunif(seq(1:length(data))/length(data))
    plot(x,y)
    abline(lm(y ~ x))
}

mydata <- read.csv(file="~/Documents/Cours/Stats/dataDS.csv", header=FALSE)$V1
# createHistogramSameWid(mydata)
# createHistogramSameEff(mydata)

# compareExp(mydata)

# plot(sort(mydata))

