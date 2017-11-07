# Title     : TODO
# Objective : TODO
# Created by: emilien
# Created on: 24/09/17

createHistogram <- function(n){
    val <- sort(runif(n))

    sturges <- 1 + log2(n)

    mini <- val[1]
    maxi <- val[n]

    ifelse(sturges < 5, sturges <- 5, ifelse(sturges > 20 , sturges <- 20, sturges <- round(sturges)))

    mini <- round(val[1]- 0.025 * (val[1]-val[n]))
    maxi <- round(val[n]+ 0.025 * (val[1]-val[n]))

    hist(val,prob=T,breaks=seq(mini,maxi,abs(maxi-mini)/sturges))
    lines(density(val))
}



createHistogramEffectif <- function(n){
    val <- sort(runif(n))

    sturges <- 1 + log2(n)

    ifelse(sturges < 5, sturges <- 5, ifelse(sturges > 20 , sturges <- 20, sturges <- round(sturges)))

    mini <- round(val[1]- 0.025 * (val[1]-val[n]))
    maxi <- round(val[n]+ 0.025 * (val[1]-val[n]))

    # print(paste(mini,"---",maxi,"---",abs(maxi-mini)/sturges))

    input = 1:n
    multiple_of_sturges = (input %% round(n/sturges)) == 0

    br <- c(mini)
    abs <- c()
    ord <- c()

    for(i in input[multiple_of_sturges]){
        br<- c(br,val[i])
    }

    for(i in 2:length(br)){
        abs <- c(abs,br[i-1]+(br[i]-br[i-1])/2)
    }

    br[length(br)+1] <- maxi

    res <- hist(val,prob=T,breaks=br)
    lines(abs,res$density[-length(res$density)])
}

# createHistogramEffectif(20)
createHistogram(1000)

