# Title     : TODO
# Objective : TODO
# Created by: emilien
# Created on: 09/10/17


compareExp = function(data) {
    x <- sort(data)
    y <- qexp(seq(1:length(data))/length(data))
    plot(x,y)
    abline(lm(y[-length(data)] ~ x[-length(data)]))
}

compareNorm = function(data) {
    x <- sort(data)
    y <- qnorm(seq(1:length(data))/length(data))
    plot(x,y)
    print(lm(y[-length(data)] ~ x[-length(data)]))
    abline(lm(y[-length(data)] ~ x[-length(data)]))
}

compareUnif = function(data) {
    x <- sort(data)
    y <- qunif(seq(1:length(data))/length(data))
    plot(x,y)
    lm(y ~ x)
    abline(lm(y ~ x))
}

data <- c(54.8,55.4,57.7,59.6,60.1,61.2,62.0,63.1,63.5,64.2,
65.2,65.4,65.9,66.0,67.6,68.1,69.5,70.6,71.5,73.4)

# compareUnif(data)
# compareNorm(data)
compareExp(data)


# page 25 en bas