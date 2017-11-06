data <- c(45/60,
1+35/60,
1+40/60,
1+45/60,
2+50/60,
3+35/60,
4+25/60,
4+45/60,
5+5/60,
5+10/60,
6+5/60,
6+15/60,
7+10/60,
7+35/60,
8+15/60,
8+25/60,
8+45/60,
9+10/60,
9+35/60,
9+40/60)

print(sum(data)/length(data))


plot(data)
t = mean(data) * 60
abline(h=t/60)

compareExp(data)
compareUnif(data)
compareNorm(data)

s = t %% 60
m = (t - s)/60
print(paste(m,'minutes',s, "secondes"))

simu <- function(n) {
    m <- c()
    j <- c()

    for (i in 1:n) {
        ru = runif(20, min = 0, max = 15)
        #ru = rnorm(20, mean=10)
        m[i] = 2 * mean(ru)
        j[i] <- max(ru)
    }

    #  print(paste('Marius : ', mean(m), 'Jeannette', mean(j)))
    return(c(mean(m), mean(j)))
}

x = simu(100)

printSimu <- function(n) {
    m <- c()
    for (i in 1:n) {
        x <- simu(i)
        m[i] = x[1]
    }
    plot(m)
}

printSimu(1000)