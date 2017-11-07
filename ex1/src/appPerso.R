# Title     : TODO
# Objective : TODO
# Created by: emilien
# Created on: 07/11/17

mydata <- c(45/60,
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

marius <- function(mydata){
    mean(mydata)*2
}

jeannette <- function(mydata) {
    max(mydata)
}

n = 10000
m = vector("integer",n)
j = vector("integer",n)
for(i in 1:n){
    jeu = runif(5,0,10)
    m[i] = marius(jeu)
    j[i] = jeannette(jeu)
}

print(mean(m))
print(var(m))
print(mean(j)*(6/5))
print(var(j))


