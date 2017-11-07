# Title     : TODO
# Objective : TODO
# Created by: emilien
# Created on: 07/11/17

mylaw <- function(mydata) {
    1/mean(mydata)
}

n = 100000
nData = 5000
m = vector("integer",n)
lambda = 2
for(i in 1:n){
    jeu = rexp(nData,lambda)
    m[i] = mylaw(jeu)
}

print(lambda/mean(m))
print(mean(m))
print(mean(m)*(nData-1)/nData)
print(var(m))