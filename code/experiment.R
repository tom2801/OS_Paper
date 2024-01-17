maxNeighbourhoodSize <- 1000
bits <- 512
time <- 5*(10^6)

#simulate Tg for neighbourhood of size n
Tg <- function (sampleSize, n, expectedTime) {
  return (rgamma(sampleSize, n, 1/expectedTime))
}

#generates |Tg-Tghi| with a neighbourhood of size n for a certain number of bits
generateDiffs <- function (n, numberOfBits, expectedTime) {
  return (abs(diff(Tg (numberOfBits, n, expectedTime))))
}

input <- 1:maxNeighbourhoodSize
meansMax <- c()
meansMin <- c()

for (i in input) {
  temp <- generateDiffs(i, bits, time)
  
  medianTemp<-median(temp)
  
  meansMax<-c(meansMax, mean(temp[temp >= medianTemp]))
  meansMin<-c(meansMin, mean(temp[temp < medianTemp]))
}

plot(input, meansMax, type ='l', xlab="Neighbourhood size", ylab="Time difference")
lines(input, meansMin)
