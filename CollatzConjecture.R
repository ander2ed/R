
rm(list = ls())

dF <- data.frame(Num = c(), Iters = c())
tdf <- dF

for (Num in 1:10000){
  x <- Num
  Iters <- 0
  
  while(x != 1) {
    if(x %% 2 == 0) {
      x <- x / 2
    }
    else {
      x <- (x * 3) + 1
    }
    Iters <- Iters + 1
    #print(x)
  }
  flush.console()
  print(Num)
  
  tdf <- cbind(Num,Iters)
  dF <- rbind(dF, tdf)
  
}

plot(Iters ~ Num, pch = 20, cex = .5,col = c("red"), data = dF)

collatz.numbers <- function(x) {
  return.list <- list()
  return.list <- rbind(return.list, x)
  while(x != 1) {
    if(x %% 2 == 0) {
      x <- x / 2
      return.list <- rbind(return.list, x)
    }
    else {
      x <- (x * 3) + 1
      return.list <- rbind(return.list, x)
    }
  }
  return.list
}

collatz.numbers(10)
collatz.numbers(19)
