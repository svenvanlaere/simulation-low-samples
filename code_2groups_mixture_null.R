mu1 <- c(0,0,4,4)
mu2 <- c(3,6,7,10)

n1.vector <- c(3,5,8)
n2.vector <- c(5,5,5)

lambda1 <- c(0.25, 0.5, 0.75)
lambda2 <- 1 - lambda1

n1.1 <- round(n1.vector*lambda1)
n1.2 <- n1.vector - n1.1
n2.1 <- round(n2.vector*lambda1)
n2.2 <- n2.vector - n2.1

power <- list(ttest = matrix(0,3,length(n1.vector)*length(mu1)),        #3 for different lambdas
              ttest.unequal = matrix(0,3,length(n1.vector)*length(mu1)),
              mww = matrix(0,3,length(n1.vector)*length(mu1)),
              perm = matrix(0,3,length(n1.vector)*length(mu1)))

for(n.ind in 1:length(n1.vector)){
  for(mu.ind in 1:length(mu1)){
    for(sim.ind in 1:1000){
      g1 <- c(rnorm(n1.1[n.ind], mean = mu1[mu.ind], sd = 1),
              rnorm(n1.2[n.ind], mean = mu2[mu.ind], sd = 1))
      g2 <- c(rnorm(n2.1[n.ind], mean = mu1[mu.ind], sd = 1),
              rnorm(n2.2[n.ind], mean = mu2[mu.ind], sd = 1))
      
      p.ttest         <- t.test(g1, g2, alternative = "two.sided", var.equal = TRUE)$p.value
      p.ttest.unequal <- t.test(g1, g2, alternative = "two.sided", var.equal = FALSE)$p.value
      p.mww           <- wilcox.test(g1, g2, alternative = "two.sided", exact = TRUE)$p.value
      value <- c(g1, g2); group <- c(rep(1,length(g1)), rep(2, length(g2)))
      data <- data.frame(value, group)
      p.perm   <- perm.test(value ~ group, data, paired = FALSE, exact = TRUE)$p.value
      
      if(p.ttest < 0.05){
        power[["ttest"]][n.ind, ((n.ind-1)*3+mu.ind)] <- power[["ttest"]][n.ind, ((n.ind-1)*3+mu.ind)] + 1
      } 
      if(p.ttest.unequal < 0.05){
        power[["ttest.unequal"]][n.ind, ((n.ind-1)*3+mu.ind)] <- power[["ttest.unequal"]][n.ind, ((n.ind-1)*3+mu.ind)] + 1
      } 
      if(p.mww < 0.05){
        power[["mww"]][n.ind, ((n.ind-1)*3+mu.ind)] <- power[["mww"]][n.ind, ((n.ind-1)*3+mu.ind)] + 1
      } 
      if(p.perm < 0.05){
        power[["perm"]][n.ind, ((n.ind-1)*3+mu.ind)] <- power[["perm"]][n.ind, ((n.ind-1)*3+mu.ind)] + 1
      }
    }
  }
}
# $`ttest`
#           lambda=0.25    |      lambda=0.50  |      lambda = 0.75    
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [3-5]    5    0    2    0    0    0    0    0    0     0     0     0
# [5-5]    0    0    0    0    0    0    0    0    0     0     0     0
# [8-5]    0    0    0    0    0    0    6    0    3     0     0     0

# $ttest.unequal
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    4    0    0    0    0    0    0    0    0     0     0     0
# [2,]    0    0    0    0    0    0    0    0    0     0     0     0
# [3,]    0    0    0    0    0    0    1    0    4     0     0     0
# 
# $mww
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    2    0    1    0    0    0    0    0    0     0     0     0
# [2,]    0    0    0    0    0    0    0    0    0     0     0     0
# [3,]    0    0    0    0    0    0    9    1    3     1     0     0
# 
# $perm
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    1    0    1    0    0    0    0    0    0     0     0     0
# [2,]    0    0    0    0    0    0    0    0    0     0     0     0
# [3,]    0    0    0    0    0    0    3    0    4     0     0     0