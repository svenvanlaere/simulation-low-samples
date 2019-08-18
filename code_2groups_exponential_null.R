lambda <- c(1, 1/4, 1/7)

n1.vector <- c(3,4,5,4,5,6,5,6,7,8,9,10)
n2.vector <- c(3,3,3,4,4,4,5,5,5,8,8,8)

power <- list(ttest = matrix(0,3,length(n1.vector)),        #3 for different lambdas
              ttest.unequal = matrix(0,3,length(n1.vector)),
              mww = matrix(0,3,length(n1.vector)),
              perm = matrix(0,3,length(n1.vector)))

for(n.ind in 1:length(n1.vector)){
  for(lambda.ind in 1:length(lambda)){
    for(sim.ind in 1:1000){
      g1 <- c(rexp(n = n1.vector[n.ind], lambda[lambda.ind]))
      g2 <- c(rexp(n = n2.vector[n.ind], lambda[lambda.ind]))
      
      p.ttest         <- t.test(g1, g2, alternative = "two.sided", var.equal = TRUE)$p.value
      p.ttest.unequal <- t.test(g1, g2, alternative = "two.sided", var.equal = FALSE)$p.value
      p.mww           <- wilcox.test(g1, g2, alternative = "two.sided", exact = TRUE)$p.value
      value <- c(g1, g2); group <- c(rep(1,length(g1)), rep(2, length(g2)))
      data <- data.frame(value, group)
      p.perm   <- perm.test(value ~ group, data, paired = FALSE, exact = TRUE)$p.value
      
      if(p.ttest < 0.05){
        power[["ttest"]][lambda.ind, n.ind] <- power[["ttest"]][lambda.ind, n.ind] + 1
      } 
      if(p.ttest.unequal < 0.05){
        power[["ttest.unequal"]][lambda.ind, n.ind] <- power[["ttest.unequal"]][lambda.ind, n.ind] + 1
      } 
      if(p.mww < 0.05){
        power[["mww"]][lambda.ind, n.ind] <- power[["mww"]][lambda.ind, n.ind] + 1
      } 
      if(p.perm < 0.05){
        power[["perm"]][lambda.ind, n.ind] <- power[["perm"]][lambda.ind, n.ind] + 1
      }
    }
  }
}
power
# $`ttest`
#                [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [lambda = 1/1]   39   38   39   37   38   32   32   42   31    42    51    44
# [lambda = 1/4]   43   36   46   34   46   43   47   33   34    34    37    37
# [lambda = 1/7]   40   41   38   33   40   44   40   36   44    44    46    47
# 
# $ttest.unequal
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   26   24   28   23   26   24   21   26   31    32    39    35
# [2,]   25   30   40   19   32   36   34   26   29    31    30    30
# [3,]   30   24   27   19   22   39   24   25   35    34    42    41
# 
# $mww
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    0    0   32   31   37   29   28   33   48    52    53    50
# [2,]    0    0   37   31   38   42   39   30   40    39    54    37
# [3,]    0    0   31   28   30   40   33   29   52    57    50    44
# 
# $perm
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    0   26   20   14   38   30   24   38   43    43    52    53
# [2,]    0   19   22   15   40   49   46   39   33    34    40    44
# [3,]    0   21   25   13   35   42   37   43   47    44    51    48