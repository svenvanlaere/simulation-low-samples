n1.vector <- c(3,3,3,4,4,4,5,5,5,8,8,8)
n2.vector <- c(3,4,5,4,5,6,5,6,7,8,9,10)
sigma <- c(1,2,3)

power <- list(ttest = matrix(0,3,length(n1.vector)),
              ttest.unequal = matrix(0,3,length(n1.vector)),
              mww = matrix(0,3,length(n1.vector)),
              perm = matrix(0,3,length(n1.vector)))

for(sigma.id in 1:length(sigma)){
  for(n.id in 1:length(n1.vector)){
    for(sim.id in 1:1000){
      g1 <- rnorm(n = n1.vector[n.id], mean = 0, sd = 1)
      g2 <- rnorm(n = n2.vector[n.id], mean = 0, sd = sigma[sigma.id])
      
      p.ttest         <- t.test(g1, g2, alternative = "two.sided", var.equal = TRUE)$p.value
      p.ttest.unequal <- t.test(g1, g2, alternative = "two.sided", var.equal = FALSE)$p.value
      p.mww           <- wilcox.test(g1, g2, alternative = "two.sided", exact = TRUE)$p.value
      value <- c(g1, g2); group <- c(rep(1,length(g1)), rep(2, length(g2)))
      data <- data.frame(value, group)
      p.perm   <- perm.test(value ~ group, data, paired = FALSE, exact = TRUE)$p.value
      
      if(p.ttest < 0.05){
        power[["ttest"]][sigma.id, n.id] <- power[["ttest"]][sigma.id, n.id] + 1
      } 
      if(p.ttest.unequal < 0.05){
        power[["ttest.unequal"]][sigma.id, n.id] <- power[["ttest.unequal"]][sigma.id, n.id] + 1
      } 
      if(p.mww < 0.05){
        power[["mww"]][sigma.id, n.id] <- power[["mww"]][sigma.id, n.id] + 1
      } 
      if(p.perm < 0.05){
        power[["perm"]][sigma.id, n.id] <- power[["perm"]][sigma.id, n.id] + 1
      }
    }
  }
}

# $`ttest`
#         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [sd = 1]  49   49   51   52   53   52   50   49   50    49    50    49
# [sd = 2]  65   45   33   59   44   34   61   46   37    55    51    41
# [sd = 3]  75   46   30   70   48   36   66   51   35    59    54    41
# 
# $ttest.unequal
#          [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [sd = 1]   36   40   46   43   48   46   44   46   47    46    49    48
# [sd = 2]   48   43   42   48   45   45   52   48   47    49    53    49
# [sd = 3]   51   46   46   53   49   50   52   53   47    48    56    52
# 
# $mww
#          [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [sd = 1]    0    0   36   28   35   39   32   31   47    49    45    42
# [sd = 2]    0    0   29   38   32   27   43   30   44    57    51    43
# [sd = 3]    0    0   29   49   37   33   46   33   51    61    59    49
# 
# $perm
#          [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [sd = 1]    0   20   26   18   40   41   36   42   42    41    46    46
# [sd = 2]    0   16   16   24   34   26   45   42   31    47    48    39
# [sd = 3]    0   16   15   34   38   31   51   45   30    53    51    38
