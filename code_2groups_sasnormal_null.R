nu.vector  <- c(  1,  1,  3,  3,  1,  1,  3,  3,  1,  1,  3,  3)
tau.vector <- c(0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5,0.5,1.5)

n1.vector <- c(3,3,3,3,5,5,5,5,8,8,8,8)
n2.vector <- c(5,5,5,5,5,5,5,5,5,5,5,5)

power <- list(ttest =         matrix(0,1,length(n1.vector)),
              ttest.unequal = matrix(0,1,length(n1.vector)),
              mww =           matrix(0,1,length(n1.vector)),
              perm =          matrix(0,1,length(n1.vector)))

for(n.ind in 1:length(n1.vector)){
  for(sim.ind in 1:1000){
    g1 <- c(rSHASHo(n = n1.vector[n.ind], nu = nu.vector[n.ind], tau = tau.vector[n.ind]))
    g2 <- c(rSHASHo(n = n2.vector[n.ind], nu = nu.vector[n.ind], tau = tau.vector[n.ind]))
    
    p.ttest         <- t.test(g1, g2, alternative = "two.sided", var.equal = TRUE)$p.value
    p.ttest.unequal <- t.test(g1, g2, alternative = "two.sided", var.equal = FALSE)$p.value
    p.mww           <- wilcox.test(g1, g2, alternative = "two.sided", exact = TRUE)$p.value
    value <- c(g1, g2); group <- c(rep(1,length(g1)), rep(2, length(g2)))
    data <- data.frame(value, group)
    p.perm   <- perm.test(value ~ group, data, paired = FALSE, exact = TRUE)$p.value
    
    
    if(p.ttest < 0.05){
      power[["ttest"]][1, n.ind] <- power[["ttest"]][1, n.ind] + 1
    } 
    if(p.ttest.unequal < 0.05){
      power[["ttest.unequal"]][1, n.ind] <- power[["ttest.unequal"]][1, n.ind] + 1
    } 
    if(p.mww < 0.05){
      power[["mww"]][1, n.ind] <- power[["mww"]][1, n.ind] + 1
    } 
    if(p.perm < 0.05){
      power[["perm"]][1, n.ind] <- power[["perm"]][1, n.ind] + 1
    }
  }
}
power
# $`ttest`
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   28   54   35   40   31   51   21   39   29    60    40    53
# 
# $ttest.unequal
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   18   50   25   36   15   44   15   32   29    54    13    52
# 
# $mww
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   48   35   43   28   37   30   37   29   51    53    48    45
# 
# $perm
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   27   29   24   14   37   33   37   29   39    52    48    42