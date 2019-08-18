settings_n_k <- list(c(3,3,3),
                     c(5,5,5),
                     c(7,7,7),
                     c(3,5,7),
                     c(3,3,3,3,3),
                     c(5,5,5,5,5),
                     c(7,7,7,7,7),
                     c(3,5,5,7,7),
                     c(3,3,3,3,3,3,3),
                     c(5,5,5,5,5,5,5),
                     c(7,7,7,7,7,7,7),
                     c(3,5,5,5,7,7,7))

power <- list(anova      = matrix(0,1,length(settings_n_k)),        #3 for different lambdas
              anova.rank = matrix(0,1,length(settings_n_k)),
              kw         = matrix(0,1,length(settings_n_k)))

for(n.ind in 1:length(settings_n_k)){
  setting <- settings_n_k[[n.ind]]
  n <- sum(setting)
  
  for(sim.ind in 1:1000){
    y <- rnorm(mean = 0, sd = 1, n = n)
    x <- vector()
    
    for(id.group in 1:length(setting)){
      x <- c(x, rep(id.group, setting[id.group]))
    }
    x <- factor(x)
    
    #One-way anova
    model.aov     <- anova(lm(y~x))
    p.aov         <- model.aov$`Pr(>F)`[1]
    
    #Kruskal-Wallis
    model.kruskal <- kruskal.test(y~x)
    p.kw          <- model.kruskal$p.value
    
    #One-way ANOVA on ranks
    y.rank        <- rank(y)
    model.rank    <- anova(lm(y.rank~x))
    p.aov.rank    <- model.rank$`Pr(>F)`[1]
    
    if(p.aov < 0.05){
      power[["anova"]][1, n.ind] <- power[["anova"]][1, n.ind] + 1
    } 
    if(p.aov.rank < 0.05){
      power[["anova.rank"]][1, n.ind] <- power[["anova.rank"]][1, n.ind] + 1
    } 
    if(p.kw < 0.05){
      power[["kw"]][1, n.ind] <- power[["kw"]][1, n.ind] + 1
    }
  }
}
power

# $`anova`
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   47   47   52   40   51   43   46   50   46    49    43    42
# 
# $anova.rank
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   81   52   53   45   53   58   47   53   53    59    51    39
# 
# $kw
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   14   39   44   31   20   35   33   34   22    42    40    27