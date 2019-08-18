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

scenarios <- list(data.frame(nu  = c(  1,  1),
                             tau = c(0.5,0.5)),
                  data.frame(nu  = c(  1,  1),
                             tau = c(1.5,1.5)),
                  data.frame(nu  = c(  3,  3),
                             tau = c(1.5,1.5)))

power <- list(anova      = matrix(0,3,length(settings_n_k)),        #3 for different lambdas
              anova.rank = matrix(0,3,length(settings_n_k)),
              kw         = matrix(0,3,length(settings_n_k)))

for(n.ind in 1:length(settings_n_k)){
  setting <- settings_n_k[[n.ind]]
  n <- sum(setting)
  for(SASnormal.setting.ind in 1:3){
    for(sim.ind in 1:1000){
      y <- vector()
      x <- vector()
      
      for(id.group in 1:length(setting)){
        if(id.group == 1){
          
          y <- c(y, rSHASHo(n = setting[id.group], mu = 0, sigma = 1,
                            nu = scenarios[[SASnormal.setting.ind]]["nu"][1,],
                            tau = scenarios[[SASnormal.setting.ind]]["tau"][1,]))
        } else{
          y <- c(y, rSHASHo(n = setting[id.group], mu = 0, sigma = 1,
                            nu = scenarios[[SASnormal.setting.ind]]["nu"][2,],
                            tau = scenarios[[SASnormal.setting.ind]]["tau"][2,]))
        }
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
        power[["anova"]][SASnormal.setting.ind, n.ind] <- power[["anova"]][SASnormal.setting.ind, n.ind] + 1
      } 
      if(p.aov.rank < 0.05){
        power[["anova.rank"]][SASnormal.setting.ind, n.ind] <- power[["anova.rank"]][SASnormal.setting.ind, n.ind] + 1
      } 
      if(p.kw < 0.05){
        power[["kw"]][SASnormal.setting.ind, n.ind] <- power[["kw"]][SASnormal.setting.ind, n.ind] + 1
      }
    }
  }
}
power

# $`anova`
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   34   17   29   34   42   39   36   45   46    40    42    46
# [2,]   63   41   60   46   38   45   43   48   64    46    58    51
# [3,]   56   51   51   50   67   46   44   44   44    58    51    50
# 
# $anova.rank
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   91   54   64   48   59   60   49   57   55    62    53    51
# [2,]   91   44   56   53   46   57   49   54   56    49    56    47
# [3,]   90   55   57   52   64   55   47   42   61    56    60    41
# 
# $kw
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   12   38   53   35   19   39   42   36   25    36    41    42
# [2,]   14   35   47   36   22   35   35   32   17    29    43    29
# [3,]   17   44   47   38   23   40   38   29   13    35    46    32