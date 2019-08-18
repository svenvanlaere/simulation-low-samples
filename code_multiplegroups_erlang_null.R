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

scenarios <- list(data.frame(shape = c(3,3),
                             rate  = c(1,1)),
                  data.frame(shape = c(15,3),
                             rate  = c( 5,1)),
                  data.frame(shape = c(3,15),
                             rate  = c(1, 5)))

power <- list(anova      = matrix(0,3,length(settings_n_k)),        #3 for different scenarios
              anova.rank = matrix(0,3,length(settings_n_k)),
              kw         = matrix(0,3,length(settings_n_k)))

for(n.ind in 1:length(settings_n_k)){
  setting <- settings_n_k[[n.ind]]
  n <- sum(setting)
  for(erlang.setting.ind in 1:3){
    for(sim.ind in 1:1000){
      y <- vector()
      x <- vector()
      
      for(id.group in 1:length(setting)){
        if(id.group == 1){
          y <- c(y, rgamma(n = setting[id.group],
                           shape = scenarios[[erlang.setting.ind]]["shape"][1,],
                           rate = scenarios[[erlang.setting.ind]]["rate"][1,]))
        } else{
          y <- c(y, rgamma(n = setting[id.group],
                           shape = scenarios[[erlang.setting.ind]]["shape"][2,],
                           rate = scenarios[[erlang.setting.ind]]["rate"][2,]))
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
        power[["anova"]][erlang.setting.ind, n.ind] <- power[["anova"]][erlang.setting.ind, n.ind] + 1
      } 
      if(p.aov.rank < 0.05){
        power[["anova.rank"]][erlang.setting.ind, n.ind] <- power[["anova.rank"]][erlang.setting.ind, n.ind] + 1
      } 
      if(p.kw < 0.05){
        power[["kw"]][erlang.setting.ind, n.ind] <- power[["kw"]][erlang.setting.ind, n.ind] + 1
      }
    }
  }
}
power
# $`anova`
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   36   47   41   48   53   49   47   37   61    40    47    41
# [2,]   57   50   69   38   66   50   58   35   48    56    53    28
# [3,]   70   81   85  140   97   92   97  127   83    91    77   126
# 
# $anova.rank
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   73   50   39   57   67   62   40   50   62    37    54    44
# [2,]   93   69   72   44   75   70   81   43   66    59    69    37
# [3,]  110   78   88  126   84   75   76   96   64    70    70    67
# 
# $kw
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   10   40   31   40   23   39   28   28   21    25    42    29
# [2,]   22   56   59   31   27   46   61   27   21    42    45    26
# [3,]   12   64   81  105   27   61   62   79   25    47    53    48