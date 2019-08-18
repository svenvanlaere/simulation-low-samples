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

scenario <- data.frame(shape = c(3,15),
                       rate  = c(1,5))

power <- list(anova      = matrix(0,4,length(settings_n_k)),       
              anova.rank = matrix(0,4,length(settings_n_k)),
              kw         = matrix(0,4,length(settings_n_k)))

for(n.ind in 1:length(settings_n_k)){
  setting <- settings_n_k[[n.ind]]
  n <- sum(setting)
  for(id_sampling in 1:4){
    for(sim.ind in 1:1000){
      y <- vector()
      x <- vector()
      
      for(id_group in 1:length(setting)){
        n_k <- setting[id_group]
        
        if(id_sampling == 1){
          if(id_group == 1){
            y <- c(y, rnorm(mean = 3, sd = 1, n = n_k))
          } else{
            y <- c(y,rgamma (n = n_k, 
                             shape = scenario["shape"][1,], 
                             rate  = scenario["rate"][1,]))
          }
        } else if (id_sampling == 2){
          if(id_group == 1){
            y <- c(y, rnorm(mean = 3, sd = 1, n = n_k))
          } else{
            y <- c(y,rgamma (n = n_k, 
                             shape = scenario["shape"][2,], 
                             rate  = scenario["rate"][2,]))
          }
        } else if (id_sampling == 3){
          if(id_group == 1){
            y <- c(y, sample <- rgamma (n = n_k, 
                                        shape = scenario["shape"][1,], 
                                        rate  = scenario["rate"][1,]))
          } else{
            y <- c(y, rnorm(mean = 3, sd = 1, n = n_k))
          }
        } else if (id_sampling == 4){
          if(id_group == 1){
            y <- c(y, rgamma (n = n_k, 
                              shape = scenario["shape"][2,], 
                              rate  = scenario["rate"][2,]))
          } else{
            y <- c(y, rnorm(mean = 3, sd = 1, n = n_k))
          }
        }
        x <- c(x, rep(id_group, setting[id_group]))
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
        power[["anova"]][id_sampling, n.ind] <- power[["anova"]][id_sampling, n.ind] + 1
      } 
      if(p.aov.rank < 0.05){
        power[["anova.rank"]][id_sampling, n.ind] <- power[["anova.rank"]][id_sampling, n.ind] + 1
      } 
      if(p.kw < 0.05){
        power[["kw"]][id_sampling, n.ind] <- power[["kw"]][id_sampling, n.ind] + 1
      }
    }
  }
}
power
# $`anova`
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   61   50   58   46   53   52   50   41   58    54    55    50
# [2,]   56   40   61   72   55   47   50   79   60    51    62    59
# [3,]   60   78   52  100   64   54   67   93   61    46    67    81
# [4,]   46   55   52   39   61   64   69   41   55    49    47    43
# 
# $anova.rank
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   96   67   73   49   66   68   55   49   60    55    61    49
# [2,]   93   50   66   67   59   51   45   79   70    51    61    54
# [3,]   99   75   59   89   58   58   62   78   59    54    72    63
# [4,]   80   59   50   50   70   62   66   43   61    46    51    46
# 
# $kw
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]   15   54   61   39   25   38   42   32   24    35    45    33
# [2,]   16   35   59   53   19   35   38   56   26    34    48    38
# [3,]   21   60   48   76   18   33   48   56   20    29    53    37
# [4,]   14   51   43   37   30   43   53   37   20    28    37    34
