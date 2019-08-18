delta   <- c(0, 0, 0, 1, 1, 1, 1, 1, 1)
shift1  <- c(0, 3, 6, 0, 0, 6, 6, 3, 6)
shift2  <- c(0, 3, 6, 0, 6, 0, 6, 6, 3)
n1      <- c(3, 5, 8)
n2      <- c(5, 5, 5)
lambda1 <- c(0.25, 0.5, 0.75)
lambda2 <- c(0.75, 0.5, 0.25)

#dim 1 = group size   , dim 2 = mixture weight , dim 3 = mean shift
power <-
  array(0, dim = c(length(n1), length(lambda1), length(shift1), 4))

for (n_ind in 1:length(n1)) {
  #print(c(n1[n_ind], n2[n_ind]))
  for (mix_ind in 1:length(lambda1)) {
    #print(c(lambda1[mix_ind], lambda2[mix_ind]))
    for (shift_ind in 1:length(shift1)) {
      n_one <- n1[n_ind]
      n_two <- n2[n_ind]
      
      p.vector.equal.var <- vector(length = num_sim)
      p.vector.unequal.var <- vector(length = num_sim)
      p.vector.mww         <- vector(length = num_sim)
      p.vector.perm       <- vector(length = num_sim)
      for (simnumber in 1:num_sim) {
        n1.1 <- round(lambda1[mix_ind]*n_one)
        n1.2 <- n_one - n1.1
        n2.1 <- round(lambda2[mix_ind]*n_two)
        n2.2 <- n_two - n2.1
        
        g1 <- c(rnorm(n = n1.1, mean = 0, sd = 1),
                rnorm(n = n1.2, mean = 0 + shift1[shift_ind], sd = 1))
        
        g2 <- c(rnorm(n = n2.1, mean = 3 + delta[shift_ind] , sd = 1),
                rnorm(n = n2.2, mean = 3 + delta[shift_ind] + shift2[shift_ind], sd = 1)
        )
        data <-
          data.frame(value = c(g1, g2),
                     group = c(rep(1, n_one), rep(2, n_two)))
        
        test.equal   <-
          t.test(g1,
                 g2,
                 alternative = "two.sided",
                 var.equal = TRUE)
        test.unequal <-
          t.test(g1,
                 g2,
                 alternative = "two.sided",
                 var.equal = FALSE)
        test.mww     <-
          wilcox.test(g1, g2, alternative = "two.sided", exact = TRUE)
        test.perm   <- independence_test(value ~ group, data)
        
        p.vector.equal.var[simnumber]   <- test.equal$p.value
        p.vector.unequal.var[simnumber] <- test.unequal$p.value
        p.vector.mww[simnumber]         <- test.mww$p.value
        p.vector.perm[simnumber]        <- pvalue(test.perm)
      }
      p.equal   <- length(which(p.vector.equal.var < 0.05)) / 1000
      p.unequal <- length(which(p.vector.unequal.var < 0.05)) / 1000
      p.mww     <- length(which(p.vector.mww < 0.05)) / 1000
      p.perm   <- length(which(p.vector.perm < 0.05)) / 1000
      
      power[n_ind, mix_ind, shift_ind, ] <-
        c(p.equal, p.unequal, p.mww, p.perm)
      
      print(c("delta", delta[shift_ind], "shift 1", shift1[shift_ind], "shift 2", shift2[shift_ind]))
      print(c(p.equal, p.unequal, p.mww, p.perm))
    }
  }
}