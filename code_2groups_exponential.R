diff.rate   <- seq(0, 6, by = 0.1)   #make a vector to shift rate from 0 to 6
diff.n   <- c(0, 1, 2)

# two groups (unequal size vector)
n.vector <- c(3, 4, 5, 8)

for(n_vector_ind in 1:length(n.vector)){
  for(diff_n_ind in 1:length(diff.n)){
    n1 <- n.vector[n_vector_ind]
    n2 <- n.vector[n_vector_ind] + diff.n[diff_n_ind]
    
    power <-
      data.frame(
        t.equal   = vector(length = length(diff.rate)),
        #first item for T-test equal variances
        t.unequal = vector(length = length(diff.rate)),
        #second item for T-test equal variances
        mww       = vector(length = length(diff.rate)),
        #third item for Mann-Whitney U test
        perm      = vector(length = length(diff.rate)),    #fourth item for permutation test
        log.t.equal   = vector(length = length(diff.rate)),
        #first item for T-test equal variances
        log.t.unequal = vector(length = length(diff.rate)),
        #second item for T-test equal variances
        log.mww       = vector(length = length(diff.rate)),
        #third item for Mann-Whitney U test
        log.perm      = vector(length = length(diff.rate))    #fourth item for permutation test
      )
    
    for(diff in 1:length(diff.rate)){
      p.vector.equal.var <- vector(length = num_sim)
      p.vector.unequal.var <- vector(length = num_sim)
      p.vector.mww         <- vector(length = num_sim)
      p.vector.perm       <- vector(length = num_sim)
      log.p.vector.equal.var <- vector(length = num_sim)
      log.p.vector.unequal.var <- vector(length = num_sim)
      log.p.vector.mww         <- vector(length = num_sim)
      log.p.vector.perm       <- vector(length = num_sim)
      for (simnumber in 1:num_sim) {
        g1 <- rexp(n = n1, rate = 1)
        g2 <- rexp(n = n2, rate = 1/(1 + diff.rate[diff]))
        log.g1 <- log(g1)
        log.g2 <- log(g2)
        
        data <-
          data.frame(value = c(g1, g2),
                     group = c(rep(1, n1), rep(2, n2)))
        
        log.data <-
          data.frame(value = c(log.g1, log.g2),
                     group = c(rep(1, n1), rep(2, n2)))
        
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
        test.perm   <- perm.test(value ~ group, data, paired = FALSE, alternative = "two.sided")
        
        
        log.test.equal   <-
          t.test(log.g1,
                 log.g2,
                 alternative = "two.sided",
                 var.equal = TRUE)
        log.test.unequal <-
          t.test(log.g1,
                 log.g2,
                 alternative = "two.sided",
                 var.equal = FALSE)
        log.test.mww     <-
          wilcox.test(log.g1, log.g2, alternative = "two.sided", exact = TRUE)
        log.test.perm   <- perm.test(value ~ group, log.data, paired = FALSE, alternative = "two.sided")
        
        p.vector.equal.var[simnumber]   <- test.equal$p.value
        p.vector.unequal.var[simnumber] <- test.unequal$p.value
        p.vector.mww[simnumber]         <- test.mww$p.value
        p.vector.perm[simnumber]        <- test.perm$p.value
        log.p.vector.equal.var[simnumber]   <- log.test.equal$p.value
        log.p.vector.unequal.var[simnumber] <- log.test.unequal$p.value
        log.p.vector.mww[simnumber]         <- log.test.mww$p.value
        log.p.vector.perm[simnumber]        <- log.test.perm$p.value
        
        count.equal   <- length(which(p.vector.equal.var < 0.05))
        count.unequal <- length(which(p.vector.unequal.var < 0.05))
        count.mww     <- length(which(p.vector.mww < 0.05))
        count.perm   <- length(which(p.vector.perm < 0.05))
        log.count.equal   <- length(which(log.p.vector.equal.var < 0.05))
        log.count.unequal <- length(which(log.p.vector.unequal.var < 0.05))
        log.count.mww     <- length(which(log.p.vector.mww < 0.05))
        log.count.perm   <- length(which(log.p.vector.perm < 0.05))
        power[[1]][diff] <- (count.equal   / num_sim)
        power[[2]][diff] <- (count.unequal / num_sim)
        power[[3]][diff] <- (count.mww     / num_sim)
        power[[4]][diff] <- (count.perm    / num_sim)
        power[[5]][diff] <- (log.count.equal   / num_sim)
        power[[6]][diff] <- (log.count.unequal / num_sim)
        power[[7]][diff] <- (log.count.mww     / num_sim)
        power[[8]][diff] <- (log.count.perm    / num_sim)
      }
      
    }
    
    result <- data.frame(diff.rate, power)
    plot <- ggplot(result) +
      geom_line(aes(x = diff.rate, y = t.equal, colour = "t.equal", lty="no_transformation"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = t.unequal, colour = "t.unequal", lty="no_transformation"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = mww, colour = "mww", lty="no_transformation"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = perm, colour = "perm", lty="no_transformation"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate,y = log.t.equal,colour = "t.equal", lty="log_transformation"),
                size = 1.2) +
      geom_line(aes(x = diff.rate,y = log.t.unequal,colour = "t.unequal", lty="log_transformation"),
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = log.mww, colour = "mww", lty="log_transformation"),
                size = 1.2) +
      geom_line(aes(x = diff.rate,y = log.perm, colour = "perm", lty="log_transformation"),
                size = 1.2) +
      scale_linetype_manual(values=c(no_transformation = "solid", 
                                     log_transformation = "dotdash"))+
      scale_colour_manual(
        values = c(
          t.equal = cols[1],
          t.unequal = cols[2],
          mww = cols[3],
          perm = cols[4]
        ),
        name = "Testing"
      ) +
      labs( title = paste("Power simulation for 2 groups"), 
            subtitle = paste("(n1 = ", n1, " & n2 = ", n2, ")", sep = ""),
            x = "Difference in means",
            y = "Power",
            color = "test"
      ) + 
      ylim(0, 1)
    
    name <- paste("Rplot2_exp_n",n1, "_", n2,".png", sep = "")
    
    png(name)
    print(plot)
    dev.off()
    
  }
}