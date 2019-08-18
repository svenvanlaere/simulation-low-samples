nu1      <- c(  1,   1,   3,   3,   1,   1,   3,   1)
tau1     <- c(1.5, 0.5, 1.5, 0.5, 1.5, 0.5, 0.5, 1.5)
nu2      <- c(  1,   1,   3,   3,   3,   3,   1,   3)
tau2     <- c(1.5, 0.5, 1.5, 0.5, 1.5, 1.5, 1.5, 1.5)
n1.vect  <- c(3, 5, 8)
n2.vect  <- c(5, 5, 5)
diff.rate <- seq(0, 6, by = 0.1)   #make a vector to shift distance from 0 to 6

#define colours
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 4
cols = gg_color_hue(n)

i <- 1

for(setting_vector_ind in 1:length(nu1)){
  for(n_vector_ind in 1:length(n1.vect)){
    n1 <- n1.vect[n_vector_ind]
    n2 <- n2.vect[n_vector_ind]
    
    power <-
      data.frame(
        t.equal   = vector(length = length(diff.rate)),
        #first item for T-test equal variances
        t.unequal = vector(length = length(diff.rate)),
        #second item for T-test equal variances
        mww       = vector(length = length(diff.rate)),
        #third item for Mann-Whitney U test
        perm      = vector(length = length(diff.rate))    #fourth item for permutation test
      )
    
    for(diff in 1:length(diff.rate)){
      p.vector.equal.var       <- vector(length = num_sim)
      p.vector.unequal.var     <- vector(length = num_sim)
      p.vector.mww             <- vector(length = num_sim)
      p.vector.perm            <- vector(length = num_sim)
      log.p.vector.equal.var   <- vector(length = num_sim)
      log.p.vector.unequal.var <- vector(length = num_sim)
      log.p.vector.mww         <- vector(length = num_sim)
      log.p.vector.perm        <- vector(length = num_sim)
      for (simnumber in 1:num_sim) {
        g1 <- rSHASHo(n = n1, mu = 0, sigma = 1, nu = nu1[setting_vector_ind], tau = tau1[setting_vector_ind])
        g2 <- diff.rate[diff] + rSHASHo(n = n2, mu = 0, sigma = 1, nu = nu2[setting_vector_ind], tau = tau2[setting_vector_ind])
        
        data <-
          data.frame(value = c(g1, g2),
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
        test.perm   <- perm.test(value ~ group, data, paired = FALSE, exact = TRUE)
        
        p.vector.equal.var[simnumber]   <- test.equal$p.value
        p.vector.unequal.var[simnumber] <- test.unequal$p.value
        p.vector.mww[simnumber]         <- test.mww$p.value
        p.vector.perm[simnumber]        <- test.perm$p.value
        
        count.equal   <- length(which(p.vector.equal.var < 0.05))
        count.unequal <- length(which(p.vector.unequal.var < 0.05))
        count.mww     <- length(which(p.vector.mww < 0.05))
        count.perm   <- length(which(p.vector.perm < 0.05))
        
        power[[1]][diff] <- (count.equal   / num_sim)
        power[[2]][diff] <- (count.unequal / num_sim)
        power[[3]][diff] <- (count.mww     / num_sim)
        power[[4]][diff] <- (count.perm    / num_sim)
      }
      
    }
    
    result <- data.frame(diff.rate, power)
    plot <- ggplot(result) +
      geom_line(aes(x = diff.rate, y = t.equal, colour = "t.equal"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = t.unequal, colour = "t.unequal"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = mww, colour = "mww"), 
                size = 1.2) +
      geom_line(aes(x = diff.rate, y = perm, colour = "perm"), 
                size = 1.2) +
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
            subtitle = paste("mu1 = mu2 = 0; sd1 = sd2 = 1;   ",
                             "nu1 = ",nu1[setting_vector_ind],
                             ", tau1 = ",tau1[setting_vector_ind],
                             ", nu2 = ",nu2[setting_vector_ind],
                             ", tau2 = ",tau2[setting_vector_ind],
                             " (n1 = ", n1, " & n2 = ", n2, ")", sep = ""),
            x = "Difference in mu",
            y = "Power",
            color = "test"
      ) + 
      ylim(0, 1)
    
    print(plot)
    
    name <- paste("Rplot", ifelse(i<10,"0",""), i, ".png", sep="")
    png(name)
    print(plot)
    dev.off()
    
    i <- i + 1
  }
}