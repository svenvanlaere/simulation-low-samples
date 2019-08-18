# Normal distribution
diff.mean <-
  seq(0, 6, by = 0.1) #make a vector to shift mean from 0 to 6
diff.sd   <-
  seq(0, 2, by = 1)   #make a vector to shift sd   from 0 to 2
diff.n   <- c(0, 1, 2)

# two groups (unequal size vector)
n.vector <- c(3, 4, 5, 8)

for (n_vector_ind in 1:length(n.vector)) {
  for (diff_n_ind in 1:length(diff.n)) {
    for (diff_sd_ind in 1:length(diff.sd)) {
      n1 <- n.vector[n_vector_ind]
      n2 <- n.vector[n_vector_ind] + diff.n[diff_n_ind]
      
      power <-
        data.frame(
          t.equal   = vector(length = length(diff.mean)),
          #first item for T-test equal variances
          t.unequal = vector(length = length(diff.mean)),
          #second item for T-test equal variances
          mww       = vector(length = length(diff.mean)),
          #third item for Mann-Whitney U test
          perm      = vector(length = length(diff.mean))    #fourth item for permutation test
        )
      for (diff in 1:length(diff.mean)) {
        p.vector.equal.var <- vector(length = num_sim)
        p.vector.unequal.var <- vector(length = num_sim)
        p.vector.mww         <- vector(length = num_sim)
        p.vector.perm       <- vector(length = num_sim)
        for (simnumber in 1:num_sim) {
          g1 <- rnorm(n = n1,
                      mean = 0,
                      sd = 1)
          g2 <- rnorm(
            n = n2,
            mean = 0 + diff.mean[diff],
            sd = 1 + diff.sd[diff_sd_ind]
          )
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
        }
        count.equal   <- length(which(p.vector.equal.var < 0.05))
        count.unequal <- length(which(p.vector.unequal.var < 0.05))
        count.mww     <- length(which(p.vector.mww < 0.05))
        count.perm   <- length(which(p.vector.perm < 0.05))
        power[[1]][diff] <- (count.equal   / num_sim)
        power[[2]][diff] <- (count.unequal / num_sim)
        power[[3]][diff] <- (count.mww     / num_sim)
        power[[4]][diff] <- (count.perm    / num_sim)
      }
      
      ### Plotting
      data <- data.frame(diff.mean, power)
      plot <- ggplot(data) +
        geom_line(aes(
          x = diff.mean,
          y = t.equal  ,
          colour = "t.equal"
        ), size =
          1.2) +
        geom_line(aes(
          x = diff.mean,
          y = t.unequal,
          colour = "t.unequal"
        ),
        size =
          1.2) +
        geom_line(aes(
          x = diff.mean,
          y = mww      ,
          colour = "mww"
        ), size = 1.2) +
        geom_line(aes(
          x = diff.mean,
          y = perm     ,
          colour = "perm"
        ), size = 1.2) +
        scale_colour_manual(
          values = c(
            t.equal = cols[1],
            t.unequal = cols[2],
            mww = cols[3],
            perm = cols[4]
          ),
          name = "Testing"
        ) +
        labs(
          title = paste("Power simulation for 2 groups"),
          subtitle = paste(
            "(n1 = ",
            n1,
            " & n2 = ",
            n2,
            ", sd1 = 1 & sd2 = ",
            (1 + diff.sd[diff_sd_ind]),
            ")",
            sep = ""
          ),
          x = "Difference in means",
          y = "Power",
          color = "test"
        ) + ylim(0, 1)
      
      print(plot)
      
      
    }
  }
}