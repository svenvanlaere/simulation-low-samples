settings_n_k <- list(
  c(3,3,3),
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
  c(3,5,5,5,7,7,7)
)

diff.mean <- seq(0, 6, by = 0.1)   #make a vector to shift distance from 0 to 6

#second scenario is the diverging one
scenarios <- list(data.frame(nu  = c(  1,  1),
                             tau = c(0.5,0.5)),
                  data.frame(nu  = c(  1,  1),
                             tau = c(1.5,1.5)),
                  data.frame(nu  = c(  3,  3),
                             tau = c(1.5,1.5)))

for(id_setting in 1:length(settings_n_k)){
  setting <- settings_n_k[[id_setting]]
  k <- length(setting)
  # diverging_distribution_as_of_k <- ceiling(k/2+1) #last half of the trt should have diverging distribution
  # #in some settings
  l <- length(diff.mean)
  
  for(id_scenario in 3:length(scenarios)){
    scenario <- scenarios[[id_scenario]]
    power <-
      data.frame(
        anova   = vector(length = l),
        #1st item for ANOVA
        kw      = vector(length = l),
        #2nd item for Kruskal-Wallis
        rank    = vector(length = l),
        #3rd item for ANOVA on ranks
        
        contr.gm         = vector(length = l),
        #4th item for grand-mean contrast (parametric)
        contr.dunnett    = vector(length = l),
        #5th item for Dunnett contrast    (parametric)
        contr.gm.np      = vector(length = l),
        #6th item for grand-mean contrast (non-parametric)
        contr.dunnett.np = vector(length = l),
        #7th item for Dunnett contrast    (non-parametric)
        contr.perm       = vector(length = l)
        #8th item for permutations        (non-parametric)
      )
    
    for(id_diff in 1:l){
      p.anova <- vector(length = num_sim)
      p.kw    <- vector(length = num_sim)
      p.rank  <- vector(length = num_sim)
      
      p.gm          <- vector(length = num_sim)
      p.dunnett     <- vector(length = num_sim)
      p.gm.np       <- vector(length = num_sim)
      p.dunnett.np  <- vector(length = num_sim)
      p.perm        <- vector(length = num_sim)
      
      for(sim_id in 1:num_sim){
        y <- vector()
        x <- vector()
        data <- data.frame(y, x)
        
        for(id_group in 1:length(setting)){
          n_k <- setting[id_group]
          
          if(id_group == 2){
            sample <- rSHASHo(n = n_k, mu = 0, sigma = 1,
                              nu = scenario["nu"][2,], 
                              tau = scenario["tau"][2,])+ diff.mean[id_diff]
          } else{
            sample <- rSHASHo(n = n_k, mu = 0, sigma = 1,
                              nu = scenario["nu"][1,], 
                              tau = scenario["tau"][1,])
          }
          
          
          y <- c(y, sample)                                   # sample data
          x <- c(x, rep(id_group, n_k))                       # set groups of sampled data
        }
        
        x <- factor(x)
        
        ####################
        ### Omnibus tests
        ####################
        
        #One-way anova
        model.aov       <- anova(lm(y~x))
        p.anova[sim_id] <- model.aov$`Pr(>F)`[1]
        
        #Kruskal-Wallis
        model.kruskal <- kruskal.test(y~x)
        p.kw[sim_id]  <- model.kruskal$p.value
        
        #One-way ANOVA on ranks
        y.rank <- rank(y)
        model.rank     <- anova(lm(y.rank~x))
        p.rank[sim_id] <- model.rank$`Pr(>F)`[1]
        
        #####################
        ### Contrast tests
        #####################
        
        #generate contrasts
        contr.gm <- build_contrast(k, "grand-mean")
        rownames(contr.gm) <- paste("C", 1:nrow(contr.gm), sep = "")
        
        contr.dunnett <- build_contrast(k, "Dunnett")
        rownames(contr.dunnett) <- paste("C", 1:nrow(contr.dunnett), sep = "")
        
        ## Parametric contrast tests
        #grand mean contrast with Bonferonni correction
        mod.gm <- glht(lm(y~x), linfct = contr.gm, adjust = "bonferonni")
        pvalues.gm <- summary(mod.gm)$test$pvalues
        p.gm[sim_id] <- min(pvalues.gm)
        
        #dunnett contrast with Bonferonni correction
        mod.dunnett <- glht(lm(y~x), linfct = contr.dunnett, adjust = "bonferonni")
        pvalues.dunnett <- summary(mod.dunnett)$test$pvalues
        p.dunnett[sim_id] <- min(pvalues.dunnett)
        
        ## Non-parametric contrast tests
        #grand mean contrast with Bonferonni correction
        mod.gm.np <- glht(lm(y.rank~x), linfct = contr.gm, adjust = "bonferonni")
        pvalues.gm.np <- summary(mod.gm.np)$test$pvalues
        p.gm.np[sim_id] <- min(pvalues.gm.np)
        
        #dunnett contrast with Bonferonni correction
        mod.dunnett.np <- glht(lm(y.rank~x), linfct = contr.dunnett, adjust = "bonferonni")
        pvalues.dunnett.np <- summary(mod.dunnett.np)$test$pvalues
        p.dunnett.np[sim_id] <- min(pvalues.dunnett.np)
        
        ## Permutation contrasts
        p.perm[sim_id] <- get_smallest_p_perm(y, x)
      }
      
      power[[1]][id_diff] <- length(which(p.anova < 0.05))/num_sim
      power[[2]][id_diff] <- length(which(p.kw    < 0.05))/num_sim
      power[[3]][id_diff] <- length(which(p.rank  < 0.05))/num_sim
      
      power[[4]][id_diff] <- length(which(p.gm          < 0.05))/num_sim
      power[[5]][id_diff] <- length(which(p.dunnett     < 0.05))/num_sim
      power[[6]][id_diff] <- length(which(p.gm.np       < 0.05))/num_sim
      power[[7]][id_diff] <- length(which(p.dunnett.np  < 0.05))/num_sim
      power[[8]][id_diff] <- length(which(p.perm        < 0.05))/num_sim
    }
    data <- data.frame(diff.mean, power)
    
    # make plot omnibus test
    plot1 <- ggplot(data) +
      geom_line(aes(x = diff.mean, y = anova, colour = "anova"), size = 1.2) +
      geom_line(aes(x = diff.mean, y = kw   , colour = "kw"   ), size = 1.2) +
      geom_line(aes(x = diff.mean, y = rank , colour = "rank" ), size = 1.2) +
      scale_colour_manual(values = c(anova = cols[1], kw = cols[2], rank = cols[3]),
                          name = "Testing"
      ) +
      labs(
        title = paste("Power simulation for multiple groups"),
        subtitle = paste("Overall test", print_setting(setting),
                         ", N = ", sum(setting), sep = ""
        ), x = "Difference in means", y = "Power", color = "test"
      ) + ylim(0, 1)
    
    picture_name.omnibus <- paste("Omnibus test ",print_setting(setting)," -- ",
                                  print_scenario(scenario), ".png",sep = "")
    png(picture_name.omnibus)
    print(plot1)
    dev.off()
    
    #make plot contrast test
    plot2 <- ggplot(data) +
      geom_line(aes(x = diff.mean, y = contr.gm         , colour = "contr.gm"), size = 1.2) +
      geom_line(aes(x = diff.mean, y = contr.dunnett    , colour = "contr.dunnett"   ), size = 1.2) +
      geom_line(aes(x = diff.mean, y = contr.gm.np      , colour = "contr.gm.np"   ), size = 1.2) +
      geom_line(aes(x = diff.mean, y = contr.dunnett.np , colour = "contr.dunnett.np" ), size = 1.2) +
      geom_line(aes(x = diff.mean, y = contr.perm       , colour = "contr.perm" ), size = 1.2) +
      scale_colour_manual(values = c(contr.gm = cols[1], contr.dunnett = cols[2], 
                                     contr.gm.np = cols[3], contr.dunnett.np = cols[4],
                                     contr.perm = cols[5]),
                          name = "Contrasts"
      ) +
      labs(
        title = paste("Power simulation for multiple groups"),
        subtitle = paste("Contrast test ", print_setting(setting),
                         ", N = ", sum(setting), sep = ""
        ), x = "Difference in means", y = "Power", color = "test"
      ) + ylim(0, 1)
    
    picture_name.contrast <- paste("Contrast test ",print_setting(setting)," -- ",
                                   print_scenario(scenario), ".png",sep = "")
    png(picture_name.contrast)
    print(plot2)
    dev.off()
  }
}