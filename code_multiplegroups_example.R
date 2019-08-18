# Example retrieved by experiments executed by Dhr. J. Boeckmans (VUB - IVTD research group)

normalized_mRNA <- c(0.44, 0.29, 0.49,
                     2.29, 2.33, 2.58,
                     0.99, 0.90, 0.83,
                     1.05, 1.30, 1.46, 
                     0.56, 1.00, 0.84)
treatment <- factor(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
                    levels = c(1,2,3,4,5),
                    labels = c("disease model (control)",
                               "disease model + drug 1",
                               "disease model + drug 2",
                               "disease model + drug 3",
                               "disease model + drug 4"))
data <- data.frame(normalized_mRNA, treatment)

plot <- ggplot(data, aes(x = treatment, y = normalized_mRNA))+
  geom_point() +
  labs(x = "", y = "Normalized mRNA level", title = "Experiment mRNA levels")+
  scale_y_continuous(limits = c(0,3),breaks=seq(0,3,0.5))
  

plot

### Omnibus test
# One-Way ANOVA              # p = 4.191487e-07
model.aov  <- anova(lm(normalized_mRNA~treatment, data))
p.anova    <- model.aov$`Pr(>F)`[1]

#Kruskal-Wallis              # p = 0.01211953
model.kruskal <- kruskal.test(normalized_mRNA~treatment, data)
p.kw          <- model.kruskal$p.value

# One-way ANOVA on ranks     # p =2.243816e-05
normalized_mRNA.rank <- rank(normalized_mRNA)
model.aov  <- anova(lm(normalized_mRNA.rank~treatment, data))
p.anova    <- model.aov$`Pr(>F)`[1]

### Contrast test
k <- length(levels(data$treatment))

contr.gm <- build_contrast(k, "grand-mean")
rownames(contr.gm) <- paste("C", 1:nrow(contr.gm), sep = "")

contr.dunnett <- build_contrast(k, "Dunnett")
rownames(contr.dunnett) <- paste("C", 1:nrow(contr.dunnett), sep = "")

## Parametric contrast tests
#grand mean contrast with Bonferonni correction
mod.gm <- glht(lm(normalized_mRNA~treatment, data), linfct = contr.gm, adjust = "bonferonni")
pvalues.gm <- summary(mod.gm)$test$pvalues
   # C  - mean : 0.000004832624       (***)
   # T1 - mean : 0.000000002207195    (***)
   # T2 - mean : 0.05964451           (.)
   # T3 - mean : 0.6028959
   # T4 - mean : 0.007732639          (**)

#dunnett contrast with Bonferonni correction
mod.dunnett <- glht(lm(normalized_mRNA~treatment, data), linfct = contr.dunnett, adjust = "bonferonni")
pvalues.dunnett <- summary(mod.dunnett)$test$pvalues
   # C - T1    : 0.000000000009154008 (***)
   # C - T2    : 0.01260662           (*)
   # C - T3    : 0.0003121213         (***)
   # C - T4    : 0.04620484           (*)

## Non-parametric contrast tests
#grand mean contrast with Bonferonni correction
mod.gm.np <- glht(lm(normalized_mRNA.rank~treatment, data), linfct = contr.gm, adjust = "bonferonni")
pvalues.gm.np <- summary(mod.gm.np)$test$pvalues
   # C  - mean : 0.00007116871        (***)
   # T1 - mean : 0.0001129543         (***)
   # T2 - mean : 0.4023985           
   # T3 - mean : 0.01541723           (*)
   # T4 - mean : 0.02246401           (*)

#dunnett contrast with Bonferonni correction
mod.dunnett.np <- glht(lm(normalized_mRNA.rank~treatment, data), linfct = contr.dunnett, adjust = "bonferonni")
pvalues.dunnett.np <- summary(mod.dunnett.np)$test$pvalues
   # C - T1    : 0.000009672334       (***)
   # C - T2    : 0.01242205           (*)
   # C - T3    : 0.00007608658        (***)
   # C - T4    : 0.01992845           (*)

## Permutation contrasts
calculate_p_perm <- function(y, x){
  levels.vector <- levels(x)
  result <- rep(0, length(levels.vector)-1)
  for(id in 2:length(levels.vector)){
    ids.vector <- which(x == levels(x)[1] | x == levels(x)[id])
    new.y <- y[ids.vector]
    new.x <- x[ids.vector]
    new.p <- perm.test(new.y ~ new.x, paired = FALSE, exact = TRUE)$p.value
    new.p <- p.adjust(new.p, method = "bonferroni", n=length(levels.vector)-1)
    result[id-1] <- new.p
  }
  return(result)
}
p.perm <- calculate_p_perm(data$normalized_mRNA, data$treatment)
   # C - T1    : 0.4
   # C - T2    : 0.4
   # C - T3    : 0.4
   # C - T4    : 0.8
