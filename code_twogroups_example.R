# Example retrieved by experiments executed by Dhr. J. Boeckmans (VUB - IVTD research group)

y <- c(0.44, 0.29, 0.49, 2.29, 2.33, 2.58, 0.99, 0.90, 0.83, 1.05, 1.30, 1.46, 0.56, 1.00, 0.84)
x <- factor(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5))

### omnibus
#ANOVA
model.aov <- anova(lm(y~x))
p.anova   <- model.aov$`Pr(>F)`[1]
p.anova                                # p = 4.191487e-07

#Kruskal-Wallis
model.kruskal <- kruskal.test(y~x)
p.kw          <- model.kruskal$p.value
p.kw                                   # p = 0.01211953

#ANOVA on ranks
y.rank <- rank(y)
model.rank <- anova(lm(y.rank~x))
p.rank     <- model.rank$`Pr(>F)`[1]
p.rank                                 # p = 2.243816e-05

### contrasts
build_contrast <- function(x, type){
  if(type == "grand-mean"){
    if(x == 3 | x == 4 | x == 5 | x == 7){
      if(x == 3){
        result <- matrix(c(0,0,0,-1/3,2/3,-1/3,-1/3,-1/3,2/3),ncol = 3)
      } else if(x == 5){
        result <- matrix(c(0,0,0,0,0,
                           -1/5,4/5,-1/5,-1/5,-1/5,
                           -1/5,-1/5,4/5,-1/5,-1/5,
                           -1/5,-1/5,-1/5,4/5,-1/5,
                           -1/5,-1/5,-1/5,-1/5,4/5),ncol = 5)
      } else if(x == 7){
        result <- matrix(c(0,0,0,0,0,0,0,
                           -1/7,6/7,-1/7,-1/7,-1/7,-1/7,-1/7,
                           -1/7,-1/7,6/7,-1/7,-1/7,-1/7,-1/7,
                           -1/7,-1/7,-1/7,6/7,-1/7,-1/7,-1/7,
                           -1/7,-1/7,-1/7,-1/7,6/7,-1/7,-1/7,
                           -1/7,-1/7,-1/7,-1/7,-1/7,6/7,-1/7,
                           -1/7,-1/7,-1/7,-1/7,-1/7,-1/7,6/7),ncol = 7)
      }
    } else {
      stop("No implementation of this contrast matrix: x <> 3,5,7")
    }
  } else if(type == "Dunnett"){
    result <- matrix(as.vector(contr.treatment(x)), byrow = TRUE, ncol = x)
  }
  
  return(result)
}

#generate contrasts
contr.gm <- build_contrast(5, "grand-mean")
rownames(contr.gm) <- paste("C", 1:nrow(contr.gm), sep = "")

contr.dunnett <- build_contrast(5, "Dunnett")
rownames(contr.dunnett) <- paste("C", 1:nrow(contr.dunnett), sep = "")

#grand-mean contrast
mod.gm <- glht(lm(y~x), linfct = contr.gm, adjust = "bonferonni")
pvalues.gm <- summary(mod.gm)$test$pvalues

#Dunnett contrast
mod.dunnett <- glht(lm(y~x), linfct = contr.dunnett, adjust = "bonferonni")
pvalues.dunnett <- summary(mod.dunnett)$test$pvalues

#non-parametric grand-mean contrast
mod.gm.np <- glht(lm(y.rank~x), linfct = contr.gm, adjust = "bonferonni")
pvalues.gm.np <- summary(mod.gm.np)$test$pvalues

#non-parametric dunnett contrast
mod.dunnett.np <- glht(lm(y.rank~x), linfct = contr.dunnett, adjust = "bonferonni")
pvalues.dunnett.np <- summary(mod.dunnett.np)$test$pvalues


