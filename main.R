# Author: Sven Van Laere
# Target: obtain Master degree Statistical Data Analysis (UGent)

set.seed(999)

# install.packages("coin")
# install.packages("ggplot")
# install.packages("exactRankTests")
# install.packages("gamlss.dist")
# install.packages("multcomp")
library(coin)
library(ggplot2)
library(exactRankTests)
library(multcomp)
library(gamlss.dist)

num_sim <- 1000

#function to define number of colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

######################
######################
###                ###
###   TWO GROUPS   ###
###                ###
######################
######################

n <- 4
cols <- gg_color_hue(n)


##############################
### TWO GROUPS (normal) ######
##############################

source(file = "code_2groups_normal.R")


# Calculate power under the null hypothesis
source(file = "code_2groups_normal_null.R")


##########################################
### TWO GROUPS (mixed: norm + norm) ######
##########################################

source(file = "code_2groups_mixture.R")


# Calculate power under the null hypothesis
source(file = "code_2groups_mixture_null.R")


###################################
### TWO GROUPS (exponential) ######
###################################

source(file = "code_2groups_exponential.R")


# Calculate power under the null hypothesis
source(file = "code_2groups_exponential_null.R")


##############################
### TWO GROUPS (Erlang) ######
##############################

source(file = "code_2groups_erlang.R")


# Calculate power under the null hypothesis
source(file = "code_2groups_erlang_null.R")


# Plot to visualize

xseq<-seq(0,20,.01)
densities1 <- dgamma (x = xseq, shape = 18, rate = 3)
densities2 <- dgamma (x = xseq, shape = 3, rate = 1)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     lwd=2, cex=2, main="PDF of Erlang", cex.axis=.8, col = "black", xlim = c(0,20), ylim = c(0,0.55))
lines(x = xseq,y = densities2, col="black")


##############################
### TWO GROUPS (SHASHo) ######
##############################

source(file = "code_2groups_sasnormal.R")


# Calculate power under the null hypothesis
source(file = "code_2groups_sasnormal_null.R")


# Plot to visualize
# plot 1
png("scenario 1 - mu = 0 - sigma = 1 - nu = 1 - tau = 1.5 [2 groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 2
png("scenario 2 - mu = 0 - sigma = 1 - nu = 1 - tau = 0.5 [2 groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 0.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 0.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 3
png("scenario 3 - mu = 0 - sigma = 1 - nu = 3 - tau = 1.5 [2 groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 4
png("scenario 4 - mu = 0 - sigma = 1 - nu = 3 - tau = 0.5 [2 groups].png")
xseq<-seq(-5,75,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 0.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 0.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 5
png("scenario 5 - nu = 3 - tau = 1.5 - nu = 1 - tau = 1.5 [2 groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq,y = densities2, lwd = 2, col="red")
dev.off()

# plot 6
png("scenario 6 - nu = 1 - tau = 0.5 - nu = 3 - tau = 1.5 [2 groups].png")
xseq<-seq(-5,25,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 0.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
plot(xseq, densities2,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, lwd = 2, col = "red")
lines(x = xseq,y = densities1, col="black")
dev.off()

# plot 7
png("scenario 7 - nu = 1 - tau = 1.5 - nu = 3 - tau = 0.5 [2 groups].png")
xseq<-seq(-5,75,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 0.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8,col="black")
lines(x = xseq,y = densities2, lwd = 2, col = "red")
dev.off()

# plot 8
png("scenario 8 - nu = 1 - tau = 1.5 - nu = 3 - tau = 1.5 [2 groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8,col="black")
lines(x = xseq,y = densities2, lwd = 2, col = "red")
dev.off()


###########################
###########################
###                     ###
###   MULTIPLE GROUPS   ###
###                     ###
###########################
###########################

# build contrast matrix
build_contrast <- function(x, type){
  if(type == "grand-mean"){
    if(x == 3 | x == 4 | x == 5 | x == 7){
      if(x == 3){
        result <- matrix(c(0,0,0,-1/3,2/3,-1/3,-1/3,-1/3,2/3),ncol = 3)
      } else if(x == 4){
        result <- matrix(c(0,0,0,0,-1/4,3/4,-1/4,-1/4,-1/4,-1/4,3/4,-1/4,-1/4,-1/4,-1/4,3/4),ncol = 4)
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
      stop("No implementation of this contrast matrix: x <> 3,4,5 or 7")
    }
  } else if(type == "Dunnett"){
    result <- matrix(as.vector(contr.treatment(x)), byrow = TRUE, ncol = x)
  }
  return(result)
}

# Change color set
n <- 5
cols <- gg_color_hue(n)

# Report on smallest p-value obtained when comparing groups
get_smallest_p_perm <- function(y, x){
  levels.vector <- levels(x)
  
  smallest.p <- 1
  for(id in 2:length(levels.vector)){
    ids.vector <- which(x == 1 | x == id)
    new.y <- y[ids.vector]
    new.x <- x[ids.vector]
    new.p <- perm.test(new.y ~ new.x, paired = FALSE, exact = TRUE)$p.value
    new.p <- p.adjust(new.p, method = "bonferroni", n=length(levels.vector)-1)
    if(new.p < smallest.p){smallest.p <- new.p}
  }
  return(smallest.p)
}

# Prints the group settings in a specific format
print_setting<- function(v){
  result <- ""
  for(ind in 1:length(v)){
    if(ind == length(v)){
      result <- paste(result, v[ind], sep="")
    } else {
      result <- paste(result, v[ind], " - ",sep = "")
    }
  }
  return(result)
}

#######################################
### NORMAL DISTRIBUTIONS (MULTIPLE) ###
#######################################

source(file = "code_multiplegroups_normal.R")


# Calculate power under the null hypothesis
source(file = "code_multiplegroups_normal_null.R")


#######################################
### ERLANG DISTRIBUTIONS (MULTIPLE) ###
#######################################

# Prints the scenario in a specific format
print_scenario <- function(df){
  result <- paste("shape = ", df[1,1],", shape.div = ", df[2,1], ", ",
                  "rate = " , df[1,2],", rate.div = " , df[2,2], sep = "")
  return(result)
}



source(file = "code_multiplegroups_erlang.R")


# Calculate power under the null hypothesis
source(file = "code_multiplegroups_erlang_null.R")

# Plot to visualize
xseq<-seq(0,20,.01)
densities1 <- dgamma (x = xseq, shape = 3, rate = 1)
densities2 <- dgamma (x = xseq, shape = 15, rate = 5)
plot(xseq, densities2,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of Erlang", cex.axis=.8, col = "black", xlim = c(0,20), ylim = c(0,0.55))
lines(x = xseq,y = densities1, col="red",lwd=2)


##############################################
### SHASHo NORMAL DISTRIBUTIONS (MULTIPLE) ###
##############################################

# Prints the scenario in a specific format
print_scenario <- function(df){
  result <- paste("xi = ", df[1,1],", xi.div = ", df[2,1], ", ",
                  "tau = " , df[1,2],", tau.div = " , df[2,2], sep = "")
  return(result)
}



source(file = "code_multiplegroups_sasnormal.R")


# Calculate power under the null hypothesis
source(file = "code_multiplegroups_sasnormal_null.R")


### Plot to visualize
# plot 1
png("scenario 1 - mu = 0 - sigma = 1 - nu = 1 - tau = 0.5 [multiple groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 0.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 0.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 2
png("scenario 2 - mu = 0 - sigma = 1 - nu = 1 - tau = 1.5 [multiple groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 1, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

# plot 3
png("scenario 3 - mu = 0 - sigma = 1 - nu = 3 - tau = 1.5 [multiple groups].png")
xseq<-seq(-5,15,.01)
densities1 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
densities2 <- dSHASHo(x = xseq, mu = 0, sigma = 1, nu = 3, tau = 1.5)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of SHASHo", cex.axis=.8, col = "black")
lines(x = xseq + 1,y = densities2, lwd=2, col="red")
dev.off()

####################################
### NORMAL + ERLANG DISTRIBUTION ###
####################################

source(file = "code_multiplegroups_normal_and_erlang.R")


# Calculate power under the null hypothesis
source(file = "code_multiplegroups_normal_and_erlang_null.R")


### Plot to visualize
xseq<-seq(0,20,.01)
densities1 <- dgamma (x = xseq, shape = 15, rate = 5)
densities2 <- dnorm (x = xseq, mean = 3, sd = 1)
plot(xseq, densities1,xlab="", ylab="Density", type="l",
     cex=2, main="PDF of Erlang", cex.axis=.8, col = "black", xlim = c(0,20), ylim = c(0,0.55))
lines(x = xseq,y = densities2, col="red",lwd=2)



##############################
##############################
###                        ###
###   EXAMPLES (APPLIED)   ###
###                        ###
##############################
##############################

# two group example
source(file = "code_twogroups_example.R")

# multiple group example
source(file = "code_multiplegroups_example.R")
