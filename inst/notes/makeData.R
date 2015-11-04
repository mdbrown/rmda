library(devtools)
install_github("trinker/wakefield")

library(wakefield)


dcaData <-
  r_data_frame(
  n = 500,
  age,
  dummy(prob = c(.46, .54), name = "Female"),
  smokes,
  Marker1 = runif,
  Marker2 = rnorm
)
attach(dcaData)
lp <-   -10 +  log(1.25)*Age + log(.75)*Female + log(2)*Smokes + log(3)*Marker1 + log(.2)*Marker2
detach(dcaData)

expit <- function(xx) exp(xx)/ (1+exp(xx))
logit <- function(xx) log(xx/(1-xx))

dcaData$Cancer <- rbinom(500, size =1 , prob = expit(lp))
table(dcaData$Cancer)

glm(Cancer~Age + Female + Smokes, data = dcaData, family = binomial())
glm(Cancer~Age + Female + Smokes + Marker1 + Marker2, data = dcaData, family = binomial())

data(dcaData)
dcaData$BasicModel <- with(dcaData, expit(-7.3 + 0.18*Age - 0.08*Female + 0.80*Smokes ) )
dcaData$FullModel <- with(dcaData, expit(-10.5 + 0.22*Age  - 0.01*Female + 0.91*Smokes + 2.03*Marker1 - 1.56*Marker2))

tmp <- DecisionCurve(dcaData,
              outcome = "Cancer", predictors = c("BasicModel", "FullModel"),
              thresholds = seq(0, .35, by = .01), lty = c(2, 1),
              standardize = FALSE,
              legend.position = "topright",
              bootstraps = 100, ylim = c(-0.05, .15),
              cost.benefit.axis = FALSE)





#####simulate data based off genprobe data

#library(devtools)
#install_github("datasynthR", "jknowles")

library(datasynthR)

struc <- list(dist=c("norm", "pois", "gamma"), rho=c(0.2, -.5, .5),
              names=c("super", "cool", "data"))

dat2 <- genNumeric(1000, pattern=struc)




dcaData <- read.csv(file = "../../../Desktop/GenProbe_PCA3_Dataset_2007.09.19.csv")
head(dcaData)



#remove missing data and variables we don't want
cc <- complete.cases(dcaData[,c("Age.At.Urine.Collect", "Hx.of.Bx", "Susp.DRE", "Cancer", "Serum.PSA", "PCA3.Score")])
GenProbe <- dcaData[cc,c("Age.At.Urine.Collect", "Hx.of.Bx", "Susp.DRE", "Cancer", "Serum.PSA", "PCA3.Score")]
GenProbe$Serum.PSA <- log(GenProbe$Serum.PSA)
GenProbe$PCA3.Score <- log(GenProbe$PCA3.Score)

#scaled data
sc.GenProb <- GenProbe

sc.GenProb$Age.At.Urine.Collect <- (GenProbe$Age.At.Urine.Collect - mean(GenProbe$Age.At.Urine.Collect))/sd(GenProbe$Age.At.Urine.Collect)
sc.GenProb$Serum.PSA <- (GenProbe$Serum.PSA - mean(GenProbe$Serum.PSA))/sd(GenProbe$Serum.PSA)
sc.GenProb$PCA3.Score <- (GenProbe$PCA3.Score - mean(GenProbe$PCA3.Score))/sd(GenProbe$PCA3.Score)



round(cor(GenProbe[,-4]), 3)
round(cor(sc.GenProb[,-4]), 3)


struc3 <- list(dist = c("norm", "binom", "binom", "norm","norm"),
               rho = c(.5, 0.046, -0.213, 0.014, 0.07),
               names = c("Age", "Hx.of.Bx", "Susp.DRE", "PSA", "PCA3"))

covmat3 <- genNumeric(570, pattern = struc3)
round(cov(covmat3), 3)
#variables of interest to reproduce.



##### lets try another way
library(corrplot)
library(MASS)
library(ggplot2)
#1. Draw any number of variables from a joint normal distribution.

mu <- colMeans(GenProbe[,-4])
Sigma <- cov(GenProbe[,-4])

set.seed(321)
rawvars <- as.data.frame(mvrnorm(n=nrow(GenProbe), mu=mu, Sigma=Sigma))

newvars <- rawvars

#2. Apply the univariate normal CDF of variables to derive probabilities for each variable.
## we only have to do this for the non - normal variables
pvars <- NULL
pvars$Bx <- pnorm(rawvars$Hx.of.Bx,
                mean = mean(GenProbe$Hx.of.Bx),
                sd = sd(GenProbe$Hx.of.Bx))

pvars$Susp.DRE <- pnorm(rawvars$Susp.DRE,
                        mean = mean(GenProbe$Susp.DRE),
                        sd = sd(GenProbe$Susp.DRE))

#3. Finally apply the inverse CDF of any distribution to simulate draws from that distribution.
# only have to do this for the non-normal vars.

newvars$Hx.of.Bx <- qbinom(pvars$Bx, size = 1, prob = mean(GenProbe$Hx.of.Bx))
newvars$Susp.DRE <- qbinom(pvars$Susp.DRE, size = 1, prob = mean(GenProbe$Susp.DRE))

round(cov(newvars), 3)
round(cov(GenProbe[,-4]), 3)

round(cov(newvars), 3) - round(cov(GenProbe[,-4]), 3)
round(cor(newvars), 3) - round(cor(GenProbe[,-4]), 3)

round(rbind(colMeans(newvars), colMeans(GenProbe[,-4])), 2)
round(rbind(apply(newvars, 2, sd), apply(GenProbe[,-4], 2, sd)), 2)



names(newvars)[1] <- "Age"
names(GenProbe)[1] <- "Age"

corrplot(cor(newvars), method = "ellipse", addCoef.col = "black")
corrplot(cor(GenProbe[,-4]), method = "ellipse", addCoef.col = "black")


long.newvars  <- melt(newvars); long.newvars$type = "simulated"
long.Genprobe <- melt(GenProbe[,-4]); long.Genprobe$type = "raw"

ggdat <- rbind(long.Genprobe, long.newvars)

ggplot(subset(ggdat, is.element(variable, c("Age", "Serum.PSA", "PCA3.Score"))), aes(value, fill = type)) +
         facet_wrap(~variable, scales = "free") +
         geom_density(alpha = .6) + theme_bw() + theme(text = element_text(size = 14))

ggplot(subset(ggdat, is.element(variable, c("Hx.of.Bx", "Susp.DRE"))), aes(factor(value), fill = type)) +
  facet_wrap(~variable, scales = "free") +
  geom_bar(position = "dodge") + theme_bw() + theme(text = element_text(size = 14))


