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



