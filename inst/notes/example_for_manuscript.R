###############
## example for manuscript
## written by: Marshall Brown
## date: Oct 26, 2015
###############

#navigate to the source package or change the path
install.packages("DecisionCurve_0.3.tar.gz", repos = NULL, type = "source")


library(DecisionCurve)
#load simulated data
dcaData <- read.csv(file = "../../../Desktop/GenProbe_simulated.csv")
  dcaData <- read.csv(file = "../../../Desktop/GenProbe_PCA3_Dataset_2007.09.19.csv")

set.seed(123) #set the random seed so that the ci's are comparable

#create a decision_curve object for a baseline model with
## Age, history of biopsy, and suspcious DRE
baseline.model <- decision_curve(cancer ~Age + Hx.of.Bx + DRE, #fitting a logistic model
                                 thresholds = seq(0, 1, by = .005),
                                 data = dcaData,
                                 bootstraps = 500)


set.seed(123)

#add log(PSA) and log(PCA3) to the model
full.model <- decision_curve(cancer ~ Age + Hx.of.Bx + DRE +
                               PSA + PCA3, #fitting a logistic model
                             thresholds = seq(0, 1, by = .005),
                             data = dcaData,
                             bootstraps = 500)

#print to png -- I have found that these look the best.
png(file = "example_dca_v2.png", width = 500, height = 750, pointsize =16)


#adjus this to make the labels on the plots larger/smaller
par(cex = 1.25, mfrow = c(1,1), xpd = FALSE)
layout(mat = as.matrix(c(1,2,3)), heights = c(1.3, 1, 1))
#plot the decision curves together
plot_decision_curve( list(baseline.model, full.model),
                     ylim = c(-.5, 1.2),
                     col = c("magenta", "blue"),
                     lty = c(2, 1),
                     curve.names = c("Clinical model", "Clinical model\n + biomarkers"))
par(xpd = TRUE)
text(x = -.10, y = 1.5, labels = "A.", cex = 2)
par(xpd = FALSE)

par(mar = c(5.1, 4.1, 1.1, 2.1))
plot_clinical_impact(full.model,
                     curve.names = "baseline model",
                     col = c("grey50", "red"),
                     population.size = 1000,
                     cost.benefit.axis = FALSE, 
                     ylim = c(0, 1150))


par(xpd = TRUE)
text(x = -.10, y = 1200, labels = "B.", cex = 2)
par(xpd = FALSE)

plot_roc_components( full.model,
                    xlim = c(0, 1),
                    col = c("grey50", "red"),
                    ylim = c(0, 1.1), 
                    cost.benefit.axis = FALSE)

par(xpd = TRUE)
text(x =-.10, y = 1.2, labels = "C.", cex = 2)
par(xpd = FALSE)


dev.off()

#par(cex = 1, cex.lab = 1)



