# Data Visualization
library('ggplot2')

data.file <- file.path('/Users/marcdupuis/src/machine_learning/data/ML_for_Hackers-master/02-Exploration/data/01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header=TRUE, sep=',')
ggplot(heights.weights, aes(x= Height)) + geom_histogram(binwidth=1)

# Change the binwidth of the histogram
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth=5)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth=0.001)

# Using density plots instead of histograms
ggplot(heights.weights, aes(x=Height)) + geom_density()

# Plot the density distinguishing the genders
ggplot(heights.weights, aes(x=Height, fill=Gender)) + geom_density()

# Scatter plot of heights and weights
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point() + geom_smooth()

# Plot according to gender
ggplot(heights.weights, aes(x=Height, y=Weight, color=Gender)) + geom_point()

# Applying a generalized linear model to the data
heights.weights <- transform(heights.weights, Male =ifelse(Gender=='Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link='logit'))
ggplot(heights.weights, aes(x=Weight, y=Height, color=Gender)) + geom_point() +
  stat_abline(intercept = - coef(logit.model)[1]/coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline',
              color = "black")

