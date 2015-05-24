# Load the data
#setwd('~/Code/Masters/IS621//Assignment5');
assignment5.data <- read.csv('classification-output.csv');

# Convert 'class' to factor type, since caret requires us to do so
assignment5.data$class = factor(assignment5.data$class)

# Get the baseline of positive events
assignment5.baseline <- sum(assignment5.data.sorted$class == "1") / nrow(assignment5.data.sorted);

# How many smaples do we have in the data set?
assignment5.numSamples <- nrow(assignment5.data.sorted);

# Create empty x and y vectors for plotting
x <- vector(length=assignment5.numSamples);
y <- vector(length=assignment5.numSamples);

i <- 1;
for (prob in assignment5.data.sorted$Scored.Probabilities) {
  assignment5.samplesFound <- (sum(assignment5.data.sorted[assignment5.data.sorted$Scored.Probabilities >= prob, ]$class == 1) / length(assignment5.data.sorted$Scored.Probabilities >= prob)) / assignment5.baseline;
  assignment5.samplesTested <- nrow(assignment5.data.sorted[assignment5.data.sorted$Scored.Probabilities >= prob, ]) / assignment5.numSamples;
  y[i] <- assignment5.samplesFound;
  x[i] <- assignment5.samplesTested;
  i <- i + 1;
}


# Create plot of generated x and y values
require(ggplot2);
p <- ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) + 
  theme(panel.background = element_blank()) + 
  geom_polygon(data=data.frame(x=c(0, assignment5.baseline, 1), y=c(0, 1, 1)), aes(x=x, y=y), fill="lightgrey") +
  geom_line(color="blue") + 
  xlab("% Samples Tested") +
  ylab("% Events Found") + 
  ggtitle("Lift Chart");
ggsave(filename="customLiftChart.png", plot=p);

# Plot lift chart from caret package
require('caret');
p2 <- xyplot(lift(class ~ Scored.Probabilities, data=assignment5.data, class="1"));
trellis.device(device="png", filename="liftChart.png")
print(p2)
dev.off()