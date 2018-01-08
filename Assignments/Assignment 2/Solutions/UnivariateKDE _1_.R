#
# Function std.norm.pdf
# Returns value of standard normal PDF at given point.
# x:  input
# 
std.norm.pdf = function(x)
{  
  pdf = 1 / sqrt(2*pi) * exp(-(x^2/2))
  return (pdf)
}

#
# Function:  est.kde
# Estimates KDE at a given point given a sample of data.
# x:  estimation point
# sample:  estimation sample
# bandwidth:  smoothing parameter
#
est.kde = function(x, sample, bandwidth)
{  
  kernel.inputs = (sample - x) / bandwidth
  kernel.values = sapply(kernel.inputs, std.norm.pdf)
  est = 1 / (length(sample) * bandwidth) * sum(kernel.values)
  return (est) 
}

# generate/visualize some random data
sample.x = c(rnorm(1000, mean = 0, sd = 1), rnorm(1000, mean = 5, sd = 1))
hist(sample.x, freq = FALSE, breaks = 100)
rug(sample.x)

# get evaluation range for KDE
eval.points = seq(-3, 10, 0.1)

# get and plot KDE with varying bandwidths
kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=0.01)
lines(x=eval.points, y=kde, type ='o', col = "red")

kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=0.2)
lines(x=eval.points, y=kde, type ='o', col = "blue")

kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=0.5)
lines(x=eval.points, y=kde, type ='o', col = "green")

kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=1)
lines(x=eval.points, y=kde, type ='o', col = "brown")

kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=2)
lines(x=eval.points, y=kde, type ='o', col = "yellow")

kde = sapply(eval.points, est.kde, sample=sample.x, bandwidth=100)
lines(x=eval.points, y=kde, type ='o', col = "pink")
