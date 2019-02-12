full_data  <- dd[1:500 , ]
n <- nrow(full_data)

#-----------validation--------
k <- 5 # number of topics

splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]


harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# The log-likelihood values are then determined by first fitting the model using for example
k = 10
burnin = 1000
iter = 1000
keep = 50

fitted<-  LDA(train_set, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

# where keep indicates that every keep iteration the log-likelihood is evaluated and stored. This returns all log-likelihood values including burnin, i.e., these need to be omitted before calculating the harmonic mean:

logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

# assuming that burnin is a multiple of keep and

harmonicMean(logLiks)


# generate numerous topic models with different numbers of topics
sequ <- seq(2, 100, 2) # in this case a sequence of numbers from 1 to 50, by ones.
fitted_many <- lapply(sequ, function(k) LDA(valid_set, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))


# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# inspect
plot(sequ, hm_many, type = "l") 

