library("ldatuning")

result3 <- FindTopicsNumber(
  dd,
  topics = seq(from = 2, to = n, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 8L,
  verbose = TRUE
)
