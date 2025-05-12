# --------------------
# SETUP AND PACKAGES
# --------------------
install.packages("topicmodels")
install.packages("reshape2")
install.packages("stm")

library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stm)

# ----------------------------
# LDA EXAMPLE ON AP DATA
# ----------------------------

# Loads a built-in dataset from topicmodels: a DocumentTermMatrix of Associated Press news articles.
data("AssociatedPress")

# Fits a Latent Dirichlet Allocation (LDA) model with 10 topics on the AP dataset. 
AP_topic_model<-LDA(AssociatedPress, k=10, control = list(seed = 321))

# Converts the model into a tidy format
AP_topics <- tidy(AP_topic_model, matrix = "beta")

# Finds the top 10 terms for each topic, based on the highest beta values (topic-word probabilities).
ap_top_terms <- AP_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# ----------------------------
# STM EXAMPLE ON POLIBLOGS
# ----------------------------

google_doc_id <- "1LcX-JnpGB0lU1iDnXnxB6WFqBywUKpew" # google file ID
poliblogs<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", 
                            google_doc_id), stringsAsFactors = FALSE)

# Preprocess and prepare documents
processed <- textProcessor(poliblogs$documents, metadata = poliblogs)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# ----------------------------
# OPTION A: Run and Save searchK 
# ----------------------------
# Warning: This process is computationally intensive, especially with a larger K range like 10:30. 
# If you're just experimenting or testing, use a smaller range (e.g., 7:10) first
# Search for Optimal Number of Topics (K)
findingk <- searchK(
  documents = docs, 
  vocab = vocab,
  K = 7:10,
  prevalence = ~ rating + s(day),
  data = meta, 
  verbose = FALSE
)
save(findingk, file = "findingk.Rda")

# ----------------------------
# OPTION B: Load precomputed searchK 
# ----------------------------
load("findingk.Rda")  
plot(findingk)

# ----------------------------
# Fit STM with chosen K (e.g., 10)
# ----------------------------
First_STM <- stm(
  documents = docs,
  vocab = vocab,
  K = 10,  # use selected K from searchK
  prevalence = ~ rating + s(day),
  max.em.its = 75,
  data = meta,
  init.type = "Spectral",
  verbose = FALSE
)

plot(First_STM)


# ----------------------------
# Examine example topics
# ---------------------------
findThoughts(First_STM, texts = poliblogs$documents,
             n = 2, topics = 3)

# ----------------------------
# Estimate and plot topic effects by covariate
# ----------------------------
predict_topics<-estimateEffect(formula = 1:10 ~ rating + s(day), stmobj = First_STM, 
                               metadata = out$meta, uncertainty = "Global")

# Liberal vs. Conservative
plot(predict_topics, covariate = "rating", topics = c(3, 5, 9),
     model = First_STM, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Topic 3', 'Topic 5', 'Topic 9'))

# Change over time
plot(predict_topics, "day", method = "continuous", topics = 3,
     model = First_STM, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")

monthseq <- seq(from = as.Date("2008-01-01"), to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1, at = as.numeric(monthseq) - min(as.numeric(monthseq)), labels = monthnames)
