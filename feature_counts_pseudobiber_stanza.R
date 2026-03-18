library(tidyverse)
library(udpipe)
library(pseudobibeR)

source("pseudobiber_changes.R")

# directory containing tagged texts:
tagged <- "output_stanza/quotes_nonquotes_corpus"

# files:
files <- list.files(tagged, pattern = "\\.conllu", full.names = TRUE)

for (file in files) {
  corpus <- str_extract(file, r"(corpus/(\w+?)_)", group = 1)
  subcorpus <- str_extract(file, r"(_((non)?quotes)\.conllu)", group = 1)
  short_name <- str_extract(file, r"(_(\w+)_(non)?quotes\.conllu)", group = 1)
  
  conllu <- udpipe_read_conllu(file) |>
    as_tibble()
  conllu$doc_id <- short_name
  
  # getting rid of contractions etc. (Stanza introduces new tokens by splitting them up):
  conllu <- conllu |>
    filter(!is.na(lemma))
  
  # simulate the output from udpipe_annotate() to get biber() to accept our CoNLL-U input:
  obj <- list(x = "", conllu = as_conllu(conllu), error = "")
  attr(obj, "class") <- "udpipe_connlu"
  
  # get pseudobiber features:
  if (!exists("features")) {
    features <- biber.udpipe_connlu(obj, measure = "MATTR", normalize = TRUE, window_size = 400L) |>
      mutate(corpus = corpus,
             subcorpus = subcorpus,
             .after = doc_id)
  } else {
    features <- bind_rows(features, biber.udpipe_connlu(obj, measure = "MATTR", normalize = TRUE, window_size = 400L) |>
                            mutate(corpus = corpus,
                                   subcorpus = subcorpus,
                                   .after = doc_id))
  }
}

features |>
  write_csv("output_pseudobiber/quotes_nonquotes_corpus/stanza_pseudobiber_features.csv")