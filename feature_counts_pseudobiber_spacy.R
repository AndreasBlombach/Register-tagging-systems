library(tidyverse)
library(spacyr)
library(pseudobibeR)
library(tictoc)

source("pseudobiber_changes.R")


Sys.setenv(VROOM_CONNECTION_SIZE = 2000000)

models <- c("sm" = "en_core_web_sm",
            "md" = "en_core_web_md",
            "lg" = "en_core_web_lg")

timings <- list()

for (model_name in names(models)) {
  model <- models[[model_name]]
  
  tic(model_name)
  
  print(str_glue("Loading model {model} ..."))
  
  # initialise spaCy
  spacy_initialize(model = model, entity = FALSE)
  
  input_dir <- "texts_raw/quotes_nonquotes_corpus"
  files <- list.files(input_dir, pattern = "\\.txt", full.names = TRUE)
  
  for (file in files) {
    corpus <- str_extract(file, r"(corpus/(\w+?)_)", group = 1)
    subcorpus <- str_extract(file, r"(_((non)?quotes)\.txt)", group = 1)
    short_name <- str_extract(file, r"(_(\w+)_(non)?quotes\.txt)", group = 1)
    
    # read in lines of file -- normally, you'd read in the whole file, but
    # spaCy doesn't like very big files, and you cannot control the options
    # (nlp.max_length) using spacyr ...
    lines <- read_lines(file, skip_empty_rows = TRUE)
    
    # parse with spaCy:
    obj <- data.table::rbindlist(lapply(lines, function(line) {
      spacy_parse(line,
                  pos = TRUE,
                  tag = TRUE,
                  dependency = TRUE,
                  entity = FALSE)
    })) |> as.data.frame()
    
    obj$doc_id <- short_name
    
    attr(obj, "class") <- c("spacyr_parsed", "data.frame") # gets deleted by rbindlist(), but is expected by biber()
    
    # get pseudobiber features:
    if (!exists("features")) {
      features <- biber.spacyr_parsed(obj, measure = "MATTR", normalize = TRUE, window_size = 400L) |>
        mutate(corpus = corpus,
               subcorpus = subcorpus,
               .after = doc_id)
    } else {
      features <- bind_rows(features, biber.spacyr_parsed(obj, measure = "MATTR", normalize = TRUE, window_size = 400L) |>
                              mutate(corpus = corpus,
                                     subcorpus = subcorpus,
                                     .after = doc_id))
    }
  
  }
  
  spacy_finalize()
  
  features |>
    write_csv(str_glue("output_pseudobiber/quotes_nonquotes_corpus/spacy_{model_name}_pseudobiber_features.csv"))
  
  rm(features)
  
  res <- toc(log = TRUE)
  timings[[model_name]] <- res$toc - res$tic
}


timing_df <- tibble(
  model = names(timings),
  time_sec = unlist(timings)
)

timing_df |>
  mutate(time_sec = round(time_sec, 2)) |>
  write_csv("output_pseudobiber/quotes_nonquotes_corpus/spacy_timings.csv")
