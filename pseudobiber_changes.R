#' @rdname biber
#' @export
biber.spacyr_parsed <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE, window_size = 400L) {
  if ("dep_rel" %in% colnames(tokens) == F) stop("be sure to set 'dependency = T' when using spacy_parse")
  if ("tag" %in% colnames(tokens) == F) stop("be sure to set 'tag = T' when using spacy_parse")
  if ("pos" %in% colnames(tokens) == F) stop("be sure to set 'pos = T' when using spacy_parse")
  
  measure <- match.arg(measure)
  
  return(parse_biber_features(tokens, measure, normalize, "spacy", window_size))
}

#' @rdname biber
#' @export
biber.udpipe_connlu <- function(tokens, measure = c("MATTR", "TTR", "CTTR", "MSTTR", "none"),
                                normalize = TRUE, window_size = 400L) {
  
  # implicitly depends on the data.frame method for udpipe_connlu from
  # udpipe, so we have to put udpipe in Suggests and try to load it
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("udpipe package must be installed to extract features from udpipe-tagged text")
  }
  
  udpipe_tks <- as.data.frame(tokens, stringsAsFactors = FALSE)
  
  if ("dep_rel" %in% colnames(udpipe_tks) == F) stop("Be sure to set parser = 'default'")
  if ("xpos" %in% colnames(udpipe_tks) == F) stop("Be sure to set tagger = 'default'")
  if ("upos" %in% colnames(udpipe_tks) == F) stop("Be sure to set tagger = 'default'")
  
  measure <- match.arg(measure)
  
  udpipe_tks <- udpipe_tks %>%
    dplyr::select("doc_id", "sentence_id", "token_id", "token", "lemma", "upos",
                  "xpos", "head_token_id", "dep_rel") %>%
    dplyr::rename(pos = "upos", tag = "xpos")
  
  udpipe_tks <- structure(udpipe_tks, class = c("spacyr_parsed", "data.frame"))
  
  return(parse_biber_features(udpipe_tks, measure, normalize, "udpipe", window_size))
}

#' @importFrom rlang .data :=
parse_biber_features <- function(tokens, measure, normalize, engine = c("spacy", "udpipe"), window_size) {
  engine <- match.arg(engine)
  
  dict <- quanteda::dictionary(pseudobibeR::dict)
  
  df <- NULL
  
  biber_tks <- quanteda::as.tokens(tokens, include_pos = "tag", concatenator = "_") %>%
    quanteda::tokens_remove(" __SP") %>%
    quanteda::tokens_tolower() %>%
    quanteda::tokens_replace("[[:punct:]]_[[:punct:]]", "_punct_", valuetype = "regex") %>%
    quanteda::tokens_replace("\n__sp", "_punct_", valuetype = "fixed") %>%
    quanteda::tokens_replace("&_cc", "and_cc", valuetype = "fixed") %>%
    quanteda::tokens_remove("^\\W_", valuetype = "regex")
  
  
  tokens <- tokens %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(token = tolower(.data$token)) %>%
    dplyr::mutate(pos = ifelse(.data$token == "\n", "PUNCT", .data$pos)) %>%
    dplyr::filter(.data$pos != "SPACE")
  
  biber_1 <- quanteda::tokens_lookup(biber_tks, dictionary = dict) %>%
    quanteda::dfm() %>%
    quanteda::convert(to = "data.frame") %>%
    dplyr::as_tibble()
  
  df[["f_02_perfect_aspect"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$lemma == "have",
      stringr::str_detect(.data$dep_rel, "aux")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_02_perfect_aspect = "n")
  
  
  df[["f_10_demonstrative_pronoun"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$tag, "DT"),
      dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == F, default = TRUE),
      stringr::str_detect(.data$dep_rel, if (engine == "udpipe") "nsubj|obj|obl|conj|nmod" else "nsubj|dobj|pobj")
    ) %>%
    dplyr::filter(.data$token %in% pseudobibeR::word_lists$pronoun_matchlist) %>%
    dplyr::tally() %>%
    dplyr::rename(f_10_demonstrative_pronoun = "n")
  
  df[["f_12_proverb_do"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$lemma == "do",
      stringr::str_detect(.data$dep_rel, "aux") == F
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_12_proverb_do = "n")
  
  df[["f_13_wh_question"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$tag, "^W") == T,
      .data$pos != "DET" & dplyr::lead(.data$dep_rel == "aux"),
      (
        dplyr::lag(.data$pos == "PUNCT", default = T) |
          dplyr::lag(.data$pos == "PUNCT", 2, default = T)
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_13_wh_question = "n")
  
  df[["f_14_nominalizations"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "NOUN",
      stringr::str_detect(.data$token, "tion$|tions$|ment$|ments$|ness$|nesses$|ity$|ities$")
    ) %>%
    dplyr::filter(
      !.data$token %in% pseudobibeR::word_lists$nominalization_stoplist
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_14_nominalizations = "n")
  
  f_15_gerunds <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$token, "ing$|ings$") == TRUE,
      stringr::str_detect(.data$dep_rel, if (engine == "spacy") "nsub|dobj|pobj" else "nsubj|obj|obl|conj|nmod")
    ) %>%
    dplyr::filter(!.data$token %in% pseudobibeR::word_lists$gerund_stoplist)
  
  gerunds_n <- f_15_gerunds %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(.data$pos == "NOUN") %>%
    dplyr::tally() %>%
    dplyr::rename(gerunds_n = "n")
  
  df[["f_15_gerunds"]] <- f_15_gerunds %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::tally() %>%
    dplyr::rename(f_15_gerunds = "n")
  
  df[["f_16_other_nouns"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "NOUN" |
        .data$pos == "PROPN"
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data$token, "-") == F
    ) %>%
    dplyr::tally() %>%
    dplyr::left_join(df[["f_14_nominalizations"]], by = "doc_id") %>%
    dplyr::left_join(gerunds_n, by = "doc_id") %>%
    replace_nas() %>%
    dplyr::mutate(n = .data$n - .data$f_14_nominalizations - .data$gerunds_n) %>%
    dplyr::select("doc_id", "n") %>%
    dplyr::rename(f_16_other_nouns = "n")
  
  df[["f_17_agentless_passives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$dep_rel == if (engine == "spacy") "auxpass" else "aux:pass",
      dplyr::lead(.data$token != "by", 2, default = T),
      dplyr::lead(.data$token != "by", 3, default = T)
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_17_agentless_passives = "n")
  
  df[["f_18_by_passives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$dep_rel == if (engine == "spacy") "auxpass" else "aux:pass",
      (
        dplyr::lead(.data$token == "by", 2) |
          dplyr::lead(.data$token == "by", 3)
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_18_by_passives = "n")
  
  df[["f_19_be_main_verb"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$lemma == "be",
      stringr::str_detect(.data$dep_rel, "aux") == F
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_19_be_main_verb = "n")
  
  df[["f_21_that_verb_comp"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "that",
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos == "VERB")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_21_that_verb_comp = "n")
  
  df[["f_22_that_adj_comp"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "that",
      .data$pos == "SCONJ",
      dplyr::lag(.data$pos == "ADJ")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_22_that_adj_comp = "n")
  
  df[["f_23_wh_clause"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$tag, "^W") == T,
      .data$token != "which",
      dplyr::lag(.data$pos == "VERB")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_23_wh_clause = "n")
  
  df[["f_25_present_participle"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "VBG",
      (
        .data$dep_rel == "advcl" |
          .data$dep_rel == "ccomp"
      ),
      # beginning of sentence:
      dplyr::lag(.data$dep_rel == "punct", default = TRUE)
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_25_present_participle = "n")
  
  df[["f_26_past_participle"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "VBN", (
        .data$dep_rel == "advcl" |
          .data$dep_rel == "ccomp"
      ),
      # beginning of sentence:
      dplyr::lag(.data$dep_rel == "punct", default = TRUE)
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_26_past_participle = "n")
  
  df[["f_27_past_participle_whiz"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "VBN",
      dplyr::lag(.data$pos == "NOUN"),
      .data$dep_rel == "acl"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_27_past_participle_whiz = "n")
  
  df[["f_28_present_participle_whiz"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "VBG",
      dplyr::lag(.data$pos == "NOUN"),
      .data$dep_rel == "acl"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_28_present_participle_whiz = "n")
  
  df[["f_29_that_subj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "that",
      dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T),
      stringr::str_detect(.data$dep_rel, "nsubj") == T
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_29_that_subj = "n")
  
  df[["f_30_that_obj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "that",
      dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T),
      .data$dep_rel == if (engine == "spacy") "dobj" else "obj"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_30_that_obj = "n")
  
  df[["f_31_wh_subj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$tag, "^W") == T,
      dplyr::lag(.data$lemma != "ask", 2),
      dplyr::lag(.data$lemma != "tell", 2),
      (
        dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T) | (
          dplyr::lag(.data$pos == "PUNCT") &
            dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T, 2) &
            .data$token == "who"
        )
      )
    ) %>%
    dplyr::filter(.data$token != "that", stringr::str_detect(.data$dep_rel, "nsubj")) %>%
    dplyr::tally() %>%
    dplyr::rename(f_31_wh_subj = "n")
  
  df[["f_32_wh_obj"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$tag, "^W") == T,
      dplyr::lag(.data$lemma != "ask", 2),
      dplyr::lag(.data$lemma != "tell", 2),
      (
        dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T)  |
          (
            dplyr::lag(.data$pos == "PUNCT") &
              dplyr::lag(stringr::str_detect(.data$tag, "^N|^CD|DT") == T, 2) &
              stringr::str_detect(.data$token, "^who") == T
          )
      )
    ) %>%
    dplyr::filter(
      .data$token != "that",
      stringr::str_detect(.data$dep_rel, "obj") == T
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_32_wh_obj = "n")
  
  df[["f_34_sentence_relatives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "which",
      dplyr::lag(.data$pos == "PUNCT")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_34_sentence_relatives = "n")
  
  df[["f_35_because"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token == "because",
      dplyr::lead(.data$token != "of")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_35_because = "n")
  
  df[["f_38_other_adv_sub"]]<- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::mutate(pre_token = dplyr::lag(.data$pos)) %>%
    dplyr::filter(
      .data$pos == "SCONJ",
      .data$dep_rel == "mark",
      .data$token != "because",
      .data$token != "if",
      .data$token != "unless",
      .data$token != "though",
      .data$token != "although",
      .data$token != "tho"
    )  %>%
    dplyr::filter(
      !(.data$token == "that" &
          .data$pre_token != "ADV"
      )
    ) %>%
    dplyr::filter(
      !(.data$token == "as" &
          .data$pre_token == "AUX"
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_38_other_adv_sub = "n")
  
  if (engine == "spacy") {
    df[["f_39_prepositions"]] <- tokens %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::filter(.data$dep_rel == "prep") %>%
      dplyr::tally() %>%
      dplyr::rename(f_39_prepositions = "n")
  } else {
    df[["f_39_prepositions"]] <- tokens %>%
      dplyr::group_by(.data$doc_id) %>%
      dplyr::filter(.data$dep_rel == "case" &
                      .data$tag == "IN") %>%
      dplyr::tally() %>%
      dplyr::rename(f_39_prepositions = "n")
  }
  
  df[["f_40_adj_attr"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      (
        dplyr::lead(.data$pos == "NOUN") |
          dplyr::lead(.data$pos == "ADJ")  |
          (
            dplyr::lead(.data$token == ",") &
              dplyr::lead(.data$pos == "ADJ", 2)
          )
      )
    ) %>%
    dplyr::filter(stringr::str_detect(.data$token, "-") == F) %>%
    dplyr::tally() %>%
    dplyr::rename(f_40_adj_attr = "n")
  
  df[["f_41_adj_pred"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$pos == "ADJ",
      dplyr::lag(.data$pos == "VERB" | .data$pos == "AUX"),
      dplyr::lag(.data$lemma %in% pseudobibeR::word_lists$linking_matchlist),
      dplyr::lead(.data$pos != "NOUN"),
      dplyr::lead(.data$pos != "ADJ"),
      dplyr::lead(.data$pos != "ADV")
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_41_adj_pred = "n")
  
  df[["f_51_demonstratives"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$token %in% pseudobibeR::word_lists$pronoun_matchlist,
      .data$dep_rel == "det"
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_51_demonstratives = "n")
  
  df[["f_60_that_deletion"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$lemma %in% pseudobibeR::word_lists$verb_matchlist,
      .data$pos == "VERB",
      (
        dplyr::lead(.data$dep_rel == "nsubj") &
          dplyr::lead(.data$pos == "VERB", 2) &
          dplyr::lead(.data$tag != "WP") &
          dplyr::lead(.data$tag != "VBG", 2)
      ) |
        (
          dplyr::lead(.data$tag == "DT") &
            dplyr::lead(.data$dep_rel == "nsubj", 2) &
            dplyr::lead(.data$pos == "VERB", 3)
        ) |
        (
          dplyr::lead(.data$tag == "DT") &
            dplyr::lead(.data$dep_rel == "amod", 2) &
            dplyr::lead(.data$dep_rel == "nsubj", 3) &
            dplyr::lead(.data$pos == "VERB", 4)
        )
    ) %>%
    dplyr::filter(.data$dep_rel != "amod") %>%
    dplyr::tally() %>%
    dplyr::rename(f_60_that_deletion = "n")
  
  df[["f_61_stranded_preposition"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "IN",
      .data$dep_rel == if (engine == "spacy") "prep" else "case",
      dplyr::lead(stringr::str_detect(.data$tag, "[[:punct:]]"))
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_61_stranded_preposition = "n")
  
  df[["f_62_split_infinitive"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "TO",
      (
        dplyr::lead(.data$tag == "RB") &
          dplyr::lead(.data$tag == "VB", 2)
      ) |
        (
          dplyr::lead(.data$tag == "RB") &
            dplyr::lead(.data$tag == "RB", 2) &
            dplyr::lead(.data$tag == "VB", 3)
        )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_62_split_infinitive = "n")
  
  df[["f_63_split_auxiliary"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      stringr::str_detect(.data$dep_rel, "aux") == T,
      (
        dplyr::lead(.data$pos == "ADV") &
          dplyr::lead(.data$pos == "VERB", 2)
      ) |
        (
          dplyr::lead(.data$pos == "ADV") &
            dplyr::lead(.data$pos == "ADV", 2) &
            dplyr::lead(.data$pos == "VERB", 3)
        )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_63_split_auxiliary = "n")
  
  df[["f_64_phrasal_coordination"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "CC",
      (
        dplyr::lead(.data$pos == "NOUN") &
          dplyr::lag(.data$pos == "NOUN")
      ) |
        (
          dplyr::lead(.data$pos == "VERB") &
            dplyr::lag(.data$pos == "VERB")
        ) |
        (
          dplyr::lead(.data$pos == "ADJ") &
            dplyr::lag(.data$pos == "ADJ")
        ) |
        (
          dplyr::lead(.data$pos == "ADV") &
            dplyr::lag(.data$pos == "ADV")
        )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_64_phrasal_coordination = "n")
  
  df[["f_65_clausal_coordination"]] <- tokens %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::filter(
      .data$tag == "CC",
      .data$dep_rel != "ROOT",
      (
        dplyr::lead(.data$dep_rel == "nsubj") |
          dplyr::lead(.data$dep_rel == "nsubj", 2) |
          dplyr::lead(.data$dep_rel == "nsubj", 3)
      )
    ) %>%
    dplyr::tally() %>%
    dplyr::rename(f_65_clausal_coordination = "n")
  
  biber_tks <- biber_tks %>%
    quanteda::tokens_remove("\\d_", valuetype = "regex") %>%
    quanteda::tokens_remove("_punct_", valuetype = "fixed")
  
  
  biber_2 <- df %>% purrr::reduce(dplyr::full_join, by = "doc_id")
  
  biber_counts <- dplyr::full_join(biber_1, biber_2, by = "doc_id") %>%
    replace_nas()
  
  if (normalize) {
    tot_counts <- data.frame(tot_counts = quanteda::ntoken(biber_tks)) %>%
      tibble::rownames_to_column("doc_id") %>%
      dplyr::as_tibble()
    
    biber_counts <- dplyr::full_join(biber_counts, tot_counts, by = "doc_id")
    
    biber_counts <- normalize_counts(biber_counts)
  }
  
  if (measure != "none") {
    if (min(quanteda::ntoken(biber_tks)) < window_size) {
      message("Setting type-to-token ratio to TTR")
      measure <- "TTR"
    }
    
    f_43_type_token <- quanteda.textstats::textstat_lexdiv(biber_tks, measure = measure, MATTR_window = window_size) %>%
      dplyr::rename(doc_id = "document", f_43_type_token := !!measure)
    
    biber_counts <- dplyr::full_join(biber_counts, f_43_type_token, by = "doc_id")
  }
  
  f_44_mean_word_length <- tokens %>%
    dplyr::filter(
      stringr::str_detect(.data$token, "^[a-z]+$")
    ) %>%
    dplyr::mutate(mean_word_length = stringr::str_length(.data$token)) %>%
    dplyr::group_by(.data$doc_id) %>%
    dplyr::summarise(f_44_mean_word_length = mean(.data$mean_word_length))
  
  biber_counts <- dplyr::full_join(biber_counts, f_44_mean_word_length, by = "doc_id")
  
  biber_counts <- biber_counts %>%
    dplyr::select(order(colnames(biber_counts)))
  
  biber_counts[] <- lapply(biber_counts, as.vector)
  
  return(biber_counts)
}

#' Normalize to counts per 1,000 tokens
#'
#' @param counts Data frame with numeric columns for counts of token, with one
#'   row per document. Must include a `tot_counts` column with the total number
#'   of tokens per document.
#' @return `counts` data frame with counts normalized to rate per 1,000 tokens,
#'   and `tot_counts` column removed
#' @keywords internal
normalize_counts <- function(counts) {
  counts %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ 1000 * . / tot_counts)) %>%
    dplyr::select(-"tot_counts")
}

#' Replace all NAs with 0
#'
#' @param x Vector potentially containing NAs
#' @keywords internal
replace_nas <- function(x) {
  replace(x, is.na(x), 0)
}