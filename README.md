# “Just do register analysis”, they said. “It’ll be easy”, they said: Evaluating the impact of lexico-grammatical tagging systems on multidimensional analysis

This repository contains data, code and analyses for our submission to LRE.

## Raw texts

[`texts_raw/quotes_nonquotes_corpus`](texts_raw/quotes_nonquotes_corpus) contains the combined corpus data from the 19C (19th century novels) and DNov (Dickens' novels) corpora also available [here](https://github.com/mahlberg-lab/corpora). In our case, however, there are two files for each novel: one containing only dialogue, one containing only narrative text. These were extracted using the [CLiC](https://clic-fiction.com) API.

## Register tagging systems

### MAT
[`output_MAT`](output_MAT) contains MAT's outputs for all individual parts of our corpus.

### pseudobibeR
[`output_pseudobiber/quotes_nonquotes_corpus`](output_pseudobiber/quotes_nonquotes_corpus) contains pseudobibeR's feature counts for Stanza and spaCy, as well as runtimes for spaCy. Stanza's CoNLL-U output and runtime can be found in [`output_stanza`](output_stanza).

To replicate these outputs, the following scripts can be used:
- `annotate_stanza.py` runs Stanza to create CoNLL-U output
- `feature_counts_pseudobiber_stanza.R` creates feature counts based on this output
- `feature_counts_pseudobiber_spacy.R` runs different spaCy models and creates feature counts based on their outputs

Note that both R scripts use an edited version of pseudobibeR (`pseudobiber_changes.R`) to be able to compute type-token ratios with a different window size. This makes them much slower (`output_pseudobiber/quotes_nonquotes_corpus/spacy_timings.csv`). Runtimes reported in the paper are therefore based on the unedited package (see `output_pseudobiber/quotes_nonquotes_corpus/spacy_timings_ttr100.csv`).

### pybiber
[`output_pybiber`](output_pseudobiber) contains pybiber's feature counts and runtimes.

To replicate these outputs, `feature_counts_pybiber.py` can be used. Alternatively, `feature_counts_pybiber_pipeline.py` could also be used -- it is, however, much slower.

### BiberPlus
[`output_biberplus`](output_biberplus) contains BiberPlus' feature counts and runtimes.

To replicate these outputs, `feature_counts_biberplus.py` can be used.

### Biberpy
[`output_biberpy`](output_biberpy) contains Biberpy's feature counts and runtimes.

To replicate these results:
- first, run `biberpy/spacydir2json.py` from the command line: `python spacydir2json.py quotes_nonquotes_corpus en_core_web_sm >quotes_nonquotes.json` (adjust paths as needed: `quotes_nonquotes_corpus` needs to point to the folder containing raw text files, `quotes_nonquotes.json` will be the output file)
- then, you will need the [Biberpy](https://github.com/ssharoff/biberpy) scripts (this is not a fully operational Python package)
- replace Biberpy's `biberpy.py` with our edited version (`biberpy/biberpy.py`) so that all features are normalised by token count (otherwise, Biberpy uses an approximation of clause count for several features)
- run Biberpy's `biber-dim.py` from the command line: `python biber-dim.py -f json -l en -i quotes_nonquotes.json -o output_biberpy_sm.tsv` (again, adjust paths as needed)


## Analysis
`analysis.qmd` is a Quarto document containing our analyses. [Easily readable HTML output is also available](https://htmlpreview.github.io/?https://github.com/AndreasBlombach/register_tagging_systems/blob/main/analysis.html).