import os
import time
import polars as pl
from datetime import timedelta
import stanza
from stanza.utils.conll import CoNLL


def main():
    # start timer
    start_time = time.perf_counter()

    # directory containing raw text files:
    raw = r'texts_raw/quotes_nonquotes_corpus'

    # directory in which to write tagged texts:
    tagged = r'output_stanza/quotes_nonquotes_corpus_new'

    # create directory if necessary:
    if not os.path.exists(tagged):
        os.makedirs(tagged)

    # NLP pipeline:
    nlp = stanza.Pipeline("en", processors="tokenize,mwt,pos,lemma,depparse")

    # traverse input directory, tag texts, write output as CONLL-U:
    for filename in sorted(os.listdir(raw)):
        name, ext = os.path.splitext(filename)
        if ext == ".txt":
            with open(os.path.join(raw, filename), encoding="utf-8") as f:
                text = f.read()
                doc = nlp(text)
                CoNLL.write_doc2conll(doc, os.path.join(tagged, name + ".conllu"))


    # compute and save timing result
    end_time = time.perf_counter()
    duration = end_time - start_time

    print(f"Done in {timedelta(seconds=round(duration))}")

    timing_results = {
        "model": "Stanza (combined)",
        "time_seconds": round(duration, 2)
    }

    timing_df = pl.DataFrame(timing_results)
    timing_df.write_csv("output_stanza/timing_results.csv")


if __name__ == "__main__":
    main()