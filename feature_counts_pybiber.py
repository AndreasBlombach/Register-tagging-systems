import spacy
import pybiber as pb
import polars as pl
import os
import time
from datetime import timedelta


def process_and_time(model_name, nlp, raw_text, processor, output_dir, timing_results):
    '''process corpus and time execution'''
    print(f"{model_name}: processing ...")
    start_time = time.perf_counter()

    # parse corpus
    df_spacy = processor.process_corpus(raw_text, nlp)
    # create Polars DataFrame with results
    df_biber = pb.biber(df_spacy, mattr_window=400)

    # save register features to CSV
    output_file = os.path.join(output_dir, f"output_pybiber_{model_name}.csv")
    df_biber.write_csv(output_file, separator='\t')

    end_time = time.perf_counter()
    duration = end_time - start_time

    print(f"{model_name}: done in {timedelta(seconds=round(duration))}")

    # append timing info
    timing_results.append({
        "model": model_name,
        "time_seconds": round(duration, 2)
    })


def main():
    # 1. create Polars DataFrame

    input_dir = r'texts_raw/quotes_nonquotes_corpus'
    output_dir = r'output_pybiber'
    # create output_dir if necessary
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    data = []

    # iterate through input_dir
    for filename in os.listdir(input_dir):
        if not filename.endswith('.txt'):
            continue

        file_path = os.path.join(input_dir, filename)
        with open(file_path, "r", encoding="utf-8") as f:
            doc_id = os.path.splitext(filename)[0]
            text = f.read()
            data.append((doc_id, text))

    # create Polars DataFrame with "doc_id" and "text" columns
    raw_text = pl.DataFrame(data, schema=["doc_id", "text"], orient="row")

    # 2. parse DataFrame and extract features with different spaCy NLPs

    # load spaCy pipelines (disable NER for speed/focus)
    nlp_models = {
        "sm": spacy.load("en_core_web_sm", disable=["ner"]),
        "md": spacy.load("en_core_web_md", disable=["ner"]),
        "lg": spacy.load("en_core_web_lg", disable=["ner"]),
        "trf": spacy.load("en_core_web_trf", disable=["ner"]),
    }

    # CorpusProcessor instance from pybiber
    processor = pb.CorpusProcessor()

    # list to collect timing results
    timing_results = []

    # run processing and timing for each model
    for model_name, nlp in nlp_models.items():
        process_and_time(model_name, nlp, raw_text, processor, output_dir, timing_results)

    # save timing results as a CSV
    timing_df = pl.DataFrame(timing_results)
    timing_df.write_csv(os.path.join(output_dir, "timing_results.csv"))

    print("All done!")


if __name__ == "__main__":
    main()