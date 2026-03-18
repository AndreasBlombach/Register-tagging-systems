import polars as pl
import os
import time
from datetime import timedelta
from biberplus.tagger import load_config, load_pipeline, calculate_tag_frequencies

def main():
    # start timer
    start_time = time.perf_counter()
    print("Loading config and pipeline...")

    # load configuration and pipeline
    config = load_config()
    config.update({'use_gpu': True, 'biber': True, 'function_words': False, 'show_progress': True, 'n_processes': 1,
                   'token_normalization': 1000})
    pipeline = load_pipeline(config)

    # set input and output directories
    input_dir = r'texts_raw/quotes_nonquotes_corpus'
    output_dir = r'output_biberplus'
    # create output_dir if necessary
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # create empty list for DataFrames with results
    df_list = []

    print("Tagging texts...")

    # iterate through input_dir and get tag frequencies for every text
    for filename in os.listdir(input_dir):
        if not filename.endswith('.txt'):
            continue

        file_path = os.path.join(input_dir, filename)
        with open(file_path, "r", encoding="utf-8") as f:
            doc_id = os.path.splitext(filename)[0]
            text = f.read()

            # get tag frequencies
            frequencies_text_df = calculate_tag_frequencies(text, pipeline=pipeline, config=config)
            print(frequencies_text_df)
            frequencies_text_df = pl.from_pandas(frequencies_text_df[["mean"]]).transpose(column_names=frequencies_text_df["tag"]).insert_column(0, pl.Series("doc_id", [doc_id]))

            # append to list of DataFrames
            df_list.append(frequencies_text_df)


    # combine outputs
    frequencies_df = pl.concat(df_list)

    # save results
    output_file = os.path.join(output_dir, f"output_biberplus.csv")
    frequencies_df.write_csv(output_file)

    # compute and save timing result
    end_time = time.perf_counter()
    duration = end_time - start_time

    print(f"Done in {timedelta(seconds=round(duration))}")

    timing_results = {
        "model": "sm",
        "time_seconds": round(duration, 2)
    }

    timing_df = pl.DataFrame(timing_results)
    timing_df.write_csv(os.path.join(output_dir, "timing_results.csv"))


if __name__ == "__main__":
    main()