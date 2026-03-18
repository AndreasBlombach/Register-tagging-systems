import time
import os
import polars as pl
from datetime import timedelta
from pybiber import PybiberPipeline


def main():
    # start timer
    start_time = time.perf_counter()

    input_dir = r'texts_raw/quotes_nonquotes_corpus'
    output_dir = r'output_pybiber'

    pipeline = PybiberPipeline(model="en_core_web_sm", disable_ner=True)

    # get features from folder of .txt files
    features_df = pipeline.run_from_folder(input_dir)

    # save results
    output_file = os.path.join(output_dir, f"output_pybiber_pipeline.csv")
    features_df.write_csv(output_file)

    # compute and save timing result
    end_time = time.perf_counter()
    duration = end_time - start_time

    print(f"Done in {timedelta(seconds=round(duration))}")

    timing_results = {
        "model": "sm",
        "time_seconds": round(duration, 2)
    }

    timing_df = pl.DataFrame(timing_results)
    timing_df.write_csv(os.path.join(output_dir, "timing_results_pipeline.csv"))


if __name__ == "__main__":
    main()