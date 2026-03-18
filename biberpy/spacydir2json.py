import sys
import json
import os
import spacy
import polars as pl
import time
from datetime import timedelta

# start timer
start_time = time.perf_counter()

# check input path
input_path = sys.argv[1] if (len(sys.argv[1]) > 1 and not sys.argv[1] == "-") else None

# load model
model = sys.argv[2] if len(sys.argv)>2 else "en_core_web_sm"
nlp = spacy.load(model, disable=["ner"])
nlp.max_length = 2000000


def process_text(text):
    doc = nlp(text)
    doc_out = []
    for token in doc:
        out = [
            token.text,
            token.lemma_,
            token.pos_,
            str(token.morph),
            token.dep_
        ]
        doc_out.append(out)
    return doc_out


# check if input is a directory, otherwise treat as single file
if input_path and os.path.isdir(input_path):
    for filename in sorted(os.listdir(input_path)):
        name, ext = os.path.splitext(filename)
        file_path = os.path.join(input_path, filename)

        if not (os.path.isfile(file_path) and ext == ".txt"):
            continue

        with open(file_path, "r", encoding="utf-8") as f:
            text = f.read()

        doc_out = process_text(text)
        print(json.dumps(doc_out))
else:
    f = open(input_path, encoding="utf-8") if input_path and input_path != "-" else sys.stdin
    text = f.read()
    doc_out = process_text(text)
    print(json.dumps(doc_out))

# compute and save timing result
end_time = time.perf_counter()
duration = end_time - start_time

timing_results = {
    "model": model,
    "time_seconds": round(duration, 2)
}

timing_df = pl.DataFrame(timing_results)
timing_df.write_csv(os.path.join(os.getcwd(), "timing_results.csv"))