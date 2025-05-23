# Predictive Text Model using N-Gram model

This project implements a simple and efficient **next-word prediction model** using **n-gram language modeling** with **Laplace smoothing** and a **backoff mechanism**. It also includes a lightweight **Shiny web app** for interactive prediction.

---

## Project Structure
```
|-- app/
|   `-- prediction_app.R         # Shiny app interface
|
|-- scripts/
|   |-- prepare_data.R          # Text cleaning + n-gram creation
|   |-- model_functions.R       # Prediction logic with smoothing + backoff
|   `-- evaluation_utils.R      # Accuracy, perplexity, and timing tools
|
|-- output/                     # Pre-computed n-gram .rds files
`-- data/                       # Raw corpus

```
 
## Download Raw Data

Due to GitHub's file size limits, raw data files are not included in this repository.

Please download the dataset manually from:

[Coursera Capstone Dataset (SwiftKey)](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

Extract the following files into the `data/` folder:

- en_US.blogs.txt
- en_US.news.txt
- en_US.twitter.txt

## Features

- **N-gram model (1–4 grams)**: Supports quadgram -> trigram -> bigram -> unigram fallback.
- **Laplace smoothing**: Assigns non-zero probabilities even to unseen n-grams.
- **Backoff mechanism**: Falls back to lower-order n-grams if higher-order not found.
- **Guaranteed Top-3 predictions**: Always returns at least 3 predicted words.
- **Shiny App**: Enter a phrase and view top-N predictions with probabilities.
- **Evaluation functions**: Measure accuracy, perplexity, and runtime.

---

## Model Details

- `next_word_pred_opt_prob_all()` is the core optimized function.
- Predictions are ranked by smoothed probability.
- Predictions are generated using data.tables with precomputed n-gram frequency tables.

Refer `Code_review.pdf` for more details.

---

## Evaluation Techniques

The following functions help assess performance:

| Function                     | Purpose                                  |
|------------------------------|------------------------------------------|
| `get_accuracy()`             | Calculates top-N match accuracy          |
| `get_perplexity()`           | Evaluates sentence prediction fluency    |
| `evaluate_model_with_timing()` | Combines accuracy, perplexity, and runtime |
| `get_test_sentences()`       | Samples clean test sentences from `crude` corpus |

---

## Shiny App

> Located in `app/prediction_app.R`

You can try out the live Next Word Prediction Shiny app here:
[Next Word Prediction](https://sarim-bit.shinyapps.io/Next_Word_Prediction/)


### Features:
- Input any phrase
- Get Top-N predicted next words
- View probabilities
- Uses the **optimized model only**

---

## R Packages Used

You’ll need the following packages installed:

```r
install.packages(c("data.table", "dplyr", "stringr", "tokenizers", "tm", "shiny", "DT"))
```
---

## Limitations & Future Work

### Known Limitations:
- **Limited vocabulary**: The model cannot predict words it hasn't seen during training .
- **Purely statistical**: The model relies only on word frequency, and cannot understand context.
- **No semantic similarity**: Words like “happy” and “joyful” are treated as unrelated.
- **Model size vs performance**: Adding more n-grams improves accuracy but increases memory and load time.

### Ideas for Future Improvement:
- **Use larger or more diverse training corpora** (e.g., Wikipedia, Twitter, Reddit).
- **Implement better smoothing**: Explore alternatives like **Kneser-Ney** or **Good-Turing**.
- **Introduce neural language models** (e.g., RNNs, transformers) for context-aware predictions.
- **Track prediction confidence**: Add entropy or confidence intervals to gauge uncertainty.
- **Analyze missed predictions** to guide targeted improvements in preprocessing or model logic.


