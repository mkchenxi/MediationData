# MediationData

**MediationData** is an R package for auditing, reconstructing, and documenting
mediation-analysis datasets from complex research codebases.

The package is designed for situations where:
- mediation models are implemented across many scripts (R, Rmd, Mplus, etc.),
- raw data availability is partial or uncertain,
- and reproducibility requires a clear, auditable record of what *can* and *cannot*
  be reconstructed from available files.

The package uses the OpenAI Responses API to assist with large-scale code inspection,
while keeping all decisions and results transparent to the user.

---

## 1. Exported functions

### `generate_mediation_repro_codes()`

This is the **main entry point** of the package.

It performs the following steps:

1. Scans a project directory to detect locally available raw data files.
2. Bundles all project code files into a single text-based PDF.
3. Uploads the PDF to the OpenAI API.
4. Prompts the model to:
   - detect mediation analyses,
   - assess reconstructibility given the available raw data,
   - generate R code for dataset reconstruction when possible,
   - generate R code defining a metadata tibble.
5. Writes the generated code to disk.
6. Validates whether the generated R code references data files that are *not*
   present locally and reports any mismatches.

**Function signature**

```r
generate_mediation_repro_codes(
  prompts,
  api_key,
  folder_dir,
  model = "gpt-4o-mini",
  temperature = 0,
  api_base = "https://api.openai.com/v1",
  recursive = TRUE,
  cleanup_uploaded_files = TRUE,
  output_data_code = "data_code.R",
  output_metadata_code = "metadata_code.R"
)
````

**Key return values**

The function returns (invisibly) a list containing:

* `detected_data_files` – raw data files found locally
* `data_code_path` – path to generated reconstruction code
* `metadata_code_path` – path to generated metadata code
* `referenced_data_files` – data files referenced by generated code
* `missing_referenced_data_files` – referenced files not found locally
* `validation_passed` – logical flag indicating whether all referenced files exist

---

### Other exported helpers

Depending on your installation, the package may also export utilities such as:

* `detect_raw_data_files()` – scan directories for candidate raw data files
* `write_code_files_pdf()` – bundle code into a PDF for upload

These are primarily intended for internal or advanced use.
Most users only need `generate_mediation_repro_codes()`.

---

## 2. Worked example

The script below is adapted from `try_package.R`.
It demonstrates a complete end-to-end run of the package.

```r
# ------------------------------------------------------------
# Example: auditing a mediation study codebase
# ------------------------------------------------------------

library(MediationData)

# Directory containing the original mediation study code and data
input_dir <- "C:/Users/chen/Downloads/mediation_studies/2nb6x"

# Instruction prompt passed to the OpenAI model
# This defines the reconstruction rules and output format
my_instructions <- "
You are an expert in statistical programming (R, Python, Stata), mediation analysis, data reconstruction, and code-base forensics. I will upload a single integrated file (pdf) that contains all project code (merged scripts).

Your tasks are to:

1. Detect all mediation analyses in the project.
2. Determine whether their datasets can be reconstructed from the raw data I actually possess.
3. Produce R code to reconstruct the mediation datasets when possible, or NULL when not.
4. Produce metadata describing all mediation models.
5. Produce R code to generate a metadata tibble.

Follow the workflow below.

--------------------------------------------------

1. Read and interpret the uploaded project code

You will receive one file that contains all source code from the project.
From this text:

1. Parse all scripts, functions, data wrangling steps, and modeling code.
2. Identify every mediation analysis, including:
   - Simple, parallel, or serial mediation
   - Moderated mediation
   - Longitudinal mediation
   - Multilevel mediation implemented in R, Python, or Stata
3. For each mediation model, extract:
   - Predictor(s)
   - Mediator(s)
   - Outcome(s)
   - Control variables
   - Clustering or grouping structure, if any
   - Dataset name used for estimation
   - Any variable transformations such as centering, scaling, or composite construction
4. Assign each mediation model a unique ID such as M1, M2, M3, and so on.

Output this information in a section titled:
List of mediation analyses detected.

--------------------------------------------------

2. Determine whether each mediation dataset can be reconstructed

Using the raw data files I provide:

1. For each mediation dataset:
   - Trace the variables back through the code to determine which raw or intermediate files they come from.
   - Determine whether all required raw variables exist in the files I possess.

2. Classify reconstruction feasibility as one of the following:
   - Fully reconstructible: all necessary raw data files exist and all variable transformations are documented.
   - Partially reconstructible: some data or steps are missing; reconstruct what can be reconstructed and document assumptions.
   - Not reconstructible: essential raw data are not available.

3. If a dataset is not reconstructible:
   - The R code output for constructing that dataset must be NULL.
   - Metadata must explicitly state that reconstruction was not possible and why.

--------------------------------------------------

3. Produce R code to reconstruct all reconstructible mediation datasets

For each mediation model:

If reconstruction is fully or partially possible, produce clean, standalone R code that:
- Loads necessary packages
- Reads in all raw data files using the filenames and formats I provide
- Performs all data cleaning, merging, recoding, and transformation steps required
- Constructs a final dataset named m1_data, m2_data, and so on

If reconstruction is not possible, output:
mX_data <- NULL

and annotate in comments which raw files or steps were missing.

Present all reconstruction code under the header:
R code to reconstruct mediation datasets.

--------------------------------------------------

4. Construct a mediation metadata table

Create a table with one row per mediation model, including the following columns:
- analysis_id
- dataset_name
- treatment_var
- mediator_vars
- outcome_var
- control_vars
- variable_descriptions
- reconstruction_status, with values full, partial, or not possible
- reason_if_not_reconstructible
- code_source_location, indicating where in the .txt file the model appeared

Present this under the header:
Metadata summarizing the mediation analyses.

--------------------------------------------------

5. Generate R code to build a metadata tibble

Write R code that constructs a tibble named mediation_metadata with the following fields:
analysis_id
dataset_name
treatment_var
mediator_vars
outcome_var
control_vars
variable_descriptions
reconstruction_status
reason_if_not_reconstructible
code_source_location

Present this under the header:
R code to define mediation_metadata tibble.

--------------------------------------------------

6. Handling missing, ambiguous, or unavailable data

If the project code references:
- A dataset not included in the files I have
- A variable that cannot be derived from any raw data available
- A preprocessing script whose inputs are unavailable

Then:
1. Clearly state what information is missing.
2. Explain why reconstruction is impossible.
3. For that mediation model, return mX_data <- NULL.
4. Fill the metadata fields with:
   - reconstruction_status set to not possible
   - reason_if_not_reconstructible describing the missing elements

If reconstruction is partially possible, provide R code that constructs what can be built and document assumptions.

--------------------------------------------------

7. Final output format

Your final answer must contain these four sections in order:

1. List of mediation analyses detected
2. R code to reconstruct mediation datasets
3. Metadata summarizing the mediation analyses
4. R code to define mediation_metadata tibble
"

# Run the main wrapper
res <- generate_mediation_repro_codes(
  prompts    = my_instructions,
  api_key    = Sys.getenv("OPENAI_API_KEY"),
  folder_dir = input_dir,
  model      = "gpt-4o-mini",
  temperature = 0
)

# Inspect validation results
res$validation_passed
res$missing_referenced_data_files
```

In this example, the package correctly identifies that the mediation dataset
used in the Mplus model cannot be reconstructed from the locally available raw data.

---

## 3. Example outputs

Below are example outputs generated by the package.

### 3.1 `data_code.R`

This file contains R code to reconstruct all datasets that *can* be reconstructed.
When a mediation dataset cannot be reconstructed, it is explicitly set to `NULL`
with detailed comments explaining why.

```r
# DATA_CODE

# ---- Packages ----
library(tidyverse)
library(here)

# ---- Raw pilot data (only files available) ----
# The only raw files you have are the pilot CSVs, which are *not* used
# in the Mplus mediation model.

mba <- read.csv(here("Raw Data", "pilot_raw_data_mba_osf.csv"),
                stringsAsFactors = FALSE)
ug  <- read.csv(here("Raw Data", "pilot_raw_data_undergrad_osf.csv"),
                stringsAsFactors = FALSE)

names(mba) <- tolower(names(mba))
names(ug)  <- tolower(names(ug))

# Clean MBA data
mba <- mba %>%
  select(q2_1:q1) %>%
  rename(gender = q1) %>%
  rename_all(.funs = ~ sub("q2_", "q", .x)) %>%
  mutate(
    population = "mba",
    gender = factor(gender,
                    levels = c(1, 2, 3),
                    labels = c("Male", "Female", "Other"))
  )

# Clean undergrad data
ug <- ug %>%
  select(gender = q180, q1:q8) %>%
  mutate(population = "undergrad")

# Combine pilot samples
pilot <- bind_rows(mba, ug) %>%
  filter(!is.na(gender))

# Create pilot scales
pilot <- pilot %>%
  mutate(
    endorse = rowMeans(select(., c(q3, q4, q6, q7)), na.rm = TRUE),
    norm    = rowMeans(select(., c(q2, q5, q8)), na.rm = TRUE)
  )

m_pilot <- pilot

# ---- Mediation dataset (M1) ----
# Required raw file "Raw Data/full_mba_data.csv" is missing.
# Therefore, the mediation dataset cannot be reconstructed.

m1_data <- NULL
```

---

### 3.2 `metadata_code.R`

This file defines a tibble summarizing each mediation analysis,
including reconstructibility status and reasons when reconstruction fails.

```r
# METADATA_CODE

library(tibble)

mediation_metadata <- tibble(
  analysis_id = "M1",
  dataset_name = "m1_data",
  treatment_var = "gcdomt1, gcpret1",
  mediator_vars = "gcdeft2",
  outcome_var = "gcprdvt2",
  control_vars = paste(
    "gcprdvt1, gcsoct1, gccmpt1, gender, rtr_gend;",
    "clusters: target, rater"
  ),
  variable_descriptions = paste(
    "gcdomt1: dominance at T1;",
    "gcpret1: prestige at T1;",
    "gcdeft2: deference at T2;",
    "gcprdvt2: social rank at T2"
  ),
  reconstruction_status = "not possible",
  reason_if_not_reconstructible =
    "Raw MBA file 'Raw Data/full_mba_data.csv' is unavailable.",
  code_source_location =
    "Mplus mediation model and supporting Rmd scripts."
)
```

---

## Summary

This package is designed to make mediation-data reconstruction:

* explicit,
* auditable,
* and reproducible.

Even when reconstruction is *not* possible, the package produces
clear, machine-readable documentation explaining why.

For questions, extensions, or collaboration, please open an issue
or contact the authors.

