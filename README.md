# Longevity bond pricing thesis code

This repository contains the R code and supporting data used for the mortality modelling, forecasting, and longevity bond pricing work in the thesis.

The main analysis is in `code/00setup.r`. The smaller `code/thesis_plots.r` script creates several standalone figures used in the thesis. Package versions are recorded in `code/renv.lock`.

## Requirements

- R 4.5.1
- Git LFS for downloading the ECB yield-curve dataset.
- RStudio is optional, but the included `code/code.Rproj` file makes it the easiest way to open the project.
- An account with the Human Mortality Database, with access to the England and Wales and Ireland datasets.
- The local input files listed under **Data files** below.

Some R packages are compiled from source, so a compiler toolchain may also be required. On macOS, install the Xcode Command Line Tools if package installation reports compiler errors.

## Set up the project

Clone the repository and enter its directory:

```sh
git clone <repository-url>
cd <repository-directory>
```

Rename the environment example inside `code`:

```sh
cd code
mv .Renviron.example .Renviron
```

Open `.Renviron` and replace the placeholder values with your Human Mortality Database username and password. The renamed `.Renviron` file is ignored by Git and must never be committed. The analysis reads it relative to the `code` directory.

Next, open `code/code.Rproj` in RStudio. Alternatively, enter the code directory from a terminal:

```sh
cd code
R
```

Restore the recorded package environment:

```r
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore()
```

## Data files

The analysis downloads mortality data directly from the Human Mortality Database using the credentials above. It also reads the following local files:

| Input | Repository location |
|---|---|
| Irish Life annuity rates | `data/Irishlife_annuity_data.xlsx` |
| Bank of England nominal daily rates | `data/glcnominalddata/BoE_daily_SpotCurve_2016-2024.xlsx` |
| ECB yield-curve observations | `data/ECB_AAA_SpotCurve.zip` |

The compressed ECB CSV is tracked with Git LFS. Install Git LFS before cloning or pulling the repository. See `data/README.md` for its expected format.

The analysis references these inputs relative to the `code` directory, so no machine-specific path changes are required.

## Run the analysis

Run commands from the `code` directory so that the R project and `renv` environment are activated correctly.

To run the complete main analysis:

```r
source("00setup.r")
```

From a terminal, the equivalent command is:

```sh
cd code
Rscript 00setup.r
```

The script downloads HMD data, performs exploratory checks, fits the mortality and forecasting models, runs the simulations, and calculates the longevity bond prices. It is a long, sequential research script, so it should be run from the beginning in a clean R session. Its runtime depends on the computer and on the installation used by `keras3`.

To create the standalone thesis figures:

```r
source("thesis plota.r")
```

or:

```sh
Rscript "thesis plota.r"
```

This script writes PDF figures into the current working directory.

## Repository layout

```text
.
├── README.md
├── code/
│   ├── .Renviron.example
│   ├── .Renviron             # created locally; not committed
│   ├── 00setup.r
│   ├── thesis plota.r
│   ├── code.Rproj
│   ├── renv.lock
│   └── renv/
└── data/
    ├── README.md
    ├── Irishlife_annuity_data.xlsx
    ├── ECB_AAA_SpotCurve.zip    # tracked with Git LFS
    └── glcnominalddata/
        └── BoE_daily_SpotCurve_2016-2024.xlsx
```

R session files, IDE settings, credentials, temporary Office files, and generated figures are excluded from version control.
