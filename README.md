# Longevity bond pricing thesis code

This repository contains the R code and supporting data used for the mortality modelling, forecasting, and longevity bond pricing work in the thesis.

The main analysis is in `Code/Thesis_Main_Code.r`. The smaller `Code/Thesis_Conceptual_Plots.r` script creates several standalone figures used in the thesis. Package versions are recorded in `Code/renv.lock`.

## Requirements

- R 4.5.1
- Git LFS for downloading the ECB yield-curve dataset.
- RStudio is optional, but the included `Code/code.Rproj` file makes it the easiest way to open the project.
- An account with the Human Mortality Database, with access to the England and Wales and Ireland datasets.
- The local input files listed under **Data files** below.

Some R packages are compiled from source, so a compiler toolchain may also be required. On macOS, install the Xcode Command Line Tools if package installation reports compiler errors.

## Set up the project

Clone the repository and enter its directory:

```sh
git clone <repository-url>
cd <repository-directory>
```

Rename the environment example inside `Code`:

```sh
cd Code
mv .Renviron.example .Renviron
```

Open `.Renviron` and replace the placeholder values with your Human Mortality Database username and password. The renamed `.Renviron` file is ignored by Git and must never be committed. The analysis reads it relative to the `Code` directory.

Next, open `Code/code.Rproj` in RStudio. Alternatively, enter the `Code` directory from a terminal:

```sh
cd Code
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
| Irish Life annuity rates | `Data/Irishlife_annuity_data.xlsx` |
| Bank of England nominal daily rates | `Data/glcnominalddata/BoE_daily_SpotCurve_2016-2024.xlsx` |
| ECB yield-curve observations | `Data/ECB_AAA_SpotCurve.zip` |

The compressed ECB CSV is tracked with Git LFS. Install Git LFS before cloning or pulling the repository. See `Data/README.md` for its expected format.

The analysis references these inputs relative to the `Code` directory, so no machine-specific path changes are required.

## Run the analysis

Run commands from the `Code` directory so that the R project and `renv` environment are activated correctly.

To run the complete main analysis:

```r
source("Thesis_Main_Code.r")
```

From a terminal, the equivalent command is:

```sh
cd Code
Rscript Thesis_Main_Code.r
```

The script downloads HMD data, performs exploratory checks, fits the mortality and forecasting models, runs the simulations, and calculates the longevity bond prices. It is a long, sequential research script, so it should be run from the beginning in a clean R session. Its runtime depends on the computer and on the installation used by `keras3`.

To create the standalone thesis figures:

```r
source("Thesis_Conceptual_Plots.r")
```

or:

```sh
Rscript Thesis_Conceptual_Plots.r
```

This script writes PDF figures into the current working directory.

## Repository layout

```text
.
├── README.md
├── Code/
│   ├── .Renviron.example
│   ├── .Renviron             # created locally; not committed
│   ├── Thesis_Main_Code.r
│   ├── Thesis_Conceptual_Plots.r
│   ├── code.Rproj
│   ├── renv.lock
│   └── renv/
└── Data/
    ├── README.md
    ├── Irishlife_annuity_data.xlsx
    ├── ECB_AAA_SpotCurve.zip    # tracked with Git LFS
    └── glcnominalddata/
        └── BoE_daily_SpotCurve_2016-2024.xlsx
```

R session files, IDE settings, credentials, temporary Office files, and generated figures are excluded from version control.
