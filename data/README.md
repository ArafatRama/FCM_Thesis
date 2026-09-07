# Local data

The main analysis uses the files in this directory together with mortality data downloaded from the Human Mortality Database.

## Included file

`Rates to send_hardcoded.xlsx` contains the Mercer annuity-rate input used by the pricing calibration.

## ECB yield-curve data

The approximately 3.3 GB `data.csv` input is stored in a compressed archive tracked with Git LFS. Install Git LFS before cloning or pulling the repository so the archive is downloaded to:

```text
data/ECB_AAA_SpotCurve.zip
```

The archive must contain `data.csv`. The supplied analysis expects that CSV's header to begin with the following fields:

```text
KEY,FREQ,REF_AREA,CURRENCY,PROVIDER_FM,INSTRUMENT_FM,PROVIDER_FM_ID,DATA_TYPE_FM,TIME_PERIOD,OBS_VALUE
```

The Git repository stores an LFS pointer while the full dataset is stored through the configured Git LFS backend.
