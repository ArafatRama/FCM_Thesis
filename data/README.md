# Local data

The main analysis uses the files in this directory together with mortality data downloaded from the Human Mortality Database.

## Included file

`Rates to send_hardcoded.xlsx` contains the Mercer annuity-rate input used by the pricing calibration.

## ECB yield-curve data

`data.csv` is not committed because the local copy is approximately 3.3 GB. Before running the pricing section, place the file here with this exact name:

```text
data/data.csv
```

The supplied analysis expects an ECB yield-curve CSV whose header begins with the following fields:

```text
KEY,FREQ,REF_AREA,CURRENCY,PROVIDER_FM,INSTRUMENT_FM,PROVIDER_FM_ID,DATA_TYPE_FM,TIME_PERIOD,OBS_VALUE
```

Do not commit this file to ordinary Git hosting. Store it in an external data archive or another large-file service and record its source alongside the repository release.
