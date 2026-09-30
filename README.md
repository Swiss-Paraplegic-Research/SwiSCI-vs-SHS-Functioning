# SwiSCI–SHS functioning comparison

R scripts accompanying **“Functioning in persons with Spinal Cord Injury compared with the general population”**, submitted to the *Journal of Clinical Epidemiology*.

This study links functioning information from the Swiss Spinal Cord Injury Cohort Study (SwiSCI) Community Survey and the Swiss Health Survey (SHS). It combines International Classification of Functioning, Disability and Health (ICF)-based conceptual harmonization with Rasch measurement methods to support comparisons between persons with spinal cord injury or disease (SCI/D) and the Swiss general population.

Data from the 2012, 2017 and 2022 survey waves were used to develop the common functioning metric. The epidemiological comparisons used the 2022 wave.

## Repository contents

The current release contains four scripts in `R syntax/`:

| Script | Purpose |
| --- | --- |
| `SwiSCI_preparation.R` | Imports the three SwiSCI survey waves, selects and aligns variables, and exports the combined functioning dataset. |
| `SHS_preparation.R` | Imports SHS telephone, questionnaire and indicator files; aligns variables across waves; handles survey filters; and exports functioning and health-condition datasets. |
| `SwiSCI_Rasch_Step3.R` | Applies previously estimated item parameters to SwiSCI data; constructs testlets; evaluates model fit, local dependence, dimensionality and differential item functioning; calculates person scores, reliability and test information; and exports item parameters. |
| `SHS_Rasch_Step3.R` | Applies previously estimated item parameters to SHS data; evaluates initial and refined item sets, model fit, local dependence, dimensionality and differential item functioning; calculates person scores, reliability and test information; and exports item parameters. |

**Scope:** these scripts cover data preparation and survey-specific Step 3 analyses. The current release does not contain the complete study workflow. In particular, the Step 2 anchor-selection analysis, pooled calibration, construction of the final calibration samples, transformation to the common 0–100 score, and matching and epidemiological comparison scripts are not included.

The preparation scripts do not directly generate all inputs required by the Step 3 scripts. Additional intermediate datasets and the previously estimated item-parameter table are required.

## Data availability

Participant-level data are not included in this repository.

- **SwiSCI:** data access requests may be directed to the SwiSCI Study Center at [contact@swisci.ch](mailto:contact@swisci.ch), subject to the applicable access conditions.
- **SHS:** anonymized microdata for scientific research may be requested from the Swiss Federal Statistical Office at [sgb@bfs.admin.ch](mailto:sgb@bfs.admin.ch), subject to approval and a data-protection agreement. The study authors are not permitted to redistribute these data.

Access to the original survey data alone is not sufficient to run the Step 3 scripts: the intermediate files below are also needed.

## Software

The manuscript reports **R 4.5.1**. The scripts load the following packages:

```r
packages <- c(
  "admisc", "car", "cowplot", "dplyr", "foreign", "ggplot2",
  "GPArotation", "gtools", "here", "Hmisc", "lordif", "lubridate",
  "mirt", "nFactors", "plyr", "polycor", "psych", "remotes",
  "sjlabelled", "tidyverse", "xlsx"
)

install.packages(packages)
```

Package versions are not pinned in the current release. Record `sessionInfo()` when running an analysis.

## Required local inputs

Paths are resolved from the project root with the `here` package. Create an RStudio project in the repository root, or otherwise configure `here` to use that directory.

| Location | Required files |
| --- | --- |
| `00_Data/2024-N-002/` | `2024-N-002_2012__2025-03-25.csv`, `2024-N-002_2017__2025-03-25.csv`, `2024-N-002_2022__2025-03-25.csv` |
| `00_Data/SHS/2012/Data/` | `indic12_CH.txt`, `sfb12_CH.txt`, `tel12_ch.txt` |
| `00_Data/SHS/2017/Data/` | `indic17_CH.txt`, `sfb17_CH.txt`, `tel17_ch.txt` |
| `00_Data/SHS/2022/Data/` | `indic22_CH.txt`, `sfb22_CH.txt`, `tel22_ch.txt` |
| `00_Data/` | `data_SCIM_rasch_allwaves_persons.csv`, `data_SHS_rasch_allwaves_persons.csv`, `item_anchor_allsample.csv` |

The final row contains the prepared survey-specific datasets and parameter table read by the Step 3 scripts. None of these input files is supplied in this repository. Filenames reflect the study exports; users with separately authorized exports may need to adapt paths and check variable names and coding.

## Using the scripts

1. Clone or download the repository and set the project root.
2. Install the required packages and obtain authorized access to the necessary inputs.
3. Create the output directories before running the preparation scripts:

   ```r
   dir.create(here::here("03_Rasch", "SwiSCI"), recursive = TRUE, showWarnings = FALSE)
   dir.create(here::here("03_Rasch", "SHS"), recursive = TRUE, showWarnings = FALSE)
   ```

4. Review and run the relevant preparation script in a clean R session. Check the input delimiters, missing-value codes and variable definitions against the data release being used.
5. Supply the appropriate Step 3 intermediate datasets and item-parameter table. These are not interchangeable with the preparation-script outputs.
6. Review the execution notes below, then run the survey-specific Step 3 script in a clean R session.

The current scripts require the study inputs and the correction noted below; they do not provide an automated, end-to-end reproduction of all manuscript results.

### Execution notes

- In `SwiSCI_Rasch_Step3.R`, the age-category assignment currently uses `data_SwiSCI_all_wave$age_quest_cat`. The dataset defined by the script is `data_SwiSCI_all_waves`; correct the assignment to `data_SwiSCI_all_waves$age_quest_cat` before running that section.
- Both Step 3 scripts import `item_anchor_allsample.csv` and set the model parameter estimation flags to `FALSE`. The imported parameters and their item names must correspond to the model being evaluated.
- The test-information plots overlay a rescaled standard-error curve. The information-axis values should not be read as raw standard errors; the unscaled values are stored in `df_info$SE`.

## Outputs

Preparation outputs are written under `03_Rasch/SwiSCI/` and `03_Rasch/SHS/`. These include the combined SwiSCI functioning dataset, wave-specific SHS functioning datasets, SHS health-condition data and descriptive frequency tables.

The Step 3 scripts export:

- `00_Data/IRT_parms_SwiSCI.csv`
- `00_Data/IRT_parms_SwiSCI_fit.csv`
- `00_Data/IRT_parms_SHS.csv`
- `00_Data/IRT_parms_SHS_fit.csv`

Additional model diagnostics, person-score estimates and plot objects are created in the R session. The information plots are stored as `p_swisci_info` and `p_shs_info`; the scripts do not automatically save them as image files.

## Methods and citation

Consult the manuscript and supplementary material for the ICF linking procedure, harmonized response categories, anchor selection, sample definitions, psychometric results and epidemiological analyses.

Until the publication details are available, refer to:

> Martinez de la Torre A, Ehrmann C, et al. *Functioning in persons with Spinal Cord Injury compared with the general population*. Manuscript submitted to the Journal of Clinical Epidemiology.

Repository: [Swiss-Paraplegic-Research/SwiSCI-vs-SHS-Functioning](https://github.com/Swiss-Paraplegic-Research/SwiSCI-vs-SHS-Functioning).

When citing the code, include the commit identifier or release used.

## Contact

For questions about the study or scripts, contact [Adrian Martinez de la Torre](mailto:adrian.martinez@paraplegie.ch). For access to participant data, contact the relevant data custodian listed above.

