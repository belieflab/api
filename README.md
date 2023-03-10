# CAPR Data Wizardry

This repository contains analysis-related materials for the CAPR study.

### Purpose
> API data pipeline to get, clean, analyze, and export data output to .csv

## Data APIs
### Secrets
To access the APIs you **must** add `secrets.R` and `rds-combined-ca-bundle.pem` to the root of this repository.
```
├── .gitignore
├── caprDataWizardry.Rproj
...
├── rds-combined-ca-bundle.pem
├── secrets.R
...
```
***DO NOT ADD ANY ADDITIONAL SECRETS TO THE REPO OR MODIFY .GITIGNORE***

### Getter Functions

`getSurvey.R`

uses the `qualtRics` API library to create data frames directly from Qualtrics

`getRedcap.R`

uses the `REDCapR` API library to generate data frames directly from REDCap

>*Must be connected to NU GlobalProtect VPN*
>*Contact: erica-karp@northwestern.edu*

`getTask.R`

uses the `mongolite` API library to generate data frames for each task

>*Must be connected to Yale Cisco VPN*
>*Contact: joshua.kenney@yale.edu*


## Data Cleaning

### Directory Structure

All data cleaning scripts are to be labeled with the standard measure alias and stored in this directory structure at the root of the repository:
```
├── qualtrics
│   └── rgpts.R
├── redcap
│   └── wtar.R
├── tasks
│   └── prl.R
```
### Use Lower Case
Assign variables using the lower case measure alias to keep things consistent:

```
source("getSurvey.R")
rgpts <- getSurvey("rgpts")
...
```

```
source("getRedcap.R")
wtar <- getRedcap("wtar")
...
```

```
source("getTask.R")
prl <- getTask("prl")
...
```


### Use Snake Case
When finished cleaning and scoring a dataset, the final output data frame should be named using the following `_clean` convention:
```
...
rgpts_clean
```
```
...
wtar_clean
```
```
...
prl_clean
```
### Helper Functions
Additional functions are located in `fn/` directory and should be called when needed in your cleaning scripts by first sourcing the appropriate script and then calling the function, for example:
```
source("fn/createCsv.R")
createCsv(rgpts_clean)
```

### Keep NDA Required Variables
The following variables should be kept when cleaning all measures:
> `src_subject_id`
`subjectkey`
`interview_age`
`sex`
`site`
`visit`
`interview_date`
`phenotype`

### Convert `src_subject_id` to numeric
> use the `as.numeric()` function in R

### Convert US-centric interview_date to ISO format so they sort correctly
> `as.Date(dsc2$interview_date, "%m/%d/%Y")`

## ...For Qualtrics Surveys

### Keep the `ResponseId` column
> in addition to the NDA required variables

### Keep all raw item level responses
> in addition to computing composite, mean, total scores, etc.

### Always check for duplicate entries
By invoking the appropriate helper function in `fn/`
```
source("fn/checkDuplicates.R")
checkQualitricsDuplicates(rgpts)
```
Report any duplicates to  joshua.kenney@yale.edu so that he can fix them in the Qualtrics database

### Rename the attention check variable
e.g., `rgpts_attention_check`
