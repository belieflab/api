
# R API submodule

## Purpose

A structured data API pipeline to get, clean, analyze, and export data and figures in a collaborative enviroment.

## About

This repository contains Getter and Helper functions which leverage the REDCapR, qualtRics, and mongolite R libraries to create data frames *directly* from REDCap, Qualtrics, and MongoDB using their respective APIs' HTTP GET request methods.

It is intended to be used as a **submodule** in the root of a parent repository, which could be a research study, project, or analysis; anything that needs data. Using a simple configuration file, this submodule can facilitate collaboration on a variety of analyses resulting from the same study or collection of data.

This submodule obviates the need to generate extracts manually -- ensuring all data cleaning and analysis efforts begin with a common, shared starting point. Collaborators will quickly discover that their individual cleaning scripts become interoperable and can be shared easily, without the need to continually modify the code. 

Additionally, data quality issues that are discovered can more easily corrected upstream, at the data source. This iterative model ensures that data will become eventually consistent as cleaning and analyses progresses.

# Getting Started!

## Directory Structure

Here is an example of how this `api/` submodule should be added to a  new or existing repository using `git submodule add git@github.com:belieflab/api.git`
```
├── .gitignore
├── api/                           <- this submodule
├── clean/                         <- cleaning scripts submodule
│   └── qualtrics/
│   └── redcap/
│   └── task/
├── config.yml                     <- global configuration file
├── export/
├── rds-combined-ca-bundle.pem
├── secrets.R
├── surveyIds.R                    <- qualtrics configuration file
├── parentRepository.Rproj         <- parent repository R project
```
### Secrets

To access the APIs you **must** configure and add `secrets.R` to the root of the parent repository:
```
# REDCap
uri   <- ""
token <- ""

# Qualtrics
apiKey  <- ""
baseUrl <- ""

# MongoDB
connectionString <- "mongodb://"
```
***DO NOT ADD ANY ADDITIONAL SECRETS TO THE REPO OR MODIFY .GITIGNORE***

### SSL PEM file
 Additionally, if you are connecting to a Mongo instance with SSL, you must also add the `rds-combined-ca-bundle.pem` to the root of the parent repository as well.
 
### Configuration File
The `config.yml` file should also be placed at the root of the parent directory. It contains two default values, `siteAlias` and `surveyIds` where the default values are defined below: 
 ```
 default:
	studyAlias: shortnameofstudy (all lowercase)
	surveyIds: "surveyIds.R"
```
 Without this file you will run into errors with the getters.
 
### Survey Ids
If using Qualtrics, you will need to add the key-value pairs or `survey_alias` and `surveyIds`:
 ```
surveyIds <- list()
surveyIds[[ "survey_alias" ]]  <- "SV_"
```

 Without this file you will run into errors with the survey getters.

## Functions

### Getters

`getQualtrics.R`

uses the **qualtRics** API library to create data frames directly from Qualtrics
set argument label to TRUE if you want choice values. set to false if you want numeric values.

`getRedcap.R`

uses the **REDCapR** API library to generate data frames directly from REDCap

`getMongo.R`

uses the **mongolite** API library to generate data frames for each MongoDB collection

### Helpers

`fn/checkDuplicates.R`

uses the **REDCapR** API library to generate data frames directly from REDCap

`fn/createCsv.R`

uses the **mongolite** API library to generate data frames for each MongoDB collection

## Data Cleaning

### Directory Structure

All data cleaning scripts are to be labeled with the standard measure alias and stored in the `clean/` directory of their respective platforms:
```
├── qualtrics
│   └── clean/
│	   └── rgpts.R
│   └── surveyIds.R/
├── redcap
│   └── clean/
│	   └── wtar.R
├── tasks
│   └── clean/
│	   └── prl.R

```
## Data Frames
### Assignment
Assign variables using the lower case measure alias to keep things consistent:

```
source("api/getSurvey.R")
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
## Data Export
Helper functions are located in `fn/` directory and should be called when needed in your cleaning scripts by first sourcing the appropriate script and then calling the function, for example:
```

source("api/fn/createCsv.R")
createCsv(rgpts_clean)
```
# Best Practices
## NDA Required Variables
NDA variables are required by the NIH and should be included in each of your datasets.
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
> `as.Date(rgpts_clean$interview_date, "%m/%d/%Y")`

## ...For Qualtrics Surveys

### Keep the `ResponseId` column
> in addition to the NDA required variables

### Keep all raw item level responses
> in addition to computing composite, mean, total scores, etc.

### Rename the attention check variable
e.g., `rgpts_attention_check`

### Always check for duplicate entries
By invoking the appropriate helper function in `fn/`
```

source("api/fn/checkDuplicates.R")
checkQualitricsDuplicates(rgpts)
```
Report any duplicates to  your data manager so they can fix them in the database.
