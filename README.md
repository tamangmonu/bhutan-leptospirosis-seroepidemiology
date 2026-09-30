# Leptospirosis seroprevalence and risk factors in subtropical Bhutan, 2024

R code for the statistical analysis in *Seroepidemiology of human leptospirosis in subtropical Bhutan, 2024* (manuscript in preparation).

The script estimates seroprevalence (ELISA IgM, ELISA IgG and MAT), describes primary reacting serovars, and fits univariable and multivariable logistic regression models for MAT seropositivity (five model-selection strategies, AIC comparison, diagnostics for the final model, and an influential-observation sensitivity analysis).

## Data availability

The participant-level dataset is **not** included in this repository because it contains health information from human participants. Data access is subject to the ethics approvals and data-sharing rules of the study. Please contact the corresponding author.

## How to run

1. Place your dataset at `data/LeptoBhutan_data.xlsx` (first sheet).
2. Open `Lepto_Seroprevalence_Analysis.R` in R/RStudio and set the working directory to the repository root.
3. Run the script. Missing packages are installed automatically; results are written to `output/`.

## Expected columns

`Sex`, `Age`, `Settlement`, `Level_of_Education`, `Occupation_Group`, `ELISA_IgM`, `ELISA_IgG`, `MAT_Result`, `Serovar`, `Contact_Rat`, `Contact_Rabbit`, `Contact_Bat`, `Contact_Goat`, `Contact_Cattle`, `Contact_Dog`, `Contact_Cat`, `Contact_Pig`, `Symp_Fever`, `Symp_Headache`, `Symp_Myalgia`, `Symp_Arthalgia`, `Symp_Vomiting`, `Symp_Diarrhea`, `Symp_stomache`, `Symp_jaundice`, `Skin_injuries`, `Fever_12months`, `Hospitalized_12months`, `Garbage_facility`, `Open_defecation`, `Batinfested_cave`, `Marshy_land`, `Contact_stagnantwater`, `Flood_exposure`, `Paddy_Plantation`, `Fishery_farming`, `Drink_pondwater`, `Drink_lakewater`, `Drink_unboiled`, `Visit_cave`, `Walk_barefoot`, `Visit_forest`, `Contact_deadanimals`, `River_bath`, `Pond_bath`, `Lake_bath`

## Reference categories used in the regression models

| Variable | Reference category |
|---|---|
| Settlement | Rural |
| Level of education | Secondary (Class 7 to 12) |
| Sex | Female |
| Binary exposures | No |
| Animal contact | No contact |

## Author

Monu Tamang

## License

Add a license of your choice (e.g. MIT) before publishing.
