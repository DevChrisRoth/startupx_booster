# Machine Learning Project: Predicting Entrepreneurship

This project analyzes the Global Entrepreneurship Monitor (GEM) 2021 dataset to build a supervised learning model that predicts an individual's likelihood of becoming a nascent entrepreneur.

## Project Structure

The project is organized as follows:

-   `project.R`: The main R script. **This is the only file that needs to be run.** It contains the entire workflow from data loading and conversion to model training and evaluation.
-   `data/`: This directory should contain the source data. The R script will automatically convert the `.sav` file to a faster `.csv` format on its first run.
-   `supplementary_material/`: Contains documentation for understanding the dataset variables.

## Data Source

The data and documentation were obtained from the official Global Entrepreneurship Monitor (GEM) consortium.

-   **Main Dataset:** `GEM2021APSGlobalIndividualLevelData_15Feb2023.sav`
    -   **Source:** GEM Consortium Data Sets
    -   **Link:** [https://www.gemconsortium.org/data/sets](https://www.gemconsortium.org/data/sets)

-   **Variable Descriptions:** `gem-2015-aps-individual-level-variable-descriptions...xlsx`
    -   **Source:** GEM Wiki Codebooks
    -   **Link:** [https://www.gemconsortium.org/about/wiki#collapse-wiki-codebooks](https://www.gemconsortium.org/about/wiki#collapse-wiki-codebooks)
    -   *Note: The 2015 codebook is used as a clear reference for variable themes, which are largely consistent across years.*

## How to Run

1.  Ensure the required R packages are installed (e.g., `haven`, `data.table`, `tidyverse`, `caret`). The script will load them.
2.  Place the original `GEM2021APSGlobalIndividualLevelData_15Feb2023.sav` file inside the `data/` directory.
3.  Run the `project.R` script in R or RStudio.

The script will automatically handle the one-time conversion from `.sav` to `.csv` and then proceed with the analysis.