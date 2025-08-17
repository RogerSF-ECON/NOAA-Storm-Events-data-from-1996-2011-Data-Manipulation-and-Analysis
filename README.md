NOAA Storm Events: Health Impacts & Economic Consequences (1996–2011)




This project analyzes the U.S. NOAA Storm Events database to answer two questions:

Which event types are most harmful to population health?

Which event types have the greatest economic consequences?

The analysis is implemented in R Markdown using a tidyverse-first workflow, and is designed to be fully reproducible from the raw compressed CSV.

🔍 Key Findings (1996–2011)

Population health:

Tornado has the highest combined injuries + fatalities (~22,178; injuries dominate).

Excessive Heat has the highest fatalities (~1,797).

Flood, Thunderstorm Wind, and Lightning are also major contributors.

Economic losses (nominal USD):

Flood: ~$149.5B (mostly property).

Hurricane (Typhoon): ~$87.1B.

Storm Surge/Tide: ~$47.8B.

Drought is crop-heavy (~$13.4B crop vs. ~$1.0B property).

(Results are shown in two figures and top-10 tables in the report.)


If you prefer not to commit the large CSV, the Rmd will auto-download it.

🧰 Requirements

R ≥ 4.2

R packages: tidyverse, lubridate, janitor, forcats, scales, readr, rmarkdown, knitr
(optional) ggrepel is not required for the final plots.

Install packages:

install.packages(c("tidyverse","lubridate","janitor","forcats","scales","readr","rmarkdown","knitr"))

📥 Data

Source file (compressed CSV):
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

The Rmd looks for repdata_data_StormData.csv.bz2 locally and downloads automatically if missing.

▶️ Reproduce the Analysis
Option A — Knit in RStudio

Open storm_analysis.Rmd.

Click Knit → HTML (or PDF).

Output appears in the same directory.

Option B — Render via script

Create run.R:

# run.R
rmarkdown::render("storm_analysis.Rmd", output_format = "html_document")


Then run:

Rscript run.R

🛠️ Methods (brief)

Scope: Restricted to 1996–2011, when NOAA standardized event types; earlier years have noisier labels.

Cleaning: Consistent snake_case names (janitor::clean_names()), date parsing (bgn_date).

Damage exponents → USD: H=1e2, K=1e3, M=1e6, B=1e9; digits 0–8 → 10^digit. Ambiguous or blank codes treated as 1 (conservative).

Event type standardization: Normalize EVTYPE (uppercase, strip punctuation, squash whitespace) and map common variants via regex (e.g., TSTM WIND → THUNDERSTORM WIND, HURRICANE/TYPHOON → HURRICANE (TYPHOON)).

Diagnostics:

Label reduction: EVTYPE 508 → 230 standardized categories (~55% reduction).

Top unmapped (already clean) labels: HAIL, THUNDERSTORM WIND, FLASH FLOOD, etc., confirming minimal over-rewriting.

Aggregations:

Health = fatalities + injuries.

Economy = property + crop damages.

📊 Figures (≤3 requirement met)

Health Impact — Top 10 (stacked injuries + fatalities).

Economic Damage — Top 10 (stacked property + crop).

(The assignment allows ≤3 figures; we use two.)

📝 RPubs

You can publish the knitted HTML from RStudio (Publish → RPubs).
Suggested description:

“NOAA Storm Events analysis (1996–2011) identifying the most harmful weather hazards for health and the most costly economically, using tidyverse-based cleaning, exponent-to-USD conversion, and standardized event labels.”

Paste your RPubs link here once published.

⚠️ Notes & Limitations

Dollar amounts are nominal (not inflation-adjusted).

A few extreme events (major floods/hurricanes) dominate totals.

Residual label heterogeneity is possible; further mapping into the official 48 NOAA categories is optional and won’t change top ranks materially.

🔄 Reproducibility

All steps are executed inside the Rmd from the raw CSV; chunks use echo = TRUE.

For strict environments, consider renv:

install.packages("renv")
renv::init()  # snapshot package versions


🙌 Acknowledgments

I thank Johns Hopkins University and the Coursera Data Science Specialization team for developing the Reproducible Research course and assignment. Special thanks to Prof. Roger D. Peng for the lectures and materials.

Also thanks to Tidyverse ecosystem for data wrangling and visualization.

🤝 Contributing

Issues and pull requests are welcome. For substantial changes, open an issue to discuss what you’d like to modify.
