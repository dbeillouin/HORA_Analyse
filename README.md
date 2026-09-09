# Non-cereal agroforestry as a globally overlooked reservoir of biodiversity and nutritional diversity

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22671247.svg)](https://doi.org/10.5281/zenodo.22671247)
[![Software Heritage](https://archive.softwareheritage.org/badge/origin/https://github.com/dbeillouin/HORA_Analyse/)](https://archive.softwareheritage.org/browse/origin/https://github.com/dbeillouin/HORA_Analyse/)

Data and analysis code for Beillouin et al., *Cell Reports Sustainability*
(manuscript CR-SUSTAINABILITY-D-25-00109).

Everything needed to reproduce the analyses and figures is contained in this
archive. No account, login or API key is required.

## Contents

```
code/    R scripts (see run order below)
data/    the study database and the tables used to classify species and outcomes
figures/ output directory, populated when the figure scripts are run
archive/ earlier exploratory scripts and intermediate tables (2022-2024), kept
         for provenance; not needed to reproduce the published analysis
```

### data/

| File | Content |
|---|---|
| `experiments.csv` | The raw extraction table, one row per experiment: species lists for the agroforestry treatment and its paired reference system, country, coordinates, farm type, system type, and the extracted outcomes. 5,311 rows from 679 articles. `Data_Load.R` applies the inclusion filters described in the paper (removal of systems whose only non-woody component is a Poaceae, and of a small number of records excluded on full-text re-reading), which yields the **4,512 experiments from 638 studies** reported in the manuscript. |
| `species_characteristics.csv` | One row per plant species (891 species): growth form, life history, woodiness, height, edibility, medicinal and timber use, FAO commodity group, IUCN Red List status, accepted botanical family. |
| `MAP_SARAH_HORA.csv` | Experiment-level summary derived from the two tables above (species counts by functional group, dominant composition, coordinates). Used by the mapping and structure figures. |
| `Outcomes.xlsx` | Ecosystem-service screening at study level (title and abstract, one column per service). |
| `Corres_outcome2.xlsx` | Look-up table mapping each reported outcome to a CICES v5.1 service category. |

### code/

Run in this order, in a single R session — the figure scripts use objects
created by `Data_Load.R` and do not reload the data themselves.

| # | Script | Produces |
|---|---|---|
| 0 | `00_run_all.R` | Runs everything below in order. |
| 1 | `Data_Load.R` | Loads and cleans the database; builds `TAB_FINALE`, `DATA2` and the species tables used by every other script. Also computes the species, family and IUCN counts reported in Results §1 and §3. |
| 2 | `Fig_1.R` | Figure 1 — global distribution of experiments. |
| 3 | `Prop_Regions.R` | Regional aggregation used in Figure 2 and Table S1. |
| 4 | `FIG_3.R` | Figure 3 — structural complexity and functional diversity. |
| 5 | `FIG_4.R` | Figure 4 — co-occurrence of FAO crop groups. |
| 6 | `Fig_5.R` | Figure 5 — threatened taxa; also the mixed-effects models reported in Results §3. |
| 7 | `FIG_6.R` | Figure 6 — ecosystem-service coverage and combinations. |
| 8 | `Pair_wise_service.R` | Pairwise service co-occurrence panel of Figure 6. |
| 9 | `Flux_sankey.R` | Supplementary species-association flow diagram. |

## Requirements

R ≥ 4.2, with:

```r
install.packages(c("tidyverse", "readr", "dplyr", "tidyr", "stringr", "ggplot2",
                   "patchwork", "scales", "ragg", "ggalluvial", "ggnewscale",
                   "ComplexUpset"))
```

## How to run

From the repository root, in a fresh R session:

```r
source("code/00_run_all.R")
```

That runs the nine scripts in the order given above and writes the figures to
`figures/`. To run them one at a time, source `code/Data_Load.R` first and keep
the same session.

## Notes

Species names were standardised with the Taxonomic Name Resolution Service;
conservation status is the IUCN Red List version 2024-1; commodity groups follow
the FAO classification, completed with Ecocrop. Searches were run on
16 March 2021 and the corpus does not include later literature.

## Archived version and citation

This repository is archived on Zenodo, which assigns a DOI to each release, and
in Software Heritage, which archives the full git history.

- **Concept DOI (all versions, use this one):** [10.5281/zenodo.22671247](https://doi.org/10.5281/zenodo.22671247)
- Version DOI (v1.0.0): [10.5281/zenodo.22671248](https://doi.org/10.5281/zenodo.22671248)
- Software Heritage identifier of the released tree:
  `swh:1:dir:108687528034ea055bab4a580c08f0cdebfd086b`

The code and the curated database were produced by D. Beillouin and S. K. Jones;
`CITATION.cff` records them as the authors of the software and the article as the
preferred citation for the study.

## Licence

Code released under the MIT licence (see `LICENSE`); the contents of `data/`
are released under CC BY 4.0.
