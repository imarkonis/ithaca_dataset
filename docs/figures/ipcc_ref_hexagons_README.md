# IPCC reference-region hexagon layout

`ipcc_ref_hexagons.csv` contains the established cartogram positions used by ITHACA. Rajani Kumar manually set the positions before completing his PhD; the exact date and original construction procedure are not documented here. This provenance was supplied by Johanna Thomson on 15 October 2026.

The CSV was copied without modification from the supplied server file, originally at `/mnt/shared/data/geodata/ipcc_v4/gloabl_ipcc_ref_hexagons.csv`. The server folder identifies the reference-region source as `ipcc_v4`; the exact source release has not been independently verified.

The committed file is unshifted. `code/06b_plot_dataset_performance.R` applies the existing fixed shifts to GIC, MDG, NAU/CAU/EAU/SAU, and NZ when reading it. Do not pre-apply those shifts to this CSV.

The file has 315 rows and 45 region acronyms. Required plotting fields are `Acronym`, `long`, `lat`, `group`, `V1`, and `V2`. SHA-256 of the unchanged supplied file: `100d3c6b81469336c1cb94c5193086c5d33e60d67b682b586ce7be01b26b1cc7`.
