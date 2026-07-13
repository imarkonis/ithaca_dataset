# ESSD pre-submission checklist

**Target journal:** *Earth System Science Data* (ESSD), Copernicus Publications  
**Confirmed manuscript type:** Data description article  
**Purpose:** Gate every manuscript draft before submission for ESSD-specific format, figure, data-policy, and structure compliance.  
**Last checked:** 2026-07-13

## 1. Scope and article fit

- [ ] The manuscript is framed as a **data description article**, not primarily as a scientific interpretation paper.
- [ ] The manuscript describes an original, reusable Earth system dataset/data product and clearly explains its generation, use cases, limitations, uncertainty, and evaluation.
- [ ] The dataset is already deposited, or ready for review, in a suitable research data repository at submission.
- [ ] The manuscript emphasizes **data significance, quality, reusability, provenance, uncertainty, and access**.

## 2. Length and file-size gates

ESSD/Copernicus does **not** publish hard word-count limits for data description article abstracts, main text, or individual sections in the current author guidance. Use the journal wording below as the gate.

- [ ] Abstract is **short, clear, concise, intelligible to a general reader**, and written in English.
- [ ] Abstract includes the dataset DOI/data citation once the accepted/final DOI exists. If there are up to 5 active data DOIs, list them in the abstract and data availability section; if more than 5, compile them in a dedicated table.
- [ ] No citations in the abstract unless urgently required.
- [ ] Abbreviations are avoided in the abstract unless defined there.
- [ ] Main text is concise but complete; no journal-specific hard main-text word cap was found for ESSD data description articles.
- [ ] Upload-system short summary is prepared: **maximum 500 characters including spaces**, English, one paragraph, no lists, avoids abbreviations.
- [ ] Review manuscript PDF is **≤ 50 MB**, portrait, 1-column format, embedded fonts, numbered pages, and line numbers.
- [ ] Overall submitted files excluding supplements are **≤ 30 MB**.
- [ ] Supplement file, if used, is **≤ 50 MB** as PDF or ZIP; larger material is deposited in a FAIR-aligned repository with DOI.

## 3. Figure, table, map, and caption gates

- [ ] Figures and tables are inserted in the manuscript near first mention, not collected at the end.
- [ ] Multi-panel figures are provided as **one figure file per figure**; do not use `\subfloat` or similar LaTeX commands.
- [ ] Production figure files are individually named with Arabic numbering, e.g. `f01`, `f02`, ..., and supplied together in a single ZIP without subfolders after acceptance.
- [ ] Accepted figure formats: **PDF, PS, EPS, JPG, PNG, TIF**.
- [ ] Figure resolution is **300 dpi**.
- [ ] Figure width is **at least 8 cm**.
- [ ] Individual figure size: **PDF ≤ 2 MB**; other figure formats **≤ 5 MB**.
- [ ] Overall submitted files excluding supplements remain **≤ 30 MB**.
- [ ] Vector graphics are preferred as **PDF or EPS**; all fonts are embedded; PDF figures contain no hidden objects.
- [ ] Use one font family in figures where possible, preferably a sans-serif font such as Arial or Helvetica.
- [ ] Bitmap graphics are saved in a non-lossy format where possible, preferably PNG; JPG is used only for photographs.
- [ ] Colour schemes are checked for colour-vision-deficiency accessibility.
- [ ] No ESSD/Copernicus-specific RGB/CMYK colour-model requirement was found; gate figures by accessibility, readability, embedded fonts, and accepted file formats.
- [ ] Legends explaining symbols appear in the figure itself where needed, not only in the caption.
- [ ] Panel labels use lower-case letters in brackets: `(a)`, `(b)`, etc.
- [ ] Captions are concise but descriptive and are included in the manuscript text file, not embedded only inside figure files.
- [ ] All non-common abbreviations used in figures are defined in the caption or in the text.
- [ ] In running text, use `Fig.` except at the beginning of a sentence; use `Figure` at sentence start.
- [ ] Tables are numbered with Arabic numerals, are included in Word/LaTeX text rather than submitted as image/PDF tables, and have concise descriptive captions.
- [ ] Coloured table cells are avoided.
- [ ] Maps follow UN naming conventions; contested borders/place names are handled neutrally.
- [ ] Maps/aerials using third-party providers include required copyright, attribution, and licence statements in the map or caption.
- [ ] Reused/adapted figures and tables include citations and required permission/licence statements in captions.

## 4. Data, code, repository, DOI, and licence gates

- [ ] The data described in the paper are stored in a suitable research data repository; cloud storage/private links outside a repository are **not acceptable**.
- [ ] Repository meets ESSD criteria:
  - [ ] assigns persistent identifiers, preferably DOI;
  - [ ] provides temporary data-in-review links where possible;
  - [ ] provides open access free of charge with minimal barriers;
  - [ ] allows a permissive licence;
  - [ ] provides rich metadata;
  - [ ] repository interface, metadata, and DOI landing page are available in English.
- [ ] Data are available for review in the selected repository at submission.
- [ ] Final registered data DOI exists before final ESSD publication.
- [ ] Data are cited as formal references in the manuscript reference list.
- [ ] Data citation includes creators, title, publisher/repository, identifier/DOI, and year.
- [ ] Accepted data licences are **CC BY 4.0 or equivalent** and **CC0 or equivalent**.
- [ ] Do **not** use CC BY-SA, CC BY-ND, ODbL, or equivalent licences.
- [ ] Non-commercial licences such as CC BY-NC are avoided unless approved by the ESSD editorial team before submission.
- [ ] Third-party input datasets are cited and described in the Data availability section.
- [ ] Plot data needed to reproduce manuscript figures are made public where possible, ideally through a FAIR-aligned repository with persistent identifier.
- [ ] Software, algorithms, model code, and notebooks are deposited in FAIR-aligned repositories where possible, cited with DOI/persistent identifier, and included in the reference list.
- [ ] Manuscript includes **Code availability** or, if data and code are combined, **Code and data availability**.
- [ ] Manuscript includes **Data availability** near the end in the required sequence.
- [ ] If data are not publicly accessible at final publication, a detailed explanation and access route is provided; figure-replication data are publicly available in any case.

### Data availability wording gate

Use project-specific wording, but it must contain these elements:

> The [dataset name/version] is archived at [repository] and is available at [DOI or review link]. The dataset is distributed under [licence]. The code used to generate/process the dataset is archived at [repository] and available at [DOI or persistent link]. Third-party input datasets used in this study are available from the sources cited in the manuscript and listed in [table/section].

Before submission:

- [ ] Replace review links with final DOI after acceptance, if the repository workflow requires temporary review links first.
- [ ] Ensure the data DOI/data citation also appears in the abstract after final DOI registration.
- [ ] Ensure all repository records and manuscript citations use the same dataset version.

## 5. Required manuscript structure and section-order gates

Use the Copernicus template and journal sequence. Required/relevant order:

1. Title page
2. Abstract
3. Copyright statement inserted by Copernicus, if applicable
4. Introduction
5. Numbered main sections; maximum three heading levels: `1`, `1.1`, `1.1.1`
6. Conclusions
7. Appendices, if needed
8. Code availability
9. Data availability
10. Interactive computing environment, if applicable
11. Sample availability, if applicable
12. Video supplement, if applicable
13. Supplement link inserted by Copernicus, if applicable
14. Team list, if applicable
15. Author contribution
16. Competing interests
17. Disclaimer, if applicable
18. Special issue statement inserted by Copernicus, if applicable
19. Acknowledgements
20. Financial support
21. Review statement inserted by Copernicus
22. References

Additional structure checks:

- [ ] Use the Copernicus LaTeX/Word template.
- [ ] LaTeX uses the `manuscript` document class, which provides 1-column format and line numbers.
- [ ] Do not define new LaTeX commands.
- [ ] Do not add unnecessary packages beyond the Copernicus template.
- [ ] Do not use `\paragraph{}`; use up to `\subsubsection{}` only.
- [ ] URLs are spelled out rather than hidden behind hyperlinked text.
- [ ] Author contribution section is present before acknowledgements; CRediT roles may be used.
- [ ] Competing interests section is present. If none: “The authors declare that they have no conflict of interest.”
- [ ] Acknowledgements include relevant research infrastructure where applicable.
- [ ] Financial support includes funder names and grant agreement numbers.
- [ ] References follow Copernicus author-year style.
- [ ] Data, code, software, and repository records with DOIs are included in the reference list.
- [ ] Appendices are used for additional figures/tables/technical details that support the paper but are not critical to the main flow.
- [ ] Supplements are used only for items that cannot reasonably be included in the main text or appendices; they must not contain scientific interpretations beyond the manuscript.
- [ ] Supplement numbering follows Copernicus style: equations `(S1)`, figures `Fig. S5`, tables `Table S6`, sections `S3`, `S3.1`, etc.

## 6. ESSD-specific final gate for this project
_**Note**:- This section is a project-specific pre-submission gate derived from ESSD/Copernicus guidance and the agreed ITHACA/TWC dataset-paper scope; it is not a verbatim journal requirement._

For the ITHACA/TWC dataset paper, request review after the following are true:

- [ ] Target journal explicitly stated as **Earth System Science Data (ESSD)**.
- [ ] Article type explicitly treated as **Data description article**.
- [ ] Manuscript distinguishes dataset description from companion-analysis interpretation.
- [ ] Data Records section lists all released records, dimensions, spatial/temporal scale, format, repository location, and licence.
- [ ] Validation/quality/uncertainty section is present and linked to released records.
- [ ] Data availability and code availability sections contain repository links/DOIs or review links.
- [ ] Figures are dataset-facing: structure, coverage, provenance, validation, uncertainty, usability, and trust limits.
- [ ] No figure or section makes the dataset paper read primarily as a scientific claim about intensification/drying.
- [ ] Any temporary review links are replaced by final DOI(s) before final publication.

## Sources checked

- ESSD submission page: https://www.earth-system-science-data.net/submission.html
- ESSD manuscript types: https://www.earth-system-science-data.net/about/manuscript_types.html
- ESSD data policy: https://www.earth-system-science-data.net/policies/data_policy.html
- ESSD repository criteria: https://www.earth-system-science-data.net/policies/repository_criteria.html
- ESSD FAQs: https://www.earth-system-science-data.net/about/faqs.html
- Copernicus manuscript preparation: https://publications.copernicus.org/for_authors/manuscript_preparation.html
- Copernicus data policy: https://publications.copernicus.org/services/data_policy.html
- ESSD publication policy: https://www.earth-system-science-data.net/policies/publication_policy.html
