# Rayyan helper functions

Set of R functions to perform and automate tasks for the screening of papers for systematic reviews and meta-analysis using Rayyan software (Ouzzani et al., 2016).

Last updated: 24/01/2023

R version: 4.2.2

------------------------------------------------------------------------

## 1. Functions

### Split references

Functions to randomly select and split a list of references from an exported Rayyan project (<https://rayyan.ai/>) for a pilot assessment or for a collaborative Rayyan project.

`getpilotref`

`splitref_prop`

`randomsplit`

### Reference deduplication

Function to

`dedup` [on-going]

### Exclusion labels formatting

Function to extract label from Rayyan exported file that includes inclusion/exclusion reason as a label.

`getlabel`

## 2. Shiny app

[on-going]

## References

Ouzzani, M., Hammady, H., Fedorowicz, Z., & Elmagarmid, A. (2016). Rayyan---A web and mobile app for systematic reviews. *Systematic Reviews*, *5*(1), 210. <https://doi.org/10.1186/s13643-016-0384-4>

R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/.>
