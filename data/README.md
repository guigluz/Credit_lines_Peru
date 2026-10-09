README

# Raw

- base_muestra_seudonimizada.parquet -- sample from the credit data (100 firms)

- Reporte(01-2001 - 09-2022).csv -- credit and deposits by institution by currency
  (source: SBS Series Estadisticas). Monthly, Jan-2001 -- Sep-2022; 60 institutions plus
  the two subsystem total rows (`Banca Múltiple`, `Empresas Financieras`).
  Drives both figures in `code/peru_dollarization.R`.

  Coverage: Banca Múltiple and Empresas Financieras only.

  Format: UTF-32LE with no BOM (4 bytes/char).
  
- sbs_boletin -- Folder for data downloaded from SBS's Información Estdística de Banca Múltiple 
  - B-2343 -- "direct" credit by type of product by bank (downloaded by 01_collect_direct.R)
  - B-2322 -- "indirect" credit by type of product by bank (downloaded by 01_collect_indirect.R)
  - B-2358 -- deposits and credit by branch and location (downloaded by 01_collect_deposits.R) 

# Processed

- b2358_branch_panel.csv.gz -- branch-level panel of deposits and credit (from B-2358)
- b2322_unused_credit_panel.csv -- bank-level panel of unused credit (from B-2322)
- b2343_total_credits.csv -- bank-level panel of total credit (from B-2343)
- crosswalks/ -- one bank-name crosswalk per dataset, written by that dataset's cleaning script. Each has
  three columns: the name as printed in that dataset (`bank_raw`), `bank_standardized`, `review_required`.
  `bank_standardized` is the only key shared across datasets: to merge A and B on banks, join
  A -> bank_standardized <- B. All are built from `documentation/bank_map_manual.csv` (see below).
  - xwalk_b2343.csv -- B-2343 (02_clean_direct.R)
  - xwalk_b2322.csv -- B-2322 (02_clean_indirect.R)
  - xwalk_b2358.csv -- B-2358 (02_clean_deposits.R)
  - xwalk_reporte.csv -- Reporte(01-2001 - 09-2022).csv, which also has financieras, edpymes and cajas rurales
    (written by 03_validate_direct.R, the only script that standardizes it)


# Documentation

- diccionario RCC.doc -- SBS RCC field dictionary
- Capitulo IV manual de cuentas.pdf -- SBS chart of accounts, Ch. IV (explains the 14-digit account codes)
- bank_map_manual.csv -- hand-curated map from each canonical bank name to `bank_standardized` (the hub key),
  with a cited note per row. Edited by hand only; the scripts read it and never write it. Covers the banca
  multiple, plus the financieras, edpymes and cajas rurales that appear in Reporte. Successions are merged
  only with a cited SBS resolution; candidate successions are recorded in `note` and left unmerged.
