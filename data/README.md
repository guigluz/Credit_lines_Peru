README

# Raw

- base_muestra_seudonimizada.parquet -- sample from the credit data (100 firms)

- Reporte_SistemaFinanciero.csv -- the extract the code uses (via `read_reporte()` in code/00_sbs_utils.R).
  Source: SBS Series Estadisticas. Monthly, Jan-2001 -- Sep-2022; 114 institutions of banca múltiple,
  empresas financieras, cajas municipales, cajas rurales and edpymes / empresas de créditos. No subsystem
  total rows (sum the institutions with `reporte_subsystem()`; this reproduces the old file's published
  totals to within S/ 8 thousand).

  Columns: Total Activo, Créditos (total / MN / ME), Depósitos (total / MN / ME), Patrimonio Contable,
  Depósitos Vista / Ahorro / Plazo, Ratio de liquidez MN / ME. Money in thousands of soles, ME already
  valued in soles; liquidity ratios in percent. Vista + Ahorro + Plazo = Depósitos (to rounding); a blank
  Vista next to positive deposits means none. The liquidity ratios have implausible values (above 1,000)
  in about 530 rows; clean them before use.

  Format: UTF-8 with BOM, comma-separated, dates dd/mm/yyyy.

- Reporte(01-2001 - 09-2022).csv -- earlier extract, kept for reference and no longer read by the code.
  Banca Múltiple and Empresas Financieras only (60 institutions plus the two subsystem total rows);
  UTF-32LE with no BOM. Its six credit / deposit columns are identical to the new file's for every
  shared institution-month.
  
- sbs_boletin -- Folder for data downloaded from SBS's Información Estdística de Banca Múltiple 
  - B-2343 -- "direct" credit by type of product by bank (downloaded by 01_collect_direct.R)
  - B-2322 -- "indirect" credit by type of product by bank (downloaded by 01_collect_indirect.R)
  - B-2358 -- deposits and credit by branch and location (downloaded by 01_collect_deposits.R) 

# Processed

- b2358_branch_panel.csv.gz -- branch-level panel of deposits and credit (from B-2358)
- b2322_unused_credit_panel.csv -- bank-level panel of unused credit (from B-2322)
- b2343_total_credits.csv -- bank-level panel of total credit (from B-2343)
- crosswalks/ -- one bank-name crosswalk per dataset, written by that dataset's cleaning script. Each has
  four columns: the name as printed in that dataset (`bank_raw`), `bank_standardized`, `entity_type`,
  `review_required`. Spokes written before 2026-11-17 (b2343, b2322, b2358) gain `entity_type` the next time
  their cleaning script runs.
  `bank_standardized` is the only key shared across datasets: to merge A and B on banks, join
  A -> bank_standardized <- B. All are built from `documentation/bank_map_manual.csv` (see below).
  - xwalk_b2343.csv -- B-2343 (02_clean_direct.R)
  - xwalk_b2322.csv -- B-2322 (02_clean_indirect.R)
  - xwalk_b2358.csv -- B-2358 (02_clean_deposits.R)
  - xwalk_reporte.csv -- Reporte_SistemaFinanciero.csv (all five subsystems)
    (written by 03_validate_direct.R, the only script that standardizes it)
  - xwalk_rcc.csv -- RCC entity code (`entidad` in the credit parquet) -> `bank_standardized`; unlike the others it
    is keyed by code, and carries `unmapped_note` for codes with no hub id (written by 11_loan_reserve_shock_regression.R)
- bank_deposit_shares.csv -- MN / ME deposits (S/ thousands) and currency shares for every Reporte institution and month,
  with `entity_type` and validation flags (10_reserve_shocks.R)
- bank_encaje_shocks.csv -- bank reserve-requirement shocks, unique by `bank_standardized` x month-end `fecha_reporte`; the
  deposit-share baseline date is stored in `baseline_date` and set by `BASELINE_DATE` at the top of 10_reserve_shocks.R
- credit_balances_by_currency.csv.gz, credit_line_growth_panel.csv.gz -- credit balances and log loan growth by
  firm-bank-credit-line status (11_loan_reserve_shock_regression.R; its header lists each output)


# Documentation

- diccionario RCC.doc -- SBS RCC field dictionary
- RCC_empresas_supervisadas_2026.xls -- RCC entity code -> name (`Codigo`, `Tipo`, `Estado`, `Nombre`). Tab-delimited text
  saved with an .xls extension, ISO-8859-1; read with `fread(sep = "\t", encoding = "Latin-1")`
- Capitulo IV manual de cuentas.pdf -- SBS chart of accounts, Ch. IV (explains the 14-digit account codes)
- bank_map_manual.csv -- hand-curated map from each canonical bank name to `bank_standardized` (the hub key),
  with a cited note per row. Edited by hand only; the scripts read it and never write it. Covers the banca
  multiple, plus every financiera, caja municipal, caja rural and edpyme / empresa de créditos in Reporte.
  `entity_type` gives each bank_standardized its SBS subsystem: `banca_multiple`, `empresa_financiera`,
  `caja_municipal`, `caja_rural`, `empresa_credito` (edpymes included), or `aggregate` for a published total row. Successions are merged
  only with a cited SBS resolution; candidate successions are recorded in `note` and left unmerged.
