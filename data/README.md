README

# Raw

- base_muestra_seudonimizada.parquet -- sample from the credit data (100 firms)

- Reporte(01-2001 - 09-2022).csv -- credit and deposits by institution by currency
  (source: SBS Series Estadisticas). Monthly, Jan-2001 -- Sep-2022; 60 institutions plus
  the two subsystem total rows (`Banca Múltiple`, `Empresas Financieras`).
  Drives both figures in `code/peru_dollarization.R`.

  Coverage: Banca Múltiple and Empresas Financieras only.

  Format: UTF-32LE with no BOM (4 bytes/char).

# Processed



# Documentation

- diccionario RCC.doc -- SBS RCC field dictionary
- Capitulo IV manual de cuentas.pdf -- SBS chart of accounts, Ch. IV (explains the 14-digit account codes)
