# Validation report — SBS B-2343, *Estructura de los Créditos Directos por Modalidad*

**Purpose.** Establish whether SBS bulletin Excel files can be reliably downloaded, parsed,
standardized and reconciled, before using the same machinery on **B-2322**
*Estructura de los Créditos Indirectos* (unused credit lines).

**Date:** 2026-08-26 · **Scope:** Enero 2001 – Junio 2026 (306 monthly workbooks)
**Reproduce with:** `code/01_collect_direct.R` → `code/02_clean_direct.R` → `code/03_validate_direct.R`

---

## Verdict

> ### Ready with documented limitations.

The bulletin workbooks parse cleanly and, at the grain where the two sources actually
overlap, they agree with Series Estadísticas essentially exactly. The limitations are
about *coverage of the comparison*, not about data quality: Source B has no modality
breakdown, so one requested grain cannot be validated at all, and there is one
18-month window where SBS's own two vintages disagree.

---

## 1. Can the B-2343 Excel files be parsed reliably?

**Yes — 305 of 306 files, with 0 unparseable cells out of 49,584.**

| Metric | Result |
|---|---|
| Files enumerated / downloaded | 306 / 306 (no HTTP failures) |
| Files parsed successfully | **305** |
| Files rejected | **1** — `B-2343-no2010.XLS` (see §5) |
| Data cells extracted | 49,584 |
| Cells that failed numeric parsing | **0** |
| Filename month ≠ in-file report date | **0** |
| Unit-scale anomalies detected (×1000, ×10⁶) | **0** |

Reliability here means *robust to format drift*, and the drift is substantial. Nothing is
addressed by fixed row or column position; every structure is located by content anchor:

| Dimension | Observed variation across the 306 files |
|---|---|
| Container format | Legacy BIFF `.xls` through **Aug 2013**; OOXML from **Sep 2013** — every file is named `.XLS` in both eras |
| Sheet name | **12 distinct values**: `32`, `Hoja1`, `P032P`, `P032`, `ESTR2`, `33-Credt`, `34-Credt`, `34-Estruct% CredtXMod`, `Estructura (P)`, … |
| Sheet count | 1 or 3 |
| Title row | row 1 or row 2 |
| Header row | row 5 or row 6 |
| Units label | `(En porcentaje)` and `( En porcentajes )` |
| Modality columns | 8 or 9, and the **column order changes** (Factoring moves between 2009 and 2013) |
| Bank rows | 11 to 20 |
| `TOTAL BANCA MÚLTIPLE` row | **absent in all 10 parsed 2001 files**, present thereafter |
| Report-date cell | Spanish text (`"Al 31 de enero de 2001"`), Excel serial (`41455`), and datetime |
| Missing marker | `-` in 2001 → `0` in 2009–2014 → `-` in 2026 (**not monotone**) |

Two traps worth naming, because both would have produced silently wrong numbers:

1. **`readxl` resolves the format from the extension before the signature.** Since SBS names
   every OOXML file `.XLS`, `read_excel()` hands it to libxls and fails — 155 files, over half
   the series. The collector now stores each file under the extension matching its true
   signature (`readxl::format_from_signature`).
2. **A tight title anchor is load-bearing.** The Nov-2010 URL serves a *different cuadro*
   whose title also begins "Estructura de los créditos directos". Requiring
   `POR MODALIDAD` is what turns that into a visible rejection instead of a silent
   mis-parse.

**Independent check of parsing correctness:** in every one of the 295 months that publish a
`TOTAL BANCA MÚLTIPLE` row, the published total equals the sum of the parsed bank rows to
within **1.2 × 10⁻⁷ miles S/**. Values read by hand from the workbooks during reconnaissance
(Jun-2014 BCP `60,915,085.48`; Jun-2013 system `155,233,731.652`; Jun-2026 system
`390,568,643.86`; Jun-2001 BCP `10,645,574`) all survive the parser unchanged.

---

## 2. Do the Excel files match the Series Estadísticas raw data?

**Yes, at the total-credit grain — with one clearly bounded exception.**

Bank × month, 3,918 comparable observations over the overlap:

| Test | Count | Share |
|---|---|---|
| Exact (\|diff\| ≤ 0.5 mil S/) | 3,340 | **85.25 %** |
| Agree within 10⁻⁴ relative | 3,891 | **99.31 %** |
| Mismatch | 27 | 0.69 % |

Of those 27: **23 are material** and **4 are ±1 mil S/ rounding**. Excluding the single
18-month window that carries all 23 (§5), the remaining **3,648 bank-months agree within
10⁻⁴ relative in 99.89 % of cases, with a maximum absolute difference of 2 mil S/** — i.e.
two thousand soles on balances in the billions.

---

## 3. For what months does the comparison work?

**260 months: Enero 2001 – Setiembre 2022.**

| | Months | Range |
|---|---|---|
| B-2343 workbooks parsed | 305 | 2001-01 … 2026-06 |
| Series Estadísticas extract | 261 | 2001-01 … 2022-09 |
| **Overlap used** | **260** | **2001-01 … 2022-09** |

The overlap is 260 rather than 261 because Nov-2010 is missing from the Excel side. The
**Oct 2022 – Jun 2026 tail (45 months) is parsed but unvalidated** against Source B, whose
extract ends in Sep 2022.

---

## 4. At what grain do the sources match?

| # | Grain | Cross-source? | n | Exact | Mismatch | Max abs diff | Max rel diff |
|---|---|---|---|---|---|---|---|
| 1 | bank × modality × month | **No — impossible** | — | — | — | — | — |
| 2 | bank × month | Yes | 3,918 | 85.25 % | 27 (0.69 %) | 122,876 mil S/ | 5.07 % |
| 3 | modality × month | Internal only | 2,631 | 99.47 % | 2 (0.08 %) | 0.0036 pp | 9.3 × 10⁻⁴ |
| 4 | system × month | Yes | 260 | 68.46 % | 18 (6.92 %) | 160,708 mil S/ | 0.43 % |
| 5 | full overlap | — | 260 months | — | — | — | — |

**Level 1 cannot be validated across sources.** `data/raw/Reporte(01-2001 - 09-2022).csv`
contains only `Créditos` and `Depósitos` (total / MN / ME) by entity-month. It has **no
modality breakdown**, so there is nothing to compare bank × modality figures against. That
grain is instead checked internally:

- 4,716 bank-months; 117 are zero-credit banks (no shares to sum).
- Of the 4,599 active bank-months, **4,575 (99.48 %) have modality shares summing to exactly
  100** within 10⁻⁶ pp. The largest deviation anywhere is **0.0049 pp**, in early-2000s
  vintages that publish fewer significant digits — SBS's own rounding, not a parsing error.
- The published system-level modality shares are reproducible from bank rows × bank totals
  to within **0.0036 pp** (level 3), which is strong indirect evidence that the modality
  columns are being read correctly even though they cannot be checked against Source B.

Note that level 4 looks *worse* than level 2 (68 % vs 85 % exact) purely because a system
aggregate sums 15–20 banks' rounding; its **relative** error is an order of magnitude
*smaller* (max 0.43 % vs 5.07 %).

---

## 5. What are the main discrepancies?

**(a) One 18-month restatement window — the only material disagreement.**

All 23 material mismatches fall in **May 2001 – Oct 2002** and belong to exactly **two
banks**: Interbank and Banco Financiero (now Banco Pichincha). The bulletin figure is
always *lower* than Series Estadísticas, by 2.5–5.1 %.

| Bank | Months in window | Mismatches | Cumulative difference (mil S/) |
|---|---|---|---|
| Interbank | 18 | 11 | −1,101,933 |
| Banco Pichincha (ex-Financiero) | 18 | 12 | −438,851 |

The system-level gap in those months **equals the sum of the two banks' gaps to within
5 mil S/** — so the discrepancy is fully additive and fully attributable. Immediately before
and after the window, both banks match to ±1 mil S/.

**(b) Nov-2010 serves the wrong cuadro.** `B-2343-no2010.XLS` contains *"Estructura de los
créditos directos según tipo y modalidad por Empresa Bancaria"* (**B-2389**), transposed
with concepts in rows and banks in columns. The B-2343 table for Nov-2010 is simply not
available at its own URL.

**(c) An SBS find-replace corrupted a column header.** In `en2024` and `ab2024` the modality
header reads **`BANCOM Exterior`** instead of `Comercio Exterior` — collateral damage from
renaming *Banco de Comercio* to *BANCOM*. Repaired in the modality crosswalk and flagged.

**(d) Rounding.** 4 bank-months differ by ≤1 mil S/ (Series stores integers; the bulletin
carries three decimals from 2013). Detectable only because the balances involved are tiny
(S/ 34.9 thousand for Banco Cencosud in Aug-2012).

**(e) Float residue.** 629 cells in the *Otros* column hold values like `2.8 × 10⁻¹⁵` or
`−9.2 × 10⁻⁸`; 7 are trivially negative. *Otros* is computed by SBS as a residual, so this
is floating-point noise. Preserved and flagged (`value_flag`), never rounded away.

---

## 6. Are discrepancies due to data quality, revisions, units, labels, or parsing?

| Cause | Verdict | Evidence |
|---|---|---|
| **Units** | Ruled out | 0 scale flags across all levels; no ×1000 / ×10⁶ ratios anywhere. Both sources are *miles de S/*. |
| **Parsing** | Ruled out | 0 unparseable cells; published totals reproduce summed bank rows to 1.2 × 10⁻⁷; hand-checked values exact. |
| **Labels / name mapping** | Ruled out | 0 unmatched entity names; mismatches are concentrated in two banks whose mappings are exact and unambiguous, and those same banks match perfectly outside the window. |
| **Scope (foreign branches)** | Ruled out | Mismatch rate is *higher* for rows **without** a scope qualifier (27 of 3,354 = 0.81 %) than for rows carrying one (**0 of 564**). BCP (260/260 months) and Scotiabank (196/196), the banks that do carry "con sucursales en el exterior", agree within 10⁻⁴ relative in every month, with a maximum absolute difference of 1 mil S/. |
| **Revisions / vintage** | **This is the cause** | Perfectly contiguous May-2001…Oct-2002 window; two banks only; one-signed; fully additive to the system level; exact agreement on both sides of the window. The archived bulletin is an as-published snapshot; Series Estadísticas serves a later restated series. SBS never rewrote the archived workbooks. |
| **Data quality (SBS-side)** | Two isolated instances | Wrong cuadro at the Nov-2010 URL; corrupted header in two 2024 files. |
| **Precision** | 4 observations | ±1 mil S/, integer-vs-decimal storage. |

---

## 7. Which issues must be resolved before analysing B-2322?

1. **⚠️ The most important one: Series Estadísticas cannot validate B-2322 at all.** The
   local extract has no créditos indirectos series — not by modality, not even in total. The
   totals-based cross-check that carried this validation **does not transfer**. Before
   trusting B-2322, obtain an independent level series for indirect credit (e.g. a Series
   Estadísticas export including *Créditos Indirectos*, or cuadro **B-2309**), or accept
   internal-consistency checks only and say so.
2. **B-2322 is also expressed in percentages.** Unused lines must be reconstructed as
   `share ÷ 100 × Total Créditos Indirectos (miles de S/)`. The target column is
   *"Líneas de Crédito no utilizadas y créditos concedidos no desembolsados"* — 71.5 % of
   system indirect credit in Jun-2026.
3. **Screen every month for a wrong-table substitution**, as Nov-2010 proves is necessary. The
   tight-title-anchor rule must be carried over, with B-2322's own title.
4. **B-2322 reaches back to Dec-1998 and the early files are a different table entirely** —
   `B-2322-di98.XLS` is *"CREDITOS CONTINGENTES Y COBRANZAS"*, in levels, with multi-row
   headers and currency splits. It also uses a **2-digit year** in the filename. Locate where
   the modern layout begins rather than assuming 1998 is comparable.
5. **Carry over the missing-marker policy.** `-` and `0` both appear and mean the same thing
   in different eras; neither may be assumed.
6. **Resolve the one open entity mapping** (§9).

---

## 8. What assumptions and transformations were made?

**Assumptions (each one testable, and tested):**
- Column K is the only level column, in *miles de S/*; columns B–J are percentage shares. *(Confirmed by the units label and by the share-sum identity.)*
- Both sources report in *miles de S/*. *(Confirmed: 0 scale flags.)*
- B-2343 covers **Banca Múltiple only**, so the 31 financieras/edpymes/CRACs in Source B are out of scope. *(Excluded list written to `seriesest_entities_excluded.csv`.)*
- Where the in-file date is missing or unparseable, the filename month governs — this never triggered (0 of 305).
- For the 10 files of 2001 with no `TOTAL` row, the system total is recomputed as the sum of bank rows and **labelled as such** in `system_source`.

**Transformations:**
- Cached files renamed to the extension matching their true signature.
- Numeric normalization: thousands separators, `,`/`.` decimal marks, parentheses-as-negative. `-` and blanks → `NA`, **never silently 0**; the zero reading is applied explicitly and only where a row total exists.
- Every original cell string is preserved in `value_text` alongside the parsed `value_num`.
- Entity names: accent-stripped, uppercased, punctuation-normalized, `B.` expanded to `BANCO`. Trailing footnote markers (`2/`, `*`, `**`) and the legal suffixes `S.A.`/`S.A.A.` are stripped **into their own column**, not deleted.
- Scope qualifiers `(con sucursales en el exterior)` are extracted to `scope_note`, never dropped — precisely so their effect could be tested (§6).
- Matching ladder: exact canonical → canonical after scope-strip → curated manual map → unmatched. **No fuzzy or similarity matching at any point.**
- Modality labels: footnote markers stripped to `footnote_ref`; `Fáctoring`→`Factoring`; `Tarjetas de Crédito*`→`Tarjetas de Crédito`; `BANCOM Exterior`→`Comercio Exterior` (flagged `label_repaired`).

**Documented entity successions** (`data/documentation/bank_map_manual.csv`) — merged only with a citation:
- Banco Azteca del Perú → **Alfin Banco** — *Res. SBS N° 03054-2021*, same legal entity, series continuous. (Ownership changed in 2020; a break for ownership analysis, not for entity identity.)
- Banco Continental → BBVA Perú; Banco Financiero → Banco Pichincha; Banco de Comercio → BANCOM — rebrands.
- **Not merged:** Wiese Sudameris and Sudamericano are kept separate from Scotiabank; HSBC Perú separate from Banco GNB; Deutsche Bank, Cencosud, Banco del Trabajo, BankBoston, Standard Chartered, BNP Paribas Andes are exits with no successor.

**Definitional break to respect in any time-series work:** *Res. SBS N° 5570-2019* moved
non-revolving financings for credit-card holders out of **Tarjetas de Crédito** and into
**Préstamos** from **January 2021**. Those two series are not comparable across that date.
`Comercio Exterior` first appears in **Nov 2003**; earlier blanks are absence of the column,
not zeros.

---

## 9. Which observations require manual review?

`output/validation/review_queue.csv` — 54 items.

| Item | Severity | n | Status |
|---|---|---|---|
| `bank_month_mismatch` (material) | high | 23 | **Explained** — the 2001–02 restatement window (§5a). Decide which vintage to use. |
| `unparsed_file` | high | 1 | **Explained** — Nov-2010 wrong cuadro. Month unavailable. |
| `bank_name_mapping` | high | **1** | **Genuinely open** — see below. |
| `bank_month_mismatch` (rounding) | medium | 4 | Benign, ≤1 mil S/. |
| `coverage_excel_only` | medium | 1 | BNP Paribas Andes, Oct-2006, total = 0. Benign. |
| `share_sum_off_100` | low | 24 | SBS rounding, max 0.0049 pp. Benign. |

**The one genuinely open question:** `B. Santander Central Hispano` (Jan 2001 – Feb 2003) is
held **separate** from `Banco Santander Perú` (from Oct 2007). There is a 4.5-year gap
between them and the corporate relationship has not been verified, so they were not merged.
If a continuous Santander series is needed, this must be researched first.

All 52 raw entity names map to 30 standardized banks plus the system aggregate; **0 remain
unmatched**. 28 raw modality labels map to 9 modalities plus the total column.

---

## 10. Is the dataset ready for the next phase?

> ### Ready with documented limitations.

**What is established.** The collector is deterministic and cached; the parser survives
12 sheet names, two container formats, three date encodings, shifting column order and a
moving header row, with zero unparseable cells; entity and modality crosswalks are complete,
auditable and free of guessed merges; and where the two sources genuinely overlap they agree
to within two thousand soles outside a single, fully-explained window.

**Limitations carried forward.**
1. **Bank × modality × month is unvalidated across sources** — Source B has no modality data. Internal consistency is strong (§4) but it is not an independent check.
2. **Oct 2022 – Jun 2026 (45 months) is unvalidated** — the Series Estadísticas extract ends Sep 2022. Re-exporting it through Jun 2026 would close this.
3. **Nov 2010 is unavailable** at its URL.
4. **May 2001 – Oct 2002 requires a vintage choice** for Interbank and Banco Financiero.
5. **The totals cross-check does not transfer to B-2322** (§7.1) — this is the main thing to solve before the créditos indirectos work, and it is a data-sourcing question, not a code question.

None of these is a defect in the pipeline; each is a stated boundary on what the available
data can prove.

---

## Outputs

| Path | Contents |
|---|---|
| `data/raw/sbs_boletin/B-2343/` | 306 cached workbooks (git-ignored) |
| `data/raw/sbs_boletin/manifest_B-2343.csv` | URL, status, bytes, sha256, true format, timestamp |
| `data/processed/b2343_long.csv` | 49,584 rows, bank × modality × month, full provenance |
| `data/processed/b2343_total_credits.csv` | bank × month total direct credit (reconciliation key) |
| `data/processed/crosswalks/xwalk_b2343.csv` | 52 raw names → 30 banks, with review flags (minimal crosswalk: `bank_raw`, `bank_standardized`, `review_required`) |
| `data/documentation/bank_map_manual.csv` | curated, cited successions and aliases |
| `output/validation/parse_diagnostics.csv` | one row per file: anchors found, counts, date check |
| `output/validation/recon_bank_month.csv` | level 2, with `mismatch_kind` |
| `output/validation/recon_system_month.csv` | level 4, with `system_source` |
| `output/validation/recon_modality_month.csv` | level 3 (internal) |
| `output/validation/internal_modality_checks.csv` | level 1 share-sum and range checks |
| `output/validation/summary_by_level.csv` | the level table in §4 |
| `output/validation/discrepancy_attribution.csv` | mismatch rates by bank, year, format, scope |
| `output/validation/revision_window_analysis.csv` | additivity test behind §5a |
| `output/validation/review_queue.csv` | 54 items needing a human decision |
| `output/validation/seriesest_entities_excluded.csv` | 31 non-banca-múltiple entities |

**Tolerance policy.** Exact = \|diff\| ≤ 0.5 mil S/ (nearest thousand soles) · absolute ≤ 1 mil S/ ·
relative ≤ 10⁻⁶ · loose ≤ 10⁻⁴ · share-sum ≤ 10⁻⁶ pp · scale flag when the ratio is within
0.1 % of 10³, 10⁻³, 10⁶ or 10⁻⁶. All reported maxima, means and medians are untruncated.
