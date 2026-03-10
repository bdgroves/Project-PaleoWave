<div align="center">

![PaleoWave Flag](https://raw.githubusercontent.com/bdgroves/Project-PaleoWave/main/assets/paleowave_flag.png)

# 🦕 Project PaleoWave

### Triassic Ichthyosaur Locality Intelligence
#### Central Nevada · Humboldt Range · Prida · Favret · Luning · Gabbs Formations

[![RF AUC](https://img.shields.io/badge/RF%20AUC-0.906%20★-1a6b5a?style=for-the-badge)](https://github.com/bdgroves/Project-PaleoWave)
[![LOO Recall](https://img.shields.io/badge/LOO%20Recall-10%2F16%2062.5%25-c9a84c?style=for-the-badge)](https://github.com/bdgroves/Project-PaleoWave)
[![Prida Core](https://img.shields.io/badge/Prida%2FFavret%20Core-10%2F10%20100%25%20◆-27ae60?style=for-the-badge)](https://github.com/bdgroves/Project-PaleoWave)
[![LiDAR](https://img.shields.io/badge/LiDAR%20TPI-11%2F20%20Basin%20Floor-7b68ee?style=for-the-badge)](https://github.com/bdgroves/Project-PaleoWave)

*Random Forest terrain model predicting undiscovered Triassic ichthyosaur fossil localities from USGS 3DEP terrain data and PBDB occurrence records. 50 priority targets ranked by composite score across the TRc formation extent in central Nevada.*

</div>

---

## 🌊 What Is PaleoWave?

Middle–Late Triassic Nevada (205–252 Ma) was a shallow marine sea. Ichthyosaurs died, sank, and were entombed in carbonate sediment. Today that limestone is exposed across the Humboldt Range as the TRc formation — and 240 million years of erosion are slowly uncovering bone.

PaleoWave trains a machine learning model on 27 PBDB ichthyosaur records from 18 unique localities, learns the terrain signature of known sites, and scores 50 candidate locations across the TRc formation extent. Phase 2 added 15m LiDAR-derived TPI to validate terrain geometry at each target.

**Anchor site:** Berlin-Ichthyosaur State Park (~40.1°N) — the world's largest ichthyosaur assemblage, equivalent role to McBones for IceWave.

---

## 🛰️ LiDAR TPI Analysis — Notebook 05

TPI (Topographic Position Index) at 15m resolution from USGS 3DEP measures how much higher or lower a pixel is than its 1500m neighborhood. Known ichthyosaur sites show strongly **negative** TPI — fossil exposure is driven by basin exhumation, not ridge erosion.

**Known site TPI: mean −42.3m · median −59.8m · 22/27 points negative**

**11 of 20 top targets confirmed as basin/exhumation terrain.**

### P01 — v1 Priority #1 (TPI-penalized in v2)
*40.4047°N, 118.2439°W · TPI=+130.1m · 52m from TRc · v1 score=1.000 → v2 #4*

![P01 LiDAR](https://raw.githubusercontent.com/bdgroves/Project-PaleoWave/main/outputs/lidar_P01.png)

> **Ridge crest terrain** — TPI=+130m places this target on the Humboldt Range backbone, not a basin floor. High ML probability and tight formation proximity drove the v1 ranking. The v2 TPI penalty (-0.05) correctly demotes it. The formation here may be dipping away from surface exposure.

---

### P02 — v1 Priority #2
*40.2092°N, 117.5878°W · TPI=+105.1m · 992m from TRc · v1 score=0.895 → v2 #9*

![P02 LiDAR](https://raw.githubusercontent.com/bdgroves/Project-PaleoWave/main/outputs/lidar_P02.png)

> **Ridge crest terrain** — 992m from TRc with strong ML probability, but positive TPI indicates upland position. No TPI adjustment applied (992m is inside 15km threshold but TPI > +3m triggers -0.05 penalty). Rank dropped 7 places in v2.

---

### P03 — v1 Priority #3
*40.5503°N, 118.2325°W · TPI=+4.4m · 1,361m from TRc · v1 score=0.895 → v2 #14*

![P03 LiDAR](https://raw.githubusercontent.com/bdgroves/Project-PaleoWave/main/outputs/lidar_P03.png)

> **Upper slope terrain** — marginally positive TPI, sitting on the formation margin. The hillshade shows dissected limestone terrain to the east with flatter basin floor approaching from the west. TPI penalty -0.05 applied; dropped 11 places in v2.

---

## 📊 Model Performance

| Version | Features | AUC | LOO Recall | Status |
|:--------|:---------|:---:|:----------:|:-------|
| **v1** | elevation, slope, aspect, TRI | **0.906** | — | Baseline |
| **v2** | v1 + TPI post-hoc adj | — | **10/16 (62.5%)** | ★ Active |
| v2 Prida/Favret core | v1 + TPI | — | **10/10 (100%)** | ◆ Sub-region |

- **Composite score:** ML probability + geo_bonus (0–0.20 by dist to TRc) + TPI adj (±0.10 within 15km)
- **TPI adjustment:** +0.10 if TPI < −10m · +0.05 if TPI < −3m · −0.05 if TPI > +3m · 0.00 beyond 15km
- **Background:** 10:1 random pixels (training) · 982-point 0.1° grid with 25km exclusion buffer (LOO)
- **LOO misses:** 3× Luning formation (38–39°N southern outliers) · 2× Favret elevation outliers · 1× unknown formation

---

## 🗺️ Top Targets — v2 Ranking

| Rank | Latitude | Longitude | Comp v2 | TPI | Dist TRc | TPI Tier |
|:----:|:--------:|:---------:|:-------:|:---:|:--------:|:--------:|
| **#1** | **40.8228°N** | **117.6969°W** | **0.990** | **−27.2m** | **1,309m** | **1A ★★** |
| #2 | 40.4189°N | 117.7044°W | 0.980 | −15.6m | 3,338m | 1A ★★ |
| #3 | 40.7872°N | 117.4564°W | 0.953 | −65.4m | 13,040m | 1A ★★ |
| #4 | 40.4047°N | 118.2439°W | 0.950 | +125.3m | 52m | X ↓ |
| #5 | 40.2653°N | 117.4797°W | 0.943 | −14.7m | 4,157m | 1A ★★ |
| #6 | 40.1131°N | 117.2025°W | 0.899 | −4.5m | 4,801m | 1B ★★ |
| #7 | 40.7997°N | 118.1403°W | 0.897 | −32.0m | 14,464m | 1A ★★ |
| #8 | 40.0956°N | 117.2456°W | 0.896 | −30.4m | 7,060m | 1A ★★ |
| #9 | 40.2092°N | 117.5878°W | 0.895 | −1.3m | 992m | 2 |
| #10 | 40.3719°N | 117.5769°W | 0.894 | −117.9m | 6,488m | 1A ★★ |

**TPI Tier:** 1A = basin floor (TPI < −10m) ★★ · 1B = lower slope (−10 to −3m) ★★ · 2 = mid slope · X = ridge/upland ↓

Full 50-target ranked list: [`data/model/paleowave_v2_top50.csv`](data/model/paleowave_v2_top50.csv) · [`data/model/paleowave_v2_top50.geojson`](data/model/paleowave_v2_top50.geojson)

---

## 📓 Notebooks

| # | Notebook | Description | Key Output |
|:--|:---------|:------------|:-----------|
| 01 | `01_pbdb_harvester.ipynb` | PBDB API harvest, Nevada ichthyosaur records | `pbdb_occurrences_clean.csv` |
| 02 | `02_terrain_analysis_final.ipynb` | 30m terrain features at PBDB localities | `features_pbdb_terrain.csv` |
| 03 | `03_ml_model_final.ipynb` | RF v1 training, 50 target scoring | `priority_targets.geojson`, AUC 0.906 |
| 04 | `04_geology.ipynb` | TRc formation distance, geo_bonus | `priority_targets.geojson` updated |
| 05 | `05_lidar_terrain_analysis.ipynb` | 15m TPI for top 20 targets + presence points | `paleowave_top20_lidar.csv`, LiDAR PNGs |
| 06 | `06_model_v2_tpi.ipynb` | TPI post-hoc adjustment, v2 re-ranking | `paleowave_v2_top50.csv` |
| 07 | `07_loo_validation.ipynb` | LOO recall, proper spatial background | `paleowave_rf_v2_final.joblib`, 10/16 recall |

---

## 📁 Repository Structure

```
Project-PaleoWave/
├── assets/
│   └── paleowave_flag.png
├── data/
│   ├── features_pbdb_terrain.csv       # 27 PBDB records with terrain features
│   ├── lidar/                          # P##_dtm.tif — 15m GeoTIFFs
│   ├── model/
│   │   ├── priority_targets.geojson    # v1 50 targets
│   │   ├── paleowave_v2_top50.csv      # v2 ranked targets with TPI
│   │   ├── paleowave_v2_top50.geojson  # v2 GeoJSON for GPS/QGIS
│   │   ├── paleowave_rf_v2_final.joblib
│   │   ├── paleowave_loo_results.csv
│   │   └── paleowave_all50_tpi.csv
│   └── pbdb/
│       ├── pbdb_occurrences_clean.csv
│       ├── pbdb_presence_tpi.csv
│       └── paleowave_background_proper.csv
├── notebooks/
│   └── 01–07 (see table above)
└── outputs/
    ├── PaleoWave_Field_Report_v2.pdf
    ├── lidar_P01.png … lidar_P20.png
    ├── tpi_distribution_paleowave.png
    ├── feature_importance_v2.png
    ├── rank_change_v1_v2.png
    └── loo_validation_paleowave.png
```

---

## 🔧 Navigation

**QGIS:** Load `paleowave_v2_top50.geojson` → style by `composite_v2` → filter `tpi_tier IN ('1A','1B')` for basin-confirmed targets only.

**Gaia GPS / Avenza:** Import GeoJSON directly. Each waypoint labeled `PW-##` with composite score in description field.

**Google Earth:** Drag GeoJSON into Earth — targets display with score attributes in popup.

---

## ⚠️ Disclaimers

**PRPA PERMITS REQUIRED** — Vertebrate fossil collection on federal land requires a permit under the Paleontological Resources Preservation Act. Unpermitted collection is a federal crime. Contact BLM Battle Mountain and Tonopah Field Offices before collecting.

**VERIFY LAND OWNERSHIP** — Coordinates are geographic analysis only. Land ownership not verified. Sites may be BLM, USFS, tribal, state, or private. Verify via BLM GeoCommunicator before entry.

**REMOTE TERRAIN** — Great Basin sites: no cell service, extreme heat, flash flood risk. Carry satellite communicator, 4L+ water/person/day, first aid. Do not go alone.

**MODEL LIMITATIONS** — Terrain features only. 18 training localities. Luning/Gabbs recall = 0% — do not use for southern formation targets without a formation-specific sub-model. Predictions are not guarantees.

Full legal disclaimers in [Field Report v2](outputs/PaleoWave_Field_Report_v2.pdf).

---

## 📡 Data Sources

| Dataset | Source | Resolution |
|:--------|:-------|:----------:|
| Ichthyosaur occurrences | [PBDB](https://paleobiodb.org) | Point |
| Terrain (30m) | USGS NED via TNM | 30m |
| Terrain (15m LiDAR) | USGS 3DEP via TNM ImageServer | 15m |
| Triassic formation extent | NBMG Nevada Geologic Map (TRc) | 1:500k |

---

## 🔭 Phase 3 Roadmap

- **Formation-stratified model** — separate Prida/Favret (north) and Luning/Gabbs (south) sub-models to address 0% Luning recall
- **Lithology-matched background** — sample background from carbonate terrain only, not random Nevada pixels
- **Expanded training data** — field-verified new localities submitted to PBDB and used for retraining
- **Structural geology integration** — incorporate formation dip/strike data to identify plunge-out zones

---

<div align="center">

*Project PaleoWave — ML-assisted ichthyosaur locality prediction, central Nevada*
*PBDB · USGS 3DEP · NBMG · github.com/bdgroves/Project-PaleoWave*

</div>
