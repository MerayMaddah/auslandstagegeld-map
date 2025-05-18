# auslandstagegeld-map

This repository hosts an interactive map visualizing the official **Auslandstage- und Auslandsübernachtungsgelder (per diem and accommodation allowances)** as specified by the **German Federal Ministry of the Interior (BMI)** in the **ARVVwV 2025 regulation**.

The data is sourced directly from the publicly available government document issued on **October 22, 2024**, and effective from **January 1, 2025**. This ensures that the information presented is authentic and reflects the latest official allowances for international business travel as per §16 of the Bundesreisekostengesetz (BRKG).

## Key Features
- Interactive Leaflet-based world map.
- Visual representation of daily and overnight allowances for over 190 countries.
- Color-coded per diem ranges with detailed tooltips.
- Data authenticity based on official German government sources.

## Data Source
- **Official Document:** ARVVwV 2025 (Allgemeine Verwaltungsvorschrift)  
- **Date Issued:** 22 October 2024  
- **Effective From:** 01 January 2025  

## Live Map  
[Click here to view the interactive map](https://meraymaddah.github.io/auslandstagegeld-map/)

## License  
This project is shared under the **CC0 1.0 Universal (Public Domain Dedication)**.  
The visualization is free to use, share, and adapt without restriction.

## Data Transparency 📊 

The `clean-data.csv` file is included in this repository to ensure full transparency regarding the data sources and processing steps used in generating the interactive map.
This allows users to reproduce, verify, or further develop the visualization based on the official values specified by the German Federal Ministry of the Interior (BMI) in the ARVVwV 2025 regulation.
 - Raw R Script for Transparency here 👉 📥 [Download the R script here](./map_script.R)

## Known Data Adjustments/Limitations ‼️ 

- Some countries and territories do NOT have complete or standardized spatial representations in the official "shapefile" used for the visualization *(this very well could be a fault of my own merging code)*.
To ensure their inclusion in the map, **approximate centroid points** were manually coded for the following cases:
  - **France (FRA)**
  - **Norway (NOR)**
  - **Kosovo (XKX)**
  - **French Guiana (GUF)**

- These adjustments ensure that official per diem as well as accommodation allowances for these locations are still properly visualized, even though their precise geographic boundaries are not displayed.

- All adjustments are clearly documented in the data processing scripts and visualization code.

## Contributing!!

Contributions to improve data accuracy, visualization design, or functionality are very welcome!

If you would like to contribute:

1. **Fork** this repository.
2. Create a new **branch** for your feature or fix.
3. **Commit** your changes with clear and descriptive messages.
4. Open a **Pull Request** explaining your proposed changes.

Or, alternatively, you can open an **Issue** if you:
- Notice discrepancies in the data.
- Have suggestions for improving the visualization.
- Encounter technical problems with the interactive map.

All contributions should maintain the project's focus on transparency, data authenticity, and open accessibility.
Und vielen Dank im Voraus an alle, die zur Verbesserung dieser Datenquelle beitragen können!! :)
