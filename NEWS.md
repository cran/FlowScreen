# FlowScreen 2.0

## Released: 2025-06-04

This release introduces major enhancements to the FlowScreen package, improving data compatibility, usability, and analytical capabilities for hydrometric data screening.

### New Features

- **Expanded Data Compatibility**:
  - Support for new data sources: ROBIN (Reference Observatory of Basins) and GRDC (Global Runoff Data Centre).
  - Automatic metadata extraction from input files.

- **Custom Metadata and Plot Titles**:
  - Users can define custom station names and metadata fields.
  - New `set.plot.titles()` function allows flexible title formatting.

- **Data Completeness Tools**:
  - `missingness()` function: Visualizes and summarizes missing, partial, and complete years.
  - `check_completeness()` function: Evaluates whether a dataset meets user-defined completeness criteria.

### Improvements

- Simplified reading of non-standard CSV files with flexible column naming.
- Enhanced default settings and plot customization options.
- Improved help files and documentation for better user guidance.

### Other Enhancements

- Improved support for screening individual metrics.
- Better handling of incomplete years and data quality flags.
- Sample datasets included for WSC, USGS, ROBIN, and GRDC.

---

For more information, visit the CRAN page: https://cran.r-project.org/web/packages/FlowScreen/index.html
