# ggsketch: Easy multi-panel figures

Drag and drop publication-ready figures without leaving your R environment.

## Overview

The `ggsketch` package provides a simple drag-and-drop interface for combining plots together, straight from your R environment.
Compatible plot objects are loaded in automatically when you start up the app--no exporting required!

Some of the current features include:
* Drag-and-drop functionality
* Compatibility with `ggplot2` and `grob`s
* Automatic panel labeling
* Complex layout handling

Planned features for future versions:
* Compatibility with `gt` tables
* Ability to upload and insert PDF or image files
* Caption text editor
* Preset output sizes based on journal specifications

## Installation

### From GitHub

```r
# install.packages("remotes")
remotes::install_github("username/package-name")
```

### Dependencies

Dependencies should be installed automatically. If needed:

```r
install.packages(c("ggplot2", "patchwork", "dplyr", "shiny"))
```

## Usage

```r
library(ggsketch)

# Open the interface:
ggsketch::run_app()
```
Then open the app in your browser--all your compatible objects will load in automatically!
PDFs generated using `ggsketch` can be downloaded straight from the app, or extracted from `/inst/shinyapp/www/`.

`ggsketch` also comes with an Addin, which you can run from the Addins dropdown in RStudio or assign to a keyboard shortcut:
```
Tools > Modify Keyboard Shortcuts... > Filter "Run ggsketch"
```


## Citation
If you use `ggsketch` to generate figures in your publication, please consider citing the package:
```
  Taylor C (2026). _ggsketch: Easy Multipanel Figures_. R package version 0.1.3, <https://github.com/clovestay/ggsketch>.
```

## Contributing

Contributions, bug reports, and feature requests are welcome. Please open an issue or submit a pull request.


## Contact

Maintained by Clove Taylor 🍀

Email: [cloves@stanford.edu](mailto:cloves@stanford.edu)
