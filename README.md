# Evaluating FAIR within the DataONE network

- **Contributors**: Jeanette Clark, Matthew B. Jones, Peter Slaughter, Ted Habermann, Chris Beltz
- **Contact**: jclark@nceas.ucsb.edu
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/NCEAS/metadig-dataone-fair)
- **Citation**: metadig-dataone-fair: Evaluating FAIR within the DataONE network. S. Jeanette Clark, Matthew B. Jones, Peter Slaughter, Ted Habermann, Chris Beltz. doi:10.18739/A2RB6W465

Provides code for analysis and graphing of FAIR qualities from the metadata in the 
[DataONE network of data repositories](https://www.dataone.org/current-member-nodes), including the
[KNB Data Repository](https://knb.ecoinformatics.org)
and the NSF [Arctic Data Center](https://arcticdata.io).
Each DataONE repository implements metadata using dialects of their choosing,
and MetaDIG provides a cross-dialect engine for running comprehensive quality
checks for improvement and guidance.  In this repository, we analyze the results
of those checks and create graphs of changes in collection quality over time
overall within the DataONE network and within individual repositories.

## Getting started

The analytical code used to generate statistics and figures is located in `inst/manuscript`. Data used in this analysis, as submitted to Nature Scientific Data, are available at [DOI]. 

This package can be installed and loaded using standard `devtools` commands after a `git clone`, or installed directly via `devtools::install_github("NCEAS/metadig-dataone-fair")`. 

## Acknowledgments

Work on this package was supported by:

- NSF DIBBS grant #1443062 to T. Habermann and M. B. Jones
- NSF-DATANET grants #0830944 and #1430508 to W. Michener, M. B. Jones, D. Vieglais, S. Allard and P. Cruse
- NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden

Additional support was provided for working group collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

![NCEAS logo](https://www.nceas.ucsb.edu/files/logos/NCEAS/NCEAS-full%20logo-4C.jpg)

