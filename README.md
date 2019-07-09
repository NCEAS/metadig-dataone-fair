#
# Evaluating FAIR within the DataONE network of repositories

- **Contributors**: Matthew B. Jones, Peter Slaughter, Ted Habermann
- **Contact**: jones@nceas.ucsb.edu
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/NCEAS/metadig-dataone-fair)

Provides code for analysis and graphing of FAIR qualities from the metadata in the 
[DataONE network of data repositories](https://www.dataone.org/current-member-nodes), including the
[KNB Data Repository](https://knb.ecoinformatics.org), [Dryad](http://datadryad.org),
and the NSF [Arctic Data Center](https://arcticdata.io).
Each DataONE repository implements metadata using dialects of their choosing,
and MetaDIG provides a cross-dialect engine for runninng comprehensive quality
checks for improvement and guidannce.  In this repository, we analze the results
of those checks and create graphs of changes in collection quality over time
overall within the DataONE network and within individual repositories.

## Getting started

The analytical code is present in the top level `plotFAIRMetrics.Rmd` file.  The data
file is not included in this repository but is accessible in the archived release on the KNB:

Matthew Jones, Peter Slaughter, and Ted Habermann. 2019. Quantifying FAIR: metadata improvement and 
guidance in the DataONE repository network. Knowledge Network for Biocomplexity. [doi:10.5063/F1KP80GX](https://doi.org/doi:10.5063/F1KP80GX).

## Acknowledgments

Work on this package was supported by:

- NSF DIBBS grant #1443062 to T. Habermann and M. B. Jones
- NSF-DATANET grants #0830944 and #1430508 to W. Michener, M. B. Jones, D. Vieglais, S. Allard and P. Cruse
- NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden

Additional support was provided for working group collaboration by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)

