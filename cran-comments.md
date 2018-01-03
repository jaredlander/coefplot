## Version Number
1.2.5

## Test environments
- travis-ci latest version
    - R-oldrel
    - R-release
    - R-devel
- Windows 10, R 3.4.2
- Windows 10, R 3.5.0 (R-Devel)
- Win Builder
- Ubuntu 16.04 (WSL), R 3.4.3

## R CMD check results
There were no ERRORs or WARNINGs other than a note to CRAN maintainers as seen below.

* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: 'Jared P. Lander <packages@jaredlander.com>'

On my PC, with R 3.4.2, I get the NOTE that some examples took over 5s but I do not see this note with other versions. Same with Win-Builder but with fewer examples.
