## Version Number

1.2.9

## Test environments

- Github Actions
    - windows-latest (release)
    - macOS-latest (release)
    - ubuntu-20.04 (release)
    - ubuntu-20.04 (devel)
    - ubuntu-22.04 (release)
    - ubuntu-22.04 (devel)
    - ubuntu-24.04 (release)
    - ubuntu-24.04 (devel)
- Ubuntu 12.04, R 4.5.1

## R CMD check results

There were no ERRORs, WARNINGs, NOTEs when checked locally on Ubuntu or the GitHub Actions for Windows or Mac. For Github Actions with Ubuntu-release there were no ERRORs or WARNINGs, just a NOTE, but that was related to the executiobn time of examples, likely due to using slow runners.

## Tests

All pass.
