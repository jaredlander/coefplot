## Version Number

1.2.8

## Test environments

- Github Actions
    - windows-latest (release)
    - macOS-latest (release)
    - ubuntu-20.04 (release)
    - ubuntu-20.04 (devel)
- Windows 10, R 4.1.0
- Ubuntu 18.04, R 4.1.2
- Win-Builder
- R-Hub

## R CMD check results

There were no ERRORs, WARNINGs, NOTEs when checked locally on Windows and Ubuntu or on GitHub Actions.

WinBuild had a note of an example taking more than 10 seconds, though I imagine that has more to do with the machine doing the testing.

R-Hub had a note of an example taking more than 5 seconds, though I imagine that has more to do with the machine doing the testing.

## Tests

All pass.
