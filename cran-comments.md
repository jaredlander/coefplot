## Version Number

1.2.7

## Test environments

- Github Actions
    - windows-latest (release)
    - macOS-latest (release)
    - ubuntu-20.04 (release)
    - ubuntu-20.04 (devel)
- Windows 10, R 4.0.2
- Ubuntu 18.04, R 4.0.3

## R CMD check results

There were no ERRORs, WARNINGs, NOTEs when checked locally on Windows and Ubuntu.

However, when checking on GitHub Actions, I get this WARNING:

> Warning: Requires orphaned package: ‘plotly’

This seems strange since I can't find anything about plotly being orphaned.

On my PC, with R 4.0.2, I sometimes get the NOTE that some examples took over 5s but I do not see this note with other versions. Same with Win-Builder but with fewer examples.

## Tests

When calling on maxLik models there is a warning about a partially matched argument. That occurs in the maxLik package and not coefplot, so it is beyond my control.
