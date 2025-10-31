Revision history for the R/ActiGlobe package

## Next Version Update: 0.x.0
- Version update and debugged: 

`CosinorM()` add MM-type esitmation
"MM": MM-type estimation (Yohai 1987) is a two-step robust estimation procedure, via \code{MASS::rlm}. It is more resistant to outliers than the typical OLS estimation. 

## Version Update: 0.2.0, 2025-10-31

- New master and class functions added: NA

- Version update and debugged: Description, README

## Version Update: 0.1.9, 2025-10-27

- New master and class functions added: NA

- Version update and debugged: ggActiGlobe, ggCosinorM, vignettes, TAdjust

## Version 0.1.8, 2025-09-23
- New master and class functions added: C2T

- Version update and debugged: CosinorM, TimeFormat

## Version 0.1.7, 2025-09-13

- New master and class functions added: CosinorM

- Version update and debugged: plot.ActiGlobe

## Version 0.1.6, 2025-09-01

- New master and class functions added: plot.ActiGlobe

- Version update and debugged: GuessTZ, DST2GL, TAdjust

- Sample data added: FlyEast, FlyEas.adj, TLog

## Cumulative Version Update 0.1.1 - 0.1.5, 2025-06-01

- New master and class functions added: DST, DST2GL, Date2TotalT, GuessTZ, TZ2UTC, UTCwDST

- Version update and debugged: BriefSum, TAdjust, R2P

## Version 0.1.0, 2025-04-01

- New master and class functions added: R2P, UTC

- Version update and debugged: BriefSum, TAdjust.

## Version 0.1.0, 2025-03-01

- A soft-coding version containing: BriefSum, DateFormat, Num2UTC TAdjust, TravelLog, TimeFormat, UTC2Num, write.act, write.cosinor.

- 2024 version of IANA time table is now added 

## Version 0.0.1, 2024-06-01

- A base version of 2024 paper with Giorgio

- All functions were originally semi-hard coded with M8W as the wrap-around pipeline function. Modification is essential to make it flexible.
