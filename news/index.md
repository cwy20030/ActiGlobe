# Changelog

## Version 0.2.1, 2025-11-30

- New master and class functions added: NA

- New internal function added: Prob.Inact, mIANA

- Version update and debugged: ggCosinorM, TAdjust, Act2Daily, GuessTZ,
  UTCwDST

- Brief Detail:

  - piecewise binomial estimation of inactive period using Prob.Inact
  - enable internal call function for data through mIANA setting
  - augment ggCosinorM legend
  - modify internal uses of IANA in Act2Daily
  - enable switches for paralelle process for GuessTZ
  - add fallback in UTCwDST: map numeric UTC offsets to IANA Etc/GMT
    zones when OlsonNames lookup fails
  - augment test suits for all internal/helper functions

## Version 0.2.0, 2025-10-31

- New master and class functions added: boot.seci

- Version update and debugged: Description, README, CosinorM.KDE,
  ggCosinorM

## Version 0.1.9, 2025-10-27

- New master and class functions added: NA

- Version update and debugged: ggActiGlobe, ggCosinorM, vignettes,
  TAdjust

## Version 0.1.8, 2025-09-23

- New master and class functions added: NA

- New internal function added: C2T

- Version update and debugged: CosinorM, TimeFormat

## Version 0.1.7, 2025-09-13

- New master and class functions added: CosinorM

- Version update and debugged: ggActiGlobe

## Version 0.1.6, 2025-09-01

- New master and class functions added: ggActiGlobe, ggCosinorM

- Version update and debugged: GuessTZ, DST2GL, TAdjust

- Sample data added: FlyEast, FlyEas.adj, TLog

## Version 0.1.0, 2025-04-01

- New master and class functions added: R2P, UTC

- Version update and debugged: BriefSum, TAdjust.

## Version 0.1.0, 2025-03-01

- A soft-coding version containing: BriefSum, DateFormat, Num2UTC
  TAdjust, TravelLog, TimeFormat, UTC2Num, write.act, write.cosinor.

- 2024 version of IANA time table is now added

## Version 0.0.1, 2024-06-01

- A base version of 2024 paper with Giorgio

- All functions were originally semi-hard coded with M8W as the
  wrap-around pipeline function. Modification is essential to make it
  flexible.
