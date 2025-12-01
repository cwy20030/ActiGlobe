---
title: 'ActiGlobe: a R package for actigraphy analysis afffected by time change'
tags:
  - R
  - actigraphy
  - time zone
  - travel
  - circadian rhythms
  - reproducibility
authors:
  - name: C. William Yao
    orcid: 0000-0002-7234-7375
    affiliation: "1, 2"
  - name: Giorgio Varesco
    orcid: 0000-0001-9385-6972
    affiliation: "3, 5"
  - name: Francois Bieuzen
    orcid: 0000-0002-9690-9168
    affiliation: 4
  - name: Guido Simonelli
    orcid: 0000-0002-5400-319X
    affiliation: "3, 5"
affiliations:
 - name: Center for Advanced Research in Sleep Medicine, Hôpital du Sacré-Cceur de Montréal, Montréal, QC, Canada
   index: 1
 - name: Department of Psychology, Université de Montréal, Montréal, QC, Canada
   index: 2
 - name: Department of Medicine, Faculty of Medicine, Université de Montréal, Montréal, QC, Canada
   index: 3
 - name: Institut National du Sport du Québec, Montréal, QC, Canada
   index: 4
 - name: Department of Neuroscience, Faculty of Medicine, Université de Montréal, Montréal, QC, Canada
   index: 5
citation_author: Yao et. al.
date: 13 August 2025
year: 2025
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---


# Summary
The adoption of actigraphy has increased in the last decades for health, chronobiology and human behavioral research using the data collected by wearables. Due to the need for uninterrupted recordings over a long period of time, many research-grade wearables do not rigorously record timestamps or geolocation during data collection. Consequently, recordings affected by time changes due to daylight-saving transitions or cross-continental travel need to be manually adjusted, which is time-consuming. ``ActiGlobe`` is an R package designed to streamline data pre-processing (with automatic time adjustment), rest-activity circadian rhythm estimation and documentation. With features supporting scientific reproducibility, ``ActiGlobe`` offers a new open-source research framework that can facilitate future large-scale studies on the influence of time changes.

# Statement of need
During long-term recording using research-grade wearables, local daylight-saving time and cross-continental travel are common scenarios that introduce time changes. Although some commercial smart wearables can triangulate wearers' geographic positions, these features are often too energy-consuming and data-intensive to sustain long-term recordings. Most research-grade devices are energy-centric, aiming to maximize uninterrupted recording time within the constraints of battery life. To ensure that storage space is prioritized for essential data during recording, many devices, such as MotionWatch [@CamNtech2021] would record only the first timestamp upon initiation. Consequently, this type of design can lead to challenges when daylight-saving time transition or cross-continental travel occurs during recording. While the task of adjusting for these time changes may seem straightforward, it can become challenging with the increase in travel frequency and in combination with local practice of daylight-saving time transition. When left unaddressed, time shifts can introduce significant bias in analysis and result interpretation (Figure 1). Although some open-source packages (e.g., GGIR) include functions to account for daylight saving time [@vanhees2025], no systematic solution has been developed for actigraphy research to adjust for time change. (Table 1)





\begin{figure}

{\centering \includegraphics{Manuscript_files/figure-latex/FigureOne-1} 

}

\caption{An Overview of Influences from Travel-induced Time Change on Long-term Recording. Dashed lines indicate midnight of each recording day. Misalignment was caused by travel-induced time change after adjusting for daylight saving transition before plot rendering.}\label{fig:FigureOne}
\end{figure}





\begin{table}
\centering
\caption{\label{tab:TableOne}Summary Comparison of Actigraphy Processing Packages in R.}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\begin{tabular}[t]{llllll}
\toprule
Package & Preprocessing & Adjust Time Change & Circadian Analyses & Consolidated Report & FAIR Support\\
\midrule
ActiGlobe & V & V & V & V & Full\\
acc & V &  & V &  & Partial\\
accelerometry & V &  & V &  & Partial\\
ActFrag & V &  & V &  & Partial\\
Actigraphy & V &  & V &  & Partial\\
\addlinespace
actigraph.sleepr & V &  & V &  & Partial\\
actverse & V &  & V &  & Partial\\
agcounts & V &  & V &  & Partial\\
CircaCP & V &  & V &  & Partial\\
GGIR & V & Partial & V & V & Full\\
\bottomrule
\multicolumn{6}{l}{\textsuperscript{} FAIR: Findable, Accessible, Interoperable, Reusable}\\
\multicolumn{6}{l}{\textsuperscript{} V: available}\\
\multicolumn{6}{l}{\textsuperscript{} Full: with a systematic design to support documentation and reproducibility}\\
\multicolumn{6}{l}{\textsuperscript{} Partial: limited functionality}\\
\end{tabular}}
\end{table}



# Features and comparison
``ActiGlobe`` is an R package designed by sleep and human performance researchers to facilitate circadian and human behavioral research using longitudinal actigraphy data, and it provides end-to-end support with functions for data preprocessing, harmonizing recordings involving cross-time-zone travel (Figure 2), rest-activity circadian rhythm estimation, consolidated report generation, and FAIR (i.e., findable, accessible, interoperable, and reusable) principle-based design for data management. The time change adjustment was designed based on the Internet Assigned Numbers Authority database. To protect individual privacy, ActiGlobe adjusts time change solely based on coordinated universal time (i.e., UTC) and applies an additional anonymization procedure when assigning wearers’ geolocations during documentation. This makes ``ActiGlobe`` well-suited for analyzing sensitive data with restricted access. Besides the traditional ordinary least squares cosinor modelling [@Cornelissen2014] and the feasible generalized least square-based modification [@Harvey1976], it also offers a new nonparametric analysis strategy based on Gaussian kernel density estimation (KDE) on circular time [@Chaubey2022]. To facilitate interoperability of results across circadian research, key rhythm parameters (e.g., acrophase; time of peak activity) can be extracted from all analytical methods. Furthermore, while the primary feature of ``ActiGlobe`` is its robust time change adjustment, all functions were designed to be generic to enhance adaptability across data formats.





\begin{figure}

{\centering \includegraphics{Manuscript_files/figure-latex/FigureTwo-1} 

}

\caption{An Overview of Recording Adjusted for Influences from Travel-induced Time Change. Dashed lines indicate midnight of each recording day. Figure was rendered using the pre-processed recording from Figure 1.}\label{fig:FigureTwo}
\end{figure}


Some of the key functions available in ActiGlobe include:

- Pre-processing 
	- BrifSum(): generates concise day-by-day summaries and documentation of adjustments and patterns
	- TAdjust(): adjusts timestamps for daylight saving and travel-related time shifts; annotates affected epochs; updates documentation of day-by-day summaries.

- Analysis
	- CosinorM(): fits linear cosinor models; estimates midline estimating statistic of rhythm (MESOR), amplitude, and acrophase with diagnostics.
	- CosinorM.KDE(): uses KDE to robustly estimate cosinor parameters under irregular patterns.
	- boot.seci(): non-parametric bootstraps circadian indices for uncertainty quantification and confidence intervals.

- Data Visualization and Documentation
	- Act2Daily(): splits harmonized recordings into day-level series for downstream analysis.
	- ggActiGlobe(): creates visualized overview of day-to-day activity across the recording period.
 	- ggCosinorM(): visualizes cosinor fits, phases, and amplitudes with model overlays.
 	- write.act(): exports processed activity data and summaries to structured and shareable subject-based files.
  	- write.cosinor(): generates subject-based PDF documentation with cosinor model results for available recording.

- Helper functions 
	- TimeFormat(): identifies or converts the input time based on the Portable Operating System Interface  measure of calendar time standard (POSIXct).
	- DateFormat(): identifies or converts the input date based on POSIXct.


# Conclusion 
``ActiGlobe`` is a new R-based open-source research tool for actigraphy research. Equipped with functions systematically designed to account for time changes of multiple sources, ``ActiGlobe`` aims to further research on human adaptation to temporal disruptions. The additional features, such as support of reproducibility and protection of participants' personal privacy, makes ``ActiGlobe`` a versatile tool for various research settings, including field research. 


# Acknowledgements 
The authors thank Dr. Antonio Martin for facilitating package documentation. We would also like to extend our gratitude to the Canadian speed-skating (short-track) athletes who participated in the study from which the data allowed this package to be developed and tested.


# References
<div id="refs"></div>
