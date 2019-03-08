### About the experiment

This experiment follows the response of a marine diatom (single-celled phytoplankton), _Skeletonema marinoi_ ([Wikipedia](https://en.wikipedia.org/wiki/Skeletonema_marinoi), [AlgaeBase](http://www.algaebase.org/search/species/detail/?species_id=N82cc88c3673218b6), [CeMEB](https://cemeb.science.gu.se/research/target-species-imago/skeletonema-marinoi)), to changes in salinity (grams of salts per kilogram of water). The cultures used in this experiment are from the [Baltic Sea](https://en.wikipedia.org/wiki/Baltic_Sea), a natural salinity gradient where _S. marinoi_ is one of the most common and important species, especially in the spring diatom bloom.

In this experiment we expose _S. marinoi_ to six salinity treatments ranging from 8 to 28 parts per thousand (ppt) with a 4 ppt increment. We monitor eight isolates from eight populations in triplicate across six salinities arriving at a total of 1152 daily measurements of relative fluorescence (RF). RF serves as a proxy for biomass and allows us to measure how quickly the cells are doubling at different salinities. The experiment is carried out in 48-well plates and relative fluorescence in each well is measured in a 3-by-3 grid of cells spanning the bottom of the well using a plate reader. Cultures are passaged in a way that ensures the same starting relative fluorescence across all wells of a plate using an automated liquid handling system.

We isolated the cultures from single cells from samples originating across the Baltic Sea. The samples have different 'native' salinities, some coming from waters with salinity > 20 ppt while others from areas with salinity as low as 5-6 ppt. The geographic locations of the samples, or Zones as we call them in the app, can be seen in the graphic below. Credit for the map goes to Geoffrey House. The salinity data are from [HELCOM](http://www.helcom.fi/).

![A map of our geographic sampling. Credit: Geoffrey House](zone_locations_small.png)

The goal of this experiment is to better understand the variation in salinity reaction norms across the Baltic Sea salinity gradient. A portion of these cultures will also be used for RNASeq experiments to help uncover some of the mechanisms involved in salinity stress response in diatoms. Ultimately, this experiment, combined with genome-scale resequencing data, will help us understand how diatoms colonize freshwaters and how they might respond to changes in ocean salinity at broader scales.

The experiment is ongoing and the data within this app are updated (almost) daily. Many of the cultures are still in the early phase of adjusting to the experimental conditions, so their growth curves and reaction norms might look wonky. It will take some time for their growth rates to stabilize across transfers and salinities.

### About the project

This work is carried at the [Alverson lab](http://alversonlab.com/) at the University of Arkansas Fayetteville. The experiment is part of a larger project called [The Genomic Basis of Microbial Adaptation to New and Changing Environments](https://www.simonsfoundation.org/team/andrew-alverson/) funded by the [Simons Foundation](https://www.simonsfoundation.org/). For this project we are collaborating with several researchers from the Baltic Sea region. Many thanks for generous help with samples, advice, and access to genomic resources for _S. marinoi_ go to [Dr. Anna Godhe](https://marine.gu.se/english/about-us/staff?languageId=100001&userId=xgodan), [Dr. Mats Topel](https://www.gu.se/english/about_the_university/staff/?languageId=100001&userId=xamatl), [Dr. Anke Kremp](https://www.researchgate.net/profile/Anke_Kremp), and [Dr. Conny Sjoqvist](https://www.connysjoqvist.com/).


### About this web application

This `R` `Shiny` web application is a tool for [us](http://alversonlab.com/people.php) to quickly assess the state of the experiment or an individual culture within it. Additional scripts are used to determine when to feed (passage/transfer) the cultures to avoid stress due to starvation.

For related work at our lab, check out these web apps on the [Baltic Sea gradient](https://diatom.shinyapps.io/baltic-sea-gradient-maps/), salinity reaction norms in [Cyclotella](https://diatom.shinyapps.io/Diatoms_and_Salinity/), and [salinity tolerance across a variety of marine and freshwater diatoms](https://diatom.shinyapps.io/diatom-salinity-niche-web/).

This app was written by [Teofil Nakov](https://teofil.discindo.org) with input on design and features from Elizabeth Ruck and Kala Downey. Kala also wrote the Help section.
