# siR
## SIR-inspired COVID-19 modeling, [Shiny-fied](https://cgrilson.shinyapps.io/model)

The model used for this simulation is an example of a [compartmental model](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology), in which the population is divided into compartments, or disease states. Here, those states are **S**usceptible, **I**nfected, and **R**emoved (recovered or dead). Individuals transition between the states according to transition rates, which we can express as a set of differential equations (see below).

The key parameters governing the dynamics of the infected are \(\beta\) and \(\gamma\), which are determined by your inputs: $c$, $p$, and $d$. \(\beta\) is the expected number of infections per infected individual per day, and \(\gamma\) is the recovery rate. The ratio of these two is more commonly known as the basic reproduction number, *R~0~*!

There are a few key assumptions for this model. Beginning with the most unrealistic:

* The population is homogeneous. Every susceptible individual is equally likely to become infected; every infected person is equally likely to transmit, recover, or die from the disease.

* Once an individual has the disease for the full infectious period, $d$, they cannot infected or be infected again; hence, they are referred to as ‘removed’.

* $d$ is also the time to death. Either individuals recover at $d$ days or they die at $d$ days (at a rate of $m$).

![](www/explanation.png)