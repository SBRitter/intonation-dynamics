# Software for paper "The dynamics of intonation: categorical and continuous variation in an attractor-based model"

This repository contains the software and data for the analyses and simulations of the manuscript "The dynamics of intonation: categorical and continuous variation in an attractor-based model" by Simon Roessig, Doris Mücke and Martine Grice published in PLOS ONE (https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0216859). The software models the intonation data of the study, i.e. nuclear pitch accents characterised by tonal onglide, using a dynamic system with two attractors. The central piece of the repository is the R script which should also be the starting point to run the modelling. The actual simulations of the dynamic system are run by the C++ program that gets called by the R script. The onglide data of 27 native German speakers are stored in the csv files. The centrepiece is the R script modelling.R. The main directory of the project has to be the current working directory in the R session when running the script.

Probably does not work on windows without tweaking. In case of problems, please write to ritter.simon(at)uni-koeln.de.
________
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
