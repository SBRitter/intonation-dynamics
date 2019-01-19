# Software for manuscript "The dynamics of intonation: categorical and continuous variation in an attractor-based model"

This repository contains the software and data for the analyses and simulations of the manuscript "The dynamics of intonation: categorical and continuous variation in an attractor-based model" by Simon Ritter, Doris Mücke and Martine Grice. The software models the intonation data of the study, i.e. nuclear pitch accents characterised by tonal onglide, using a dynamic system with two attractors. The central piece of the repository is the R script which should also be the starting point to run the modelling. The actual simulations of the dynamic system are run by the C++ program that gets called by the R script. The onglide data of 27 native German speakers are stored in the csv files.

How to make it work (on a Mac):

1. Clone git repository: ```git clone https://github.com/SBRitter/intonation-dynamics```

2. Change to project folder: ```cd intonation-dynamics```

3. Open R Script. Make sure you keep all files (R script, compiled simulation program and csv data files) in one directory.

4. Install required R dependencies if you do not have them already

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
