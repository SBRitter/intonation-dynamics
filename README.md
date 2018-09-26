# Software for manuscript "..."

This repository contains the software and data for the analyses and simulations of the manuscript "..." by Simon Ritter, Doris Mücke and Martine Grice. The software models the intonation data of the study, i.e. nuclear pitch accents characterised by tonal onglide, using a dynamic system with two attractors. The central piece of the repository is the R script which should also be the starting point to run the modelling. The actual simulations of the dynamic system are run by the C++ program that gets called by the R script. The onglide data of 27 native German speakers is stored in the csv files.

How to make it work (on a Mac):

1. Clone git repository: ```git clone https://github.com/SBRitter/intonation-dynamics```

2. Change to project folder: ```cd intonation-dynamics```

3. Compile C++ code: ```g++ simulate.cpp -o simulate```

4. Open R Script. Make sure you keep all files (R script, compiled simulation program and csv data files) in one directory.

5. Install required R dependencies if you do not have them already
