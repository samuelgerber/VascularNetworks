# VascularNetworks

R scripts to analyze vascular networks.

Currently there is code to compare trees based on optimal transport. 

## Usage


The scripts require:
* The R packages:
  * mop: https://bitbucket.org/suppechasper/optimaltransport
  * gmra: https://bitbucket.org/suppechasper/gmra
  * data.table, RANN, rgl (for 3d visualizations)
* Vascular tree networks data from https://data.kitware.com/#collection/591086ee8d777f16d01e0724/folder/58a372e38d777f0721a64dc6
  * Processed tree data is availabe in the Data folder of the repository (scripts need to be adapated to find the data

Edit scripts to point to the right data or process new data


## Optimal Transport

Optimal transport interpolation between to vascular networks:

![Alt text](/Scripts/ip.gif "Simple optimal transport interpolation between to brain vasacular networks")

### Multiresolution Optimal Transport
Decompoition of vascular network into a multiresolution representation:

![Alt text](/Scripts/multiresolution.png "Multiresolution decomposition of vascular network")

Optimal transport interpolation between to vascular networks using a multiresoultion approach:

![Alt text](/Scripts/mv2ip.gif "Multiresolution optimal transport interpolation between to brain vasacular networks")
