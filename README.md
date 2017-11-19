# VascularNetworks

Code and scripts to analyze vascular networks.

## Optimal Transport

Currently there is code to compare trees based on optimal transport. 

The scripts require:
* The R package mop: https://bitbucket.org/suppechasper/optimaltransport
* Vascular tree networks data from https://data.kitware.com/#collection/591086ee8d777f16d01e0724/folder/58a372e38d777f0721a64dc6
  * Processed tree data is availabe in the Data folder of the repository (scripts need to be adapated to find the data)

Optimal transport interpolation between to vascular networks:
![Alt text](/Scripts/ip.gif "Simple optimal transport interpolation between to brain vasacular networks")

### Multiresolution optinal transport

Optimal transport interpolation between to vascular networks using a multiresoultion approach:
![Alt text](/Scripts/mv2ip.gif "Multiresolution optimal transport interpolation between to brain vasacular networks")
