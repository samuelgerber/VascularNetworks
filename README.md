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

Optimal transport interpolation between two vascular networks:
![Alt text](/Figures/ip.gif "Simple optimal transport interpolation between two brain vasacular networks")

Interpolation using only x,y for transportation cost:

![Alt text](/Figures/ip2d.gif "Simple optimal transport interpolation between two brain vasacular networks")


Result of projection of the pairwise optimal transport maps onto the direction best seperating male versus female (blue male surplus mass, orange female surplus mass)

![Alt text](/Figures/movie-loop.gif "Female versus male optimal transport projection")




### Multiresolution Optimal Transport
Decompoition of vascular network into a multiresolution representation:

![Alt text](/Figures/multiresolution.png "Multiresolution decomposition of vascular network")

Optimal transport interpolation between two vascular networks using multiresoultion approach 1:
Transport among scales within subject and map to different subject at coarsest scale.

![Alt text](/Figures/mv2ip.gif "Multiresolution optimal transport interpolation between two brain vasacular networks")



Optimal transport interpolation between two vascular networks using multiresoultion approach o2:
Transport between subjects at finest scale, decompose transport plan according to multiresolution representation of the two subjects.

![Alt text](/Figures/mip.gif "Multiresolution optimal transport interpolation between two brain vasacular networks")
