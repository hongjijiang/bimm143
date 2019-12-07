Class\_11:structural bioinformatics
================

The PDB database for biomolecular structure
-------------------------------------------

> Q1

download csv file from PDB website

``` r
data <- read.csv("Data Export Summary.csv")
data
```

    ##   Experimental.Method Proteins Nucleic.Acids Protein.NA.Complex Other  Total
    ## 1               X-Ray   131278          2059               6759     8 140104
    ## 2                 NMR    11235          1303                261     8  12807
    ## 3 Electron Microscopy     2899            32                999     0   3930
    ## 4               Other      280             4                  6    13    303
    ## 5        Multi Method      144             5                  2     1    152

The percentage of X-ray and electron microscopy

``` r
data$Total/sum(data$Total)*100
```

    ## [1] 89.0702879  8.1419744  2.4984742  0.1926305  0.0966331

The proportion of proteins

``` r
round(sum(data$Proteins)/sum(data$Total)*100,2)
```

    ## [1] 92.71

HIV-Pr structure analysis
-------------------------

``` r
library(bio3d)
pdb <- read.pdb('1hsg.pdb')

##atom.select()
##write.pdb()
##trim.pdb()
```

``` r
ligand <- atom.select(pdb, 'ligand',value=TRUE)
write.pdb(ligand, file = "1hsg_ligand.pdb")
```

``` r
?read
```

    ## No documentation for 'read' in specified packages and libraries:
    ## you could try '??read'
