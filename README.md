# haskell-cli-example

This package contains the source code for my [blog post about command line interface design in Haskell](https://www.stephanschiffels.de/posts/2021-03-24-Haskell-CLI/).

If you want to compile this example, first install [stack](https://docs.haskellstack.org).
Then clone or download this repository and run `stack run -- --help` from inside. You should then find the help output for this command line interface

```
```
Usage: haskell-cli-example-exe [-m|--missingness NUMBER] [-v|--verbose] 
                               (--heterozygosity | --segregatingSites | 
                                 --hardyWeinbergDev) 
                               (--genoFile FILE --snpFile FILE --indFile FILE | 
                                 --vcfFile FILE) 
                               (--individualsFile FILE | --ind NAME)
  Hello, this is a toy example for how to design command line interfaces in
  Haskell

Available options:
  -m,--missingness NUMBER  A missingness threshold (default: 0.5)
  -v,--verbose             verbose output
  --heterozygosity         compute the rate of heterozygosity for each
                           individual
  --segregatingSites       compute the rate of segregating sites for each
                           individual
  --hardyWeinbergDev       compute the average deviation from Hardy-Weinberg
                           equilibrium for each individual
  --genoFile FILE          the input genotype file
  --snpFile FILE           the input snp file
  --indFile FILE           the input individual file
  --vcfFile FILE           the input VCF file
  --individualsFile FILE   list individuals in the file given
  --ind NAME               list individuals directly on the command line. Option
                           can be given multiple times, once for each individual
  -h,--help                Show this help text
```
