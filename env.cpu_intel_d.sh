INTELONEAPI="intel/oneapi/2023.2"
MPIPACKAGE="mpi/2021.10.0"
MKL="mkl/2023.2.0"
COMPILER="compiler/2023.2.0"
module purge                   2>/dev/null
module load $INTELONEAPI       2>/dev/null
module load $COMPILER          2>/dev/null
module load $MKL               2>/dev/null
module load $MPIPACKAGE        2>/dev/null
module load gcc/9.2.0          2>/dev/null


export PATH=/home/gmap/mrpm/marguina/fxtran-acdc/manyblocks/bin:$PATH
