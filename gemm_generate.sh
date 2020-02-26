#bin/bash

#setup environment
export LD_LIBRARY_PATH=/usr/local/lib
export _JAVA_OPTIONS="-Xmx4g"

#generate spatial compiler
sbt compile

for i in {1..16}
do
    name=GEMM_NCubed_$i
    echo "Running $name"
    sbt "runMain $name --synth --fpga=zynq"
    cd gen/$name
    echo Entered $PWD
    #current default board is ZC706, which we don't use in our synthesis
    mv zynq.hw-resources zynq.hw-resources-ZC706
    #copy Vivado commands for ZedBoard Synthesis
    cp ../zynq.hw-resources-zedboard zynq.hw-resources
    make hw
    #copy the synthesis report 
    cp /verilog-zynq/par_utilization.rpt ../../reports/
    cd ../..
    
done

