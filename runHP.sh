#!/bin/bash
echo 'Welcome to the cooled plate'
gfortran -o hPlate hPlate.f90
./hPlate
echo 'Sit tight while we cool the plate'
python hpGraph.py
echo 'Thank you for experiencing the cooled plate and all its holy-ness'
