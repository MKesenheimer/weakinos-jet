#!/bin/bash

make pastegnudata
cd ./chaIchaJ+jet && make -j4 all
cd ../neuIchaJ+jet && make -j4 all
cd ../neuIneuJ+jet && make -j4 all
