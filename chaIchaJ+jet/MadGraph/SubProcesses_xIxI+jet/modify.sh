#!/bin/bash

mv b_cl_001_001.f b_cl_011_001.f
mv b_cl_001_002.f b_cl_011_002.f
mv b_cl_001_003.f b_cl_011_003.f

mv b_cl_002_001.f b_cl_012_001.f
mv b_cl_002_002.f b_cl_012_002.f
mv b_cl_002_003.f b_cl_012_003.f

mv b_cl_003_001.f b_cl_013_001.f
mv b_cl_003_002.f b_cl_013_002.f
mv b_cl_003_003.f b_cl_013_003.f

mv b_cl_004_001.f b_cl_014_001.f
mv b_cl_004_002.f b_cl_014_002.f
mv b_cl_004_003.f b_cl_014_003.f

mv b_cl_005_001.f b_cl_015_001.f
mv b_cl_005_002.f b_cl_015_002.f
mv b_cl_005_003.f b_cl_015_003.f

mv b_cl_006_001.f b_cl_016_001.f
mv b_cl_006_002.f b_cl_016_002.f
mv b_cl_006_003.f b_cl_016_003.f

mv b_cl_007_001.f b_cl_017_001.f
mv b_cl_007_002.f b_cl_017_002.f
mv b_cl_007_003.f b_cl_017_003.f

mv b_cl_008_001.f b_cl_018_001.f
mv b_cl_008_002.f b_cl_018_002.f
mv b_cl_008_003.f b_cl_018_003.f


mv b_cl_021_001.f b_cl_031_001.f
mv b_cl_021_002.f b_cl_031_002.f
mv b_cl_021_003.f b_cl_031_003.f

mv b_cl_022_001.f b_cl_032_001.f
mv b_cl_022_002.f b_cl_032_002.f
mv b_cl_022_003.f b_cl_032_003.f

mv b_cl_023_001.f b_cl_033_001.f
mv b_cl_023_002.f b_cl_033_002.f
mv b_cl_023_003.f b_cl_033_003.f

mv b_cl_024_001.f b_cl_034_001.f
mv b_cl_024_002.f b_cl_034_002.f
mv b_cl_024_003.f b_cl_034_003.f


for filename in $(ls b_cl_*.f); do
    sed -i -e 's/_001_/_011_/g' $filename
    sed -i -e 's/_002_/_012_/g' $filename
    sed -i -e 's/_003_/_013_/g' $filename
    sed -i -e 's/_004_/_014_/g' $filename
    sed -i -e 's/_005_/_015_/g' $filename
    sed -i -e 's/_006_/_016_/g' $filename
    sed -i -e 's/_007_/_017_/g' $filename
    sed -i -e 's/_008_/_018_/g' $filename

    sed -i -e 's/_021_/_031_/g' $filename
    sed -i -e 's/_022_/_032_/g' $filename
    sed -i -e 's/_023_/_033_/g' $filename
    sed -i -e 's/_024_/_034_/g' $filename

    sed -i -e 's/001\//011\//g' $filename
    sed -i -e 's/002\//012\//g' $filename
    sed -i -e 's/003\//013\//g' $filename
    sed -i -e 's/004\//014\//g' $filename
    sed -i -e 's/005\//015\//g' $filename
    sed -i -e 's/006\//016\//g' $filename
    sed -i -e 's/007\//017\//g' $filename
    sed -i -e 's/008\//018\//g' $filename

    sed -i -e 's/021\//031\//g' $filename
    sed -i -e 's/022\//032\//g' $filename
    sed -i -e 's/023\//033\//g' $filename
    sed -i -e 's/024\//034\//g' $filename

#    sed -i -e 's/011_011/011_001/g' $filename
#    sed -i -e 's/011_012/011_002/g' $filename
#    sed -i -e 's/011_013/011_003/g' $filename

#    sed -i -e 's/012_011/012_001/g' $filename
#    sed -i -e 's/012_012/012_002/g' $filename
#    sed -i -e 's/012_013/012_003/g' $filename

#    sed -i -e 's/013_011/013_001/g' $filename
#    sed -i -e 's/013_012/013_002/g' $filename
#    sed -i -e 's/013_013/013_003/g' $filename
done


mv sborn_cl_001.f sborn_cl_011.f
mv sborn_cl_002.f sborn_cl_012.f
mv sborn_cl_003.f sborn_cl_013.f
mv sborn_cl_004.f sborn_cl_014.f
mv sborn_cl_005.f sborn_cl_015.f
mv sborn_cl_006.f sborn_cl_016.f
mv sborn_cl_007.f sborn_cl_017.f
mv sborn_cl_008.f sborn_cl_018.f

mv sborn_cl_021.f sborn_cl_031.f
mv sborn_cl_022.f sborn_cl_032.f
mv sborn_cl_023.f sborn_cl_033.f
mv sborn_cl_024.f sborn_cl_034.f

for filename in $(ls sborn_*.f); do

    sed -i -e 's/sborn_cl_001/sborn_cl_011/g' $filename
    sed -i -e 's/sb_cl_001/sb_cl_011/g' $filename

    sed -i -e 's/sborn_cl_002/sborn_cl_012/g' $filename
    sed -i -e 's/sb_cl_002/sb_cl_012/g' $filename

    sed -i -e 's/sborn_cl_003/sborn_cl_013/g' $filename
    sed -i -e 's/sb_cl_003/sb_cl_013/g' $filename

    sed -i -e 's/sborn_cl_004/sborn_cl_014/g' $filename
    sed -i -e 's/sb_cl_004/sb_cl_014/g' $filename

    sed -i -e 's/sborn_cl_005/sborn_cl_015/g' $filename
    sed -i -e 's/sb_cl_005/sb_cl_015/g' $filename

    sed -i -e 's/sborn_cl_006/sborn_cl_016/g' $filename
    sed -i -e 's/sb_cl_006/sb_cl_016/g' $filename

    sed -i -e 's/sborn_cl_007/sborn_cl_017/g' $filename
    sed -i -e 's/sb_cl_007/sb_cl_017/g' $filename

    sed -i -e 's/sborn_cl_008/sborn_cl_018/g' $filename
    sed -i -e 's/sb_cl_008/sb_cl_018/g' $filename


    sed -i -e 's/sborn_cl_021/sborn_cl_031/g' $filename
    sed -i -e 's/sb_cl_021/sb_cl_031/g' $filename

    sed -i -e 's/sborn_cl_022/sborn_cl_032/g' $filename
    sed -i -e 's/sb_cl_022/sb_cl_032/g' $filename

    sed -i -e 's/sborn_cl_023/sborn_cl_033/g' $filename
    sed -i -e 's/sb_cl_023/sb_cl_033/g' $filename

    sed -i -e 's/sborn_cl_024/sborn_cl_034/g' $filename
    sed -i -e 's/sb_cl_024/sb_cl_034/g' $filename


    #remove junk files
    find . -type f -name '*.f-e'  -exec rm -f '{}' \;
done