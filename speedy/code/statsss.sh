#!/bin/bash
set -ex 
rm -rf /daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_prod/plots_outdated/plots
rm -rf /daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_min/plots_outdated/plots






./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/natu_run/'
echo '---------------------------------------------------------------------------------------'
echo 'NATURE FINISHED'
./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/free_run__m24/'
echo '----------------------------------------------------------------------------------------'
echo 'FREE FINISHED'
./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_min'
./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__stationSet8__obs_op_norm_min'

echo '----------------------------------------------------------------------------------------'
echo 'MIN FINISHED'
./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_prod'
./das.sh --task=stat '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__stationSet8__obs_op_norm_prod'

echo '----------------------------------------------------------------------------------------'
echo 'Prod Finished'
./das.sh --task=docu '/daten/cady/das_archive_for_paper/batch10/speedy/SST-1854_2010__slab_model/run_length_1800mo/assi_run__m24__hLoc500km__InfFac1__stationSet8__obs_op_norm_prod'

