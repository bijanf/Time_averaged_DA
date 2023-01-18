#!/usr/bin/env bash
#SBATCH --job-name="L63_2s__exp05__14_05_07__calc01"
#SBATCH --output="/daten/model/acevedo/das_archive/logs/L63_2s__exp05__14_05_07__calc01_slurm-%j.log"
#SBATCH --error="/daten/model/acevedo/das_archive/logs/L63_2s__exp05__14_05_07__calc01_slurm-%j.log"
#SBATCH --mail-user=walter.acevedo@met.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1-1
#SBATCH --ntasks-per-node=12
#SBATCH --partition=main

echo " Simulation ID exp05"
bash "/home/acevedo/Desktop/das/lorenz/code/models/L63_2s/model_script.sh" exp05
