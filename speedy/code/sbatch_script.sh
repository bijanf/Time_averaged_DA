#!/usr/bin/env bash
#SBATCH --job-name="speedy__exp50__16_02_05__tux05"
#SBATCH --output="/daten/cady/DATA_ASSIMILATION/model/logs/speedy__exp50__16_02_05__tux05_slurm-%j.log"
#SBATCH --error="/daten/cady/DATA_ASSIMILATION/model/logs/speedy__exp50__16_02_05__tux05_slurm-%j.err"
#SBATCH --mail-user=bijan.fallah@met.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1-1
#SBATCH --ntasks-per-node=4
#SBATCH --mem=17500
#SBATCH --time=2:00:00
#SBATCH --partition=main

echo " Simulation ID exp50"
echo " STORE dir: /daten/cady/DATA_ASSIMILATION/model/batch10"

bash "/daten/cady/das_01/speedy/code/speedy_tools/model_script.sh" exp50
sacct --format="JobID,CPUTime,MaxRSS"
