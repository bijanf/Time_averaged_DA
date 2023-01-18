/*!

\page tada General Time-averaged DA problem

\image html TADA_simple_schema.png "Simple TA-DA schema" width=6cm
## Topics
Hakim et al hipothesis: DA on time-averaged state is as efficient as DA on time augmented state (~ 4D EnkF) provided a linear obs. operator and negligible covariance between fast and slow variables.


Hakim et al diferentiate between \f$ \tau_{ta}^{obs} \f$ and \f$ \tau_{ta}^{state} \f$.

Three time constants:
-----------------------
  Period                   | Description                 
:--------------------------|:--------------------------------
 \f$ \tau_{assim}\f$       | Observation period
 \f$ \tau_{ta}^{obs}\f$    | Observation time averaging span 
 \f$ \tau_{ta}^{state}\f$  | state time averaging span 

- Hakim et al papers normally assume \f$ \tau_{ta}^{state} = \tau_{assim}\f$.
- \f$ \tau_{ta}^{state} = \tau_{ta}^{obs}\f$ might look more natural but much more complicated.
  - <a href="../TA analysis cycle.pdf" target="_blank"> TA analysis cycle </a>.
  - Cases \f$ \tau_{assim} > \tau_{ta}^{obs}\f$ and \f$ \tau_{assim} > \tau_{ta}^{obs}\f$ are very different.
  - Validation is only straightforward at assimilation instants. Elsewhere there can be several analysis.

  
- For \f$ \tau_{assim} = \tau_{ta}^{obs}\f$ the confusion disappears.


\page newresults Possible New Model: L63 with slow-fast components.

\image html L96_2s_7.png

<a href="../L63_2s_7_results.pdf" target="_blank"> Preliminary results </a>
----------------------------------------------------------------------------
- Time state augmentation clearly outperforms time-averaged DA. Nevertheless it is very expensive especially for high dimensional models.
- Fast-slow variables not covarying assumption seems not to hold for this system.
- Smooth shifting DA improvement is not vary noticeable when Time state augmentation is used.


\page speepyDA SPEEDY-DA intro

The DAS (Data Assimilation system) allows to perform instantaneous and 
time-averaged kalman filtering for speedy model.
@note This code is based on the code [SpeedyLetkf](http://code.google.com/p/miyoshi/wiki/SpeedyLetkf) written by Takemasa Miyoshi. 
Scripts were completely rewritten and and speedy-LETKF interface considerably changed.

@section sec1 Instructions
- First of all, go to code folder and execute:
  - $ . das_initialize.sh (or)
  - $ source das_initialize.sh
- Afterwards, you can use the different system functions:
  - Simulation launcher to create new simulation datasets:
    - $ das_compute.sh (or)
    - $ nohup das_compute.sh & (in the background allowing to close the terminal)
  - System test (should be run after every potentially harmfull code change).
    - $ das_test 
  - Documentation creator:
    - $ create_doxygen_doc.sh
  - Lower level functions
    - $ ...

@attention Commands above only work if executed from the folder "code"

@section versioning Versioning commands
Version archive creation
- $ git init

Normal commiting cycle (only after a succesfull test)
- $ git status          (find what should be added)
- $ git add new_files?? (add new stuff)
- $ git commit -a       (Save code snapshot)
- $ git log             (to see development log)
- git add .
- git commit --amend

See @ref devel if you want to know what is to be done.

(Currently disabled)
@par Comments
 The amount of info sent to screen is controled with the parameter verbose.
For maximum info set verbose to 2 and run the script via bash -x

*/

