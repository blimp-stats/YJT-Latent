#!/usr/bin/env python3
import subprocess as sp
from multiprocessing import Pool
import os, sys
import random

# Call script
def call_script(args):
    rep, seed = args  # unpack arguments
    
    # Skip if exists
    if os.path.exists('./results/rep' + str(rep) + '.rds'):
        print('    rep ' + str(rep) + ' skipped')
        return
    
    env = {
        'SEED': str(seed),
        'REP':  str(rep),
    }

    try:
        sp.check_call(['/usr/local/bin/Rscript', '--vanilla', 'one-rep.R'],
            env = env, stdout = sp.DEVNULL, stderr = sp.DEVNULL)
        print('    rep ' + str(rep)  + ' complete')
    except:
        print('    rep ' + str(rep) + ' errored with ' + str(seed))
        
# Run
def main():
    # Total Number of Reps
    n_reps = 1000
    
    # Get Seeds and Reps
    random.seed(1387122)
        
    # Using for loop
    args = [(i, random.randint(0, 2147483647)) for i in range(1, n_reps + 1)]
    
    pool = Pool(5)
    # pool = Pool()  # would use the number of available processors instead
    pool.map(call_script,  args)
    pool.close()
    pool.join()

if __name__ == '__main__':
    main()
    
    