import subprocess
import os
import shutil
from multiprocessing import Process, Pool

test_name = 'just_playing'
data_dir = test_name + '_data'
res_dir = test_name + '_results'

# unchanging values
member_contrib = 1.0
member_detriment = 0.5
num_joiners = 7
max_groups = 10
trials = 10

# changing values
# for every selflessness value, run every split chance value test
# selflessness values
selflessness = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

# split chance values
split_chance = [0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

def run_test(s, sp):
    vals = (member_contrib, member_detriment, s, sp, num_joiners, max_groups, trials)
    fname = "mc%0.2f_md%0.2f_s%0.2f_sp%0.2f_j%d_mg%d_t%d" % vals
    fname = "%s_data/%s" % (test_name, fname)
    exe = "./sim %f %f %f %f %d %d %d" % vals

    result = subprocess.check_output(exe, shell=True)

    return (s, sp, result, fname)

# start here

# delete existing dirs for this test name
if os.path.exists(data_dir):
    shutil.rmtree(data_dir)

if os.path.exists(res_dir):
    shutil.rmtree(res_dir)

# make dirs for this test
os.makedirs(data_dir)
os.makedirs(res_dir)

# dump parameters
pdumpfname = res_dir + '/params.txt'
pdumpf = open(pdumpfname, 'w')
pdumpf.write('member_contrib=%f\n' % member_contrib);
pdumpf.write('member_detriment=%f\n' % member_detriment);
pdumpf.write('num_joiners=%d\n' % num_joiners);
pdumpf.write('max_groups=%d\n' % max_groups);
pdumpf.write('trials=%d\n' % trials);
pdumpf.write('\nselflessness array:\n')
pdumpf.write(str(selflessness))
pdumpf.write('\n\nsplit_chance array:\n')
pdumpf.write(str(split_chance))
pdumpf.write('\n')
pdumpf.close()

# create pool of workers, 8 cores = 8 workers
pool = Pool(8)

# submit all the tasks
results = []
for s in selflessness:
    for sp in split_chance:
        results.append(pool.apply_async(run_test, (s, sp)))

# start building data store
datums = {}
for s in selflessness:
    datums[s] = {}

print "Running simulations"
# collect results as they come in
for result in results:
    s, sp, results, fname = result.get()

    # write the output somewhere
    f = open(fname, 'w')
    f.write(results)
    f.close()

    # get values we care about
    # we already have s and sp
    eff = float(results.split('\n')[7].split(':')[1])
    datums[s][sp] = eff

print "Generating image"
# write results as a matrix
# for vis with R
matrix_name = res_dir + '/' + test_name + '.matrix'
xyz = open(matrix_name, 'w')
for s in selflessness:
    i = 0
    for sp in split_chance:
        if i != 0:
            xyz.write(',')
        xyz.write(str(datums[s][sp]))
        i += 1
    xyz.write('\n')
xyz.close()

pdf_name = res_dir + '/' + test_name + '.pdf'
print subprocess.check_output("Rscript vis.R %s %s" % (matrix_name, pdf_name), shell=True)
