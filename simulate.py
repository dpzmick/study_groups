from math import pow
from random import choice
import matplotlib.pyplot as plt

###### Parameters
# a - the individual contribution factor. This is how much of a contribution on
# individual will bring to the study group (assumed constant across all people
# that may join the group)
a = 2.0

# b - the individual detriment factor. As the size of the group increases, the
# and individuals ability to contribute decreases by this factor
b = 0.5

# max groups - how many groups can for at maximum
maxgroups = 15

# numjoiners - number of people to try and get in study group
numjoiners = 10

trials = 5000

##### Objects
class Group:
    def __init__(self, a, b):
        self.num_members = 0
        self.a = a
        self.b = b

    def fitness(self):
        return self.a * self.num_members - (b/2)*pow(self.num_members, 2)

    def member_join(self):
        self.num_members += 1

    def members(self):
        return self.num_members

    def __str__(self):
        return str(self.num_members)

    def __repr__(self):
        return str(self)

def best_group(groups):
    return max(groups, key=lambda g: g.fitness())

##### Simulation
def simulation():
    pool = [Group(a,b) for i in range(0, maxgroups)]
    for i in xrange(0, numjoiners):
        best = best_group(pool)
        # find all other groups with same fitness and randomly join one of them
        bests = filter(lambda g: g.members() == best.members(), pool)
        choice(bests).member_join()

    non_empty = filter( lambda g: g.members() > 0, pool )
    return non_empty

if __name__ == "__main__":
    print("optimal: %d" % (a/b))
    simulation_results = []
    for i in xrange(0,trials):
        simulation_results += map(lambda g: g.members(), simulation())


    flattened = [0] * (max(simulation_results) + 1)
    for result in simulation_results:
        flattened[result] += 1

    counters = [i for i in range(0, max(simulation_results) + 1)]

    plt.title("a=%f, b=%f, trials=%d\nnumjoiners=%d, maxgroups=%d, optimal=%f" %
            (a, b, trials, numjoiners, maxgroups, (a/b)))
    plt.bar([i for i in range(0, max(simulation_results) + 1)], flattened,
            align='center')
    plt.show()
