/**
 * simple program that prints to stdout the results of a simulation in which
 * people join study groups
 *
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct _params_t {
    double member_contrib;
    double member_detriment;
    int num_groups;
    int num_joiners;
} params_t;

double fitness_func(int n, params_t * ps) {
    return ps->member_contrib * n
        - (ps->member_detriment / 2) * n * n;
}

int fitness_comp(const void * g1, const void * g2, void * ps) {
    int size1 = * (int*) g1;
    int size2 = * (int*) g2;
    return fitness_func(size1, ps) < fitness_func(size2, ps);
}

// side effects out the wazoo (wahzoo?)
int ** one_join(int ** all_groups, size_t * len, params_t * ps) {
    // for every group, find the group we can best fit it, if there is a tie,
    // increase the number of groups and add it to the end of the list

    size_t orig_len = *len; // save the original fitness

    for (size_t group_index = 0; group_index < orig_len; group_index++) {
        // sort the list by fitness
        qsort_r(all_groups[group_index], ps->num_groups, sizeof ** all_groups,
                fitness_comp, ps);

        // for every group with the same fitness, add a new case by adding one
        // to that group
        double fitness = fitness_func( all_groups[group_index][0], ps);
        for (size_t i = 0; i < ps->num_groups; i++) {
            if ( fitness_func( all_groups[group_index][i], ps ) != fitness ) {
                break;
            }

            // add a new group at the end of the list, delete the one at the
            // front
            if ( i == 0 ) {
                // reuse the first element
                int tmp_groups[ps->num_groups];
                mempcpy(tmp_groups, all_groups[group_index], ps->num_groups * sizeof ** all_groups);
                all_groups[group_index][0] += 1;
            } else {
                *len += 1;

                all_groups = realloc( all_groups, *len * sizeof * all_groups );
                all_groups[*len-1] = malloc( ps->num_groups * sizeof ** all_groups);
                mempcpy(all_groups[*len-1], all_groups[group_index], ps->num_groups * sizeof ** all_groups);
                all_groups[*len-1][i] += 1;
            }
        }
    }
    return all_groups;
}

void print_all_groups(int ** all_groups, int len, params_t * ps) {
    for (size_t g = 0; g < len; g++) {
        printf("{ ");
        for (size_t i = 0; i < ps->num_groups; i++) {
            printf("%d ", all_groups[g][i]);
        }
        printf("}\n");
    }
}

// returns array of all the group numbers, not sorted in any way
// if I hadn't used pointer 2d arrays I could cheat here...
int * trial( params_t * ps, size_t * ret_len ) {
    // set up starting groups
    size_t len = 1;
    int ** all_groups = malloc( len * sizeof * all_groups );
    all_groups[0] = calloc( ps->num_groups, sizeof ** all_groups );

    for (size_t j = 0; j < ps->num_joiners; j++) {
        all_groups = one_join( all_groups, &len, ps );
    }

    *ret_len = ps->num_joiners * len;
    int * all_values = malloc( *ret_len * sizeof * all_values);

    int easy_counter = 0;
    for (size_t i = 0; i < len; i++) {
        for (size_t m = 0; m < ps->num_joiners; m++) {
            all_values[easy_counter++] = all_groups[i][m];
        }
        free( all_groups[i] );
    }
    free( all_groups );

    return all_values;
}

int main(int argc, const char ** argv) {
    // simulation parameters
    params_t ps;
    ps.member_contrib = 1.0;
    ps.member_detriment = 0.5;
    ps.num_groups = 5;
    ps.num_joiners = 4;

    if (argc != 2) {
        printf("usage: sim num_trials\n");
        return 0;
    }

    size_t trials = atoi(argv[1]);

    for (size_t t = 0; t < trials; t++) {
        size_t vlen;
        int * values = trial(&ps, &vlen);

        for (size_t j = 0; j < vlen; j++) {
            printf("%d\n", values[j]);
        }

        free(values);
    }

    return 0;
}