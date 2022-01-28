### Metric explanation

Volume is calculated as the sum of estimated one-rep-maxes across sets and exercises. Recall that I calculate estimated one-rep-max (1RM) of a set as `1RM = weight * (1 + (reps / x))` with `x = 25`.

### Plot options

Selecting "aggregate hypertrophy-adjusted 1RM" gives the estimated 1RM a weight of `0.5` if a set's reps are in `{[1, 5], [15, 20]}`, `0.75` if in `{[6, 7], [13, 14]}`, `1` if in `[8, 12]`, and `0` otherwise.

Selecting "yes" the including cardio calories option adds my three times my calorie expenditure from soccer, running, swimming, hiking, or stair-climbing sessions as a pseudo-estimated one-rep-max. (Arbitrary, but it puts these workouts on a similar scale to weightlifting workouts.)