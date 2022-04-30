### Metric explanation

#### Mental health

The (obviously arbitrary!) mental health metric used is

```
mental_health = (elevated^2.8675) - (
          (0.5*(anxiety^6.6582)) +
          (0.5*(depressed^6.6582))
        )^0.5
```

Why are the bases and multiplicative factors are as they are?

1. They put the score on a scale between `(-100, 100)`, since the individual components vary between `{1, 2, 3, 4}` (except `elevated` for which I manually add `0.5` or `1` for truly peak experiences). Recording the lowest score on all variables maps to a `mental_health` value of `0`.
2. Very elevated days are much better than elevated days. Think: peak experiences!
3. Very depressed (anxious) days are much worse than depressed (anxious) days. On the other hand, a very depressed __or__ very anxious day is worse than a depressed __and__ anxious day. The formula for negative experiences captures these features.

#### Life satisfaction, work satisfaction, subjective well-being

These metrics are an even more arbitrary (i.e. not having an easy-to-interpret 0-point) mix of the following recordings:

1. elevated
2. anxiety
3. depressed
4. fun
5. relationship_satisfaction
6. energy
7. optimism
8. positive_liberty
9. negative_liberty
10. health
11. shame
12. self_acceptance
13. integrity
14. value_alignment
15. security
16. achievement
17. learning
18. work_depth
19. professional_mastery
20. dog_interaction
21. conflict
22. suicidality

I hope to slightly trim this list soon by removing highly correlated measures (e.g. positive liberty and negative liberty, shame and self acceptance, integrity and value alignment).