# Advent of Code 2020!
This (messy) repo includes both some 2019 I was practicing on earlier as well as 2020 - reflected in year sub-folders.
## Running Score
Well one thing's clear... I'm not going to break any speed records. I'm happy to take this as a fun exercise and a good excuse to play with Haskell and hopefully learn some useful stuff over the next 18 days! 😃

```
      -------Part 1--------   -------Part 2--------
Day       Time  Rank  Score       Time  Rank  Score
  7   01:08:17  5997      0   02:05:18  6641      0
  6   00:11:19  4241      0   00:35:12  6222      0
  5   00:32:31  5803      0   01:13:10  7978      0
  4   00:45:58  7893      0   02:01:49  8469      0
  3   00:39:20  7494      0   01:04:53  8232      0
  2   00:17:50  4007      0   00:38:56  5446      0
  1   00:20:45  3412      0   00:25:13  3229      0
```

### Day 7 complete
This one was a bit more drawn out; a bit of parsing into a graph of nodes, where each node is a bag that might contain one or more other bags. 


### Day 6 complete
Fastest time so far for part 1 - good for my standards 🙂

Days 6 and 5 were both easier than the days that preceded them. Not complaining b/c I'm sure there'll be some whoppers coming before long. It took the longest by a good margin (actually I don't know what took me so long on part 2 of day 4). Good fun though!

#### Benchmarks!
Came across a few new things last couple of days, most interesting is the [Criterion](http://www.serpentine.com/criterion/) library for Benchmarking in Haskell. It'll be interesting when I have an algorithm that is in obvious need of improvement. Day 6 results:

||lower bound	|estimate	|upper bound|
|--|--:|--:|--:|
|OLS regression	|2.57 ms	|2.66 ms	|2.77 ms
|R² goodness-of-fit	|0.967	|0.985	|0.997
|Mean execution time	|2.83 ms	|2.91 ms	|3.12 ms
|Standard deviation	|193 μs	|428 μs|	713 μs
> Outlying measurements have severe (81.4%) effect on estimated standard deviation.

