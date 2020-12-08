# Advent of Code 2020!
This is the first year that I'm doing AoC in real time. The only thing that makes this possible is my chronically bad sleeping habits and remote work. I can still get 6-7h sleep after doing these. Not sure how long that will last...
## Running Statistics

```
      -------Part 1--------   -------Part 2--------
Day       Time  Rank  Score       Time  Rank  Score
  8   01:01:15  9520      0   01:30:45  8273      0
  7   01:08:17  5997      0   02:05:18  6641      0
  6   00:11:19  4241      0   00:35:12  6222      0
  5   00:32:31  5803      0   01:13:10  7978      0
  4   00:45:58  7893      0   02:01:49  8469      0
  3   00:39:20  7494      0   01:04:53  8232      0
  2   00:17:50  4007      0   00:38:56  5446      0
  1   00:20:45  3412      0   00:25:13  3229      0
```
### Day 8 complete
Echos of the opcode computer from last year (which I really enjoyed). This one was fun. Hopefully, like last year, we build on the idea of a simulating a computer like thing (they're not true general purpose computers after all) and expand it with more capabilities.

Neither part 1 (at `1.72ms`) or part 2 (at `6.75ms`) took so long that I needed to think about optimizing. It's reasonable the way it is, I use [`Data.IntMap`](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-IntMap.html) for the memory which is about as [quick as you can get](https://github.com/haskell-perf/dictionaries) with `Int` indexed maps in Haskell without unusual effort. Of course most people using imperative languages are probably mutating an array. But for me part of the fun of Haskell is finding a solution that's purely immutable and yet performs reasonably well.

One fun note about this one; both part 1 and part 2 worked correctly the first time I ran them. This happens far more often with functional languages like Haskell and F# in my experience. The strong type system, used correctly, makes it far more likely to be correct when it compiles. 

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

