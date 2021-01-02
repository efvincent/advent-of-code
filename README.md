# Advent of Code 2020!
Completed for the first time! I had to seek help on 2 days... I don't recall which ones exactly atm but I'll get back to writing up some notes at some point. Day 20 part 2 was actually the last one I finished, it took me the longest I think. I had started over 3x before I landed on an approach I was happy with.

Several days can use refactoring, but I doubt I'll do that. This was a great experience and a fun way to beef up my Haskell skills. I'm already looking forward to next year :)
## Running Statistics

```
      --------Part 1--------   --------Part 2--------
Day       Time   Rank  Score       Time   Rank  Score
 25       >24h  13132      0       >24h  10812      0
 24       >24h  13644      0       >24h  12806      0
 23       >24h  14596      0       >24h  12147      0
 22       >24h  16326      0       >24h  13863      0
 21       >24h  14239      0       >24h  14031      0
 20       >24h  14005      0       >24h  12983      0
 19   01:49:16   2950      0       >24h  13173      0
 18   04:42:13   8473      0   16:41:26  14852      0
 17   02:23:08   5038      0   02:35:27   4688      0
 16   01:15:11   6148      0   13:32:24  15018      0
 15   01:51:46   7515      0   01:58:59   6329      0
 14   03:09:28   9380      0   18:26:08  20561      0
 13       >24h  31407      0       >24h  25555      0
 12       >24h  34149      0       >24h  31591      0
 11       >24h  31752      0       >24h  32055      0
 10   00:15:11   4386      0   01:35:32   4929      0
  9   01:17:21  10175      0   01:49:11  10261      0
  8   01:01:15   9520      0   01:30:45   8273      0
  7   01:08:17   5997      0   02:05:18   6641      0
  6   00:11:19   4241      0   00:35:12   6222      0
  5   00:32:31   5803      0   01:13:10   7978      0
  4   00:45:58   7893      0   02:01:49   8469      0
  3   00:39:20   7494      0   01:04:53   8232      0
  2   00:17:50   4007      0   00:38:56   5446      0
  1   00:20:45   3412      0   00:25:13   3229      0
```

### Day 19: Monster Messages
This puzzle has you evaluate nested "rules" to build a predicate that can be used to test input data, then count the number of items that pass the predicate. The predicate is most obviously a simple, if absurdly long, regular expression. Build that, then fold it over the test cases and done.

Part 2 I haven't gotten to - for alas I'm am completely cached; this one is for tomorrow.

#### Day 19: Part 2
Well... I don't know if I was on a wild goose chase and there was a way easier way to do this, but after probably 4h on and off pondering, doodling, and debugging, I've finally got it down. The end result is far better than what I did in part 1 - assembling regex, which wouldn't work for part 2 anyway. Some snippets to explain:

Each rule is a sum type (aka Discriminated Union in F#, or more generally a sum type). 
```haskell
data Rule = Seq [M.Key]         -- ^ Must evaluate all rules in order
          | Or [M.Key] [M.Key]  -- ^ Must evaluate either left or right rules in order
          | Match Char          -- ^ Matches exactly one character
          deriving (Eq, Show)
```
I've left out parsing the input, that's not the tricky part.

##### Main Test Function
```haskell
runTest :: Rules -> String -> Bool
runTest rm candidate =
  -- if any of the results is null (ie ""), that means it has no "leftovers", 
  -- then there's at least one path to a result for the candidate
  any null (go 0 [candidate])
  where
    go :: M.Key -> [String] -> [String]
    go _ [] = []
    go id ss =
      case rm M.! id of
        Match c      -> concatMap (runMatch c) ss      
        Seq ids      -> runSeq ids ss
        Or lIds rIds -> runOr lIds rIds ss
        
    runMatch :: Char -> String -> [String]
    runMatch _ [] = []
    runMatch c' (c:cs)
      | c' == c   = [cs]
      | otherwise = []

    runSeq :: [M.Key] -> [String] -> [String]
    runSeq [] ss = ss
    runSeq (id:ids) ss =
      case go id ss of
        [] -> []
        ss' -> runSeq ids ss'

    runOr :: [M.Key] -> [M.Key] -> [String] -> [String]
    runOr lIds rIds ss = runSeq lIds ss ++ runSeq rIds ss
```

As is convention in Haskell, the `go` function is the recursive loop for the `runTest` function (I've only just learned about this convention, I had always used `loop` in F# and Haskell to date). The key insight that I finally reached is that `go` takes a `M.Key`, which is the key to the map of rules, and a **LIST** of `String` which are the possible remaining strings to test. The reason it has to be a list is because when we get to the `Or [M.Key] [M.Key]` type rule, _both_ branches may match! If that happens, there are two possible paths to continue down...

I had missed this possibility, and was only checking the second path if the first didn't succeed. This works in cases where only one case succeeds, and in cases where both cases succeed and you happen to get lucky and the first path is ultimately successful. But in the real data, there are several cases where the `Or` produces two results and the second one is part of the eventual solution.  It took me too long to see that possibility.

Anyway, from `go` we look up the rule in the map, then hand off to a small function to handle each of the different `Rule` cases. It's reasonably self explanatory from there. Whew.

**Benchmark Part 1**
```
time                 119.5 ms   (118.2 ms .. 121.6 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 118.1 ms   (117.2 ms .. 119.1 ms)
std dev              1.392 ms   (1.060 ms .. 1.720 ms)
variance introduced by outliers: 11% (moderately inflated)
```

**Benchmark Part 2**
```
time                 514.1 ms   (510.9 ms .. 520.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 514.0 ms   (512.9 ms .. 514.8 ms)
std dev              1.240 ms   (726.4 μs .. 1.529 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Day 18: Operation Order
Expression parser... I did part 1 ad-hoc, and it was cumbersome but doable. That approach was bug ridden and difficult, and left me too tired for part 2. Picked it up the next evening, and this time decided to take my time and try using a parser built as part of Chapter 13 of Graham Hutton's book - Programming in Haskell. 

In that chapter he walks us through building a parser combinator library from scratch, and it's a really good exercise. I haven't gone through it in more than a year and I've completely forgotten everything about it, but that's why I'm doing AoC in Haskell.

Using a parser combinator, part 2 is cake. But that's only because there's clear operator precedence. For part 1, it wouldn't work because it considers `+` and `*` of equal precedence, and therefore is free to evaluate them in what amounts to backwards order, due to the way the parser recurses and then evaluates.

So my manual method works fine in part 1, and a proper parser is good for part 2. It'll be an interesting exercise to move to [Megaparsec](https://hackage.haskell.org/package/megaparsec) which is the parser library of choice, and see what I can do with that.

### Day 17: Conway Cubes
This one wasn't bad, I'd call it a 3-dimensional game of life. I improved upon the earlier game of life algo by using a set rather than actual characters, but other than that pretty straight forward. Still not the fastest with Haskell, could have gone quicker. 

Part 2 was an easy extension of part 1 - just taken to the 4th dimension. Not bad so long as you don't worry about picturing it. Although I did take a very inelegant approach to part 2, copying the entire part 1 and tweaking each function for the extra dimension. A fun exercise would be to parameterize the solution on dimension...
### Day 16: Ticket Translation
Day 16 I found more meticulous than difficult. I was on my way to both parts 1 & 2 inside of 2h, but I made a critical error in interpretation of part 2. As you know from the clue - there were many sample "tickets", each with the same number of fields, and each ticket at each index was for the same field. I incorrectly assumed that there'd only be one range criteria that could fit each field, which was wrong.

When I ran my solution, I exhausted fields before I ran out of indexes, because some indexes matched on more than one field, and they were in effect "greedy" - allocating > 1 field each and leaving too few fields to satisfy all the indexes. I immediately realized my mistake, but by then it was too late to finish the puzzle last night.

The correct approach (or at least the one I took) was for each index to collect all the rules that passed, then to sort the indexes by the number of matching rules. Take the head of that list, and if there's one rule in it, remove that rule from all subsequent indexes, and recurse of the list of remaining indexes. If there's more than one rule, then you'd have to branch into N passes where N is the number of rules that match the index at the head. Only one of those branches should work, the others will come across an index that matches no rule; that branch should be abandoned.

Luckily as I was working through that approach in the repl, I realized that the number of matching rules per index increase monotonically from one to N where N was the number of indexes. This means there's no branching and the `toSingle` function is far simpler.
### Day 15: Rambunctious Recitation
Day 15's puzzle is one where some optimization is in order. I'm running in 95 seconds right now, and clearly there'd be benefit to higher performing storage I think. I doubt it would get me down to sub-second though. I'll eventually have to search for a better algorithm as I'm sure I'm too brute force on this one. But its done!

_as my stats show, I ran into a delay in days 11-14, basically real life took priority!_ 🙂
### Day 10: Adapter Array
That one was EASY! ... and then it was **HARD**. Tired 🥱

Must have been hard for everyone; in spite of the fact that part 2 took me maybe longest of any part 2, still came in 2nd best place ever, so happy with that. Need to clean that code up a bit later.

**Benchmark Part 1**
```
time                 236.2 μs   (224.9 μs .. 256.2 μs)
                     0.958 R²   (0.913 R² .. 0.992 R²)
mean                 244.6 μs   (234.9 μs .. 257.6 μs)
std dev              40.12 μs   (28.86 μs .. 64.34 μs)
variance introduced by outliers: 91% (severely inflated)
```

**Benchmark Part 2**
```
time                 261.4 μs   (256.4 μs .. 269.8 μs)
                     0.979 R²   (0.942 R² .. 0.999 R²)
mean                 265.8 μs   (259.3 μs .. 285.1 μs)
std dev              35.60 μs   (9.227 μs .. 72.80 μs)
variance introduced by outliers: 87% (severely inflated)
```

### Day 9: Encoding Error
AAARRRGGG... this was one of those cases where one tiny error cost me probably 40 minutes!! 🤦🏾‍♂️ 
That made this the worst part 1 so far. So annoying. It happened here in this function from part 1:
```haskell
-- | The list of candidates is found by getting the cross product pairs within the range
-- of `pre` indexes before the current index. Note the condition `, a < b` eliminates cases
-- where `(a,b) == (b,a)` which we don't need to check b/c the sum would be the same
candidates :: Preable -> PreCalcMap -> Int -> [Int]
candidates pre m idx =
  if idx < pre then [] else
  map (m !) keys
  where
    i = idx - pre
    j = idx - 1
    keys = [(a,b) | a <- [i,j], b <- [i,j], a < b]
```
Grrr it's so obvious now. See it?
The `[i,j]` should be `[i..j]` because I'm generating a range 😢... This is one case where Haskell's legendary type system did not help unfortunately. Perhaps one day when we have dependent types there'll be a way to describe this that's stronger.

**Benchmark Part 1**
```
time                 228.4 ms   (223.4 ms .. 234.7 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 235.8 ms   (231.4 ms .. 244.6 ms)
std dev              8.414 ms   (1.269 ms .. 11.78 ms)
variance introduced by outliers: 14% (moderately inflated)
```

**Benchmark Part 2**
```
time                 286.0 ms   (244.4 ms .. 329.8 ms)
                     0.989 R²   (0.956 R² .. 1.000 R²)
mean                 291.8 ms   (282.4 ms .. 300.1 ms)
std dev              11.93 ms   (7.002 ms .. 16.41 ms)
variance introduced by outliers: 16% (moderately inflated)
```

### Day 8: Handheld Halting
Echos of the opcode computer from last year (which I really enjoyed). This one was fun. Hopefully, like last year, we build on the idea of a simulating a computer like thing (they're not true general purpose computers after all) and expand it with more capabilities.

Neither part 1 (at `1.72ms`) or part 2 (at `6.75ms`) took so long that I needed to think about optimizing. It's reasonable the way it is, I use [`Data.IntMap`](https://hackage.haskell.org/package/containers-0.6.4.1/docs/Data-IntMap.html) for the memory which is about as [quick as you can get](https://github.com/haskell-perf/dictionaries) with `Int` indexed maps in Haskell without unusual effort. Of course most people using imperative languages are probably mutating an array. But for me part of the fun of Haskell is finding a solution that's purely immutable and yet performs reasonably well.

One fun note about this one; both part 1 and part 2 worked correctly the first time I ran them. This happens far more often with functional languages like Haskell and F# in my experience. The strong type system, used correctly, makes it far more likely to be correct when it compiles. 

### Day 7: Handy Haversacks
This one was a bit more drawn out; a bit of parsing into a graph of nodes, where each node is a bag that might contain one or more other bags. 


### Day 6: Custom Customs
Fastest time so far for part 1 - good for my standards 🙂

Days 6 and 5 were both easier than the days that preceded them. Not complaining b/c I'm sure there'll be some whoppers coming before long. It took the longest by a good margin (actually I don't know what took me so long on part 2 of day 4). Good fun though!

#### Benchmarks!
Came across a few new things last couple of days, most interesting is the [Criterion](http://www.serpentine.com/criterion/) library for Benchmarking in Haskell. It'll be interesting when I have an algorithm that is in obvious need of improvement. 
_**note:** I've stopped generating the html Criterion generates, it wasn't useful_ 
Day 6 results:

||lower bound	|estimate	|upper bound|
|--|--:|--:|--:|
|OLS regression	|2.57 ms	|2.66 ms	|2.77 ms
|R² goodness-of-fit	|0.967	|0.985	|0.997
|Mean execution time	|2.83 ms	|2.91 ms	|3.12 ms
|Standard deviation	|193 μs	|428 μs|	713 μs
> Outlying measurements have severe (81.4%) effect on estimated standard deviation.

