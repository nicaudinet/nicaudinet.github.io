---
layout: post
title: Water Sort in Haskell
excerpt: Implementing the water sort puzzle in game in Haskell
---

Water Sort is a puzzle game where you have to sort coloured water into bottles.
If you haven't tried it yet, there are many clones of it
[online](https://watersort.org/) or [on your
phone](https://play.google.com/store/apps/details?id=com.JindoBlu.OfflineGames&hl=en_US&pli=1).
I started playing the game a while ago and, after getting a little addicted to
it, I decided to implement it in Haskell for fun (and to immunise myself against
it).

In this post I'll give an overview of how I implemented the game with a simple
terminal-based UI and a list-based solver which takes advantage of laziness. My
goal is to show people that are perhaps not very familiar with Haskell that
implementing a simple game like this can be fun and rewarding, and to show a
couple cool ideas along the way like using the
[MonadPlus](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Monad.html#t:MonadPlus)
instance of the list monad.

All the code for the game [is on
GitHub](https://github.com/nicaudinet/bottles/tree/water-sort-simple), and I
will be linking to various files there throughout the post!

## Water Sort

The game starts with a bunch of bottles with colour layers stacked on top of one
other, together with some empty bottles. The version of the game I played has
bottles with four layers of colours and two empty bottles:

![intro-puzzle.svg]({{ site.baseurl }}public/images/water-sort/intro-puzzle.svg){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}

The goal of the game is to sort the colours into single bottles:

![intro-solution.svg]({{ site.baseurl }}public/images/water-sort/intro-solution.svg){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}

You progress in the game by pouring the top colour layer of a bottle into another
bottle:

![pouring.svg]({{ site.baseurl }}public/images/water-sort/pouring.svg){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}


However, pouring has some simple restrictions:

1. You can only pour a colour into an empty bottle or a bottle with the same
   colour on top
2. You cannot overfill bottles

![rules.svg]({{ site.baseurl }}public/images/water-sort/rules.svg){:width="60%" style="display:block; margin-left:auto; margin-right:auto"}

# Implementing the game

To implement the game I used a simple variation of the model-view-update
pattern, also known as [the Elm
architecture](https://guide.elm-lang.org/architecture/). In brief, the model is
the data structure containing the state of the game, the view is a function
which takes the model and renders it to the screen, and the update is a function
which takes user inputs and adjusts the model accordingly. The game itself
consists of a game loop that repeats either until the bottles are sorted (the
player wins) or there are no more available actions (the player loses).

## The Model

My first step was to model the game state with data types. The whole game state is
captured by the `Bottles` type, which is
[a
Map](https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Lazy.html#g:1)
of bottles to their identifiers:

```haskell
type Bottles = M.Map BottleId Bottle
```

A `Bottle` is modelled as a list of `Color`s, where the head of the list
corresponds to the topmost colour in the bottle. A `BottleId` is just a type
alias for an `Int`:

```haskell
type BottleId = Int
type Bottle = [Color]
```

Since we will only support levels of a fixed size, we only need our types to
represent a dozen colours, so I decided to model `Color` with a sum type: 

```haskell
data Color
  = Yellow
  | White
  | Red
  | LightBlue
  | LightGreen
  | Pink
  | Brown
  | DarkGreen
  | Orange
  | DarkBlue
  | DarkRed
```

The full code for the model can be found in the
[Model.hs](https://github.com/nicaudinet/bottles/blob/water-sort-simple/src/Bottles/Model.hs)
module.

## The View

For simplicity, I chose to use the terminal as the UI for my implementation.

To print colours to the terminal, I used two space characters (which on my
terminal roughly form a square) and coloured them using [xterm-256 colour
codes](https://stackabuse.com/how-to-print-colored-text-in-python/) . The final
outcome looks like this:

![view.png]({{ site.baseurl }}public/images/water-sort/view.png){:width="70%" style="display:block; margin-left:auto; margin-right:auto"}

This worked well, but requires one to have a terminal that supports 256 colours
and a mono-space font so that two spaces roughly form a square. In code, the
function to print a colour looks like this:

```haskell
showColor :: Color -> String
showColor color = "\x1b[48;5;" <> show (colorCode color) <> "m  \x1b[0m"
  where
    colorCode :: Color -> Int
    colorCode Yellow = 220
    colorCode LightBlue = 44
    colorCode DarkBlue = 21
    colorCode Brown = 130
    colorCode LightGreen = 47
    colorCode DarkGreen = 22
    colorCode Pink = 201
    colorCode White = 255
    colorCode Red = 196
    colorCode Orange = 208
    colorCode DarkRed = 88
```

`colorCode` here is the mapping from `Color` to `xterm-256` colour codes. The
weird-looking strings on either side of the colour code are the escape codes to
set the background colour.

After figuring out how to print a colour, the next challenge was to print the
bottles side by side. Since we chose the terminal as our UI this is a little
tricky because we can only print one row at time from top to bottom. This means
we need to transform our `Bottles` map into a list of rows and then print them
in sequence. To do this, I created a `Grid` type consisting of a list of `Row`s,
which in turn consist of a list of `Square`s. A `Square` is either empty, a
separator, or a colour:

```haskell
data Square = Empty | Separator | Full Color
type Row = [Square]
type Grid = [Row]
```

Then, I used an `unfoldr` to generate a `Grid` from the `Bottles`:

```haskell
bottlesToGrid :: Bottles -> Grid
bottlesToGrid = reverse . unfoldr makeRow . map reverse . M.elems
  where
    getSquare :: Bottle -> Square
    getSquare = maybe Empty Full . headMaybe

    makeRow :: [Bottle] -> Maybe (Row, [Bottle])
    makeRow xs
      | all null xs = Nothing
      | otherwise = Just (map getSquare xs, map tailSafe xs)
```

This function works by constructing the rows from the bottom of the bottles to
the top, and then reversing the order of the rows at the end. The `unfoldr` uses
a `makeRow` function which takes a list of `Bottle`s and constructs a single row
from it. The row is constructed by mapping over the bottles with the `getSquare`
function, which returns an `Empty` square if the bottle is empty and a `Full`
square with the colour if it's not. If all the bottles are empty, `makeRow`
returns `Nothing` to signal the end of the unfold. I also used two helper
functions:
- `headMaybe :: [a] -> Maybe a` is a safe version of `head` that returns
`Nothing` when given an empty list
- `tailSafe :: [a] -> [a]` is a version of `tail` that returns an empty list
when given an empty list.

The final step was to actually make the functions to print the rows to the
screen:

```haskell
showSquare :: Square -> String
showSquare Empty = "  "
showSquare Separator = "|"
showSquare (Full color) = showColor color

showRow :: Row -> String
showRow row =
  let
    squares = map showSquare row
    separator = showSquare Separator
  in separator <> intercalate separator squares <> separator
```

The `showSquare` function uses the `showColor` function from before to print the
full squares, and otherwise maps the other types of squares to simple strings.
The `showRow` function maps `showSquare` over the rows created by
`bottlesToGrid` and places a separator between square with `intercalate`. Check
out the
[View.hs](https://github.com/nicaudinet/bottles/blob/water-sort-simple/src/Bottles/View.hs)
file for the full code to show the game.

## The Update

Players in this game can only make a single type of action: a pour from one
bottle to another. Pours are captured with a product type of two bottle ids:

```haskell
data Pour = Pour
  { from :: BottleId
  , to :: BottleId
  }
```

For each turn in the game the user is asked to input a pour as two numbers
separated by an arrow (e.g. "2 -> 3"). The game then takes the line as input,
splits it on the `"->"` string, and parses the two numbers on either side. This
is done by the `parsePour` function:

```haskell
parsePour :: String -> Either GameError Pour
parsePour line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- maybeThrow (InvalidBottleId from) (readMaybe from)
    toBottle <- maybeThrow (InvalidBottleId to) (readMaybe to)
    pure (Pour fromBottle toBottle)
  _ -> Left (InvalidInput line)
```

Parsing errors are handled explicitly by returning an `Either GameError` type.
`GameError` is a sum type that captures all the logical errors that can occur in
the game:

```haskell
data GameError
  = InvalidPuzzleType String
  | InvalidBottleId String
  | InvalidInput String
  ...
```

In larger projects it could be a good idea to split up error types like this
into several smaller types, but for a small project like this I decided that
having a single error type was good enough.

In `parsePour` we also make use of the monad instance of `Either` which
short-circuits the function as soon as it encounters a `Left` value, thus
functioning as a simple error handling mechanism. The `maybeThrow` function is a
wrapper function which throws the given error if it encounters a `Nothing`
value:

```haskell
maybeThrow :: GameError -> Maybe a -> Either GameError a
maybeThrow err Nothing = Left err
maybeThrow _ (Just a) = Right a
```

These errors are then handled in the top-level game loop function. If an error
is encountered, the user is shown a message which explains the `GameError` and
restarts the current loop. Otherwise, the game continues as normal.

Once a `Pour` is parsed from the user it is passed to the `update` function
which contains the main game state update logic:

```haskell
update :: Bottles -> Pour -> Either GameError Bottles
update bs (Pour from to) = do
  -- Get the bottles
  fromBottle <- maybeThrow (BottleNotFound from) (M.lookup from bs)
  toBottle <- maybeThrow (BottleNotFound to) (M.lookup to bs)
  -- Check that from and to are different
  whenThrow (from == to) (FromAndToAreTheSame from)
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> Left (FromBottleIsEmpty from)
    (x : xs) -> Right (x, concat xs)
  -- Check we're not just swapping bottles (no cycles) 
  whenThrow (null fromTail && null toBottle) NoOpAction
  -- Check there's space in the to bottle
  whenThrow (length toBottle > 4 - length fromHead) (ToBottleIsTooFull to)
  -- Check the colors match
  let fromColor = head fromHead
  case headMaybe toBottle of
    Nothing -> Right ()
    Just toColor ->
      whenThrow (fromColor /= toColor) (ColorsDontMatch fromColor toColor)
  -- Update and return the game state
  pure . M.insert from fromTail . M.insert to (fromHead <> toBottle) $ bs
```

Although the looks a bit long and scary, it is essentially simple in structure:

1. Get the two bottles from the game state
2. Check that the pour is valid
3. Update the game state

Like `parsePour`, it also uses `GameError` and the `Either` monad instance to
handle errors. `whenThrow` is another helper function which takes a `Bool` and
throws an error if it's `True`:

```haskell
whenThrow :: Bool -> GameError -> Either GameError ()
whenThrow True err = Left err
whenThrow False _ = Right ()
```

The full code for the update is in the
[Update.hs](https://github.com/nicaudinet/bottles/blob/water-sort-simple/src/Bottles/Update.hs)
module.

##  Putting it all together

Finally, we have all the components we need to implement the full game loop! The
body of the loop is contained in the `step` function:

```haskell
step :: Bottles -> IO Bottles
step bottles = do
  putStrLn (showGame bottles)
  line <- getLine
  case (parsePour line >>= update bottles) of
    Right newBottles -> pure newBottles
    Left gameError -> do
      print gameError
      pure bottles
```

In other words, for each step we:

1. Show the current state of the game
2. Ask the user for input
3. Parse the input
4. Update the game state based on the input

If we encounter an error in step 3 or 4, we print out the error to the user and
return the original game state. Otherwise, we return the new game state. Also,
notice that this is the first function we've seen that actually uses `IO`!

The last part is the actual game loop itself, which I implemented as a recursive
function which repeatedly calls itself until the game is over:

```haskell
loop :: Bottles -> IO Bottles
loop bottles = do
  newBottles <- step bottles
  if gameOver newBottles
    then pure newBottles
    else loop newBottles
```

The `gameOver` function checks whether all the bottles are properly sorted or
not:

```haskell
validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a, b, c, d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Bottles -> Bool
gameOver = all validBottle . M.elems
```

# Creating puzzles

Now that we have a working game, we need a way to generate new puzzles so that
users can play the game to their heart's content.

If we have enough empty bottles, the problem is always solvable.

[Paper](https://arxiv.org/pdf/2202.09495) with bounds for when the puzzle is
solvable or not. 

Set the heights of the bottles to 4 and the number of empty bottles to 2.

Not all configurations are solvable for enough colours, but many are. So our
strategy is:

1. Randomly generate a configuration
2. Try to solve it
3. Check if the solution is solvable
    - If it is solvable, return the configuration
    - If it's not solvable, repeat the whole process

```haskell
data PuzzleSize = Small | Medium | Large

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  idxs <- replicateM (length xs) getRandom
  pure (map fst (sortBy (comparing snd) (zip xs (idxs :: [Int]))))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | length xs < n = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)

randomBottles :: MonadRandom m => Int -> m Bottles
randomBottles n = do
  let initColors = concat $ replicate 4 $ map toEnum [0..n-1]
  randomColors <- shuffle initColors
  let bottles = chunksOf 4 randomColors <> [[], []]
  pure (M.fromList $ zip [0..] bottles)

sizeToInt :: PuzzleSize -> Int
sizeToInt Small = 4
sizeToInt Medium = 7
sizeToInt Large = 10

createPuzzle :: MonadRandom m => PuzzleSize -> m Bottles
createPuzzle size = do
  bottles <- randomBottles (sizeToInt size)
  if isJust (solve bottles)
  then pure bottles
  else createPuzzle size
```

# Solving the game

To solve the game we need to find a sequence of pours that will sort an initial
set of bottles (where possible). In my case, I implemented the solver as
follows:

```haskell
solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solutions . SolverState []
```

The first thing this function does is to transform the input `Bottles` into a
`SolverState`, which is a record of the current game state and the history of
pours that led to it:

```haskell
data SolverState = SolverState
  { history :: [Pour]
  , current :: Bottles
  }
```

The function then passes the initial solver state to the `solver` function,
which is where most the magic happens:

```haskell
solutions :: SolverState -> [SolverState]
solutions state
  | gameOver (current state) = pure state
  | otherwise = do
      (pour, newBottles) <- possiblePours (current state)
      solutions (SolverState (pour : history state) newBottles)
```

The `solutions` function returns a list of all possible solution states coupled
with their pour history. Given a solver state, it first checks if the game is
over using the same function as before. If the game is indeed over, it returns
the current solver state and stops the recursion. Otherwise, it finds the list
of all valid pours from the current state and recursively calls `solutions` on
each of them [1]. In this way, it traverses the entire tree of possible sequences of
pours, gathering all the solutions as it goes.

The list of valid pours is created by the `possiblePours` function, which first
creates a list of all possible pours from the bottle ids and then filters out
the invalid ones using the `tryPour` function:

```haskell
possiblePours :: Bottles -> [(Pour, Bottles)]
possiblePours bottles =
  let bottleIds = M.keys bottles
      pours = liftA2 Pour bottleIds bottleIds
   in catMaybes (map (tryPour bottles) pours)
```

As its name implies, the `tryPour` function "tries" to perform a pour in the
current game state using the `update` function from before, and returns
`Nothing` if the pour is not successful:

```haskell
tryPour :: Bottles -> Pour -> Maybe (Pour, Bottles)
tryPour bottles pour =
  case update bottles pour of
    Left _ -> Nothing
    Right newBottles -> Just (pour, newBottles)
```

Infinite cycles, for example where a colour is poured back and forth between two
bottles forever, are prevented by an extra check in the `update` function that
ensures these types of pours are considered invalid. In turn, this ensures that
the `solutions` function is always guaranteed to terminate.

However, the careful reader however might notice that traversing the entire tree
of possible pour sequences can quickly become computationally infeasible. This
is where the second bit of magic comes in: we take the head of the list of
solutions with `headMaybe`. Thanks to Haskell's
[laziness](https://en.wikibooks.org/wiki/Haskell/Laziness), taking the head of
the list of possible solutions means that we actually stop the traversal once
the first solution is found, effectively transforming the solver into a
[depth-first search](https://en.wikipedia.org/wiki/Depth-first_search) [2]! Neat
right?

The final step taken by `solve` is to extract the pour history from our solution
state and to reverse it to get the correct order of pours. The full code for the
solver is contained in the
[Solver.hs](https://github.com/nicaudinet/bottles/blob/water-sort-simple/src/Bottles/Solver.hs)
module.

And that's it! Hope you enjoyed this little journey through my implementation of
a Haskell clone of Water Sort, and learned a new trick or two along the way.
'Till next time!



---

[1] Note that I used the [monad instance for
lists](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List) here,
but I could have just as easily used a list comprehension instead:

```haskell
solutions :: SolverState -> [SolverState]
solutions state
  | gameOver (current state) = pure state
  | otherwise =
      concat
        [ solutions (SolverState (pour : history state) newBottles)
        | (pour, newBottles) <- possiblePours (current state)
        ]
```

[2] I used lists and laziness in this case because it was the simplest way to
implement and explain the solver that I could think of. However, using lists in
this way could still consume tons of memory as it keeps track of previous
unsuccessful branches in memory (I believe). Instead, one could use `Maybe` and
its
[MonadPlus](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Monad.html#t:MonadPlus)
instance to implement a constant-space search.
