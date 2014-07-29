ICFP 2014 Contest
==================================================

Team Cannon Brawl members
--------------------------------------------------
* Ryan Ingram
* Jeff Gates
* Tod Semple
* Tim Martin
* Yee Cheng Chin

Approach
--------------------------------------------------
We initially tried writing the ghost AI in the raw assembly, but soon found out it was quite tedious. The approach then
turned to writing a compiler in haskell that turns abstract haskell data structures into compiled Lambda Man AI
assembly. Concurrently some of us started to implement a simulator for the game using Haskell so we could prototype AI
strategies without a working compiler first.

Features of the compiler including typed expressions using Haskell's type checker, automatic tail call optimization,
reference types to make use of pascal extension. We have written a quad tree to optimize location lookups, and also a path
finder using the quad tree and breadth first search.

Potential Optimizations
--------------------------------------------------
Due to trying to pathfind every step our code does run a little slow for large maps. Potential possibilities for
optimizations include:

* Compiler improvements such as automatically inlining when using let statements, or group multiple nested lets into one single command
* Cache the state better across steps. We had a quad tree working so could potentially improve it to mark eaten/uneaten pallets
  instead of having to scan the whole map again every step just to find where the closest pallet is.
* Doing multiple pathfinding per step is kind of inefficient. Could use some sort of lazy tree structure to cache pathfinding
  info to avoid redoing calculations.

Ghost AI
--------------------------------------------------

For the ghost AI we wrote a simple preprocessor to allow for symbolic jumps, symbolic globals, and a call stack.
See code/flanker3.ghc for an example.

Features of our submitted ghost:

 * We keeps a ring buffer of lambdaman positions and find a point in front of lambdaman.
 * The target position is offset in a different direction for each ghost based on its index. This makes the ghosts tend to flank lambdaman. We found it important that ghosts not bunch up so they could surround lamdbaman in a corridor.
 * There is a little randomness so ghosts can get out of dead ends.
 * In fright mode, the ghost run away from lambdaman.

We ran a tests in our local game simulation to evaluate the AI and optimized the constants.

To Build and Run
--------------------------------------------------
The code is mostly written in Haskell, so you will need to have GHC installed.

The code for the Lambda Man logic is in lambda_utils.hs. To compile it, use command "runhaskell lambda_utils.hs" and the compiled assembly
code will be outputted to stdout in the console. Alternatively if you are using Windows / Visual Studio you can open code/icfp2014.sln
which has build steps set up already.

The code for the simulator is in main.hs. Just compile that and run it.

The pre-compiled ghost AI is in code/ghosts/flanker3.ghc.
