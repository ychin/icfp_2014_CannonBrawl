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
We initially tried writing the ghost AI in the raw assembly, but soon found out it was quite tedius. The approach then
turned to writing a compiler in haskell that turns abstract haskell data structures into compiled Lambda Man AI
assembly. Concurrently some of us started to implement a simulator for the game using Haskell so we could prototype AI
strategies without a working compiler first.

Features of the compiler including typed expressions using Haskell's type checker, automatic tail call optimization,
reference types to make use of pascal extension. We have written a quad tree to optimize location lookups, and also a path
finder using the quad tree and breadth first search.

For the ghost AI we wrote a simple assembler to allow for jumps, and wrote the assembly directly.


Potential Optimizations
--------------------------------------------------
Due to trying to pathfind every step our code does run a little slow for large maps. Potential possibilities for
optimizations include:

* Compiler improvements such as automatically inlining when using let statements, or group multiple nested lets into one single command
* Cache the state better across steps. We had a quad tree working so could potentially improve it to mark eaten/uneaten pallets
  instead of having to scan the whole map again every step just to find where the closest pallet is.
* Doing multiple pathfinding per step is kind of inefficient. Could use some sort of lazy tree structure to cache pathfinding
  info to avoid redoing calculations.


To Build and Run
--------------------------------------------------
The code is mostly written in Haskell, so you will need to have GHC installed.

The code for the Lambda Man logic is in lambda_utils.hs. To compile it, run lambda_utils.hs in GHC, and the compiled code
will be outputted to stdout in the console.

