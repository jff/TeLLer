# TeLLer v0.1.9

TeLLer is a collection of tools that explores the use of linear logic applied to story telling.
Currently, there are two available tools:

 1. TeLLer: an interactive linear logic prover and proof explorer that is currently being used to study causality in interactive story telling.
 2. CelfToGraph: a frontend to the [Celf system](http://clf.github.io/celf). It allows the transformation of Celf's solutions into structured graphs that can be queried.

## Compiling the TeLLer tool suite
TeLLer is written in Haskell. Currently, we only have available the source distribution via github. 
Assuming that you have the [Haskell platform](http://www.haskell.org/platform/) installed in your system,
you can install TeLLer following the steps:

    git clone https://github.com/jff/TeLLer.git
    cd TeLLer
    cabal configure
    cabal build

At this point the executables are in the directory `dist/`. So, for example, if you want to run CelfToGraph,
you can execute:

    dist/build/CelfToGraph/CelfToGraph

**Important:** CelfToGraph depends on Celf version 2.9 and, currently, you need to set the path to celf manually
in the file `src/CelfToGraphConf.hs`. *(Oh! the joy of research tools...)*

## Querying graphs generated by CelfToGraph
CelfToGraph supports a mini query language that can be used to analyse the graphs generated. The queries
currently supported are:

 * `exists a`: checks if action a exists in the generated graphs
 * `link a1 a2`: checks if action a2 is caused by a1 in the generated graphs
  
The commands `exists` and `link` can be combined with the boolean operators:

    ~, &&, ||, <=, =>, and <=>.

For example: 

    link a1 a2  <=  exists a3 && exists a4


 

## Research team behind TeLLer (ordered alphabetically by surname)

 * Anne-Gwenn Bosser (Teesside University, UK)
 * Marc Cavazza (Teesside University, UK)
 * João F. Ferreira (Teesside University, UK)
 * James Harland (RMIT University, Australia)
 * Andy Kitchen (RMIT University, Australia)
 * Chris Martens (Carnegie Mellon University, US)

## A bit of history

TeLLer started being developed by Andy Kitchen (see [Andy's github repository](https://github.com/andykitchen/linear-logic)).
It was further extended in January 2013 by João F. Ferreira to provide, among other features, support for focusing granularity and structured
graphs generation. In the spring of 2013, CelfToGraph was added to the tool suite.
