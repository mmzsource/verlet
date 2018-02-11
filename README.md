# Verlet Integration

This repo uses verlet integration to simulate a world with:

- Particles
- Sticks connecting particles
- A cloth

It was mostly inspired by [this paper by Thomas
Jakobsen](http://graphics.cs.cmu.edu/nsp/course/15-869/2006/papers/jakobsen.htm).

## Installation

Prerequisites:

- [JVM](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
  installed
- [Leiningen](https://leiningen.org) installed

`git clone <this repo>`

## Usage

Running `lein run` in the root of this project will start an applet where you
can interact with mouse and keyboard.

Keybindings:

- `p` (re)start particle simulation
- `s` (re)start stick simulation
- `c` (re)start cloth simulation

Mouse interactions:

- Click and drag any point
- Unpin a pinned point

## TODO

- [ ] update-point and constrain-point contain duplicate code (vx, vy calc)
- [ ] apply-stick-constraints seems wrong from a concurrency perspective
- [ ] function naming could be more consistent constrain-point should be calc-world-constraint?
- [ ] Run cloc after adding function documentation. Before function
  documentation: 137 locs (commit hash c67a5eef92504b14f9ac63a2a5750a755f58fa5b)


## License

Use the code the way you want it at your own risk. It is not copyrighted.
