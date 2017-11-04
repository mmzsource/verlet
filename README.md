# Verlet Integration

This repo uses verlet integration to simulate a world with:

- Particles
- Sticks connecting particles
- A Cloth

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

- Click and drag any 'point'
- 'unpin' a pinned point

## License

Use the code the way you want it at your own risk. It is not copyrighted.
