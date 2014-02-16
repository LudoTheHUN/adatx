# adatx

A Clojure library for Automatic Design of Algorithms Through X.

Initially X will be a brute force search through the space of all programs.


It is highly recommended that none of the functions (their symbols) you place into the symlookup map have any side effects.


Inspired by:
  * http://www-ia.hiof.no/~rolando/
  * http://www.wolframscience.com/
  * http://www.idsia.ch/~juergen/goedelmachine.html


You will need to have a '''~/.java.policy''', an example can be found here: https://github.com/flatland/clojail

If you are under windows, you many find this issue resolution helpfull in setting up the .java.policy  https://github.com/Raynes/clojail/issues/4


### Methodology.

In a lisp, programs are just ASTs and valid symbols, which can be sequentially enumerated.

Initial, development is focused on efficiently covering the program space by never attempting to test the same program twice and to make the tests run quickly.

One of the challenges is surviving execution of potentially crippling non terminating programs. Each attempt is given a time-out, after which the thread it runs on is killed. It may be necessary to increase this time-out on slower computers so that we minimise the chance of killing a correct program. Java's thread.stop is deprecated, but used heavily by this project via the clojail library.




## Usage

WIP

## License

Copyright © 2013 LudoTheHUN

Distributed under the Eclipse Public License, the same as Clojure.

