# Simple Type-Level Unsupervised Part-Of-Speech Tagger #

This is a short self-contained Clojure implementation of:

[Simple Type-Level Unsupervised POS Tagging](http://www.cs.berkeley.edu/~aria42/pubs/typetagging.pdf)
Yoong Keok Lee, Aria Haghighi and Regina Barzilay 
To appear in proceedings of EMNLP 2010

# Running #

Simply run the script with the following arguments

	infile: path to a file where each line is a sentence and tokens are space separate
	outfile: path to write mapping of words to tags (represented by an integer)
	num-iters: number of iterations to run Gibbs Sampler
	K: number of tag states to use
	alpha: hyper-parameter for type-level distributions (try 1)
	beta: hyper-parameter for token-level distributions (try 0.1)

# Author #
Aria Haghighi (aria42@gmail.com) 

[My Website](http://csail.mit.edu/~aria42)

# Support #

Email author with any issues.

# License #

Copyright (C) 2010 Aria Haghighi

Distributed under the Eclipse Public License, the same as Clojure uses. See the file License.
