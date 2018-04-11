# SolarSimulator
Simulator for solar vehicle race from Darwin to Adelaide written in highly non-idiomatic Scala and some Java 'borrowed' and hacked up from https://github.com/KlausBrunner/solarpositioning

Calculates the position of the sun, relative solar intensity and power usage of an electric engine and assesses the
current battery capacity at all points in time.

An experiment to test out whether using mutable data structures in Scala would yield a non-negligible speed improvement 
for a workload which could be heavily vectorised.

Can I convince the JVM to vectorise my code in Java / Scala, what are the benefits?
When does vectorisation occur? Do I need to be using primitives, ie. non-boxed values for the JIT to attempt vector instructions?

On the `Optimised_single_threaded branch` I also played around with calling out to the fantastic ND4J library
(N-dimensional arrays for Java and Scala), basically NumPy for the JVM.

https://github.com/deeplearning4j/nd4j
https://nd4j.org/

ND4J makes JNI calls to C++ and passes around off-heap byte buffers using https://github.com/bytedeco/javacpp.
In the solar simulator application I achieved 2 - 2.5X performance improvement on a single thread, either due to the performance
of C++ vs Java / Scala, or simply because raw C++ arrays have less memory overhead and therefore I was seeing fewer caches misses.

Unfortunately the process of going between on-heap JVM objects and off-heap C++ arrays using ND4J ended up outweighing the performance
benefit if you need to go back and forth more than once. Which unfortunately I did, as the NDArray 'dataframe' did not have 
all the operators I required.

I blame inefficient Java object serialisation for the conversion cost, so maybe I can take a page out of Spark's book and steal 
some optimised serialisers (better than Kryo) and submit a patch to ND4J.
