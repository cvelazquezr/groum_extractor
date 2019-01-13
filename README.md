## Control and Data Flow Extractor

The main purpose of this project is the extraction of several graphs from source code.
The graphs result from the outcome of the analysis are the following: Control Flow Graph, 
Control Data Flow Graph, Control Abstract Control Data Flow Graph (Groum).


### Observations

1. Most of the code came from the work by Mover et al. "Mining framework usage graphs 
from app corpora" in https://github.com/cuplv/biggroum. The goal of this paper is the 
construction of Groums from Android applications (client) that use classes from 
Android SDK (library), extraction of the frequent API calls and isomorphism matching 
of these frequent groums in a large corpora.

2. The original Groum extractor https://github.com/cuplv/FixrGraphExtractor is based 
in Soot: https://github.com/Sable/soot, which is an optimization framework for 
JVM-based languages (Java, Scala, ...), providing intermediate representations for 
analyzing and transforming Java bytecode (Baf, Jimple, Shimple, Grimp).

3. This project is an adaptation of the mentioned extractor tool. Most of the libraries
used were updated and also some deprecated Scala procedures like `JavaConvertions`. The
adaptation pursuit the goal of use the extraction applied to client of libraries in
Maven Ecosystem. Currently is functional with `jar` files and specified classes in 
bytecode `.class` files, but it needs to be adopted to client source code.

### Building and Execution

#### Requirements

* A suitable version of Docker

##### Building the tool

- Start the Docker Container
```
docker-compose up
```

- Pull Docker Maven image from JDK 8
```
docker pull maven:3.6.0-jdk-8-alpine
```

##### Running the tool

###### Command line arguments of the tool
- Classpath: `-l class-path`

In the case of single classes to analyze, the path is the location of the JVM-based 
project. However if the analysis is for a `jar` file, this path is the location of the
`rt.jar` library. In the case of the Alpine Container is similar to the following:

```
/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar
```

- Input folder: `-p path-to-jar-files`

Path to compiled `jar` files that will be going to be analyzed. In the case the
analysis only comprehend a single class, this parameter is NOT necessary.

- Enable jphantom: `-j true|false`

In many cases we do not have all the libraries needed by the project. It's used a 
modified version of jphantom (https://github.com/gbalats/jphantom) to create a stub 
of the missing classes. In this way, Soot can be invoked.

- Output jphantom: `-z path-to-the-output-folder`

Is the directory used by jphantom to store temporary class files.

- Filter on the API packages: `-f package_1:package_2...:package_n`

Filter the ACDFG to use only methods from the listed packages. You must define a 
package, otherwise the tool will NOT work.

- Output path: `-o path-to-the-output-folder`

For each class in analysis is created a folder inside the parameter specified folder.
Inside, exists several folders, each with the name of a method that exist in the 
current class.

In the method's folders exist an homonymous HTML file a folder called `provenance/`.

The `provenance/` folder contains the dot representation of the ACDFG, the original 
and sliced jimple code and the dot representation of the Control Flow Graph and 
Control Data Flow Graph.

The page shows images of these graphs.

#### A simple example

Get a Java program to test:

```
git clone https://github.com/innokenty/swing-file-browser
cd swing-file-browser
docker run -v `pwd`:`pwd` -w `pwd` maven:3.6.0-jdk-8-alpine mvn compile
```

You can run the graph extractor as follows (from the path to the graph extractor):

```
docker run -v `pwd`:`pwd` -w `pwd` ecosystem_extractor:latest java -jar 
/app/target/scala-2.12/ecosystem-assembly-0.1.jar -l <path-to-swing-file-browser>/target/classes 
-f javax.swing:java.awt -c com.example.components.GoUpButton -j true -z phantom/ -o out/
```