# Printing numbers in words using:

- scala 2.12.8
- gradle wrapper 5.4.1
- dockerfile
- scalatest
- log4j2


### Building

The starter script runs a few `./gradlew` tasks as the last step in the setup. Additional builds, testing, and 
bundling can be had by running the following `./gradlew` tasks:
```
# Run tests
./gradlew clean test

# Bundle the project
./gradlew clean shadowjar

# Run the project
./gradlew clean shadowjar
java -jar build/libs/scala-numbers-in-words-all.jar
```

### IDE Tips 

If using IntelliJ IDEA:
* close any existing projects to arrive at the "Welcome" menu
* select "Import Project"
* select your local project directory
* choose "Import project from external model", with Gradle as the model option
* choose "Use default gradle wrapper" and press "Finish"

### Build Docker Image
```
# Build the bundled jar
./gradlew clean shadowJar

# Build the docker image and tag it with your project name (e.g. myproject)
docker build -t myproject .

# Run the docker image
docker run myproject:latest
```
