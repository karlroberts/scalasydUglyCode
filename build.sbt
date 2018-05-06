name := "historicprices"

version := "0.1"

scalaVersion := "2.12.4"

// https://mvnrepository.com/artifact/org.jsoup/jsoup for screen scrapping html
//libraryDependencies += "org.jsoup" % "jsoup" % "1.11.2"

// for FP ad concurrency
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.19"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.19"

// for JSON
libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.1"

libraryDependencies +="io.argonaut" %% "argonaut-scalaz" % "6.2.1"

libraryDependencies += "io.argonaut" %% "argonaut-monocle" % "6.2.1"

libraryDependencies += "io.argonaut" %% "argonaut-cats" % "6.2.1"
