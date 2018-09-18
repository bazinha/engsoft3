name := "engsoft3"
 
version := "1.0"
 
scalaVersion := "2.11.6"
 
cancelable in Global := true
 
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
 
libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.3.8"