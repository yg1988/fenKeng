import sbt._

resolvers += "Typesafe repository" at
  "http://repo.typesafe.com/typesafe/releases/"

//addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.6.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.4")

addSbtPlugin("com.lihaoyi" % "workbench" % "0.1.5")


libraryDependencies += "commons-codec" % "commons-codec" % "1.9"

libraryDependencies += "com.lihaoyi" %% "scalarx" % "0.2.8"





