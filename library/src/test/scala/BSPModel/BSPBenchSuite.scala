package BSPModel
package test

import org.scalatest._
import funsuite._
import java.io.{File, PrintWriter}

trait BSPBenchSuite extends AnyFunSuite {
    val totalRounds: Int 
    val experimentName: String

    implicit def toVectorGraphLong(g: Map[Long, Iterable[Long]]): Map[Long, Vector[Long]] = {
        g.map(i => (i._1, i._2.toVector))
    } 

    implicit def toVectorGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Vector[Int]] = {
        g.map(i => (i._1.toInt, i._2.map(_.toInt).toVector))
    } 

    implicit def toGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Iterable[Int]] = {
        g.map(i => (i._1.toInt, i._2.map(_.toInt)))
    } 

    implicit def toVertexSetInt(g: Iterable[Long]): Set[Int] = g.map(_.toInt).toSet
    implicit def toVertexSetLong(g: Iterable[Long]): Set[Long] = g.toSet
    implicit def toEdgeVecLong(g: Iterable[Long]): Vector[Long] = g.toVector
    implicit def toEdgeVecInt(g: Iterable[Long]): Vector[Int] = g.map(_.toInt).toVector

    lazy val writer = {
        val x = new File(f"${experimentName}")
        if (x.exists()) {
            val version: Int = getVersion(x)    
            val versionedFile = new File(f"${experimentName}_v${version}")
            x.renameTo(versionedFile)
        }
        new PrintWriter(f"${experimentName}")
    }
    
    def getVersion(file: File): Int = {
        val dir = Option(file.getParentFile).getOrElse(new File("."))
        val baseName = file.getName
        val versions = dir.listFiles()
            .filter(f => f.getName.startsWith(f"${baseName}_"))
            .map(f => f.getName.split("_").last.split("v").last.toInt)
        if (versions.size == 0) {
            1
        } else {
            versions.max + 1
        }
    }

    def benchmarkTool[R](writer: PrintWriter, block: => R): Unit = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        writer.write("Elapsed time: " + (t1 - t0) + "ms\n")
        writer.flush()
    }

    def benchmarkTool[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0) + "ms")
        result
    }
}