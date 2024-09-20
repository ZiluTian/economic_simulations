package BSPModel
package test

import org.scalatest._
import funsuite._
import java.io.{File, PrintWriter}
import scala.collection.mutable.Queue
import scala.collection.mutable.{Map => MutMap}
import scala.util.Random

trait BSPBenchSuite extends AnyFunSuite {
    val totalRounds: Int 
    val experimentName: String

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
        writer.write("Average time: " + (t1 - t0)/totalRounds + "ms\n")
        writer.flush()
    }

    def benchmarkTool[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0)/totalRounds + "ms")
        result
    }
}