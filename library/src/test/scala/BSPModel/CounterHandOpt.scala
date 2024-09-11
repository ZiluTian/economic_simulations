package BSPModel
package test

class handOptCounterTest extends BSPBenchSuite {

    val width: Int = 10
    val height: Int = 10
    val totalRounds = 5
    val experimentName = "CounterTest (graph-centric)"

    test("The hand-optimized counter example (shallow copy)"){
        var readOnly = Array.tabulate(width, height)((x, y) => 1)
        var readWrite = Array.tabulate(width, height)((x, y) => 1)

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                Range(0, width).foreach(row => {
                    Range(0, height).foreach(col => {
                        // swap the pointers
                        readWrite(row)(col) = readOnly(row)(col)
                        for {
                            i <- -1 to 1
                            j <- -1 to 1
                            if !(i == 0 && j == 0)
                                dx = (col + i + width) % width
                                dy = (row + j + height) % height
                        } {
                            readWrite(row)(col) += readOnly(dx)(dy)
                        }
                    })
                })
                // swap the pointers
                val tmp = readOnly
                readOnly = readWrite
                readWrite = tmp
            })
        }
    }
    
    test("The hand-optimized counter example (deep copy)"){

        var readOnly = Array.tabulate(width, height)((x, y) => 1)
        var readWrite = Array.tabulate(width, height)((x, y) => 1)

        benchmarkTool[Unit]{
            Range(1, totalRounds).foreach(_ => {
                Range(0, width).foreach(row => {
                    Range(0, height).foreach(col => {
                        for {
                            i <- -1 to 1
                            j <- -1 to 1
                            if !(i == 0 && j == 0)
                                dx = (col + i + width) % width
                                dy = (row + j + height) % height
                        } {
                            readWrite(row)(col) += readOnly(dx)(dy)
                        }
                    })
                })
                readOnly = readWrite.map(_.clone)
            })
        }
    }
}