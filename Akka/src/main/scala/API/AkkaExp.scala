package simulation.akka.API

import akka.cluster.typed.Cluster
import meta.runtime.Actor
import com.typesafe.config.ConfigFactory
import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import scala.collection.JavaConversions._
import akka.actor.typed.{Behavior}
import akka.actor.typed.scaladsl.Behaviors

object AkkaExp {
    sealed trait Command
    final case class SpawnDriver(totalWorkers: Int, totalTurn: Int) extends Command
    final case class SpawnWorker(workerId: Int, sims: Seq[Actor], totalWorkers: Int) extends Command
    final case class DriverStopped() extends Command
    final case class WorkerStopped(workerId: Int, sims: Seq[Actor]) extends Command

    var cluster: Cluster = null
    var totalWorkers: Int = 0
    val stoppedWorkers: ConcurrentLinkedQueue[Int] = new ConcurrentLinkedQueue[Int]()
    var activeWorkers: ConcurrentLinkedQueue[Int] = new ConcurrentLinkedQueue[Int]()
    var finalAgents: ConcurrentLinkedQueue[Actor] = new ConcurrentLinkedQueue[Actor]()
    private var merged: Boolean = false

    def apply(totalTurn: Int, totalWorkers: Int, actors: List[Actor], merged: Boolean): Behavior[Command] = 
        Behaviors.setup { ctx => 
            cluster = Cluster(ctx.system)
            this.totalWorkers = totalWorkers
            this.merged = merged
            val roles: Set[String] = cluster.selfMember.getRoles.toSet
            val totalActors = actors.size
            var actorsPerWorker = totalActors/totalWorkers
            if (totalActors % totalWorkers > 0){
                actorsPerWorker += 1
            }
            stoppedWorkers.clear()
            activeWorkers.clear()
            finalAgents.clear()
            
            ctx.log.debug(f"${actorsPerWorker} actors per worker")

            if (roles.exists(p => p.startsWith("Worker"))) {
                ctx.log.debug(f"Creating a worker!")
                val wid = roles.head.split("-").last.toInt
                val containedAgents = actors.slice(wid*actorsPerWorker, List((wid+1)*actorsPerWorker, totalActors).min)        
                // if (containedAgents.size > 0){
                    ctx.self ! SpawnWorker(wid, containedAgents, totalWorkers)
                // }
            } 
            
            if (cluster.selfMember.hasRole("Driver")) {
                ctx.log.debug(f"Creating a driver!")
                ctx.self ! SpawnDriver(totalWorkers, totalTurn)
            } 

            if (cluster.selfMember.hasRole("Standalone")) {
                ctx.log.debug(f"Standalone mode")
                ctx.self ! SpawnDriver(totalWorkers, totalTurn)
                for (i <- Range(0, totalWorkers)){
                    val containedAgents = actors.slice(i*actorsPerWorker, List((i+1)*actorsPerWorker, totalActors).min)        
                    // if (containedAgents.size > 0){
                        ctx.self ! SpawnWorker(i, containedAgents, totalWorkers)
                    // }
                }
            }
            waitTillFinish(Vector.empty)
        }

    def waitTillFinish(finalAgents: IndexedSeq[Actor]): Behavior[Command] = {
        Behaviors.receive { (ctx, message) => 
            message match {
                case SpawnDriver(totalWorkers, totalTurn) => 
                    val driver = ctx.spawn((new simulation.akka.core.Driver).apply(totalWorkers, totalTurn), "driver")
                    ctx.watchWith(driver, DriverStopped())
                    Behaviors.same

                case SpawnWorker(workerId, agents, totalWorkers) =>
                    val sim = if (merged) {
                      ctx.spawn((new simulation.akka.core.sequential.Worker).apply(workerId, agents, totalWorkers), f"worker${workerId}")
                    } else {
                      ctx.spawn((new simulation.akka.core.concurrent.Worker).apply(workerId, agents, totalWorkers), f"worker${workerId}")
                    }
                     
                    activeWorkers.add(workerId)
                    ctx.watchWith(sim, WorkerStopped(workerId, agents))
                    Behaviors.same
                
                case DriverStopped() =>
                    if (cluster.selfMember.hasRole("Standalone")) {
                        Behaviors.same
                    } else {
                        Behaviors.stopped {() =>
                            ctx.system.terminate()
                        }
                    }

                case WorkerStopped(workerId, agents) =>
                    if (cluster.selfMember.hasRole("Standalone")) {
                        ctx.log.debug(f"Worker stop signal received! ${workerId} Stopped workers: ${stoppedWorkers}")
                        if (!stoppedWorkers.contains(workerId)){
                            stoppedWorkers.add(workerId)
                            if (activeWorkers.toSet.diff(stoppedWorkers.toSet).isEmpty){
                                Simulate.addStoppedAgents(finalAgents ++ agents)
                                Behaviors.stopped {() =>
                                    ctx.system.terminate()
                                }
                            } else {
                                waitTillFinish(finalAgents ++ agents)
                            }
                        } else {
                            if (activeWorkers.toSet.diff(stoppedWorkers.toSet).isEmpty){
                                Behaviors.stopped {() =>
                                    ctx.system.terminate()
                                }
                            } else {
                                waitTillFinish(finalAgents)
                            }
                        }
                    } else {
                        Behaviors.stopped {() =>
                            ctx.system.terminate()
                        }
                    }                   
            }

        }
    }
}