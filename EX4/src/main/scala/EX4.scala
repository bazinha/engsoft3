import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.duration._
import scala.io._
import scala.util.Random

sealed trait Message

case object Gerar extends Message
case class Work() extends Message
case class Resultado(n:Int) extends Message


class Maquina(workers: Int) extends Actor
{
    
    
    val worker = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(workers)), "worker")
    
    def numero_aleatorio() : Int = {
        val r = scala.util.Random
        val numero_correto = r.nextInt(199) + 1
        return numero_correto;
    } 
    
    
    override def receive:Receive = {
        case Gerar =>
        {
            for(i<- 0 until 32)
            worker !  Work()
        }
        
        case Resultado(res) => 
        {
            val cont:Int = 0
            var resultados:Array[Int] = new Array (0)
            resultados = res +: resultados
            
            for(i<-0 until resultados.length)
            {
                if(resultados(i) == numero_aleatorio())
                {
                    println(s"A maquina $i acertou o resultado: $resultados(i)")
                    cont+1
                }
                
                else if(cont == 0)
                {
                    println(s"A maquina $i não acertou ")
                }
            }
        }
    }

}

class Worker extends Actor
{
    def Sorteio(): Int = {
        val r = scala.util.Random
        val numero = r.nextInt(199) + 1
        return numero
    } 
    
    
    override def receive:Receive = 
    {
        case Work() => sender ! Resultado(Sorteio)
        
    }
    
   
}

object Teste{
    
     def main(args: Array[String]): Unit = {
         
         val system = ActorSystem("MainSystem")
         val masterActor = system.actorOf(Props(new Maquina(32)),"masterActor")
         
         masterActor ! Gerar
     }
    
    
    
}

