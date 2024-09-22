package meta.runtime

import Actor.AgentId
import com.fasterxml.jackson.annotation.{JsonTypeInfo, JsonSubTypes, JsonTypeName}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value=classOf[BoolMessage], name = "boolMessage"),
    new JsonSubTypes.Type(value=classOf[IntMessage], name = "intMessage"),
    new JsonSubTypes.Type(value=classOf[DoubleMessage], name = "doubleMessage"),
    new JsonSubTypes.Type(value = classOf[BoolArrayMessage], name = "boolArrayMessage"),
    new JsonSubTypes.Type(value = classOf[IntArrayMessage], name = "intArrayMessage"),
    new JsonSubTypes.Type(value = classOf[DoubleArrayMessage], name = "doubleArrayMessage"),
    new JsonSubTypes.Type(value = classOf[BoolVectorMessage], name = "boolVectorMessage"),
    new JsonSubTypes.Type(value = classOf[IntVectorMessage], name = "intVectorMessage"),
    new JsonSubTypes.Type(value = classOf[DoubleVectorMessage], name = "doubleVectorMessage"),
    new JsonSubTypes.Type(value=classOf[RequestMessage], name = "requestMessage"),
    new JsonSubTypes.Type(value=classOf[ResponseMessage], name = "responseMessage")))
trait Message extends JsonSerializable {
  def value: Any
}

@JsonTypeName("boolMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class BoolMessage(value: Boolean) extends Message

@JsonTypeName("intMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class IntMessage(value: Int) extends Message

@JsonTypeName("doubleMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleMessage(value: Double) extends Message

@JsonTypeName("boolArrayMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class BoolArrayMessage(value: Array[Boolean]) extends Message

@JsonTypeName("intArrayMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class IntArrayMessage(value: Array[Int]) extends Message

@JsonTypeName("doubleArrayMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleArrayMessage(value: Array[Double]) extends Message

@JsonTypeName("boolVectorMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class BoolVectorMessage(value: Vector[Boolean]) extends Message

@JsonTypeName("intVectorMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class IntVectorMessage(value: Vector[Int]) extends Message

@JsonTypeName("doubleVectorMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleVectorMessage(value: Vector[Double]) extends Message

@JsonTypeName("doubleVectorVectorMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleVectorVectorMessage(value: Vector[Vector[Double]]) extends Message

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value=classOf[RequestMessage], name = "requestMessage"),
    new JsonSubTypes.Type(value=classOf[ResponseMessage], name = "responseMessage")))
trait Timed extends JsonSerializable {
  var send_time: Long = 0
  var latency: Int = 1  // the allowed delay of the message
}

/**
  * This represents a message, which is used for sending something to another actor
  * @param senderId the id of the sender
  * @param sessionId If not None, then the sender expects a reply
  * @param methodId the id of the method that should be called. Label local RPC methods with 1 to n.
  * @param send_time the timestamp of the send message
  * @param argss the arguments of the method
  */
@JsonTypeName("requestMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class RequestMessage(senderId: AgentId,
                          sessionId: Option[String],
                          methodInfo: String,
                          value: List[List[Any]])
    extends Message with Timed 

/**
  * This class is used to answer to a received message.
  * @param arg the return value of the method/answer of the request message
  * @param sessionId the same as that of the request
  */
@JsonTypeName("responseMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class ResponseMessage(value: Any, sessionId: String)
    extends Message with Timed