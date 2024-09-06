package meta.runtime

import Actor.AgentId
import com.fasterxml.jackson.annotation.{JsonTypeInfo, JsonSubTypes, JsonTypeName}

@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(value=classOf[IntMessage], name = "intMessage"),
    new JsonSubTypes.Type(value=classOf[DoubleMessage], name = "doubleMessage"),
    new JsonSubTypes.Type(value = classOf[DoubleArrayMessage], name = "doubleArrayMessage"),
    new JsonSubTypes.Type(value=classOf[RequestMessage], name = "requestMessage"),
    new JsonSubTypes.Type(value=classOf[ResponseMessage], name = "responseMessage")))
trait Message extends JsonSerializable {
  def value: Any
}

@JsonTypeName("intMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class IntMessage(value: Int) extends Message

@JsonTypeName("doubleMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleMessage(value: Double) extends Message


@JsonTypeName("doubleArrayMessage")
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
case class DoubleArrayMessage(value: Array[Double]) extends Message

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