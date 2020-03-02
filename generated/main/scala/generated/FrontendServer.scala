package generated

trait FrontendServerTrait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.newItem {
    var backendServer: generated.BackendServerTrait = null

    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: java.lang.String = null;
  private var bindingMut_4: java.lang.String = null;
  private var bindingMut_5: generated.BackendServerTrait = null;
  private var bindingMut_6: java.lang.String = null;
  private var bindingMut_7: java.lang.String = null;
  var timeVar: scala.Int = 0;
  private var positionVar_9: scala.Int = 0;
  
  val commands_68 = (() => {
  val data_10 = new scala.Array[scala.Function0[scala.Unit]](15);
  data_10.update(0, (() => positionVar_9 = 1));
  data_10.update(1, (() => if (true)
    positionVar_9 = 2
  else
    positionVar_9 = 14));
  data_10.update(2, (() => {
    positionVar_9 = 3;
    val x_11 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_12 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_11, 13);
    val x_13 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_12);
    resetData_1.prepend(x_13)
  }));
  data_10.update(3, (() => {
    val x_14 = this.backendServer;
    resetData_0 = x_14;
    val x_15 = resetData_0;
    val x_16 = x_15.asInstanceOf[generated.BackendServerTrait];
    bindingMut_5 = x_16;
    val x_17 = ((this): meta.deep.runtime.Actor).id;
    val x_19 = {
      val x_18 = bindingMut_5;
      x_18.asInstanceOf[generated.BackendServerTrait]
    };
    val x_20 = x_19.id;
    val x_21 = meta.deep.runtime.RequestMessage.apply(x_17, x_20, 0, scala.collection.immutable.Nil);
    ((this): meta.deep.runtime.Actor).sendMessage(x_21);
    val x_22 = x_21.sessionId;
    ((this): meta.deep.runtime.Actor).setMessageResponseHandler(x_22, ((response_23: meta.deep.runtime.Message) => {
      val x_24 = response_23.asInstanceOf[meta.deep.runtime.ResponseMessage];
      resetData_2 = x_24
    }));
    resetData_0 = null;
    positionVar_9 = 4
  }));
  data_10.update(4, (() => {
    positionVar_9 = 5;
    val x_25 = timeVar;
    val x_26 = x_25.+(1);
    timeVar = x_26
  }));
  data_10.update(5, (() => {
    val x_27 = resetData_2;
    val x_28 = x_27.==(null);
    if (x_28)
      positionVar_9 = 4
    else
      positionVar_9 = 6
  }));
  data_10.update(6, (() => {
    val x_29 = resetData_2;
    val x_30 = x_29.!=(null);
    if (x_30)
      {
        val x_31 = resetData_2;
        val x_32 = x_31.arg;
        resetData_0 = x_32;
        resetData_2 = null;
        val x_33 = resetData_0;
        val x_34 = x_33.asInstanceOf[java.lang.String];
        bindingMut_4 = x_34;
        val x_35 = bindingMut_4;
        val x_36 = x_35.asInstanceOf[java.lang.String];
        val x_37 = "<html>".+(x_36);
        resetData_0 = x_37;
        val x_38 = resetData_0;
        val x_39 = x_38.asInstanceOf[java.lang.String];
        bindingMut_3 = x_39;
        val x_40 = bindingMut_3;
        val x_41 = x_40.asInstanceOf[java.lang.String];
        val x_42 = x_41.+("</html>");
        resetData_0 = x_42;
        val x_43 = resetData_1.remove(0);
        val x_47 = x_43.find(((x_44: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_45 = x_44._1;
          val x_46 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_45.==(x_46)
        }));
        val x_48 = x_47.get;
        val x_49 = x_48._2;
        positionVar_9 = x_49
      }
    else
      ()
  }));
  data_10.update(7, (() => {
    val x_50 = resetData_0;
    val x_51 = x_50.asInstanceOf[java.lang.String];
    bindingMut_7 = x_51;
    val x_52 = bindingMut_7;
    val x_53 = x_52.asInstanceOf[java.lang.String];
    val x_54 = "requestPage content is ".+(x_53);
    resetData_0 = x_54;
    val x_55 = resetData_0;
    val x_56 = x_55.asInstanceOf[java.lang.String];
    bindingMut_6 = x_56;
    val x_57 = bindingMut_6;
    val x_58 = x_57.asInstanceOf[java.lang.String];
    scala.Predef.println(x_58);
    resetData_0 = ();
    positionVar_9 = 8;
    val x_59 = timeVar;
    val x_60 = x_59.+(1);
    timeVar = x_60
  }));
  data_10.update(8, (() => if (true)
    positionVar_9 = 2
  else
    positionVar_9 = 9));
  data_10.update(9, (() => {
    val x_61 = true.`unary_!`;
    if (x_61)
      positionVar_9 = 10
    else
      ()
  }));
  data_10.update(10, (() => positionVar_9 = 11));
  data_10.update(11, (() => {
    positionVar_9 = 12;
    val x_62 = timeVar;
    val x_63 = x_62.+(1);
    timeVar = x_63
  }));
  data_10.update(12, (() => positionVar_9 = 11));
  data_10.update(13, (() => {
    positionVar_9 = 3;
    val x_64 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_65 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_64, 7);
    val x_66 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_65);
    resetData_1.prepend(x_66)
  }));
  data_10.update(14, (() => {
    val x_67 = true.`unary_!`;
    if (x_67)
      positionVar_9 = 10
    else
      ()
  }));
  data_10
}).apply();
  
  override def run_until(until_69: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_70 = timeVar;
      val x_71 = x_70.<=(until_69);
      x_71.&&({
        val x_72 = positionVar_9;
        val x_73 = commands_68.length;
        x_72.<(x_73)
      })
    }) 
      {
        val x_74 = positionVar_9;
        val x_75 = commands_68.apply(x_74);
        x_75.apply()
      }
    ;
    this
  }
}

class FrontendServer extends FrontendServerTrait