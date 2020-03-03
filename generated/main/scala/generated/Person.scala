package generated

trait PersonTrait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.newItem {
    var trafficLight: generated.TrafficLightTrait = null

    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: generated.TrafficLightTrait = null;
  private var bindingMut_4: scala.Boolean = false;
  private var bindingMut_5: scala.Int = 0;
  private var bindingMut_6: generated.TrafficLightTrait = null;
  private var bindingMut_7: scala.Boolean = false;
  private var bindingMut_8: scala.Int = 0;
  private var bindingMut_9: scala.Int = 0;
  private var bindingMut_10: scala.util.Random = null;
  var timeVar: scala.Int = 0;
  private var positionVar_12: scala.Int = 0;
  
  val commands_81 = (() => {
  val data_13 = new scala.Array[scala.Function0[scala.Unit]](18);
  data_13.update(0, (() => positionVar_12 = 1));
  data_13.update(1, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 17));
  data_13.update(2, (() => {
    val x_14 = new scala.util.Random();
    resetData_0 = x_14;
    val x_15 = resetData_0;
    val x_16 = x_15.asInstanceOf[scala.util.Random];
    bindingMut_10 = x_16;
    val x_17 = bindingMut_10;
    val x_18 = x_17.asInstanceOf[scala.util.Random];
    val x_19 = x_18.nextInt(100);
    resetData_0 = x_19;
    val x_20 = resetData_0;
    val x_21 = x_20.asInstanceOf[scala.Int];
    bindingMut_9 = x_21;
    val x_22 = bindingMut_9;
    val x_23 = x_22.asInstanceOf[scala.Int];
    val x_24 = x_23.%(3);
    resetData_0 = x_24;
    val x_25 = resetData_0;
    val x_26 = x_25.asInstanceOf[scala.Int];
    bindingMut_8 = x_26;
    val x_27 = bindingMut_8;
    val x_28 = x_27.asInstanceOf[scala.Int];
    val x_29 = x_28.==(1);
    resetData_0 = x_29;
    val x_30 = resetData_0;
    val x_31 = x_30.asInstanceOf[scala.Boolean];
    bindingMut_7 = x_31;
    positionVar_12 = 3
  }));
  data_13.update(3, (() => {
    val x_32 = bindingMut_7;
    val x_33 = x_32.asInstanceOf[scala.Boolean];
    val x_34 = x_33.`unary_!`;
    if (x_34)
      positionVar_12 = 4
    else
      positionVar_12 = 10
  }));
  data_13.update(4, (() => {
    positionVar_12 = 5;
    val x_35 = timeVar;
    val x_36 = x_35.+(1);
    timeVar = x_36
  }));
  data_13.update(5, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 6));
  data_13.update(6, (() => {
    val x_37 = true.`unary_!`;
    if (x_37)
      positionVar_12 = 7
    else
      ()
  }));
  data_13.update(7, (() => positionVar_12 = 8));
  data_13.update(8, (() => {
    positionVar_12 = 9;
    val x_38 = timeVar;
    val x_39 = x_38.+(1);
    timeVar = x_39
  }));
  data_13.update(9, (() => positionVar_12 = 8));
  data_13.update(10, (() => {
    val x_40 = bindingMut_7;
    val x_41 = x_40.asInstanceOf[scala.Boolean];
    if (x_41)
      {
        val x_42 = this.trafficLight;
        resetData_0 = x_42;
        val x_43 = resetData_0;
        val x_44 = x_43.asInstanceOf[generated.TrafficLightTrait];
        bindingMut_6 = x_44;
        val x_45 = bindingMut_6;
        val x_46 = x_45.asInstanceOf[generated.TrafficLightTrait];
        val x_47 = x_46.state;
        resetData_0 = x_47;
        val x_48 = resetData_0;
        val x_49 = x_48.asInstanceOf[scala.Int];
        bindingMut_5 = x_49;
        val x_50 = bindingMut_5;
        val x_51 = x_50.asInstanceOf[scala.Int];
        val x_52 = x_51.==(0);
        resetData_0 = x_52;
        val x_53 = resetData_0;
        val x_54 = x_53.asInstanceOf[scala.Boolean];
        bindingMut_4 = x_54;
        positionVar_12 = 11
      }
    else
      ()
  }));
  data_13.update(11, (() => {
    val x_55 = bindingMut_4;
    val x_56 = x_55.asInstanceOf[scala.Boolean];
    val x_57 = x_56.`unary_!`;
    if (x_57)
      {
        scala.Predef.println("The person is crossing the road");
        resetData_0 = ();
        positionVar_12 = 12
      }
    else
      positionVar_12 = 13
  }));
  data_13.update(12, (() => positionVar_12 = 4));
  data_13.update(13, (() => {
    val x_58 = bindingMut_4;
    val x_59 = x_58.asInstanceOf[scala.Boolean];
    if (x_59)
      {
        scala.Predef.println("The person asks to cross the road!");
        resetData_0 = ();
        val x_60 = this.trafficLight;
        resetData_0 = x_60;
        val x_61 = resetData_0;
        val x_62 = x_61.asInstanceOf[generated.TrafficLightTrait];
        bindingMut_3 = x_62;
        val x_63 = ((this): meta.deep.runtime.Actor).id;
        val x_65 = {
          val x_64 = bindingMut_3;
          x_64.asInstanceOf[generated.TrafficLightTrait]
        };
        val x_66 = x_65.id;
        val x_67 = scala.collection.immutable.Nil.::[scala.collection.immutable.List[scala.Any]](((scala.collection.immutable.Nil): scala.collection.immutable.List[scala.Any]));
        val x_68 = meta.deep.runtime.RequestMessage.apply(x_63, x_66, 0, x_67);
        ((this): meta.deep.runtime.Actor).sendMessage(x_68);
        val x_69 = x_68.sessionId;
        ((this): meta.deep.runtime.Actor).setMessageResponseHandler(x_69, ((response_70: meta.deep.runtime.Message) => {
          val x_71 = response_70.asInstanceOf[meta.deep.runtime.ResponseMessage];
          resetData_2 = x_71
        }));
        resetData_0 = null;
        positionVar_12 = 14
      }
    else
      ()
  }));
  data_13.update(14, (() => {
    positionVar_12 = 15;
    val x_72 = timeVar;
    val x_73 = x_72.+(1);
    timeVar = x_73
  }));
  data_13.update(15, (() => {
    val x_74 = resetData_2;
    val x_75 = x_74.==(null);
    if (x_75)
      positionVar_12 = 14
    else
      positionVar_12 = 16
  }));
  data_13.update(16, (() => {
    val x_76 = resetData_2;
    val x_77 = x_76.!=(null);
    if (x_77)
      {
        val x_78 = resetData_2;
        val x_79 = x_78.arg;
        resetData_0 = x_79;
        resetData_2 = null;
        scala.Predef.println("The person is crossing the road");
        resetData_0 = ();
        positionVar_12 = 12
      }
    else
      ()
  }));
  data_13.update(17, (() => {
    val x_80 = true.`unary_!`;
    if (x_80)
      positionVar_12 = 7
    else
      ()
  }));
  data_13
}).apply();
  
  override def run_until(until_82: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_83 = timeVar;
      val x_84 = x_83.<=(until_82);
      x_84.&&({
        val x_85 = positionVar_12;
        val x_86 = commands_81.length;
        x_85.<(x_86)
      })
    }) 
      {
        val x_87 = positionVar_12;
        val x_88 = commands_81.apply(x_87);
        x_88.apply()
      }
    ;
    this
  }
}

class Person extends PersonTrait