package generated

trait VoterTrait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.newItem {
    var consensus_object: generated.ConsensusTrait = null

    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: java.lang.String = null;
  private var bindingMut_4: scala.Long = 0L;
  private var bindingMut_5: generated.ConsensusTrait = null;
  private var bindingMut_6: java.lang.String = null;
  private var bindingMut_7: java.lang.String = null;
  private var bindingMut_8: scala.Long = 0L;
  private var bindingMut_9: java.lang.String = null;
  private var bindingMut_10: java.lang.String = null;
  var timeVar: scala.Int = 0;
  private var positionVar_12: scala.Int = 0;
  
  val commands_75 = (() => {
  val data_13 = new scala.Array[scala.Function0[scala.Unit]](12);
  data_13.update(0, (() => positionVar_12 = 1));
  data_13.update(1, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 11));
  data_13.update(2, (() => {
    val x_14 = this.id;
    resetData_0 = x_14;
    val x_15 = resetData_0;
    val x_16 = x_15.asInstanceOf[scala.Long];
    bindingMut_8 = x_16;
    val x_17 = bindingMut_8;
    val x_18 = x_17.asInstanceOf[scala.Long];
    val x_19 = x_18.toString();
    resetData_0 = x_19;
    val x_20 = resetData_0;
    val x_21 = x_20.asInstanceOf[java.lang.String];
    bindingMut_7 = x_21;
    val x_22 = bindingMut_7;
    val x_23 = x_22.asInstanceOf[java.lang.String];
    val x_24 = "Voter proposes! ".+(x_23);
    resetData_0 = x_24;
    val x_25 = resetData_0;
    val x_26 = x_25.asInstanceOf[java.lang.String];
    bindingMut_6 = x_26;
    val x_27 = bindingMut_6;
    val x_28 = x_27.asInstanceOf[java.lang.String];
    scala.Predef.println(x_28);
    resetData_0 = ();
    val x_29 = this.consensus_object;
    resetData_0 = x_29;
    val x_30 = resetData_0;
    val x_31 = x_30.asInstanceOf[generated.ConsensusTrait];
    bindingMut_5 = x_31;
    val x_32 = this.id;
    resetData_0 = x_32;
    val x_33 = resetData_0;
    val x_34 = x_33.asInstanceOf[scala.Long];
    bindingMut_4 = x_34;
    val x_35 = bindingMut_4;
    val x_36 = x_35.asInstanceOf[scala.Long];
    val x_37 = x_36.toString();
    resetData_0 = x_37;
    val x_38 = resetData_0;
    val x_39 = x_38.asInstanceOf[java.lang.String];
    bindingMut_3 = x_39;
    val x_40 = ((this): meta.deep.runtime.Actor).id;
    val x_42 = {
      val x_41 = bindingMut_5;
      x_41.asInstanceOf[generated.ConsensusTrait]
    };
    val x_43 = x_42.id;
    val x_44 = bindingMut_3;
    val x_45 = x_44.asInstanceOf[java.lang.String];
    val x_46 = scala.collection.immutable.Nil.::[scala.Any](x_45);
    val x_47 = scala.collection.immutable.Nil.::[scala.collection.immutable.List[scala.Any]](x_46);
    val x_48 = meta.deep.runtime.RequestMessage.apply(x_40, x_43, 0, x_47);
    ((this): meta.deep.runtime.Actor).sendMessage(x_48);
    val x_49 = x_48.sessionId;
    ((this): meta.deep.runtime.Actor).setMessageResponseHandler(x_49, ((response_50: meta.deep.runtime.Message) => {
      val x_51 = response_50.asInstanceOf[meta.deep.runtime.ResponseMessage];
      resetData_2 = x_51
    }));
    resetData_0 = null;
    positionVar_12 = 3
  }));
  data_13.update(3, (() => {
    positionVar_12 = 4;
    val x_52 = timeVar;
    val x_53 = x_52.+(1);
    timeVar = x_53
  }));
  data_13.update(4, (() => {
    val x_54 = resetData_2;
    val x_55 = x_54.==(null);
    if (x_55)
      positionVar_12 = 3
    else
      positionVar_12 = 5
  }));
  data_13.update(5, (() => {
    val x_56 = resetData_2;
    val x_57 = x_56.!=(null);
    if (x_57)
      {
        val x_58 = resetData_2;
        val x_59 = x_58.arg;
        resetData_0 = x_59;
        resetData_2 = null;
        val x_60 = resetData_0;
        val x_61 = x_60.asInstanceOf[java.lang.String];
        bindingMut_10 = x_61;
        val x_62 = bindingMut_10;
        val x_63 = x_62.asInstanceOf[java.lang.String];
        val x_64 = "winner is ".+(x_63);
        resetData_0 = x_64;
        val x_65 = resetData_0;
        val x_66 = x_65.asInstanceOf[java.lang.String];
        bindingMut_9 = x_66;
        val x_67 = bindingMut_9;
        val x_68 = x_67.asInstanceOf[java.lang.String];
        scala.Predef.println(x_68);
        resetData_0 = ();
        positionVar_12 = 6
      }
    else
      ();
    val x_69 = timeVar;
    val x_70 = x_69.+(1);
    timeVar = x_70
  }));
  data_13.update(6, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 7));
  data_13.update(7, (() => {
    val x_71 = true.`unary_!`;
    if (x_71)
      positionVar_12 = 8
    else
      ()
  }));
  data_13.update(8, (() => positionVar_12 = 9));
  data_13.update(9, (() => {
    positionVar_12 = 10;
    val x_72 = timeVar;
    val x_73 = x_72.+(1);
    timeVar = x_73
  }));
  data_13.update(10, (() => positionVar_12 = 9));
  data_13.update(11, (() => {
    val x_74 = true.`unary_!`;
    if (x_74)
      positionVar_12 = 8
    else
      ()
  }));
  data_13
}).apply();
  
  override def run_until(until_76: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_77 = timeVar;
      val x_78 = x_77.<=(until_76);
      x_78.&&({
        val x_79 = positionVar_12;
        val x_80 = commands_75.length;
        x_79.<(x_80)
      })
    }) 
      {
        val x_81 = positionVar_12;
        val x_82 = commands_75.apply(x_81);
        x_82.apply()
      }
    ;
    this
  }
}

class Voter extends VoterTrait