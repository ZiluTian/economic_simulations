package generated

trait BackendServerTrait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.newItem {
  
    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private var bindingMut_3: scala.Long = 0L;
  private var bindingMut_4: scala.Any = null;
  private var listValMut_5: meta.deep.runtime.RequestMessage = null;
  private var iterMut_6: scala.collection.Iterator[meta.deep.runtime.RequestMessage] = null;
  var timeVar: scala.Int = 0;
  private var positionVar_8: scala.Int = 0;
  
  val commands_88 = (() => {
  val data_9 = new scala.Array[scala.Function0[scala.Unit]](17);
  data_9.update(0, (() => {
    positionVar_8 = 1;
    val x_10 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_11 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_10, 13);
    val x_12 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_11);
    resetData_1.prepend(x_12)
  }));
  data_9.update(1, (() => if (true)
    positionVar_8 = 2
  else
    positionVar_8 = 16));
  data_9.update(2, (() => {
    val x_13 = this.popRequestMessages;
    val x_14 = x_13.iterator;
    iterMut_6 = x_14;
    positionVar_8 = 3
  }));
  data_9.update(3, (() => {
    val x_15 = iterMut_6;
    val x_16 = x_15.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_17 = x_16.hasNext;
    if (x_17)
      {
        val x_18 = iterMut_6;
        val x_19 = x_18.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
        val x_20 = x_19.next();
        listValMut_5 = x_20;
        positionVar_8 = 4
      }
    else
      positionVar_8 = 9
  }));
  data_9.update(4, (() => {
    val x_21 = listValMut_5;
    val x_22 = x_21.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_23 = x_22.methodId;
    val x_24 = x_23.==(0);
    val x_25 = x_24.`unary_!`;
    if (x_25)
      positionVar_8 = 5
    else
      positionVar_8 = 8
  }));
  data_9.update(5, (() => {
    val x_26 = listValMut_5;
    val x_27 = x_26.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_28 = x_27.methodId;
    val x_29 = x_28.==(1);
    val x_30 = x_29.`unary_!`;
    if (x_30)
      {
        val x_31 = listValMut_5;
        val x_32 = x_31.asInstanceOf[meta.deep.runtime.RequestMessage];
        val x_33 = scala.collection.immutable.List.apply[meta.deep.runtime.RequestMessage](x_32);
        val x_34 = this.addReceiveMessages(x_33);
        resetData_0 = x_34;
        positionVar_8 = 6
      }
    else
      positionVar_8 = 7
  }));
  data_9.update(6, (() => positionVar_8 = 3));
  data_9.update(7, (() => {
    val x_35 = listValMut_5;
    val x_36 = x_35.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_37 = x_36.methodId;
    val x_38 = x_37.==(1);
    if (x_38)
      positionVar_8 = 1
    else
      ();
    val x_39 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_40 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_39, 12);
    val x_41 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_40);
    resetData_1.prepend(x_41)
  }));
  data_9.update(8, (() => {
    val x_42 = listValMut_5;
    val x_43 = x_42.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_44 = x_43.methodId;
    val x_45 = x_44.==(0);
    if (x_45)
      {
        val x_46 = java.lang.System.nanoTime();
        resetData_0 = x_46;
        val x_47 = resetData_0;
        val x_48 = x_47.asInstanceOf[scala.Long];
        bindingMut_3 = x_48;
        val x_49 = bindingMut_3;
        val x_50 = x_49.asInstanceOf[scala.Long];
        val x_51 = x_50.toString();
        resetData_0 = x_51;
        val x_52 = resetData_0;
        val x_53 = x_52.asInstanceOf[scala.Any];
        bindingMut_4 = x_53;
        val x_54 = bindingMut_4;
        val x_55 = x_54.asInstanceOf[scala.Any];
        val x_56 = listValMut_5;
        val x_57 = x_56.asInstanceOf[meta.deep.runtime.RequestMessage];
        x_57.reply(this, x_55);
        resetData_0 = ();
        positionVar_8 = 6
      }
    else
      ()
  }));
  data_9.update(9, (() => {
    val x_58 = iterMut_6;
    val x_59 = x_58.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_60 = x_59.hasNext;
    val x_61 = x_60.`unary_!`;
    if (x_61)
      positionVar_8 = 10
    else
      ();
    val x_62 = timeVar;
    val x_63 = x_62.+(1);
    timeVar = x_63
  }));
  data_9.update(10, (() => if (true)
    positionVar_8 = 2
  else
    positionVar_8 = 11));
  data_9.update(11, (() => {
    val x_64 = true.`unary_!`;
    if (x_64)
      {
        val x_65 = resetData_1.remove(0);
        val x_69 = x_65.find(((x_66: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_67 = x_66._1;
          val x_68 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_67.==(x_68)
        }));
        val x_70 = x_69.get;
        val x_71 = x_70._2;
        positionVar_8 = x_71
      }
    else
      ()
  }));
  data_9.update(12, (() => {
    val x_72 = resetData_0;
    val x_73 = x_72.asInstanceOf[scala.Any];
    bindingMut_4 = x_73;
    val x_74 = bindingMut_4;
    val x_75 = x_74.asInstanceOf[scala.Any];
    val x_76 = listValMut_5;
    val x_77 = x_76.asInstanceOf[meta.deep.runtime.RequestMessage];
    x_77.reply(this, x_75);
    resetData_0 = ();
    positionVar_8 = 6
  }));
  data_9.update(13, (() => positionVar_8 = 14));
  data_9.update(14, (() => {
    positionVar_8 = 15;
    val x_78 = timeVar;
    val x_79 = x_78.+(1);
    timeVar = x_79
  }));
  data_9.update(15, (() => positionVar_8 = 14));
  data_9.update(16, (() => {
    val x_80 = true.`unary_!`;
    if (x_80)
      {
        val x_81 = resetData_1.remove(0);
        val x_85 = x_81.find(((x_82: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_83 = x_82._1;
          val x_84 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_83.==(x_84)
        }));
        val x_86 = x_85.get;
        val x_87 = x_86._2;
        positionVar_8 = x_87
      }
    else
      ()
  }));
  data_9
}).apply();
  
  override def run_until(until_89: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_90 = timeVar;
      val x_91 = x_90.<=(until_89);
      x_91.&&({
        val x_92 = positionVar_8;
        val x_93 = commands_88.length;
        x_92.<(x_93)
      })
    }) 
      {
        val x_94 = positionVar_8;
        val x_95 = commands_88.apply(x_94);
        x_95.apply()
      }
    ;
    this
  }
}

class BackendServer extends BackendServerTrait