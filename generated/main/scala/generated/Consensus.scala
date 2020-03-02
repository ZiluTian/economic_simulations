package generated

trait ConsensusTrait extends meta.deep.runtime.Actor with meta.example.supermarket.goods.newItem {
    var isLocked: Boolean = false
  var winner: String = ""

    private var resetData_0: scala.Any = null;
  private val resetData_1 = scala.collection.mutable.ListBuffer.apply[scala.collection.immutable.List[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]]]();
  private var resetData_2: meta.deep.runtime.ResponseMessage = null;
  private val x_3 = scala.collection.mutable.ListBuffer.apply[scala.Any]();
  private var methodArgsMut_4: java.lang.String = null;
  private var bindingMut_5: scala.Boolean = false;
  private var bindingMut_6: scala.Boolean = false;
  private var bindingMut_7: java.lang.String = null;
  private var bindingMut_8: scala.Any = null;
  private var listValMut_9: meta.deep.runtime.RequestMessage = null;
  private var iterMut_10: scala.collection.Iterator[meta.deep.runtime.RequestMessage] = null;
  var timeVar: scala.Int = 0;
  private var positionVar_12: scala.Int = 0;
  
  val commands_124 = (() => {
  val data_13 = new scala.Array[scala.Function0[scala.Unit]](20);
  data_13.update(0, (() => {
    positionVar_12 = 1;
    val x_14 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_15 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_14, 16);
    val x_16 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_15);
    resetData_1.prepend(x_16)
  }));
  data_13.update(1, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 19));
  data_13.update(2, (() => {
    val x_17 = this.popRequestMessages;
    val x_18 = x_17.iterator;
    iterMut_10 = x_18;
    positionVar_12 = 3
  }));
  data_13.update(3, (() => {
    val x_19 = iterMut_10;
    val x_20 = x_19.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_21 = x_20.hasNext;
    if (x_21)
      {
        val x_22 = iterMut_10;
        val x_23 = x_22.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
        val x_24 = x_23.next();
        listValMut_9 = x_24;
        positionVar_12 = 4
      }
    else
      positionVar_12 = 12
  }));
  data_13.update(4, (() => {
    val x_25 = listValMut_9;
    val x_26 = x_25.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_27 = x_26.methodId;
    val x_28 = x_27.==(0);
    val x_29 = x_28.`unary_!`;
    if (x_29)
      positionVar_12 = 5
    else
      positionVar_12 = 8
  }));
  data_13.update(5, (() => {
    val x_30 = listValMut_9;
    val x_31 = x_30.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_32 = x_31.methodId;
    val x_33 = x_32.==(1);
    val x_34 = x_33.`unary_!`;
    if (x_34)
      {
        val x_35 = listValMut_9;
        val x_36 = x_35.asInstanceOf[meta.deep.runtime.RequestMessage];
        val x_37 = scala.collection.immutable.List.apply[meta.deep.runtime.RequestMessage](x_36);
        val x_38 = this.addReceiveMessages(x_37);
        resetData_0 = x_38;
        positionVar_12 = 6
      }
    else
      positionVar_12 = 7
  }));
  data_13.update(6, (() => positionVar_12 = 3));
  data_13.update(7, (() => {
    val x_39 = listValMut_9;
    val x_40 = x_39.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_41 = x_40.methodId;
    val x_42 = x_41.==(1);
    if (x_42)
      positionVar_12 = 1
    else
      ();
    val x_43 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
    val x_44 = scala.Tuple2.apply[scala.Tuple2[scala.Int, scala.Int], scala.Int](x_43, 15);
    val x_45 = scala.collection.immutable.Nil.::[scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]](x_44);
    resetData_1.prepend(x_45)
  }));
  data_13.update(8, (() => {
    val x_46 = listValMut_9;
    val x_47 = x_46.asInstanceOf[meta.deep.runtime.RequestMessage];
    val x_48 = x_47.methodId;
    val x_49 = x_48.==(0);
    if (x_49)
      {
        val x_50 = listValMut_9;
        val x_51 = x_50.asInstanceOf[meta.deep.runtime.RequestMessage];
        val x_52 = x_51.argss;
        val x_53 = x_52(0);
        val x_54 = x_53(0);
        x_3.prepend(x_54);
        val x_55 = listValMut_9;
        val x_56 = x_55.asInstanceOf[meta.deep.runtime.RequestMessage];
        val x_57 = x_56.argss;
        val x_58 = x_57(0);
        val x_59 = x_58(0);
        val x_60 = x_59.asInstanceOf[java.lang.String];
        methodArgsMut_4 = x_60;
        val x_61 = methodArgsMut_4;
        val x_62 = x_61.asInstanceOf[java.lang.String];
        val x_63 = "Vote received for ".+(x_62);
        resetData_0 = x_63;
        val x_64 = resetData_0;
        val x_65 = x_64.asInstanceOf[java.lang.String];
        bindingMut_7 = x_65;
        val x_66 = bindingMut_7;
        val x_67 = x_66.asInstanceOf[java.lang.String];
        scala.Predef.println(x_67);
        resetData_0 = ();
        val x_68 = this.isLocked;
        resetData_0 = x_68;
        val x_69 = resetData_0;
        val x_70 = x_69.asInstanceOf[scala.Boolean];
        bindingMut_6 = x_70;
        val x_71 = bindingMut_6;
        val x_72 = x_71.asInstanceOf[scala.Boolean];
        val x_73 = x_72.`unary_!`;
        resetData_0 = x_73;
        val x_74 = resetData_0;
        val x_75 = x_74.asInstanceOf[scala.Boolean];
        bindingMut_5 = x_75;
        positionVar_12 = 9
      }
    else
      ()
  }));
  data_13.update(9, (() => {
    val x_76 = bindingMut_5;
    val x_77 = x_76.asInstanceOf[scala.Boolean];
    val x_78 = x_77.`unary_!`;
    if (x_78)
      positionVar_12 = 10
    else
      positionVar_12 = 11
  }));
  data_13.update(10, (() => {
    val x_79 = this.winner;
    resetData_0 = x_79;
    x_3.remove(0);
    val x_80 = x_3.isEmpty;
    val x_81 = x_80.`unary_!`;
    if (x_81)
      {
        val x_82 = x_3(0);
        val x_83 = x_82.asInstanceOf[java.lang.String];
        methodArgsMut_4 = x_83
      }
    else
      ();
    val x_84 = resetData_0;
    val x_85 = x_84.asInstanceOf[scala.Any];
    bindingMut_8 = x_85;
    val x_86 = bindingMut_8;
    val x_87 = x_86.asInstanceOf[scala.Any];
    val x_88 = listValMut_9;
    val x_89 = x_88.asInstanceOf[meta.deep.runtime.RequestMessage];
    x_89.reply(this, x_87);
    resetData_0 = ();
    positionVar_12 = 6
  }));
  data_13.update(11, (() => {
    val x_90 = bindingMut_5;
    val x_91 = x_90.asInstanceOf[scala.Boolean];
    if (x_91)
      {
        this.`isLocked_=`(true);
        resetData_0 = ();
        val x_92 = methodArgsMut_4;
        val x_93 = x_92.asInstanceOf[java.lang.String];
        this.`winner_=`(x_93);
        resetData_0 = ();
        positionVar_12 = 10
      }
    else
      ()
  }));
  data_13.update(12, (() => {
    val x_94 = iterMut_10;
    val x_95 = x_94.asInstanceOf[scala.collection.Iterator[meta.deep.runtime.RequestMessage]];
    val x_96 = x_95.hasNext;
    val x_97 = x_96.`unary_!`;
    if (x_97)
      {
        this.`isLocked_=`(false);
        resetData_0 = ();
        positionVar_12 = 13
      }
    else
      ();
    val x_98 = timeVar;
    val x_99 = x_98.+(1);
    timeVar = x_99
  }));
  data_13.update(13, (() => if (true)
    positionVar_12 = 2
  else
    positionVar_12 = 14));
  data_13.update(14, (() => {
    val x_100 = true.`unary_!`;
    if (x_100)
      {
        val x_101 = resetData_1.remove(0);
        val x_105 = x_101.find(((x_102: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_103 = x_102._1;
          val x_104 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_103.==(x_104)
        }));
        val x_106 = x_105.get;
        val x_107 = x_106._2;
        positionVar_12 = x_107
      }
    else
      ()
  }));
  data_13.update(15, (() => {
    val x_108 = resetData_0;
    val x_109 = x_108.asInstanceOf[scala.Any];
    bindingMut_8 = x_109;
    val x_110 = bindingMut_8;
    val x_111 = x_110.asInstanceOf[scala.Any];
    val x_112 = listValMut_9;
    val x_113 = x_112.asInstanceOf[meta.deep.runtime.RequestMessage];
    x_113.reply(this, x_111);
    resetData_0 = ();
    positionVar_12 = 6
  }));
  data_13.update(16, (() => positionVar_12 = 17));
  data_13.update(17, (() => {
    positionVar_12 = 18;
    val x_114 = timeVar;
    val x_115 = x_114.+(1);
    timeVar = x_115
  }));
  data_13.update(18, (() => positionVar_12 = 17));
  data_13.update(19, (() => {
    val x_116 = true.`unary_!`;
    if (x_116)
      {
        val x_117 = resetData_1.remove(0);
        val x_121 = x_117.find(((x_118: scala.Tuple2[scala.Tuple2[scala.Int, scala.Int], scala.Int]) => {
          val x_119 = x_118._1;
          val x_120 = scala.Tuple2.apply[scala.Int, scala.Int](-1, -1);
          x_119.==(x_120)
        }));
        val x_122 = x_121.get;
        val x_123 = x_122._2;
        positionVar_12 = x_123
      }
    else
      ()
  }));
  data_13
}).apply();
  
  override def run_until(until_125: scala.Int) : meta.deep.runtime.Actor =  {
    while ({
      val x_126 = timeVar;
      val x_127 = x_126.<=(until_125);
      x_127.&&({
        val x_128 = positionVar_12;
        val x_129 = commands_124.length;
        x_128.<(x_129)
      })
    }) 
      {
        val x_130 = positionVar_12;
        val x_131 = commands_124.apply(x_130);
        x_131.apply()
      }
    ;
    this
  }
}

class Consensus extends ConsensusTrait