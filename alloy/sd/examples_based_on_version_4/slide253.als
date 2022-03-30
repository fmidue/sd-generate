module diagram // name: "", (irrelevant) label: 86
open uml_state_diagram

one sig N_1 extends RegionsStates{}{
  contains = R_1_1 + R_1_2
}
one sig R_1_1 extends Regions{}{
  name = Name1 // "Stunden"
  contains = S_1_1 + N_1_1_1 + N_1_1_2 + N_1_1_3 + N_1_1_4 + N_1_1_5 + N_1_1_6 + N_1_1_7 + N_1_1_8 + N_1_1_9 + N_1_1_10 + N_1_1_11 + N_1_1_12 + N_1_1_13 + N_1_1_14 + N_1_1_15 + N_1_1_16 + N_1_1_17 + N_1_1_18 + N_1_1_19 + N_1_1_20 + N_1_1_21 + N_1_1_22 + N_1_1_23 + N_1_1_24
}
one sig S_1_1 extends StartNodes{}
one sig S_1_1Flow extends Flows{}{
  from = S_1_1
  label = EmptyTrigger
  to = N_1_1_1
}
one sig N_1_1_1 extends NormalStates{}{
  name = Name2 // "0"
}
one sig N_1_1_2 extends NormalStates{}{
  name = Name3 // "1"
}
one sig N_1_1_3 extends NormalStates{}{
  name = Name4 // "2"
}
one sig N_1_1_4 extends NormalStates{}{
  name = Name5 // "3"
}
one sig N_1_1_5 extends NormalStates{}{
  name = Name6 // "4"
}
one sig N_1_1_6 extends NormalStates{}{
  name = Name7 // "5"
}
one sig N_1_1_7 extends NormalStates{}{
  name = Name8 // "6"
}
one sig N_1_1_8 extends NormalStates{}{
  name = Name9 // "7"
}
one sig N_1_1_9 extends NormalStates{}{
  name = Name10 // "8"
}
one sig N_1_1_10 extends NormalStates{}{
  name = Name11 // "9"
}
one sig N_1_1_11 extends NormalStates{}{
  name = Name12 // "10"
}
one sig N_1_1_12 extends NormalStates{}{
  name = Name13 // "11"
}
one sig N_1_1_13 extends NormalStates{}{
  name = Name14 // "12"
}
one sig N_1_1_14 extends NormalStates{}{
  name = Name15 // "13"
}
one sig N_1_1_15 extends NormalStates{}{
  name = Name16 // "14"
}
one sig N_1_1_16 extends NormalStates{}{
  name = Name17 // "15"
}
one sig N_1_1_17 extends NormalStates{}{
  name = Name18 // "16"
}
one sig N_1_1_18 extends NormalStates{}{
  name = Name19 // "17"
}
one sig N_1_1_19 extends NormalStates{}{
  name = Name20 // "18"
}
one sig N_1_1_20 extends NormalStates{}{
  name = Name21 // "19"
}
one sig N_1_1_21 extends NormalStates{}{
  name = Name22 // "20"
}
one sig N_1_1_22 extends NormalStates{}{
  name = Name23 // "21"
}
one sig N_1_1_23 extends NormalStates{}{
  name = Name24 // "22"
}
one sig N_1_1_24 extends NormalStates{}{
  name = Name25 // "23"
}


one sig R_1_2 extends Regions{}{
  name = Name26 // "Minuten"
  contains = S_1_2 + N_1_2_1 + N_1_2_2 + N_1_2_3 + N_1_2_4 + N_1_2_5 + N_1_2_6 + N_1_2_7 + N_1_2_8 + N_1_2_9 + N_1_2_10 + N_1_2_11 + N_1_2_12 + N_1_2_13 + N_1_2_14 + N_1_2_15 + N_1_2_16 + N_1_2_17 + N_1_2_18 + N_1_2_19 + N_1_2_20 + N_1_2_21 + N_1_2_22 + N_1_2_23 + N_1_2_24 + N_1_2_25 + N_1_2_26 + N_1_2_27 + N_1_2_28 + N_1_2_29 + N_1_2_30 + N_1_2_31 + N_1_2_32 + N_1_2_33 + N_1_2_34 + N_1_2_35 + N_1_2_36 + N_1_2_37 + N_1_2_38 + N_1_2_39 + N_1_2_40 + N_1_2_41 + N_1_2_42 + N_1_2_43 + N_1_2_44 + N_1_2_45 + N_1_2_46 + N_1_2_47 + N_1_2_48 + N_1_2_49 + N_1_2_50 + N_1_2_51 + N_1_2_52 + N_1_2_53 + N_1_2_54 + N_1_2_55 + N_1_2_56 + N_1_2_57 + N_1_2_58 + N_1_2_59 + N_1_2_60
}
one sig S_1_2 extends StartNodes{}
one sig S_1_2Flow extends Flows{}{
  from = S_1_2
  label = EmptyTrigger
  to = N_1_2_1
}
one sig N_1_2_1 extends NormalStates{}{
  name = Name2 // "0"
}
one sig N_1_2_2 extends NormalStates{}{
  name = Name3 // "1"
}
one sig N_1_2_3 extends NormalStates{}{
  name = Name4 // "2"
}
one sig N_1_2_4 extends NormalStates{}{
  name = Name5 // "3"
}
one sig N_1_2_5 extends NormalStates{}{
  name = Name6 // "4"
}
one sig N_1_2_6 extends NormalStates{}{
  name = Name7 // "5"
}
one sig N_1_2_7 extends NormalStates{}{
  name = Name8 // "6"
}
one sig N_1_2_8 extends NormalStates{}{
  name = Name9 // "7"
}
one sig N_1_2_9 extends NormalStates{}{
  name = Name10 // "8"
}
one sig N_1_2_10 extends NormalStates{}{
  name = Name11 // "9"
}
one sig N_1_2_11 extends NormalStates{}{
  name = Name12 // "10"
}
one sig N_1_2_12 extends NormalStates{}{
  name = Name13 // "11"
}
one sig N_1_2_13 extends NormalStates{}{
  name = Name14 // "12"
}
one sig N_1_2_14 extends NormalStates{}{
  name = Name15 // "13"
}
one sig N_1_2_15 extends NormalStates{}{
  name = Name16 // "14"
}
one sig N_1_2_16 extends NormalStates{}{
  name = Name17 // "15"
}
one sig N_1_2_17 extends NormalStates{}{
  name = Name18 // "16"
}
one sig N_1_2_18 extends NormalStates{}{
  name = Name19 // "17"
}
one sig N_1_2_19 extends NormalStates{}{
  name = Name20 // "18"
}
one sig N_1_2_20 extends NormalStates{}{
  name = Name21 // "19"
}
one sig N_1_2_21 extends NormalStates{}{
  name = Name22 // "20"
}
one sig N_1_2_22 extends NormalStates{}{
  name = Name23 // "21"
}
one sig N_1_2_23 extends NormalStates{}{
  name = Name24 // "22"
}
one sig N_1_2_24 extends NormalStates{}{
  name = Name25 // "23"
}
one sig N_1_2_25 extends NormalStates{}{
  name = Name27 // "24"
}
one sig N_1_2_26 extends NormalStates{}{
  name = Name28 // "25"
}
one sig N_1_2_27 extends NormalStates{}{
  name = Name29 // "26"
}
one sig N_1_2_28 extends NormalStates{}{
  name = Name30 // "27"
}
one sig N_1_2_29 extends NormalStates{}{
  name = Name31 // "28"
}
one sig N_1_2_30 extends NormalStates{}{
  name = Name32 // "29"
}
one sig N_1_2_31 extends NormalStates{}{
  name = Name33 // "30"
}
one sig N_1_2_32 extends NormalStates{}{
  name = Name34 // "31"
}
one sig N_1_2_33 extends NormalStates{}{
  name = Name35 // "32"
}
one sig N_1_2_34 extends NormalStates{}{
  name = Name36 // "33"
}
one sig N_1_2_35 extends NormalStates{}{
  name = Name37 // "34"
}
one sig N_1_2_36 extends NormalStates{}{
  name = Name38 // "35"
}
one sig N_1_2_37 extends NormalStates{}{
  name = Name39 // "36"
}
one sig N_1_2_38 extends NormalStates{}{
  name = Name40 // "37"
}
one sig N_1_2_39 extends NormalStates{}{
  name = Name41 // "38"
}
one sig N_1_2_40 extends NormalStates{}{
  name = Name42 // "39"
}
one sig N_1_2_41 extends NormalStates{}{
  name = Name43 // "40"
}
one sig N_1_2_42 extends NormalStates{}{
  name = Name44 // "41"
}
one sig N_1_2_43 extends NormalStates{}{
  name = Name45 // "42"
}
one sig N_1_2_44 extends NormalStates{}{
  name = Name46 // "43"
}
one sig N_1_2_45 extends NormalStates{}{
  name = Name47 // "44"
}
one sig N_1_2_46 extends NormalStates{}{
  name = Name48 // "45"
}
one sig N_1_2_47 extends NormalStates{}{
  name = Name49 // "46"
}
one sig N_1_2_48 extends NormalStates{}{
  name = Name50 // "47"
}
one sig N_1_2_49 extends NormalStates{}{
  name = Name51 // "48"
}
one sig N_1_2_50 extends NormalStates{}{
  name = Name52 // "49"
}
one sig N_1_2_51 extends NormalStates{}{
  name = Name53 // "50"
}
one sig N_1_2_52 extends NormalStates{}{
  name = Name54 // "51"
}
one sig N_1_2_53 extends NormalStates{}{
  name = Name55 // "52"
}
one sig N_1_2_54 extends NormalStates{}{
  name = Name56 // "53"
}
one sig N_1_2_55 extends NormalStates{}{
  name = Name57 // "54"
}
one sig N_1_2_56 extends NormalStates{}{
  name = Name58 // "55"
}
one sig N_1_2_57 extends NormalStates{}{
  name = Name59 // "56"
}
one sig N_1_2_58 extends NormalStates{}{
  name = Name60 // "57"
}
one sig N_1_2_59 extends NormalStates{}{
  name = Name61 // "58"
}
one sig N_1_2_60 extends NormalStates{}{
  name = Name62 // "59"
}





one sig Connection1 extends Flows{}{
  from = N_1_1_1
  label = T1 // "h"
  to = N_1_1_2
}
one sig Connection2 extends Flows{}{
  from = N_1_1_2
  label = T1 // "h"
  to = N_1_1_3
}
one sig Connection3 extends Flows{}{
  from = N_1_1_3
  label = T1 // "h"
  to = N_1_1_4
}
one sig Connection4 extends Flows{}{
  from = N_1_1_4
  label = T1 // "h"
  to = N_1_1_5
}
one sig Connection5 extends Flows{}{
  from = N_1_1_5
  label = T1 // "h"
  to = N_1_1_6
}
one sig Connection6 extends Flows{}{
  from = N_1_1_6
  label = T1 // "h"
  to = N_1_1_7
}
one sig Connection7 extends Flows{}{
  from = N_1_1_7
  label = T1 // "h"
  to = N_1_1_8
}
one sig Connection8 extends Flows{}{
  from = N_1_1_8
  label = T1 // "h"
  to = N_1_1_9
}
one sig Connection9 extends Flows{}{
  from = N_1_1_9
  label = T1 // "h"
  to = N_1_1_10
}
one sig Connection10 extends Flows{}{
  from = N_1_1_10
  label = T1 // "h"
  to = N_1_1_11
}
one sig Connection11 extends Flows{}{
  from = N_1_1_11
  label = T1 // "h"
  to = N_1_1_12
}
one sig Connection12 extends Flows{}{
  from = N_1_1_12
  label = T1 // "h"
  to = N_1_1_13
}
one sig Connection13 extends Flows{}{
  from = N_1_1_13
  label = T1 // "h"
  to = N_1_1_14
}
one sig Connection14 extends Flows{}{
  from = N_1_1_14
  label = T1 // "h"
  to = N_1_1_15
}
one sig Connection15 extends Flows{}{
  from = N_1_1_15
  label = T1 // "h"
  to = N_1_1_16
}
one sig Connection16 extends Flows{}{
  from = N_1_1_16
  label = T1 // "h"
  to = N_1_1_17
}
one sig Connection17 extends Flows{}{
  from = N_1_1_17
  label = T1 // "h"
  to = N_1_1_18
}
one sig Connection18 extends Flows{}{
  from = N_1_1_18
  label = T1 // "h"
  to = N_1_1_19
}
one sig Connection19 extends Flows{}{
  from = N_1_1_19
  label = T1 // "h"
  to = N_1_1_20
}
one sig Connection20 extends Flows{}{
  from = N_1_1_20
  label = T1 // "h"
  to = N_1_1_21
}
one sig Connection21 extends Flows{}{
  from = N_1_1_21
  label = T1 // "h"
  to = N_1_1_22
}
one sig Connection22 extends Flows{}{
  from = N_1_1_22
  label = T1 // "h"
  to = N_1_1_23
}
one sig Connection23 extends Flows{}{
  from = N_1_1_23
  label = T1 // "h"
  to = N_1_1_24
}
one sig Connection24 extends Flows{}{
  from = N_1_1_24
  label = T1 // "h"
  to = N_1_1_1
}
one sig Connection25 extends Flows{}{
  from = N_1_2_1
  label = T2 // "after(1min)"
  to = N_1_2_2
}
one sig Connection26 extends Flows{}{
  from = N_1_2_2
  label = T2 // "after(1min)"
  to = N_1_2_3
}
one sig Connection27 extends Flows{}{
  from = N_1_2_3
  label = T2 // "after(1min)"
  to = N_1_2_4
}
one sig Connection28 extends Flows{}{
  from = N_1_2_4
  label = T2 // "after(1min)"
  to = N_1_2_5
}
one sig Connection29 extends Flows{}{
  from = N_1_2_5
  label = T2 // "after(1min)"
  to = N_1_2_6
}
one sig Connection30 extends Flows{}{
  from = N_1_2_6
  label = T2 // "after(1min)"
  to = N_1_2_7
}
one sig Connection31 extends Flows{}{
  from = N_1_2_7
  label = T2 // "after(1min)"
  to = N_1_2_8
}
one sig Connection32 extends Flows{}{
  from = N_1_2_8
  label = T2 // "after(1min)"
  to = N_1_2_9
}
one sig Connection33 extends Flows{}{
  from = N_1_2_9
  label = T2 // "after(1min)"
  to = N_1_2_10
}
one sig Connection34 extends Flows{}{
  from = N_1_2_10
  label = T2 // "after(1min)"
  to = N_1_2_11
}
one sig Connection35 extends Flows{}{
  from = N_1_2_11
  label = T2 // "after(1min)"
  to = N_1_2_12
}
one sig Connection36 extends Flows{}{
  from = N_1_2_12
  label = T2 // "after(1min)"
  to = N_1_2_13
}
one sig Connection37 extends Flows{}{
  from = N_1_2_13
  label = T2 // "after(1min)"
  to = N_1_2_14
}
one sig Connection38 extends Flows{}{
  from = N_1_2_14
  label = T2 // "after(1min)"
  to = N_1_2_15
}
one sig Connection39 extends Flows{}{
  from = N_1_2_15
  label = T2 // "after(1min)"
  to = N_1_2_16
}
one sig Connection40 extends Flows{}{
  from = N_1_2_16
  label = T2 // "after(1min)"
  to = N_1_2_17
}
one sig Connection41 extends Flows{}{
  from = N_1_2_17
  label = T2 // "after(1min)"
  to = N_1_2_18
}
one sig Connection42 extends Flows{}{
  from = N_1_2_18
  label = T2 // "after(1min)"
  to = N_1_2_19
}
one sig Connection43 extends Flows{}{
  from = N_1_2_19
  label = T2 // "after(1min)"
  to = N_1_2_20
}
one sig Connection44 extends Flows{}{
  from = N_1_2_20
  label = T2 // "after(1min)"
  to = N_1_2_21
}
one sig Connection45 extends Flows{}{
  from = N_1_2_21
  label = T2 // "after(1min)"
  to = N_1_2_22
}
one sig Connection46 extends Flows{}{
  from = N_1_2_22
  label = T2 // "after(1min)"
  to = N_1_2_23
}
one sig Connection47 extends Flows{}{
  from = N_1_2_23
  label = T2 // "after(1min)"
  to = N_1_2_24
}
one sig Connection48 extends Flows{}{
  from = N_1_2_24
  label = T2 // "after(1min)"
  to = N_1_2_25
}
one sig Connection49 extends Flows{}{
  from = N_1_2_25
  label = T2 // "after(1min)"
  to = N_1_2_26
}
one sig Connection50 extends Flows{}{
  from = N_1_2_26
  label = T2 // "after(1min)"
  to = N_1_2_27
}
one sig Connection51 extends Flows{}{
  from = N_1_2_27
  label = T2 // "after(1min)"
  to = N_1_2_28
}
one sig Connection52 extends Flows{}{
  from = N_1_2_28
  label = T2 // "after(1min)"
  to = N_1_2_29
}
one sig Connection53 extends Flows{}{
  from = N_1_2_29
  label = T2 // "after(1min)"
  to = N_1_2_30
}
one sig Connection54 extends Flows{}{
  from = N_1_2_30
  label = T2 // "after(1min)"
  to = N_1_2_31
}
one sig Connection55 extends Flows{}{
  from = N_1_2_31
  label = T2 // "after(1min)"
  to = N_1_2_32
}
one sig Connection56 extends Flows{}{
  from = N_1_2_32
  label = T2 // "after(1min)"
  to = N_1_2_33
}
one sig Connection57 extends Flows{}{
  from = N_1_2_33
  label = T2 // "after(1min)"
  to = N_1_2_34
}
one sig Connection58 extends Flows{}{
  from = N_1_2_34
  label = T2 // "after(1min)"
  to = N_1_2_35
}
one sig Connection59 extends Flows{}{
  from = N_1_2_35
  label = T2 // "after(1min)"
  to = N_1_2_36
}
one sig Connection60 extends Flows{}{
  from = N_1_2_36
  label = T2 // "after(1min)"
  to = N_1_2_37
}
one sig Connection61 extends Flows{}{
  from = N_1_2_37
  label = T2 // "after(1min)"
  to = N_1_2_38
}
one sig Connection62 extends Flows{}{
  from = N_1_2_38
  label = T2 // "after(1min)"
  to = N_1_2_39
}
one sig Connection63 extends Flows{}{
  from = N_1_2_39
  label = T2 // "after(1min)"
  to = N_1_2_40
}
one sig Connection64 extends Flows{}{
  from = N_1_2_40
  label = T2 // "after(1min)"
  to = N_1_2_41
}
one sig Connection65 extends Flows{}{
  from = N_1_2_41
  label = T2 // "after(1min)"
  to = N_1_2_42
}
one sig Connection66 extends Flows{}{
  from = N_1_2_42
  label = T2 // "after(1min)"
  to = N_1_2_43
}
one sig Connection67 extends Flows{}{
  from = N_1_2_43
  label = T2 // "after(1min)"
  to = N_1_2_44
}
one sig Connection68 extends Flows{}{
  from = N_1_2_44
  label = T2 // "after(1min)"
  to = N_1_2_45
}
one sig Connection69 extends Flows{}{
  from = N_1_2_45
  label = T2 // "after(1min)"
  to = N_1_2_46
}
one sig Connection70 extends Flows{}{
  from = N_1_2_46
  label = T2 // "after(1min)"
  to = N_1_2_47
}
one sig Connection71 extends Flows{}{
  from = N_1_2_47
  label = T2 // "after(1min)"
  to = N_1_2_48
}
one sig Connection72 extends Flows{}{
  from = N_1_2_48
  label = T2 // "after(1min)"
  to = N_1_2_49
}
one sig Connection73 extends Flows{}{
  from = N_1_2_49
  label = T2 // "after(1min)"
  to = N_1_2_50
}
one sig Connection74 extends Flows{}{
  from = N_1_2_50
  label = T2 // "after(1min)"
  to = N_1_2_51
}
one sig Connection75 extends Flows{}{
  from = N_1_2_51
  label = T2 // "after(1min)"
  to = N_1_2_52
}
one sig Connection76 extends Flows{}{
  from = N_1_2_52
  label = T2 // "after(1min)"
  to = N_1_2_53
}
one sig Connection77 extends Flows{}{
  from = N_1_2_53
  label = T2 // "after(1min)"
  to = N_1_2_54
}
one sig Connection78 extends Flows{}{
  from = N_1_2_54
  label = T2 // "after(1min)"
  to = N_1_2_55
}
one sig Connection79 extends Flows{}{
  from = N_1_2_55
  label = T2 // "after(1min)"
  to = N_1_2_56
}
one sig Connection80 extends Flows{}{
  from = N_1_2_56
  label = T2 // "after(1min)"
  to = N_1_2_57
}
one sig Connection81 extends Flows{}{
  from = N_1_2_57
  label = T2 // "after(1min)"
  to = N_1_2_58
}
one sig Connection82 extends Flows{}{
  from = N_1_2_58
  label = T2 // "after(1min)"
  to = N_1_2_59
}
one sig Connection83 extends Flows{}{
  from = N_1_2_59
  label = T2 // "after(1min)"
  to = N_1_2_60
}
one sig Connection84 extends Flows{}{
  from = N_1_2_60
  label = T2 // "after(1min)"
  to = N_1_2_1
}

one sig Name1 extends ComponentNames{}
one sig Name2 extends ComponentNames{}
one sig Name3 extends ComponentNames{}
one sig Name4 extends ComponentNames{}
one sig Name5 extends ComponentNames{}
one sig Name6 extends ComponentNames{}
one sig Name7 extends ComponentNames{}
one sig Name8 extends ComponentNames{}
one sig Name9 extends ComponentNames{}
one sig Name10 extends ComponentNames{}
one sig Name11 extends ComponentNames{}
one sig Name12 extends ComponentNames{}
one sig Name13 extends ComponentNames{}
one sig Name14 extends ComponentNames{}
one sig Name15 extends ComponentNames{}
one sig Name16 extends ComponentNames{}
one sig Name17 extends ComponentNames{}
one sig Name18 extends ComponentNames{}
one sig Name19 extends ComponentNames{}
one sig Name20 extends ComponentNames{}
one sig Name21 extends ComponentNames{}
one sig Name22 extends ComponentNames{}
one sig Name23 extends ComponentNames{}
one sig Name24 extends ComponentNames{}
one sig Name25 extends ComponentNames{}
one sig Name26 extends ComponentNames{}
one sig Name27 extends ComponentNames{}
one sig Name28 extends ComponentNames{}
one sig Name29 extends ComponentNames{}
one sig Name30 extends ComponentNames{}
one sig Name31 extends ComponentNames{}
one sig Name32 extends ComponentNames{}
one sig Name33 extends ComponentNames{}
one sig Name34 extends ComponentNames{}
one sig Name35 extends ComponentNames{}
one sig Name36 extends ComponentNames{}
one sig Name37 extends ComponentNames{}
one sig Name38 extends ComponentNames{}
one sig Name39 extends ComponentNames{}
one sig Name40 extends ComponentNames{}
one sig Name41 extends ComponentNames{}
one sig Name42 extends ComponentNames{}
one sig Name43 extends ComponentNames{}
one sig Name44 extends ComponentNames{}
one sig Name45 extends ComponentNames{}
one sig Name46 extends ComponentNames{}
one sig Name47 extends ComponentNames{}
one sig Name48 extends ComponentNames{}
one sig Name49 extends ComponentNames{}
one sig Name50 extends ComponentNames{}
one sig Name51 extends ComponentNames{}
one sig Name52 extends ComponentNames{}
one sig Name53 extends ComponentNames{}
one sig Name54 extends ComponentNames{}
one sig Name55 extends ComponentNames{}
one sig Name56 extends ComponentNames{}
one sig Name57 extends ComponentNames{}
one sig Name58 extends ComponentNames{}
one sig Name59 extends ComponentNames{}
one sig Name60 extends ComponentNames{}
one sig Name61 extends ComponentNames{}
one sig Name62 extends ComponentNames{}

one sig T1 extends TriggerNames{}
one sig T2 extends TriggerNames{}

run {} for
  0 EndNodes,
  0 HierarchicalStates,
  0 DeepHistoryNodes,
  0 ShallowHistoryNodes,
  0 ForkNodes,
  0 JoinNodes,
  86 ProtoFlows, exactly 86 Flows // concerning ProtoFlows, a temporary hack for manual scope setting
